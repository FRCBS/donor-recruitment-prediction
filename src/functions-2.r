# 2025-07-20 way forward
# compute.csm below is fixed for counties are the grouping variable
# Should really utilise the computations in getGroupEstimates
# The data could more easily be derived similarly as et.test

prd.cumulative2density = function(pah) {
        dpah=(rbind(pah[,1:3],0*pah[1,1:3])-rbind(0*pah[1,1:3],pah[,1:3]))[1:nrow(pah),] # nämä siis saa vähentämällä
        return(cbind(dpah,pah[,4:ncol(pah)]))
}

# 2025-09-14 must add the computations for relative activity here (summing errors)
predictDonations2 = function(rv,prd.start=2000,prd.len=55,model='cdon.a-x',multiplier=NULL,trend.years=5,cumulative=TRUE) {
	if (cumulative) {
		rv$prdct[is.na(rv$prdct$year0),'year0']=0
		tmp=by(rv$prdct,rv$prdct[,c('year0','phase','rw')],FUN=prd.cumulative2density)
		pred.d=do.call(rbind,tmp[lengths(tmp)!=0])
		pred.d[pred.d$year0==0,'year0']=NA
	} else {
		pred.d=rv$prdct
		for (col.index in 1:4) 
			pred.d[,col.index]=pmax(pred.d[,col.index],0)
	}

	prd.years=data.frame(prd.year=prd.start:(prd.start+prd.len)) # nb! hard-coded parameters

	# This applies to cases where only one year is selected and the model is estimated
	# with no year0 term (multi.year==FALSE)
	if (all(pred.d$year0.lo==pred.d$year0.hi)) {
		model=sub('.year0','',model)
	}

	prd.data=pred.d[pred.d$phase==model,]

	agedist.local=rv$agedist

	sizes.data = rv$data[rv$data$ord==1,c('rw','year0','n2')] 

	# 2025-08-19 These should probably be moved out and parameterised
	# so that they can (with parameters other than x preset)
	# by used as arguments.
	# repeatLastYear: add multiplier, could be 0.99 or 0 (to show the effect of existing donors)
	repeatLastYear = function(x) {
		year.max=max(x$year0)
		n2.max=x[x$year0==year.max,'n2']
		year0.v=(year.max+1):max(prd.years$prd.year)
		mult.v=multiplier^seq(1,by=1,along=year0.v)
		df.new=data.frame(rw=max(x$rw),year0=year0.v,n2.max*mult.v)
		return(rbind(x,df.new))
	}

	estimateLinearTrend = function(x) {
		m=lm(n2~year0,data=x[(nrow(x)-trend.years):nrow(x),])
		year.max=max(x$year0)
		new.data=data.frame(year0=(year.max+1):max(prd.years$prd.year))
		ndf=predict(m,newdata=new.data)
		df.new=data.frame(rw=max(x$rw),year0=new.data$year0,n2=ndf)
		
		return(rbind(x,df.new))
	}

	forecast.n2.fun=estimateLinearTrend
	if (!is.null(multiplier)) {
		forecast.n2.fun=repeatLastYear
	}

	# nb! should probably cut the sizes as well earlier, not just for predictions (skip.last)
	tmp=by(sizes.data,sizes.data[,c('rw')],FUN=forecast.n2.fun)
	sizes.data=array2DF(tmp)[,-1]

	pph=prd.data %>%
		filter(lwr>upr)
	if (nrow(pph) > 0) {
		print(pph)
	} else 
		print('nothing to report')

bsAssign('prd.data')

	if (all(is.na(prd.data$year0))) {
		pah=cross_join(prd.years,sizes.data) %>%
			inner_join(prd.data[,colnames(prd.data) != 'year0'],
				join_by(rw,between(x$year0,y$year0.lo,y$year0.hi))) %>%
			mutate(year=year0+x-1) %>%
			filter(year==prd.year) %>%
			mutate(est=n2*fit,est.lo=n2*lwr,est.hi=n2*upr,error=n2*error)
	} else {
		# prd.years ~ yksinkertainen lista ennustettavista vuosista
		# sizes.data ~ tunnetut uusien luovuttajien lukumäärät (jatkettu ennustevälille)
		pah.has.year0=cross_join(prd.years,sizes.data) %>%
			inner_join(prd.data,join_by(rw,year0)) %>%
			mutate(year=year0+x-1) %>%
			filter(year==prd.year) %>%
			mutate(est=n2*fit,est.lo=n2*lwr,est.hi=n2*upr,error=n2*error)

		pah=cross_join(prd.years,sizes.data) %>%
			left_join(prd.data,join_by(rw,year0)) %>%
			filter(is.na(fit)) %>%
			dplyr::select(prd.year,rw,year0,n2) %>%
			# copied part: roughly the same as above
			inner_join(pred.d[pred.d$phase==sub('.year0','',model),colnames(prd.data) != 'year0'],
				join_by(rw,between(x$year0,y$year0.lo,y$year0.hi))) %>%
			mutate(year=year0+x-1) %>%
			filter(year==prd.year) %>%
			mutate(est=n2*fit,est.lo=n2*lwr,est.hi=n2*upr,error=n2*error) %>%
			rbind(pah.has.year0)
	}

	return(pah)

	tmp=by(agedist.local,agedist.local[,c('rw')],FUN=function(x) {
		x$cumulative=cumsum(x$density)
		x
	})
	agedist.cum=array2DF(tmp,simplify=TRUE)[,-1]

	pah2=pah %>%
		# 66: jos täytti vuonna year0=2010 esim. 50 vuotta, ja x rivillä on esim. 15, kyse on 
		# vuoden 2024 ennusteesta, jolloin täyttää 64 vuotta. Mukaan otetaan se vuosi, jolloin täyttää 65
		# vuotta mutta ei enää seuraavaa. Eli tämä lienee oikein.
		mutate(age0.max=66-x) %>%
		inner_join(agedist.cum[,c('rw','age','cumulative')],join_by(rw,x$age0.max==y$age)) %>%
		mutate(est.trnc=est*cumulative,est.trnc.lo=est.lo*cumulative,est.trnc.hi=est.hi*cumulative)

	return(pah2)
}

# TODO
# year0 should be added to the spec
# similarly for year to use maybe reference year +/- offset
getGroupEstimates2 = function(et,spec,lwd=3,plot='orig',years.ahead=55,try.nls=FALSE,year0.ord=3:100,skip.last=TRUE,
	agedist=NULL,save.years.from.end=0) {
	# reference.years.local=reference.years
	# reference.years.local$year=reference.years.local$year+year.offset
	et.test = et %>%
		filter(!is.na(cdon),!is.na(don)) %>%
		group_by(!!!syms(setdiff(colnames(et),setdiff(dim.cols,spec$dim.keep)))) %>%
		summarise(cdon=sum(n*cdon),don=sum(n*don),.groups='drop') %>%
		group_by(!!!syms(c(spec$dim.keep,'year0','ord','year'))) %>% # nb! year0 is here
		summarise(n2=sum(n),cdon=sum(cdon)/n2,don=sum(don)/n2,.groups='drop')

	grps=data.frame(unique(et.test[,spec$dim.keep]))

	res.all=list
	simple.data=NULL
	sp.data=NULL
	sp.coeff=NULL
	sp.dftot=NULL
	sp.m.year0=list()

	sm.extract = function(m,phase) {
		sm=data.frame(summary(m)$coeff)
		sm['r.squared','Estimate']=summary(m)$r.squared
		data.frame(parameter=rownames(sm),sm,phase=phase)
	}

	m.predict = function(m,phase,power.term=NULL) {
		years=NULL
		if ('year0' %in% names(m$model)) {
			# summary(m)
			coeff=summary(m)$coeff
			years=sub('year0','',grep('year0',rownames(coeff),value=TRUE))
			# as.integer(sub('year0','',grep('year0',rownames(coeff),value=TRUE)))
		}

		# frml=as.character(m$call)[2]
		new.data=data.frame(x=1:55)
		if ('sq.x' %in% names(m$model))
			new.data$sq.x=new.data$x^2
		if ('sqrt.x' %in% names(m$model))
			new.data$sqrt.x=new.data$x^0.5
		if ('x.pwr' %in% names(m$model))
			new.data$x.pwr=new.data$x^power.term
		if ('log(x)' %in% names(m$model))
			new.data[['log(x)']]=log(new.data$x)
		if ('x1' %in% names(m$model)) {
			new.data$x1=0
			new.data$x1[new.data$x==1]=1
		}

		year0.col=NULL
		if (!is.null(years)) {
			new.data=cross_join(new.data,data.frame(year0=years))
			year0.col=ncol(new.data)
		}

		esq=data.frame(predict(m,newdata=new.data,interval='confidence')) %>%
			cbind(new.data)
		
		# esq=esq[,1:3]
		if (!is.null(year0.col) && !'year0' %in% colnames(esq)) {
			esq=cbind(esq,year0=as.integer(new.data$year0))
		} else if (!is.null(year0.col) && 'year0' %in% colnames(esq)) {
			esq$year0=as.integer(esq$year0)
		} else {
			esq$year0=as.integer(NA)
		}
		esq$x=new.data[,1]
		esq$phase=phase

# esq0=esq
bsAssign('m')
bsAssign('esq')

		#  fit       lwr      upr year0  x   phase
		mm=m$model
		if ('year0' %in% colnames(mm))
			mm$year0=as.integer(as.character(mm$year0))
# str(esq)
# str(mm)

		# colnames(mm)=sub('(log.x.)|(x.pwr)','x',colnames(mm))
		join.cols=grep('(log.x.)|(x.pwr)|(sqrt.x)|(sq.x)|(^x$)',colnames(esq),value=TRUE) %>%
			intersect(colnames(mm))
		nc=ncol(esq)
		esq=left_join(esq,mm,join_by(!!!syms(c(join.cols,if('year0' %in% names(mm)) 'year0' else NULL))))

		# columns to have fit, lwr, upr (1:3), year0.col
		colnames(esq)[nc+1]='actual'
		esq=esq[,c(1:3,nc+1,which(colnames(esq)=='year0'),which(colnames(esq)=='x'),which(colnames(esq)=='phase'))]

		if (grepl('^log',names(m$model[1]))) {
			# print(paste(phase,'exp applied'))
			esq[,1:4]=exp(esq[,1:4])
		} else if (!is.null(power.term) && !('x.pwr' %in% names(m$model))) {
			esq[,1:4]=esq[,1:4]^power.term
		}

		esq$error=esq$fit-esq$actual

cat(colnames(esq))
cat('\n')

		return(esq)
	}

	dftot=NULL
	for (rw in 1:nrow(grps)) {
		data = et.test

		print(paste('*************',grps[rw,],collapse=', '))

		# Add the group info to the data set
		for (cn in 1:ncol(grps)) {
			grp.name=colnames(grps)[cn]
			grp.value=data.frame(grps)[rw,cn]
			data=data[data[[grp.name]]==grp.value,]
		}

		data$rw=rw
		export.data=data

		if (nrow(data) < 3)
			next

		data$x=data$ord
		data$sqrt.x=sqrt(data$x)
		data$year0.int=data$year0
		
		year.start = min(data$year0.int)

		if (save.years.from.end >= 0) {
			data = data %>% 
				filter(year0.int>=year.start+min(year0.ord)-1,year0.int<=year.start+max(year0.ord)-1)
		} else if (save.years.from.end < 0) {
			data = data %>% 
				filter(year0.int-save.years.from.end>max(year0.int))
			year.start=min(data$year0.int) # needed to override the computation
				# esim. max == 10; viisi vuotta mukaan -> 6:10; 6+5=11
		}

		# esim. max.ord==9, 5 jäätävä -> 4; nelonen on vielä ok
		# if (dim(data)[1] == 0 || (max(data$ord) - save.years.from.end < min(year0.ord) && length(year0.ord) == 1)) {
		if (dim(data)[1] == 0 || (save.years.from.end > 0 && max(data$year) - save.years.from.end < min(data$year0.int) && length(year0.ord) == 1)) {
			# print('skipping due to lack of sufficient data')
			# print(paste(length(unique(data$year0)),year0.ord))
			next
		}

		# compute the max year to cut the final years out
		if (skip.last && FALSE) {
			# nb! This is not working: country is not considered
			# Should of course be group based, but no need to fix here
			data.ord.max = data %>%
				group_by(year0.int) %>%
				summarise(ord.max=max(ord),.groups='drop') 

			data = data %>%
				inner_join(data.ord.max,join_by(year0.int,x$ord<y$ord.max)) %>%
				dplyr::select(-ord.max)
		}

		data$year0=as.factor(data$year0)

		# The model with a common intercept/multiplier
		m=lm(log(cdon)~log(x),data=data)
		# sm=summary(m)
		power.term=summary(m)$coeff[2,1]
		intercept=summary(m)$coeff[1,1]
		b=exp(intercept)

		# initialise the coeff-structure
		coeff = sm.extract(m,'log-log')
		prdct = m.predict(m,'log-log')

		# experimental: use don to estimate the models
		m=lm(log(don)~log(x),data=data)
		power.term.d=summary(m)$coeff[2,1]
		intercept=summary(m)$coeff[1,1]
		bd=exp(intercept)
		coeff = rbind(coeff,sm.extract(m,'log(don)-log'))
		prdct = rbind(prdct,m.predict(m,'log(don)-log'))

		data$x.pwr=data$x^power.term.d
		m=lm(don~x.pwr,data=data)
		coeff = rbind(coeff,sm.extract(m,'don-x.a'))
		prdct = rbind(prdct,m.predict(m,'don-x.a',power.term=power.term.d))

		# 2025-08-25
		m=lm(don~x.pwr,data=data[data$x>1,])
		coeff = rbind(coeff,sm.extract(m,'don-x.a+x1'))
		prdct0=m.predict(m,'don-x.a+x1',power.term=power.term.d)
		prdct0[1,1:3]=prdct0[1,1:3]+as.numeric((data[1,'don']-prdct0[1,1])) # this doesn't work too well with multiple years
		# m=lm(don~x.pwr,data=data)
		# prdct2=m.predict(m,'don-x.a+x1',power.term=power.term.d)

		if (FALSE) {
			# comparing the squared error between the alternate models
			print(paste('cut','original','cut-original'))
			print(paste(
				sum((prdct0$fit[1:length(data$don)]-data$don)^2),
				sum((prdct2$fit[1:length(data$don)]-data$don)^2),
				sum((prdct0$fit[1:length(data$don)]-data$don)^2)-
				sum((prdct2$fit[1:length(data$don)]-data$don)^2)))
		}

		prdct = rbind(prdct,prdct0)

		data2=data

		# the power-conversion happens here
		data2$y=data2$cdon^(1/power.term)

		plot.FALSE=FALSE
		if (plot.FALSE) {
			resolution=150
			filename=paste0('../fig/',paste(grps[rw,],collapse='-'),'-fund-plot.png')
			png(filename,res=resolution,width=9*resolution,height=7*resolution)
			plot(y~x,data=data2,main=paste0('b=',b,', a=',power.term,', y50=',round(b*50^power.term,1)))
			for(yr in unique(data2$year0)) {
				data3=data2[data2$year0==yr,]
				lines(data3$x,data3$y,col=as.integer(yr))
			}
			dev.off()
		}

		# 2025-08-17 model with x converted to a power with the exponent found previously
		data$x.pwr=data$x^power.term
		m=lm(cdon~x.pwr,data=data)
		coeff = rbind(coeff,sm.extract(m,'cdon-x.a'))
		prdct = rbind(prdct,m.predict(m,'cdon-x.a',power.term=power.term)) # power.term converts the predictions

		# model with year0-based slopes (b_i)
		multi.year = (length(year0.ord) > 1)
		if (multi.year) {
			m=lm(log(cdon)~0+year0+log(x),data=data)
			sm=summary(m)
			# print(sm)
			coeff = rbind(coeff,sm.extract(m,'log-year0-log'))
			prdct = rbind(prdct,m.predict(m,'log-year0-log'))

			m=lm(cdon~x.pwr+year0+0,data=data)
			coeff = rbind(coeff,sm.extract(m,'cdon-x.a+year0'))
			prdct = rbind(prdct,m.predict(m,'cdon-x.a+year0',power.term=power.term)) # power.term converts the predictions

			### 2025-08-22 newly added things
			data$x.pwr=data$x^power.term.d
			m=lm(log(don)~0+year0+log(x),data=data)
			sm=summary(m)
			coeff = rbind(coeff,sm.extract(m,'log(don)-year0-log'))
			prdct = rbind(prdct,m.predict(m,'log(don)-year0-log'))

			m=lm(don~x.pwr+year0+0,data=data)
			coeff = rbind(coeff,sm.extract(m,'don-x.a+year0'))
			prdct = rbind(prdct,m.predict(m,'don-x.a+year0',power.term=power.term.d)) # power.term converts the predictions
			data$x.pwr=data$x^power.term

bsAssign('data')
bsAssign('power.term.d')
			data$x1=0
			data$x1[data$ord==1]=1
			data$x.pwr=data$x^power.term.d
			m=lm(don~x.pwr+year0+0+x1,data=data)
			coeff = rbind(coeff,sm.extract(m,'don-x.a+year0+x1'))
			prdct = rbind(prdct,m.predict(m,'don-x.a+year0+x1',power.term=power.term.d))
			###

			# linear model with converted response variable
			data2$sq.x=data2$x^2
			m=lm(y~x+year0+0,data=data2)
			sm=summary(m)
			# print(sm)
			coeff = rbind(coeff,sm.extract(m,'cdon.a-x-year0'))
			prdct = rbind(prdct,m.predict(m,'cdon.a-x-year0',power.term=power.term))
		}

		# linear model converted response variable, no year0
		data2$sq.x=data2$x^2
		m=lm(y~x,data=data2)
		sm=summary(m)
		coeff = rbind(coeff,sm.extract(m,'cdon.a-x'))
		prdct = rbind(prdct,m.predict(m,'cdon.a-x',power.term=power.term))

		# model with the added squared regressor
		data2$sq.x=data2$x^2
		m=lm(y~x+sq.x,data=data2)
		sm=summary(m)
		# print(sm)
		coeff = rbind(coeff,sm.extract(m,'cdon.a-x-sq.x'))
		prdct = rbind(prdct,m.predict(m,'cdon.a-x-sq.x',power.term=power.term))

		if (multi.year) {
			# the x+sqrt.x model (kind of old-fashioned already)
			frml.char = paste0('cdon~x+sqrt.x',if('year0' %in% colnames(data) && length(unique(data$year0)) > 1) '+year0+0' else '')
			m=lm(formula(frml.char),data=data)
			sm=summary(m)
			coeff = rbind(coeff,sm.extract(m,'cdon-sqrt-x-year0'))
			# prdct = rbind(prdct,m.predict(m,'cdon-sqrt-x-year0'))
		}
		
		# the x+sqrt.x model (kind of old-fashioned already)
		frml.char = paste0('cdon~x+sqrt.x')
		m=lm(formula(frml.char),data=data)
		sm=summary(m)
		coeff = rbind(coeff,sm.extract(m,'cdon-sqrt-x'))
		prdct = rbind(prdct,m.predict(m,'cdon-sqrt-x'))

		# the surely outdated exponential model
		if (try.nls) {
			m.nls=NULL
			# data$cdon0=data$cdon-1
			# data$x0=data$x-1
			try(m.nls <- nls(cdon~c1*exp(-lambda*(x))+c0,data=data,
                     start=list(c1=-(max(data$cdon)-min(data$cdon)),lambda=0.2,c0=0)))
			if (!is.null(m.nls))
				1 # print(summary(m.nls))
		}

		# copied from the newRegression procedure
		# This seems to work, and it is worth using the year-specific trajectories when
		# predicting each year
		new.data=expand.grid(year0=unique(data$year0),x=1:years.ahead)
		new.data$sqrt.x=sqrt(new.data$x)
		esq=predict(m,newdata=new.data,interval='confidence')
		dftot=cbind(new.data,esq,rw=rw)

		# estimate a trend for the year-based constant terms - not yet used
		# todo: these should be included in the results as well

		for (ci in colnames(grps)) {
			data[[ci]] = grps[rw,ci]
		}

		col.start=spec$colours[[grps[rw,spec$col.dim]]]
		colfunc <- colorRampPalette(c(col.start, "white"))
		colfun=colfunc(20)
		col0=colfun[rw] # 2025-08-06 -> index

		if (plot.FALSE) {
			resolution=150
			filename=paste0('../fig/',paste(grps[rw,],collapse='-'),'-sqrt-multiple-new.png')
			# if (plot=='all') {
				png(filename,res=resolution,width=9*resolution,height=7*resolution)
				plot(x=NULL,ylim=c(0,50),xlim=c(0,55))

			# plot.terms=c('lambda','y0')
			plot.terms=c('x','sqrt.x') # These are kind of obsolete
			if ('orig' %in% plot) {
				points(coeff[plot.terms[1],'Estimate'],coeff[plot.terms[2],'Estimate'],
					col=col0,lwd=lwd,pch=spec$pch(grps[rw,spec$pch.dim]))
			}
			if ('alt' %in% plot) {
				points(x.half,y.max,
					col=col0,lwd=lwd,pch=spec$pch(grps[rw,spec$pch.dim]))
			}

			dev.off()
		}
		
		coeff$rw=rw
		prdct$rw=rw

		# Add these to enable joining with forecasted years
		prdct$year0.lo=year.start+min(year0.ord)-1
		prdct$year0.hi=year.start+max(year0.ord)-1

		if (is.null(sp.data)) {
			sp.data=export.data 
			sp.coeff=coeff
			sp.dftot=dftot
			sp.prdct=prdct
			et.data=data
		} else {
			sp.data=rbind(sp.data,export.data)
			sp.coeff=rbind(sp.coeff,coeff)
			sp.dftot=rbind(sp.dftot,dftot)
			sp.prdct=rbind(sp.prdct,prdct)
			et.data=rbind(et.data,data)
		}
	}

	if (is.null(dftot))
		# All the groups were skipped
		return(NULL)

	agedist.local=NULL
	if (!is.null(agedist) && 'country' %in% colnames(et.test)) {
		# cols2=c('age',setdiff(colnames(et),c('don','cdon')))
		agedist.int = et %>%
			filter(et$ord==1) %>%
			dplyr::select(!!!syms(c('country','Name','n','year0',spec$dim.keep))) %>% # (-cdon,-don) 
			# filter(!is.na(cdon),!is.na(don)) %>%
			inner_join(agedist,join_by(country,x$Name==y$name),relationship = "many-to-many") %>%
			mutate(n.age=n*density)

		dens= agedist.int %>%
			group_by(!!!syms(c('age',spec$dim.keep))) %>%
			summarise(n.age2=sum(n.age),.groups='drop')
		agedist.local = agedist.int %>%
			group_by(!!!syms(c(spec$dim.keep))) %>%
			summarise(n2=sum(n.age),.groups='drop') %>%
			inner_join(dens,join_by(!!!syms(spec$dim.keep))) %>% 
			mutate(density=n.age2/n2) %>%
			inner_join(data.frame(grps,rw=1:nrow(grps)),join_by(!!!syms(spec$dim.keep)))
	}

str(sp.prdct)

	return(list(et.data=et.data,data=sp.data,grps=data.frame(grps,rw=1:nrow(grps)),
		coeff=sp.coeff,fit=dftot,m.year0=sp.m.year0,prdct=sp.prdct,
		agedist=agedist.local))
}

# 2025-09-14
plotCountrySummaries = function(et,rv,estimates,spec,xlim=c(2000,2035),ylim=c(0,2e3)) {
	actual.don = et %>%
		filter(!is.na(cdon),!is.na(don)) %>%
		group_by(!!!syms(c('year',spec$dim.keep))) %>%
		summarise(don2=sum(n*don),.groups='drop') %>%
		arrange(country,year)

	new.donors = et %>%
		filter(!is.na(cdon),!is.na(don),ord==1) %>%
		group_by(!!!syms(c('year',spec$dim.keep))) %>%
		summarise(n2=sum(n),.groups='drop') %>%
		arrange(country,year) 

	# ... as a data frame
	df.ad=data.frame(pivot_wider(actual.don,values_from='don2',names_from=c('country'))) %>% arrange(year)
	rownames(df.ad)=as.character(df.ad$year)

	pah=estimates %>% 
		group_by(rw,prd.year) %>%
		summarise(est=sum(est),est.lo=sum(est.lo),est.hi=sum(est.hi),error=sum(error,na.rm=TRUE),.groups='drop') %>% 
		rename(year=prd.year) %>%
		inner_join(grps,join_by(rw)) # nb! global variable

	df3=data.frame(pivot_wider(pah[,!colnames(pah) %in% c('rw','est.lo','est.hi','error')],values_from='est',names_from=c('country'))) %>%
		arrange(year)
	df3.lo=data.frame(pivot_wider(pah[,!colnames(pah) %in% c('rw','est','est.hi','error')],values_from='est.lo',names_from=c('country'))) %>%
		arrange(year)
	df3.hi=data.frame(pivot_wider(pah[,!colnames(pah) %in% c('rw','est.lo','est','error')],values_from='est.hi',names_from=c('country'))) %>%
		arrange(year)
	df3.err=data.frame(pivot_wider(pah[,!colnames(pah) %in% c('rw','est.lo','est.hi','est')],values_from='error',names_from=c('country'))) %>%
		arrange(year)
bsAssign('pah')
bsAssign('df3.err')
	for (rw in rv$grps$rw) {
		cn=rv$grps$country[rw]

		wh.min=min(which(!is.na(df3[[cn]])))
		y0=df3[[cn]][wh.min]
		y.max=max(df3.hi[[cn]],na.rm=TRUE)

		filename=paste0('../submit/summary-',cn,'.png')
		resolution=150
		png(filename,res=resolution,width=9*resolution,height=7*resolution)
		par(mar=c(2.2,4.1,0.5,0.6)) # no space at the top
		# y.max=max(df3.hi[[cn]]/1000,na.rm=TRUE)
		plot(x=NULL,xlim=xlim,ylim=c(0,100*y.max/y0),
			xlab='year',ylab='indexed values()'
		)

		lines(df3$year,100*df3[[cn]]/y0,type='l',lwd=2,lty='solid',col=colfun(cn)) 
		lines(df3.lo$year,100*df3.lo[[cn]]/y0,type='l',lwd=1,lty='dotted',col=colfun(cn))
		lines(df3.hi$year,100*df3.hi[[cn]]/y0,type='l',lwd=1,lty='dotted',col=colfun(cn))

		# actual donations
		points(df.ad$year,100*df.ad[[cn]]/y0,type='p',col=colfun(cn))

		# number of new donations
		nd=new.donors %>% filter(country==cn)
		rect(nd$year-0.3,0,nd$year+0.3,100*nd$n2/y0,col=colfun(cn))

		# activity scales/errors
		# nb! for some reason, there are values 
		wh=which(abs(df3.err[[cn]])>1 & df3.err$year <= max(nd$year))
		points(df3.err$year[wh],50+50*df3.err[[cn]][wh]/y0,col=colfun(cn),pch=2)
		lines(min(df3.err$year[wh]),50,min(df3.err$year[50]),50,lty='dotted')

		dev.off()
	}
}

### --- ennustekäyrät: tuotetaan kuvat vertailua varten
plotPredictions = function(rv,xlim=c(1,55),ylim=c(1,25),models=c('cdon.a-x','cdon-x.a')) {
	for (rw in rv$grps$rw) {
		filename=paste0('../fig/',paste(grps[rw,],collapse='-'),'-predictions.png')
		resolution=150
		png(filename,res=resolution,width=9*resolution,height=7*resolution)
		plot(x=NULL,xlim=xlim,ylim=ylim,
			xlab='years since first donation',ylab='cumulative donations per donor',
			main=paste(rv$grps[rw,setdiff(colnames(rv$grps),'rw')]))
		phases=unique(rv$prdct$phase)
		
		if ('all' %in% models)
			models=phases[!grepl('year0',phases)]

		col=0
		for (ph in phases) {
			col=col+1
			if (!ph %in% models)
				next

			data5=rv$prdct[rv$prdct$phase==ph&rv$prdct$rw==rw,]
			lines(data5$x,data5$fit,lty='solid',lwd=3,col=col)
			lines(data5$x,data5$lwr,lty='dashed',lwd=1.5,col=col)
			lines(data5$x,data5$upr,lty='dashed',lwd=1.5,col=col)
		}

		# observed data
		rw0=rw
		pt.data=rv$et.data %>% filter(rw==rw0)
		points(pt.data$ord,pt.data$cdon)

		legend(x='topleft',fill=1:length(phases),legend=phases)
		dev.off()
	}
}

plotEstimatesVsActual = function(et,estimates,spec,filename=NULL,resolution=150,main=NULL,lty='solid',xlim=NULL,ylim=NULL,grps=NULL) {
	# actual donations
	actual.don = et %>%
		filter(!is.na(cdon),!is.na(don)) %>%
		group_by(!!!syms(c('year',spec$dim.keep))) %>%
		summarise(don2=sum(n*don),.groups='drop') %>%
		arrange(country,year)

	# ... as a data frame
	df.ad=data.frame(pivot_wider(actual.don,values_from='don2',names_from=c('country'))) %>% arrange(year)
	rownames(df.ad)=as.character(df.ad$year)

	pah=estimates %>% 
		group_by(rw,prd.year) %>%
		summarise(est=sum(est),est.lo=sum(est.lo),est.hi=sum(est.hi),.groups='drop') %>% 
		rename(year=prd.year) %>%
		inner_join(grps,join_by(rw)) # nb! global variable

	df3=data.frame(pivot_wider(pah[,!colnames(pah) %in% c('rw','est.lo','est.hi')],values_from='est',names_from=c('country'))) %>%
		arrange(year)
	df3.lo=data.frame(pivot_wider(pah[,!colnames(pah) %in% c('rw','est','est.hi')],values_from='est.lo',names_from=c('country'))) %>%
		arrange(year)
	df3.hi=data.frame(pivot_wider(pah[,!colnames(pah) %in% c('rw','est.lo','est')],values_from='est.hi',names_from=c('country'))) %>%
		arrange(year)

	# plot predictions
	if (!is.null(filename)) {
		png.res=150
		png(filename,width=9*png.res,height=7*png.res,res=png.res)
			# par('mar')
			# default: 5.1  4.1  4.1   2.1
			#      bottom, left, top, right
		if (is.null(main)) {
			par(mar=c(2.2,4.1,0.5,0.6))
		}
	}

	# TODO There is significant hard-coding here: nc multiplier and using countries instead of groups
	# But groups in general would be hard to plot
	if (is.null(xlim)) 
		xlim=c(2000,2035)
	if (is.null(ylim)) 
		ylim=c(0,2e3)
	
	plot(NULL,xlim=xlim,ylim=ylim,ylab='number of donations (in 1,000)',xlab='year',main=main)
	cns=grep('..',colnames(df3),value=TRUE)
	for (cn in cns) {
		multiplier = (if (cn=='nc') 100 else 1) # nb! Navarre hard-coded thing
		lines(df3$year,multiplier*df3[[cn]]/1000,type='l',lwd=2,lty=lty,col=colfun(cn)) # col=unlist(sapply(colnames(df3),FUN=colfun)))
		lines(df3.lo$year,multiplier*df3.lo[[cn]]/1000,type='l',lwd=1,lty='dotted',col=colfun(cn)) # col=unlist(sapply(colnames(df3),FUN=colfun)))
		lines(df3.hi$year,multiplier*df3.hi[[cn]]/1000,type='l',lwd=1,lty='dotted',col=colfun(cn)) # col=unlist(sapply(colnames(df3),FUN=colfun)))
		points(df.ad$year,multiplier*df.ad[[cn]]/1000,type='p',col=colfun(cn))
	}

	legend('topleft',fill=unlist(sapply(grps$country,FUN=colfun)),legend=sapply(sort(names(spec$colours)),FUN=function(x) cn.names[[x]]))

	if (!is.null(filename))
		dev.off()
}

plotCoeffData=function(data,spec,grps,phase,dparam,vfun,error.bars=TRUE) {
	data0=data[data$phase==phase,]

	if (!'year0' %in% colnames(data0)) {
		data0$year0=0
	}

	wh.x=which(grepl(dparam[1],data0$parameter))
	wh.y=which(grepl(dparam[2],data0$parameter))
	est.x=data0[wh.x,'Estimate']
	est.y=data0[wh.y,'Estimate']

	lo.x=est.x+qnorm(0.025)*data0[wh.x,'Std..Error']
	hi.x=est.x+qnorm(0.975)*data0[wh.x,'Std..Error']
	if (!is.na(vfun[1])) {
		est.x=vfun[[1]](est.x)
		lo.x=vfun[[1]](lo.x)
		hi.x=vfun[[1]](hi.x)
	}

	lo.y=est.y+qnorm(0.025)*data0[wh.y,'Std..Error']
	hi.y=est.y+qnorm(0.975)*data0[wh.y,'Std..Error']
	if (!is.na(vfun[2])) {
		est.y=vfun[[2]](est.y)
		lo.y=vfun[[2]](lo.y)
		hi.y=vfun[[2]](hi.y)
	}

	df.x=data.frame(est.x,lo.x,hi.x,rw=data0[wh.x,'rw'],year0=data0[wh.x,'year0'])
	df.y=data.frame(est.y,lo.y,hi.y,rw=data0[wh.y,'rw'],year0=data0[wh.y,'year0'])

	df=full_join(df.x,df.y,join_by(rw,year0)) %>%
		inner_join(grps,join_by(rw))

	df = df %>%
		arrange(rw,desc(year0))

	# lines connecting the points (if multiple points/years are available)
	vd=by(df,df[,c('rw')],FUN=function(x) {
			if (dim(x)[1] <= 1) {
				return(NULL)
			}
			lines(x$est.x,x$est.y,lty='dashed',col=spec$colours[[unique(x[[spec$col.dim]])]])
			points(x$est.x[1],x$est.y[1])
			return(x)
		})

	col=unlist(spec$colours[df[,spec$col.dim]])
	pch=sapply(df[,spec$pch.dim],spec$pch ) 
	points(df$est.x,df$est.y,col=col,pch=pch)

	vd=by(df,df[,c('rw')],FUN=function(x) {
			if (dim(x)[1] <= 1) {
				return(NULL)
			}

			col.start=spec$colours[[unique(x[[spec$col.dim]])]]
			colfunc.a = colorRampPalette(c(col.start, "white"))
			colfun.a = colfunc.a(nrow(x)+3)
			lines(x$est.x,x$est.y,lty='dashed',col=)
			points(x$est.x,x$est.y,col=colfun.a[1:nrow(x)],pch=pch)
			return(x)
		})


	if (!error.bars)
		return()

	arrows(df$est.x,df$lo.y,df$est.x,df$hi.y,length=0.05,angle=90,code=3,col=col)
	arrows(df$lo.x,df$est.y,df$hi.x,df$est.y,length=0.05,angle=90,code=3,col=col)

	# x ~ exponent
	# y ~ multiplier
	u=50
	df$lo.u=df$lo.y*u^df$lo.x
	df$hi.u=df$hi.y*u^df$hi.x
	df$est.u=df$est.y*u^df$est.x

	return(df)
}

# The remaining parts are legacy functions that may still be useful
# Obsolete functions deleted in dev branch on 2025-08-22 (cut locally to removed-parts.r)
plotDistibutionMatrix = function(distm,diff=FALSE,skip.years=1,main='') {
	if (!diff) {
		colfunc <- colorRampPalette(c("white","green"))
		cut.lower	= min(distm,na.rm=TRUE)-0.1
		cut.higher = max(distm,na.rm=TRUE)+0.1
	} else {
		colfunc <- colorRampPalette(c('red','white','blue'))
		abs.extreme = max(abs(distm),na.rm=TRUE) + 0.1
		cut.lower = -abs.extreme
		cut.higher = abs.extreme
	}
	
	# fill.cut=cut(value,seq(min(distm,na.rm=TRUE)-0.1,max(distm,na.rm=TRUE)+0.1,length.out=50))
	fill.seq = seq(cut.lower, cut.higher, length.out = 51)
	# indexing: the skip.years first lines are removed to counter for the effect of pre-2000 donors
	p=ggplot(melt(t(distm[(1+skip.years):dim(distm)[1],])), aes(Var1, Var2, fill=value, label=round(value, 1))) + # cut(value,fill.seq)
		geom_tile()
	if (diff)
		p = p + scale_fill_gradient2(midpoint = 0, low = "red", mid = "white", high = "blue") 
	else
		p = p + scale_fill_gradient2(low = "white", high='green')
	p = p + geom_text(color="black") + 
		guides(fill="none") +
		xlab('time since first donation') +
		ylab('year of first donation') + 
		labs(title=main) + 
		scale_y_reverse()
	print(p)
}

plotDelayBySex = function(activity.stats.sex,country) {
	# plot(activity.stats$ord,activity.stats$delay,
	#			xlab='number of previous donations',ylab='delay until next donation',type='l',lwd=3,main=country)
	
	plot(NULL,xlab='number of previous donations',ylab='delay until next donation',
			 type='n',lwd=3,xlim=c(0,max(activity.stats.sex$ord)),ylim=c(0,max(activity.stats.sex$delay)),main=country)
	if (is.factor(activity.stats.sex$Sex)) {
		sexes = levels(activity.stats.sex$Sex)
	} else
		sexes = unique(activity.stats.sex$Sex)
	for (i in 1:length(sexes)) {
		sx = sexes[i]
		data = activity.stats.sex[activity.stats.sex$Sex==sx,]
		lines(data$ord,data$delay,col=i+1,lwd=2)
	}
	legend('bottomright',legend=sexes,fill=1+1:length(sexes))
}

plotPropBySex = function(activity.stats.sex,country) {
	# plot(activity.stats$ord,activity.stats$prop*100,col='blue',xlim=c(0,150),ylim=c(0,100*max(activity.stats$prop)),
	# type='l',lwd=3,xlab='number of previous donations',ylab='probability of next donation (%)',main=country)
	
	plot(NULL,xlab='number of previous donations',ylab='probability of next donation (%)',
			 type='n',lwd=3,xlim=c(0,max(activity.stats.sex$ord)),ylim=c(0,max(activity.stats.sex$prop)),main=country)
	
	if (is.factor(activity.stats.sex$Sex)) {
		sexes = levels(activity.stats.sex$Sex)
	} else
		sexes = unique(activity.stats.sex$Sex)
	for (i in 1:length(sexes)) {
		sx = sexes[i]
		data = activity.stats.sex[activity.stats.sex$Sex==sx,]
		lines(data$ord,data$prop,col=i+1,lwd=2)
	}
	legend('bottomright',legend=sexes,fill=1+1:length(sexes))
}

# utility functions
bsAssign = function(name) {
	obj = get(name,envir=parent.frame())
	assign(name,obj,.GlobalEnv)
}

## ----produce-explore-data,echo=FALSE------------------------------------------
# This is a copy from blood-donor-recruitment-prediction.R
# Unfortunately does not work for matrices
cumulativeToDensity = function(dist) {
	return((c(dist,0)-c(0,dist))[1:length(dist)])
}

cdm2pdm = function(distm) {
	pdm = cbind(cdm)-cbind(data.frame(rep(0,nrow(cdm))),cdm[,-ncol(cdm)])
	colnames(pdm)=colnames(distm)
	return(pdm)
}
