# This could be utilised in getGroupEstimates2: no more like hand stitched
# mspecs=expand.grid(family=c('low','pwr'),dep=c('don','cdon'),year0=c('','year0'),x1=c('','x1'))

prd.cumulative2density = function(pah) {
        dpah=(rbind(pah[,1:3],0*pah[1,1:3])-rbind(0*pah[1,1:3],pah[,1:3]))[1:nrow(pah),] # nämä siis saa vähentämällä
        return(cbind(dpah,pah[,4:ncol(pah)]))
}

predictDonations2 = function(rv,prd.start=1993,prd.len=55+7,model='cdon.a-x',multiplier=NULL,trend.years=5,cumulative=TRUE) {
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

	prd.years=data.frame(prd.year=prd.start:(prd.start+prd.len)) 

	# This applies to cases where only one year is selected and the model is estimated
	# with no year0 term (multi.year==FALSE)
	if (all(pred.d$year0.lo==pred.d$year0.hi)) {
		# print('swithing model')
		# print(model)
		model=sub('0\\+year0\\+','',model)
		# print(model)
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

	tmp=by(sizes.data,sizes.data[,c('rw')],FUN=forecast.n2.fun)
	sizes.data=array2DF(tmp)[,-1]

	pph=prd.data %>%
		filter(lwr>upr)
	if (nrow(pph) > 0) {
		print(pph)
		error('tangled confidence intervals')
	} 

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
			inner_join(pred.d[pred.d$phase==sub('0\\+year0\\+','',model),colnames(prd.data) != 'year0'],
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

getGroupEstimates2 = function(et,spec,lwd=3,plot='orig',years.ahead=55,try.nls=FALSE,year0.ord=3:100,skip.last=TRUE,
	agedist=NULL,save.years.from.end=0,save.years.overlap=0,filter.threshold=0.3) {
	et.test = et %>%
		filter(!is.na(cdon),!is.na(don)) %>%
		group_by(!!!syms(setdiff(colnames(et),setdiff(dim.cols,spec$dim.keep)))) %>%
		summarise(cdon=sum(n*cdon),don=sum(n*don),.groups='drop') %>%
		group_by(!!!syms(c(spec$dim.keep,'year0','ord','year'))) %>% # 
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

	m.predict = function(m,phase,power.term=NULL,recursive=0) {
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

		mm=m$model
		if ('year0' %in% colnames(mm))
			mm$year0=as.integer(as.character(mm$year0))

		mm$rownr=1:nrow(mm)

		# colnames(mm)=sub('(log.x.)|(x.pwr)','x',colnames(mm))
		join.cols=grep('(log.x.)|(x.pwr)|(sqrt.x)|(sq.x)|(^x$)',colnames(esq),value=TRUE) %>%
			intersect(colnames(mm))

		# in case year0 is not present but there are multiple years, there may actually be
		# multiple so let's average over them here (weights would be nice)
		st0=by(mm,mm[,join.cols],FUN=function(x) {
				rv=x[1,]
				rv[1,1]=mean(rv[,1])
				return(rv)
			})
		mm=do.call(rbind,st0)

		nc=ncol(esq)
		esq=left_join(esq,mm,join_by(!!!syms(c(join.cols,if('year0' %in% names(mm)) 'year0' else NULL))))

		# columns to have fit, lwr, upr (1:3), year0.col
		colnames(esq)[nc+1]='actual'
		esq=esq[,c(1:3,nc+1,which(colnames(esq)=='year0'),which(colnames(esq)=='x'),which(colnames(esq)=='phase'),which(colnames(esq)=='rownr'))]

		if (grepl('^log',names(m$model[1]))) {
			# print(paste(phase,'exp applied'))
			esq[,1:4]=exp(esq[,1:4])
		} else if (!is.null(power.term) && !('x.pwr' %in% names(m$model))) {
			esq[,1:4]=esq[,1:4]^power.term
		}

		esq$error=esq$fit-esq$actual
		esq$abs.errorperc=abs(esq$error)/esq$fit

		wh = which(!is.na(esq$abs.errorperc)&esq$abs.errorperc>filter.threshold)
		if (length(wh) > 0 && !is.null(filter.threshold)) {
			print(paste('found outliers ',phase,length(wh),recursive,paste(wh,collapse=' ')),sep=' ')
			subset=setdiff(1:nrow(mm),unique(esq$rownr[wh]))
			data=m$model[subset,]
			phase0=gsub('log\\(([^)]+)\\)',paste0('`log(\\1)`'),phase)
			m2=NULL
			m2=try(lm(formula(phase0),data=data))
			if (!is.null(names(m2))) {
				esq0 = m.predict(m2,phase,power.term=power.term,recursive=recursive+1)
				return(esq0)
			} else {
				print('reestimation failed')
			}
		}

		return(esq)
	}

	dftot=NULL
	for (rw in 1:nrow(grps)) {
		data = et.test

		# print(paste('*************',grps[rw,],collapse=', '))

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

		data$x1=0
		data$x1[data$ord==1]=1
		
		year.start = min(data$year0.int)

		cn.overlap=save.years.overlap
		if ('country' %in% colnames(grps) && save.years.overlap != 0) {
			cn.overlap=min(max.overlap[[grps[rw,'country']]],save.years.overlap) # -save.years.from.end+
		}

		if (save.years.from.end >= 0) {
			data = data %>% 
				filter(year0.int>=year.start+min(year0.ord)-1,year0.int<=year.start+max(year0.ord)-1)
		} else if (save.years.from.end < 0) {
			data = data %>% 
				filter(year0.int-save.years.from.end+cn.overlap>max(year0.int))
			year.start=min(data$year0.int) # needed to override the computation
			# e.g. max == 10; save 5 years -> 6:10; 6+5=11
		}

		if (dim(data)[1] == 0 || 
			(
				save.years.from.end > 0 && 
				max(data$year) - save.years.from.end < min(data$year0.int) 
				&& length(year0.ord) == 1)
			) {
			next
		}

		data$year0=as.factor(data$year0)

		# The model with a common intercept/multiplier
		phase='log(cdon)~log(x)'
		m=lm(formula(phase),data=data)
		power.term=summary(m)$coeff[2,1]
		intercept=summary(m)$coeff[1,1]
		b=exp(intercept)

		# initialise the coeff-structure
		coeff = sm.extract(m,phase)
		prdct = m.predict(m,phase)

		# 2025-09-22 new
		phase='log(cdon)~log(x)+x1'
		m=lm(formula(phase),data=data)
		coeff = rbind(coeff,sm.extract(m,phase))
		prdct = rbind(prdct,m.predict(m,phase))

		phase='log(don)~log(x)'
		m=lm(formula(phase),data=data)
		coeff = rbind(coeff,sm.extract(m,phase))
		prdct = rbind(prdct,m.predict(m,phase))

		phase='log(don)~log(x)+x1'
		m=lm(formula(phase),data=data)
		coeff = rbind(coeff,sm.extract(m,phase))
		prdct = rbind(prdct,m.predict(m,phase))

		power.term.d=summary(m)$coeff[2,1]


		phase='don~x.pwr'
		data$x.pwr=data$x^power.term.d
		m=lm(formula(phase),data=data)
		coeff = rbind(coeff,sm.extract(m,phase))
		prdct = rbind(prdct,m.predict(m,phase,power.term=power.term.d))

		# 2025-09-22 new
		phase='cdon~x.pwr+x1'
		data$x.pwr=data$x^power.term
		m=lm(formula(phase),data=data)
		coeff = rbind(coeff,sm.extract(m,phase))
		prdct = rbind(prdct,m.predict(m,phase,power.term=power.term.d))

		phase='don~x.pwr+x1'
		data$x.pwr=data$x^power.term.d
		m=lm(formula(phase),data=data)
		coeff = rbind(coeff,sm.extract(m,phase))
		prdct = rbind(prdct,m.predict(m,phase,power.term=power.term.d))

		data2=data

		# the power-conversion happens here
		data2$y=data2$cdon^(1/power.term)

		# 2025-08-17 model with x converted to a power with the exponent found previously
		data$x.pwr=data$x^power.term
		phase='cdon~x.pwr'
		m=lm(formula(phase),data=data)
		coeff = rbind(coeff,sm.extract(m,phase))
		prdct = rbind(prdct,m.predict(m,phase,power.term=power.term)) # power.term converts the predictions

		# model with year0-based slopes (b_i)
		multi.year = (length(year0.ord) > 1)
		if (multi.year) {
			phase='log(cdon)~log(x)+0+year0'
			m=lm(formula(phase),data=data)
			sm=summary(m)
			coeff = rbind(coeff,sm.extract(m,phase))
			prdct = rbind(prdct,m.predict(m,phase))

			phase='log(don)~log(x)+0+year0'
			m=lm(formula(phase),data=data)
			sm=summary(m)
			coeff = rbind(coeff,sm.extract(m,phase))
			prdct = rbind(prdct,m.predict(m,phase))

bsAssign('data')
print(power.term.d)
			phase='don~x.pwr+0+year0+x1'
			data$x.pwr=data$x^power.term.d
			m=lm(formula(phase),data=data)
			sm=summary(m)
			coeff = rbind(coeff,sm.extract(m,phase))
			prdct = rbind(prdct,m.predict(m,phase,power.term=power.term.d))

			# 2025-09-22 new 
			phase='log(cdon)~log(x)+0+year0+x1'
			data$x.pwr=data$x^power.term
			m=lm(formula(phase),data=data)
			sm=summary(m)
			coeff = rbind(coeff,sm.extract(m,phase))
			prdct = rbind(prdct,m.predict(m,phase))

			# 2025-09-22 new 
			phase='log(don)~log(x)+0+year0+x1'
			m=lm(formula(phase),data=data)
			sm=summary(m)
			coeff = rbind(coeff,sm.extract(m,phase))
			prdct = rbind(prdct,m.predict(m,phase))

			phase='cdon~x.pwr+0+year0'
			data$x.pwr=data$x^power.term
			m=lm(formula(phase),data=data)
			coeff = rbind(coeff,sm.extract(m,phase))
			prdct = rbind(prdct,m.predict(m,phase,power.term=power.term)) # power.term converts the predictions

			phase='cdon~x.pwr+0+year0+x1'
			data$x.pwr=data$x^power.term
			m=lm(formula(phase),data=data)
			coeff = rbind(coeff,sm.extract(m,phase))
			prdct = rbind(prdct,m.predict(m,phase,power.term=power.term)) # power.term converts the predictions

			### 2025-08-22 newly added things
			data$x.pwr=data$x^power.term.d

			phase='don~x.pwr+0+year0'
			data$x.pwr=data$x^power.term.d
			m=lm(formula(phase),data=data)
			coeff = rbind(coeff,sm.extract(m,phase))
			prdct = rbind(prdct,m.predict(m,phase,power.term=power.term.d)) # power.term converts the predictions
			# data$x.pwr=data$x^power.term

			# linear model with converted response variable
			data2$sq.x=data2$x^2
			phase='y~x+0+year0'
			m=lm(formula(phase),data=data2)
			sm=summary(m)
			# print(sm)
			coeff = rbind(coeff,sm.extract(m,phase))
			prdct = rbind(prdct,m.predict(m,phase,power.term=power.term))
		}

		# linear model converted response variable, no year0
		data2$sq.x=data2$x^2
		phase='y~x'
		m=lm(formula(phase),data=data2)
		sm=summary(m)
		coeff = rbind(coeff,sm.extract(m,phase))
		prdct = rbind(prdct,m.predict(m,phase,power.term=power.term))

		# model with the added squared regressor
		data2$sq.x=data2$x^2
		phase='y~x+sq.x'
		m=lm(formula(phase),data=data2)
		sm=summary(m)
		# print(sm)
		coeff = rbind(coeff,sm.extract(m,phase))
		prdct = rbind(prdct,m.predict(m,phase,power.term=power.term))

		if (multi.year) {
			# the x+sqrt.x model (kind of old-fashioned already)
			phase = paste0('cdon~x+sqrt.x',if('year0' %in% colnames(data) && length(unique(data$year0)) > 1) '+0+year0' else '')
			m=lm(formula(phase),data=data)
			sm=summary(m)
			coeff = rbind(coeff,sm.extract(m,phase))
			# prdct = rbind(prdct,m.predict(m,phase))
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
		col0=colfun[rw]
		
		coeff$rw=rw
		prdct$rw=rw

		# Add these to enable joining with forecasted years
		# This is where to adjust for the extended/overlapping period
		# 2025-09-15
# bsAssign('year.start')
# bsAssign('save.years.overlap')
# bsAssign('save.years.overlap')
# min0=min(year0.ord)
# bsAssign('min0')

		# -save.years.from.end
		prdct$year0.lo=year.start+min(year0.ord)+cn.overlap-1
		prdct$year0.hi=year.start+max(year0.ord)+cn.overlap-1

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

	return(list(et.data=et.data,data=sp.data,grps=data.frame(grps,rw=1:nrow(grps)),
		coeff=sp.coeff,fit=dftot,m.year0=sp.m.year0,prdct=sp.prdct,
		agedist=agedist.local))
}

# 2025-09-14
plotCountrySummaries = function(et,grps,estimates,spec,coeff.data,xlim=c(1993,2035),ylim=c(0,2e3),include.errors=FALSE) {
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

	processEstimates = function(estimates) {
		pah=estimates %>% 
			group_by(rw,prd.year) %>%
			summarise(
				avg.age=sum(est*x,na.rm=TRUE)/sum(est,na.rm=TRUE),
				est=sum(est),
				lo=sum(est.lo),
				hi=sum(est.hi),
				error=sum(error,na.rm=TRUE),
				.groups='drop') %>% 
			rename(year=prd.year) %>%
			inner_join(grps,join_by(rw))

		extr=c('est','lo','hi','error','avg.age')
		tmp=lapply(extr,FUN=function(x) {
			pivot_wider(pah[,c('country','year',x)],values_from=x,names_from='country') %>% arrange(year)
			})
		names(tmp)=extr
		tmp$lty='dashed'

		return(tmp)
	}

	if (!is.data.frame(estimates)) {
		estimates.list=lapply(estimates,processEstimates)
	} else 
		estimates.list=list(sole=processEstimates(estimates))
	estimates.list[[1]]$lty='solid'

	coeff.data = coeff.data %>% arrange(rw,year0)
	coeff.data$cdon50=with(coeff.data,est.y*50^est.x)
	coeff.data$cdon50.lo=with(coeff.data,lo.y*50^lo.x)
	coeff.data$cdon50.hi=with(coeff.data,hi.y*50^hi.x)

	estimates0=estimates.list[[1]]
	for (rw in grps$rw) {
		cn=grps$country[rw]

		y.max=max(estimates0$hi[[cn]],na.rm=TRUE)

		filename=paste0(param$shared.dir,'fig/summary-',cn,'.png')
		resolution=param$png.resolution
		pdfOrPng(filename,width=9,height=7)
		par(mar=c(2.2,4.1,0.5,0.6)) # no space at the top

		nf <- layout(
			matrix(c(1,2,if(include.errors) 3 else NULL),ncol=1,byrow=TRUE), 
			# widths=c(3,1), 
			heights=c(2,1,if(include.errors) 1 else NULL)
		)

		plot(x=NULL,xlim=xlim,ylim=c(0,y.max)/1000,
			xlab='year',ylab='donations (in 1,000)'
		)

		lapply(estimates.list,FUN=function(x) {
				lines(x$est$year,x$est[[cn]]/1000,type='l',lwd=2,lty=x$lty,col=colfun(cn)) 
				lines(x$lo$year,x$lo[[cn]]/1000,type='l',lwd=1,lty='dotted',col=colfun(cn))
				lines(x$hi$year,x$hi[[cn]]/1000,type='l',lwd=1,lty='dotted',col=colfun(cn))
			})

		# actual donations
		points(df.ad$year,df.ad[[cn]]/1000,type='p',col=colfun(cn))

		# number of new donations
		nd=new.donors %>% filter(country==cn)
		rect(nd$year-0.3,0,nd$year+0.3,nd$n2/1000,col=colfun(cn))

		# activity scales/errors
		err=estimates0$error
		wh=which(abs(err[[cn]])>1 & err$year <= max(nd$year))

		err.data=data.frame(year=err$year[wh],error=err[[cn]][wh])
		cmb.data=inner_join(err.data,estimates0$est[,c(cn,'year')],join_by(year))
		cmb.data$perc=100*cmb.data$error/cmb.data[[cn]]

		# 2nd panel: average errors
		if (include.errors) {
			max.y=max(abs(cmb.data$perc),na.rm=TRUE)
			plot(x=NULL,xlim=xlim,ylim=c(-max.y,max.y),xlab='year',ylab='error-%')
			points(cmb.data$year,cmb.data$perc,col=colfun(cn),pch=2)
			abline(h=0,lty='dotted')
		}

		# 3rd panel: cdon50/average age (since first donation) of donor
		ced=coeff.data[coeff.data$rw==rw,]
		plot(x=NULL,xlim=xlim,ylim=c(0,max(ced$cdon50.hi)),xlab='year',ylab='cdon50')
		lines(ced$year,ced$cdon50,col=colfun(cn),lwd=2)
		lines(ced$year,ced$cdon50.lo,lty='dashed',col=colfun(cn))
		lines(ced$year,ced$cdon50.hi,lty='dashed',col=colfun(cn))

		points(estimates0$avg.age$year,estimates0$avg.age[[cn]],col=colfun(cn))
		wh=min(which(!is.na(estimates0$avg.age[[cn]])))
		year0=estimates0$avg.age$year[wh]
		lines(c(year0,year0+55),c(0,0.5*55),lty='dashed',col=colfun(cn))

		dev.off()
	}
}

### --- ennustekäyrät: tuotetaan kuvat vertailua varten
plotPredictions = function(rv,xlim=c(1,55),ylim=c(1,25),models=c('cdon.a-x','cdon-x.a')) {
	for (rw in rv$grps$rw) {
		filename=paste0('../fig/',paste(grps[rw,],collapse='-'),'-predictions.png')
		# resolution=150
		resolution=param$png.resolution
		pdfOrPng(filename,width=9,height=7)
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

plotEstimatesVsActual = function(et,estimates,spec,filename=NULL,resolution=150,main=NULL,lty='solid',
	xlim=NULL,ylim=NULL,grps=NULL,multipliers=list(nc=100),mode='single') {
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
		inner_join(grps,join_by(rw))

	# TODO There is significant hard-coding here: nc multiplier and using countries instead of groups
	# But groups in general would be hard to plot
	if (is.null(xlim)) 
		xlim=c(1993,2035)
	if (is.null(ylim)) 
		ylim=c(0,2e3)

	df3=data.frame(pivot_wider(pah[pah$year<=xlim[2],!colnames(pah) %in% c('rw','est.lo','est.hi')],values_from='est',names_from=c('country'))) %>%
		arrange(year)
	df3.lo=data.frame(pivot_wider(pah[pah$year<=xlim[2],!colnames(pah) %in% c('rw','est','est.hi')],values_from='est.lo',names_from=c('country'))) %>%
		arrange(year)
	df3.hi=data.frame(pivot_wider(pah[pah$year<=xlim[2],!colnames(pah) %in% c('rw','est.lo','est')],values_from='est.hi',names_from=c('country'))) %>%
		arrange(year)

	x2=which(df3$year==xlim[2])

	# plot predictions
	if (!is.null(filename)) {
		resolution=param$png.resolution
		pdfOrPng(filename,width=9,height=7)
			# par('mar')
			# default: 5.1  4.1  4.1   2.1
			#      bottom, left, top, right
		if (is.null(main)) {
			par(mar=c(2.2,4.1,0.5,0.6))
		}
	}	

	cns=grep('..',colnames(df3),value=TRUE)[-1]
	if (mode=='single') {
		plot(NULL,xlim=xlim,ylim=ylim,ylab='number of donations (in 1,000)',xlab='year',main=main)
	} else {
		# par(mfrow=c(length(cns),1))
		par(mar=c(2.2,1,0.5,0.6))
		plot(NULL,xlim=xlim,ylim=c(0,length(cns)),ylab='',xlab='year',main=main,yaxt='n')
	}
	for (cn in cns) {
		if (mode=='single') {
			i = 0
			multiplier = 0.001 * (if (cn %in% names(multipliers)) multipliers[[cn]] else 1) # nb! Navarre hard-coded thing
			y0=0
		} else {
			i=length(cns)-which(cns==cn)
			y.max=max(df.ad[[cn]],na.rm=TRUE) 
			y00=min(df.ad[[cn]],na.rm=TRUE) 
			y0=0.75*y00 # decreasing the multiplier raises the curves
			multiplier=0.60/(y.max-y0) # increasing the multiplier streches the curves vertically
		}
		lines(df3$year,i+multiplier*(df3[[cn]]-y0),type='l',lwd=2,lty=lty,col=colfun(cn)) 
		lines(df3.lo$year,i+multiplier*(df3.lo[[cn]]-y0),type='l',lwd=1,lty='dotted',col=colfun(cn)) 
		lines(df3.hi$year,i+multiplier*(df3.hi[[cn]]-y0),type='l',lwd=1,lty='dotted',col=colfun(cn)) 
		points(df.ad$year,i+multiplier*(df.ad[[cn]]-y0),type='p',col=colfun(cn))
	}

	# single-mode: all curves in a single plotting are/same scale
	# other: each mode with its own 'lane'
	if (mode=='single') {
		legend('topleft',fill=unlist(sapply(grps$country,FUN=colfun)),legend=sapply(sort(names(spec$colours)),FUN=function(cn) {
			paste0(cn.names[[cn]],if (cn %in% names(multipliers)) paste0(' (times ',multipliers[[cn]],')') else '') }))
	} else {
		sapply(1:length(cns),FUN=function(x) abline(h=x,lwd=1,lty='dotted',col='black'))
		legend('topleft',fill=unlist(sapply(grps$country,FUN=colfun)),legend=sapply(sort(names(spec$colours)),FUN=function(cn) {
			paste0(cn.names[[cn]]) }),bty='s',bg='white')
	}

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
		return(df)

	arrows(df$est.x,df$lo.y,df$est.x,df$hi.y,length=0.05,angle=90,code=3,col=col)
	arrows(df$lo.x,df$est.y,df$hi.x,df$est.y,length=0.05,angle=90,code=3,col=col)

	# x ~ exponent
	# y ~ multiplier
	u=50
	df$est.u=df$est.y*u^df$est.x
	df$lo.u=df$lo.y*u^df$lo.x
	df$hi.u=df$hi.y*u^df$hi.x

	return(df)
}

###
# utility functions
bsAssign = function(name) {
	obj = get(name,envir=parent.frame())
	assign(name,obj,.GlobalEnv)
}

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


	firstUp <- function(x) {
		substr(x,1,1) <- toupper(substr(x,1,1))
		return(x)
	}

# Below are some function that are still used
pdfOrPng = function(filename,width,height) {
	if (param$figure.format == 'png') {
		png(sub('.pdf$','.png',filename),height=8*param$png.resolution,width=8*param$png.resolution,res=param$png.resolution)
	} else {
		pdf(sub('.png$','.pdf',filename),height=height,width=width)
	}
}

# Using latex, create a pdf figure from a latex fragment
# Captions may also be included in figures.table.latex, as not all editorial sites allow them as metadata
# and compile them with the figures; and this is not done for commenting anyway
composeFigure = function(figures.table.latex,paper.dir,figures.file) {
	tex.pre='\\documentclass{standalone}\n\\usepackage[pdftex]{color,graphicx}\n\\begin{document}'
	tex.post='\n\\end{document}'

	cont = figures.table.latex
	cont=gsub('.hspace[^\\}]+\\}','',cont)
	cont=gsub('.(caption|clearpage)','%\\\\\\1',cont)
	cont=gsub('(.(begin|end).figure)','%\\1',cont)
	cont=paste(tex.pre,cont,tex.post,collapse='\n',sep='\n')

	figures.file=gsub('([\\/]+)','/',figures.file)
	figures.file=sub('.+[\\/]','composed-',figures.file)
	latexCompile(cont,paper.dir,sub(paper.dir,'',figures.file))
}

latexCompile = function(content,workdir,filename) {
	oldwd = getwd()
	setwd(workdir)

	tex.file = sub('.pdf$','.tex',filename)

	pdflatex = 'C:\\Users\\super\\AppData\\Local\\Programs\\MiKTeX\\miktex\\bin\\x64\\pdflatex.exe'

	cat(content,file=tex.file)
	system(paste(pdflatex,paste0('"',tex.file,'"')),intern=TRUE)
	setwd(oldwd)
}

# A simple html->latex conversion, mainly for the purpose of this paper
convertOutput = function(html,file) {
	if (param$figure.format == 'png') {
		cat(html,file)
	} else {
		html.0=html.file
		tex=sub('.+[<]body[>]','',html.file)
		tex=gsub('[<]table[>].tr.','\\\\begin{tabular}{cc}',tex)
		tex=gsub('[<]/table[>]','\\\\end{tabular}\n\\\\end{center}\n',tex)
		tex=gsub('[<]/tr[>][<]tr[>]','\\\\\\\\\n',tex)
		tex=gsub('[<]/td[>][\n ]*[^>]+td[^>]*[>]',' & ',tex)
		tex=gsub('[<]/td[>][\n ]*[^>]+td[^>]*[>]',' & ',tex)
		tex=gsub('[<]/tr[>]','\\\\\\\\\n',tex)
		tex=gsub('[<]b[>](.+)[<]/b[>]','\\\\textbf{\\1}',tex)
		tex=gsub('[<]/body[>].+','\n\\\\end{document}',tex)
		tex=gsub('&nbsp;','\\\\ ',tex)
		tex=gsub('&frac12;','$\\\\frac{1}{2}$',tex)
		tex=gsub('&ndash;','--',tex)
		tex=gsub('&middot;','\\\\cdot',tex)
		tex=gsub('(log|exp)[(]','\\\\\\1(',tex)
		tex=gsub('src=.([^>]+).[>]','>\\\\includegraphics[width=9cm]{\\1}',tex)
		tex=gsub('[.]png','.pdf',tex)
		tex=gsub('[<]span[>]([^<]+)[<]/span[>]','\\$\\1\\$',tex)
		tex=gsub('[<][^>]+[>]','',tex)


		if (!grepl('end.center',tex)) {
			tex=sub('.textbf','\\\\end{center}\n\\\\textbf',tex)
			tex=gsub('9cm','16cm',tex)
		}

		tex.pre='\\documentclass[varwidth=20cm,border=2mm]{standalone}\n\\usepackage[pdftex]{color,graphicx}\n\\begin{document} \\begin{center}'

		wd=sub('^(.+[/\\]).+','\\1',file)
		bare.file=sub(wd,'',file,fixed=TRUE)
		bare.file=sub('.html','.tex',bare.file)
		tex=paste(tex.pre,tex,collapse='\n')
		latexCompile(tex,param$shared.dir,bare.file)
	}

	suffix=c('aux','log','tex')
	dev.null=sapply(suffix,FUN=function(x) file.remove(sub('[.][a-z]+$',paste0('.',x),file)) )
}
