# 2025-07-20 way forward
# compute.csm below is fixed for counties are the grouping variable
# Should really utilise the computations in getGroupEstimates
# The data could more easily be derived similarly as et.test
predictDonations2 = function(rv,prd.start=2000,prd.len=55,model='cdon.a-x') {
bsAssign('rv')
	# alright, so the model is not included there because yearx0 is not set
	# as there is only a single year there and the model is not even estimated
	# dummy=rv$prdct[1,]
	rv$prdct[is.na(rv$prdct$year0),'year0']=0
	# rv$prdct$year0=as.integer(rv$prdct$year0)
	tmp=by(rv$prdct,rv$prdct[,c('year0','phase','rw')],FUN=prd.cumulative2density)
	# unlist(lapply(tmp,FUN=is.null))
	# tmp=by(rv$prdct,rv$prdct[,c('year0','phase','rw')],FUN=function(x) {print(is.null(x)); print(dim(x)); print(summary(x$year0)); x})
	pred.d=do.call(rbind,tmp[lengths(tmp)!=0]) # array2DF(tmp,simplify=TRUE)
	# pred.d=pred.d[,3:ncol(pred.d)]

	pred.d[pred.d$year0==0,'year0']=NA

	prd.years=data.frame(prd.year=prd.start:(prd.start+prd.len)) # nb! hard-coded parameters

	# This applies to cases where only one year is selected and the model is estimated
	# with no year0 term (multi.year==FALSE)
	if (all(pred.d$year0.lo==pred.d$year0.hi)) {
		model=sub('.year0','',model)
	}

	prd.data=pred.d[pred.d$phase==model,]

	agedist.local=rv$agedist

	# rw year0     n2
	# nb! agedist-data should be augmented with rw-numbers to facilitate things
	sizes.data = rv$data[rv$data$ord==1,c('rw','year0','n2')] 

	# str(rv.1$agedist)
	# tibble [255 × 5] (S3: tbl_df/tbl/data.frame)
	# $ country: chr [1:255] "au" "au" "au" "au" ...
	# $ n2     : num [1:255] 1470840 1470840 1470840 1470840 1470840 ...
	# $ age    : num [1:255] 14 15 16 17 18 19 20 21 22 23 ...
	# $ n.age2 : num [1:255] 1.77 2779.44 68503.52 53050.87 70472.53 ...
	# $ density: num [1:255] 1.20e-06 1.89e-03 4.66e-02 3.61e-02 4.79e-02 ...

	# nb! should probably cut the sizes as well earlier, not just for predictions (skip.last)
	tmp=by(sizes.data,sizes.data[,c('rw')],FUN=function(x) {
			year.max=max(x$year0)
			n2.max=x[x$year0==year.max,'n2']
			df.new=data.frame(rw=max(x$rw),year0=(year.max+1):max(prd.years$prd.year),n2.max)
			return(rbind(x,df.new))
		})
	sizes.data=array2DF(tmp)[,-1]

bsAssign('prd.data')
# prd.data %>%
# 	filter(rw==2,year0==2002)

	if (all(is.na(prd.data$year0))) {
		pah=cross_join(prd.years,sizes.data) %>%
			inner_join(prd.data[,colnames(prd.data) != 'year0'],
				join_by(rw,between(x$year0,y$year0.lo,y$year0.hi))) %>%
			mutate(year=year0+x-1) %>%
			filter(year==prd.year) %>%
			mutate(est=n2*fit,est.lo=n2*lwr,est.hi=n2*upr)
	} else {
		pah.has.year0=cross_join(prd.years,sizes.data) %>%
			inner_join(prd.data,join_by(rw,year0)) %>%
			mutate(year=year0+x-1) %>%
			filter(year==prd.year) %>%
			mutate(est=n2*fit,est.lo=n2*lwr,est.hi=n2*upr)

		pah=cross_join(prd.years,sizes.data) %>%
			left_join(prd.data,join_by(rw,year0)) %>%
			filter(is.na(fit)) %>%
			dplyr::select(prd.year,rw,year0,n2) %>%
			# copied part
			inner_join(pred.d[pred.d$phase==sub('.year0','',model),colnames(prd.data) != 'year0'],
				join_by(rw,between(x$year0,y$year0.lo,y$year0.hi))) %>%
			mutate(year=year0+x-1) %>%
			filter(year==prd.year) %>%
			mutate(est=n2*fit,est.lo=n2*lwr,est.hi=n2*upr) %>%
			rbind(pah.has.year0)
	}

	tmp=by(agedist.local,agedist.local[,c('rw')],FUN=function(x) {
		x$cumulative=cumsum(x$density)
		x
	})
	agedist.cum=array2DF(tmp,simplify=TRUE)[,-1]

	# return(pah)

	pah2=pah %>%
		# 66: jos täytti vuonna year0=2010 esim. 50 vuotta, ja x rivillä on esim. 15, kyse on 
		# vuoden 2024 ennusteesta, jolloin täyttää 64 vuotta. Mukaan otetaan se vuosi, jolloin täyttää 65
		# vuotta mutta ei enää seuraavaa. Eli tämä lienee oikein.
		mutate(age0.max=66-x) %>%
		inner_join(agedist.cum[,c('rw','age','cumulative')],join_by(rw,x$age0.max==y$age)) %>%
		mutate(est.trnc=est*cumulative,est.lo=est.lo*cumulative,est.hi=est.hi*cumulative)

	return(pah2)
}

# TODO
# year0 should be added to the spec
# similarly for year to use maybe reference year +/- offset
getGroupEstimates2 = function(et,spec,lwd=3,plot='orig',years.ahead=55,try.nls=FALSE,year0.ord=3:100,skip.last=TRUE,agedist=NULL) { # skip.start
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
		sm=summary(m)$coeff
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

bsAssign('m')

		# frml=as.character(m$call)[2]
		new.data=data.frame(x=1:55)
		if ('sq.x' %in% names(m$model))
			new.data$sq.x=new.data$x^2
		if ('sqrt.x' %in% names(m$model))
			new.data$sqrt.x=new.data$x^0.5
		if ('x.pwr' %in% names(m$model))
			new.data$x.pwr=new.data$x^power.term

print(paste('***',phase))
if (phase == 'cdon-x.a+year0') {
	print(summary(m))
#	error(777)
}

		year0.col=NULL
		if (!is.null(years)) {
			new.data=cross_join(new.data,data.frame(year0=years))
			year0.col=ncol(new.data)
		}

		esq=data.frame(predict(m,newdata=new.data,interval='confidence')) %>%
			cbind(new.data)
		
		if (grepl('^log',names(m$model[1]))) {
			# print(paste(phase,'exp applied'))
			esq=exp(esq[,1:3])
		} else if (!is.null(power.term) && !('x.pwr' %in% names(m$model))) {
			esq=esq[,1:3]^power.term
		}

		# esq=esq[,c(1:3,if(!is.null(year0.col)) ncol(esq) else NULL)]
		esq=esq[,1:3]
		if (!is.null(year0.col)) 
			esq=cbind(esq,year0=as.integer(new.data$year0))
		else 
			esq$year0=as.integer(NA)
		esq$x=new.data[,1]
		esq$phase=phase

if (phase == 'cdon-x.a+year0')
print(esq[1:10,])

		return(esq)
	}

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
		data = data %>% filter(year0.int>=year.start+min(year0.ord)-1,year0.int<=year.start+max(year0.ord)-1)
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
		bsAssign('data')

		# The model with a common intercept/multiplier
		m=lm(log(cdon)~log(x),data=data)
		sm=summary(m)
		power.term=summary(m)$coeff[2,1]
		intercept=summary(m)$coeff[1,1]
		b=exp(intercept)

		# initialise the coeff-structure
		# coeff=data.frame(sm$coeff,phase='log-log')
		coeff = sm.extract(m,'log-log')
		prdct = m.predict(m,'log-log')

		resolution=150
		filename=paste0('../fig/',paste(grps[rw,],collapse='-'),'-fund-plot.png')
		png(filename,res=resolution,width=9*resolution,height=7*resolution)
		# data2=data[as.integer(as.character(data$year0))>=2012,]
		data2=data

		# the power-conversion happens here
		data2$y=data2$cdon^(1/power.term)

		plot(y~x,data=data2,main=paste0('b=',b,', a=',power.term,', y50=',round(b*50^power.term,1)))
		for(yr in unique(data2$year0)) {
			data3=data2[data2$year0==yr,]
			lines(data3$x,data3$y,col=as.integer(yr))
		}
		dev.off()

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

			# linear model with converted response variable
			data2$sq.x=data2$x^2
			m=lm(y~x+year0+0,data=data2)
			sm=summary(m)
			# print(sm)
			coeff = rbind(coeff,sm.extract(m,'cdon.a-x-year0'))
			prdct = rbind(prdct,m.predict(m,'cdon.a-x-year0',power.term=power.term))

			m=lm(cdon~x.pwr+year0+0,data=data)
			coeff = rbind(coeff,sm.extract(m,'cdon-x.a+year0'))
			prdct = rbind(prdct,m.predict(m,'cdon-x.a+year0',power.term=power.term)) # power.term converts the predictions
		}

		# linear model converted response variable, no year0
		data2$sq.x=data2$x^2
		m=lm(y~x,data=data2)
		sm=summary(m)
		# print(sm)
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
				print(summary(m.nls))
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
#		if ('curve' %in% plot) {
#			lines(x.m,y.m,col=col0,lwd=lwd)
#		}

		dev.off()

		coeff$rw=rw
		prdct$rw=rw
		# coeff=coeff[5:7,1:4)]

		# Add these to enable joining with forecasted years
		prdct$year0.lo=year.start+min(year0.ord)-1
		prdct$year0.hi=year.start+max(year0.ord)-1

		if (is.null(sp.data)) {
			sp.data=export.data 
			sp.coeff=coeff
			sp.dftot=dftot
			sp.prdct=prdct
		} else {
			sp.data=rbind(sp.data,export.data)
			sp.coeff=rbind(sp.coeff,coeff)
			sp.dftot=rbind(sp.dftot,dftot)
			sp.prdct=rbind(sp.prdct,prdct)
		}
	}

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

	return(list(et.data=et.test,data=sp.data,grps=data.frame(grps,rw=1:nrow(grps)),
		coeff=sp.coeff,fit=dftot,m.year0=sp.m.year0,prdct=sp.prdct,
		agedist=agedist.local))
}

### --- ennustekäyrät: tuotetaan kuvat vertailua varten
plotPredictions = function(rv,xlim=c(1,55),ylim=c(1,25),models=c('cdon.a-x','cdon-x.a')) {
	for (rw in rv$grps$rw) {
		filename=paste0('../fig/',paste(grps[rw,],collapse='-'),'-predictions.png')
		resolution=150
		png(filename,res=resolution,width=9*resolution,height=7*resolution)
		plot(x=NULL,xlim=xlim,ylim=ylim)
		phases=unique(rv$prdct$phase)
		
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
		legend(x='bottom',fill=1:length(phases),legend=phases)
		dev.off()
	}
}

plotCoeffData=function(data,spec,grps,phase,dparam,vfun,error.bars=TRUE) {
	data0=data[data$phase==phase,]

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

	df.x=data.frame(est.x,lo.x,hi.x,rw=data0[wh.x,'rw'])
	df.y=data.frame(est.y,lo.y,hi.y,rw=data0[wh.y,'rw'])

	df=full_join(df.x,df.y,join_by(rw)) %>%
		inner_join(grps,join_by(rw))

	col=unlist(spec$colours[df[,spec$col.dim]])
	pch=sapply(df[,spec$pch.dim],spec$pch ) # spec$pch(df[,spec$pch.dim])
	points(df$est.x,df$est.y,col=col,pch=pch)

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

getAgeDistributionMatrix = function(data) {
  # The two out-commented conditions on the following row are unnecessary at this point (at least they should be)
  # Restricting to first time donors should be done in the first stage, if at all
  
  # nb! hard coded
  byAgeYear=data[year(data$date)>=2003,c('age','date')] # data$type=='donation'&data$ord==1&
  stats=byAgeYear %>%
    mutate(year=year(date),age=as.integer(age)) %>%
    mutate(year=year-(year %% 5)) %>%
    group_by(age,year) %>%
    summarise(n=n(),avgAge=mean(age,na.rm=TRUE),.groups='drop')
  
  mat=pivot_wider(stats[,c('age','year','n')],names_from=year,values_from=n,values_fn=mean)
  m=mat[,2:dim(mat)[2]]
  ptbl=sweep(m,2,colSums(m,na.rm=TRUE),`/`)
  
  ctbl=apply(ptbl,2,cumsum)
  p2=cbind(mat[,1],ctbl)
  wh=which(is.na(p2$age))
  if (length(wh) > 0) {
    wh0=min(wh)
    p2=p2[1:(wh0-1),]
  }
  rownames(p2)=p2$age
  return(p2)
}

# estimate and plot countries using the sqrt-models all years at once, plot the results
# csm ~ estimated coefficients based on the sqrt-model, total data and varying intercept (year0)
compute.csm = function(dfr,plot='all',return.fit=FALSE) {
	csm=NULL
	fit=list()
	for (cn in unique(dfr$country)) {
		data=dfr %>% filter(country==cn)
		data$year0=as.factor(data$year0)

		# lm is called directly here: no function for this
		m=lm(y~0+year0+x+sqrt.x,data=data)
		sm=summary(m)
		new.data=expand.grid(year0=unique(data$year0),x=1:50)
		new.data$sqrt.x=sqrt(new.data$x)
		esq=predict(m,new.data,interval='confidence')
		dftot=cbind(new.data,esq)

		# plotting: copied from the newRegression function
		offset=2
		resolution=150
		filename=paste0('../fig/',cn,'-sqrt-multiple.png')
		if (plot=='all') {
			png(filename,res=resolution,width=9*resolution,height=7*resolution)
			plot(x=NULL,ylim=c(0,50),xlim=c(0,55))
		}
			yrs=sort(unique(data$year0))
			for (i2 in 1:length(yrs)) {
				y2 = yrs[i2]
				dfslc=dftot[dftot$year0==y2,]
				ddf = data[data$year0==y2,]

				if (reference.years[cn,'year'] == y2) {
					fit[[cn]]=dfslc
					y.m = dfslc$fit
					y.max = max(y.m[1:50])
					y.star=y.max/2
					x0=max(which(y.m<y.star))
					x1=x0+1
					y0=y.m[x0]
					y1=y.m[x1]
					x.half=(y.star-y0)/(y1-y0)+x0
				}

				if (plot=='all') {
				y2i=as.integer(y2)-1
				lines(dfslc$x,dfslc$fit+(offset*y2i),lty='dashed')
				lines(dfslc$x,dfslc$lwr+(offset*y2i),lty='dotted')	
				lines(dfslc$x,dfslc$upr+(offset*y2i),lty='dotted')

				points(ddf$x,(ddf$y)+(offset*y2i),lwd=2)
				}
			}
			if (plot=='all') {
			dev.off()
			}
		# }

		coeff=cbind(country=cn,data.frame(sm$coeff))
		coeff$var=rownames(sm$coeff)
		coeff['r.squared',c('Estimate','var','country')]=c(sm$r.squared,'r.squared',cn)
		coeff['y.max',c('Estimate','var','country')]=c(y.max,'y.max',cn)
		coeff['x.half',c('Estimate','var','country')]=c(x.half,'x.half',cn)
		if (is.null(csm)) {
			csm=coeff
		} else
			csm=rbind(csm,coeff)
	}
	# csm
	csm$Estimate=as.numeric(csm$Estimate)

	if (return.fit)
		return(list(csm=csm,fit=fit))

	return(csm)
} # compute.csm

getGroupEstimates = function(et,spec,lwd=3,plot='orig',index=1,year.offset=0,years.ahead=55) {
	reference.years.local=reference.years
	reference.years.local$year=reference.years.local$year+year.offset
	et.test = et %>%
		filter(!is.na(cdon),!is.na(don)) %>%
		inner_join(reference.years.local,join_by(x$year0==y$year,country)) %>%
		group_by(!!!syms(setdiff(colnames(et),setdiff(dim.cols,spec$dim.keep)))) %>%
		summarise(cdon=sum(n*cdon),don=sum(n*don),.groups='drop') %>%
		group_by(!!!syms(c(spec$dim.keep,'year0','ord','year'))) %>% # nb! year0 is here
		summarise(n2=sum(n),cdon=sum(cdon)/n2,don=sum(don)/n2,.groups='drop') 

	grps=data.frame(unique(et.test[,spec$dim.keep]))
	res.all=list
	simple.data=NULL
	sp.data=NULL
	for (rw in 1:nrow(grps)) {
		data = et.test

		for (cn in 1:ncol(grps)) {
			grp.name=colnames(grps)[cn]
			grp.value=data.frame(grps)[rw,cn]
			data=data[data[[grp.name]]==grp.value,]
		}

		if (nrow(data) < 3)
			next

		m0 = estimate.predict2(data$cdon,try.nls=FALSE)
		res=list()
		res$index=i
		res$year=year
		res$m = m0$m
		res$m.nls = m0$m.nls
		coeff=data.frame(summary(res$m)$coeff)

		simple.data=m0$data
		for (ci in colnames(grps)) {
			simple.data[[ci]] = grps[rw,ci]
		}

		# nb! assuming a fixed year here
		simple.data$year0=min(data$year0)

if (FALSE) {
		x.m=1:years.ahead
		y.m=coeff['(Intercept)','Estimate']+x.m*coeff['x','Estimate']+sqrt(x.m)*coeff['sqrt.x','Estimate']
		y.max=max(y.m)
		y.star=y.max/2
		x0=max(which(y.m<y.star))
		x1=x0+1
		y0=y.m[x0]
		y1=y.m[x1]
		x.half=(y.star-y0)/(y1-y0)+x0
}

		col.start=spec$colours[[grps[rw,spec$col.dim]]]
		colfunc <- colorRampPalette(c(col.start, "white"))
		colfun=colfunc(20)
		col0=colfun[index]

		if (plot=='orig') {
			points(coeff[plot.terms[1],'Estimate'],coeff[plot.terms[2],'Estimate'],
				col=col0,lwd=lwd,pch=spec$pch(grps[rw,spec$pch.dim]))
		} else if (plot=='alt') {
			points(x.half,y.max,
				col=col0,lwd=lwd,pch=spec$pch(grps[rw,spec$pch.dim]))
		} else if (plot=='curve') {
			lines(x.m,y.m,col=col0,lwd=lwd)
		}

		if (is.null(sp.data)) {
			sp.data=simple.data 
		} else
			sp.data=rbind(sp.data,simple.data)
	}

	return(list(et.data=et.test,data=sp.data))
}

prd.cumulative2density = function(pah) {
	dpah=(rbind(pah[,1:3],0*pah[1,1:3])-rbind(0*pah[1,1:3],pah[,1:3]))[1:nrow(pah),] # nämä siis saa vähentämällä
	return(cbind(dpah,pah[,4:ncol(pah)]))
}

getAgeDistributionMatrix = function(data) {
  # The two out-commented conditions on the following row are unnecessary at this point (at least they should be)
  # Restricting to first time donors should be done in the first stage, if at all
  
  # nb! hard coded
  byAgeYear=data[year(data$date)>=2003,c('age','date')] # data$type=='donation'&data$ord==1&
  stats=byAgeYear %>%
    mutate(year=year(date),age=as.integer(age)) %>%
    mutate(year=year-(year %% 5)) %>%
    group_by(age,year) %>%
    summarise(n=n(),avgAge=mean(age,na.rm=TRUE),.groups='drop')
  
  mat=pivot_wider(stats[,c('age','year','n')],names_from=year,values_from=n,values_fn=mean)
  m=mat[,2:dim(mat)[2]]
  ptbl=sweep(m,2,colSums(m,na.rm=TRUE),`/`)
  
  ctbl=apply(ptbl,2,cumsum)
  p2=cbind(mat[,1],ctbl)
  wh=which(is.na(p2$age))
  if (length(wh) > 0) {
    wh0=min(wh)
    p2=p2[1:(wh0-1),]
  }
  rownames(p2)=p2$age
  return(p2)
}

# This functon creates the plot but is not actually specific to cumulative distributions
plotAgeDistributionMatrix = function(data,years=17:65,main='') {
  plot(x=NULL,t='n',xaxt='n',xlim=c(min(data$age),max(data$age)),ylim=c(0,1.1),
       xlab='age at first donation',ylab='cumulative proportion of donors up to age',main=main)
  axis(1,at=years,labels=years)
  for (i in 2:dim(data)[2]) {
    lines(years,data[as.character(years),i],col=i+1,lwd=3)
  }
  legend('bottomright',legend=colnames(data)[2:dim(data)[2]],fill=1+2:dim(data)[2])
}

estimate.predict2 = function(y,years.ahead=55,main='',sub='',try.nls=FALSE) {
	dist=matrix(y,nr=1,nc=length(y))
	rownames(dist)='1995'
	if (all(!is.na(y))) {
		nr.of.years=length(y)+1
	} else 
		nr.of.years=min(which(is.na(y)))
	return(estimate.predict(dist,ref.year="1995",last.data.year=(1995+nr.of.years-2)-1, # nb 2025-07-01
		years.ahead=years.ahead,try.nls=try.nls))
}
# estimate.predict(dist,ref.year='1995',last.data.year=1995+12)

# two new parameters, more intuitive and can be computed
# - Estimated donations at 50 years since start
# - Time needed to reach half of the 50 year donations

estimate.predict = function(dist,ref.year="2003",last.data.year=2023,years.ahead=55,main='',sub='',try.nls=FALSE) {
  ref.year=as.character(ref.year)
  ref.year.numeric = as.integer(ref.year)
  years.to.use = 1 + (as.integer(last.data.year) - ref.year.numeric)

  data=data.frame(y=dist[as.character(ref.year),1:years.to.use],x=1:years.to.use) # dim(dist)[2])
  
  # assume the data follows the square root form
  data$sqrt.x=data$x^0.5
  m=lm(y~x+sqrt.x,data=data)
  
  if (try.nls) {
    m.nls = NULL
    sample2=data
    sample2$x=data$x-1
    try(m.nls <- nls(y~y1*exp(-lambda*x)+y0,data=sample2,
                     start=list(y1=-(max(sample2$y)-min(sample2$y)),lambda=0.2,y0=max(sample2$y))))
    
    if (is.null(m.nls)) {
      # print('m.nls failed')
    } else {
      # print(summary(m.nls))
    }
  }
  
  # Values are predicted here only if the main parameter is given. Otherwise years.ahead remains unused as well
  new=data.frame(x=1:years.ahead,sqrt.x=(1:years.ahead)^0.5)
  pred.w.plim <- predict(m, new, interval = "prediction")
  pred.w.clim <- predict(m, new, interval = "confidence")
  if (main != '') {
    matplot(new$x, cbind(pred.w.clim, pred.w.plim[,-1]),
            lty = c(1,2,2,3,3), type = "l", ylab = "predicted y",main=main,sub=sub)
    points(data$x,data$y)
    
    if (!is.null(m.nls)) {
      new = data.frame(x=0:(years.ahead-1))
      new$y = predict(m.nls,new=new)
      new$x = new$x + 1
      
      lines(new$x,new$y,col='purple',lwd=2,lty='dashed')
    }
  }
  
  if (try.nls) {
    return(list(m=m,m.nls=m.nls,data=data))
  }
  
  return(list(m=m,data=data))
}

plot.estimated = function(nres,gt,bundle=FALSE,years.ahead=55) {
  # nres = res.oneg
  # gt = gt.oneg
  # bundle=TRUE
  
  models=list()
  models.nls = list()
  for (i in 1:length(nres)) {
    group = nres[[i]]
    ms = estimate.predict(group$distm,
                          main = if (bundle) '' else gt$Name[i],
                          ref.year=param$reference.year,last.data.year=param$last.data.year,
                          sub=paste('n=',dim(group$data)[1],sep=''),try.nls=TRUE)
    models[[i]] = ms$m
    models.nls[[i]] = ms$m.nls
  }
  
  mean.y = NULL
  
  # Create a plot with all the model estimates if requested
  if (bundle) {
    # nb! should set a reasonable value for the maximum
    plot(x=NULL,type='n',xlim=c(0,55),ylim=c(0,20),ylab='cumulative number of donations',xlab='years since first donation')
    for (i in 1:length(models)) {
      new=data.frame(x=0:years.ahead,sqrt.x=(0:years.ahead)^0.5)
      pred.w.plim <- predict(models[[i]], new, interval = "prediction")
      # pred.w.clim <- predict(models[[i]], new, interval = "confidence")
      lines(new$x, pred.w.plim[,1],lwd=3,col=i+1)
      lines(new$x, pred.w.plim[,2],lwd=1,col=i+1,lty='dashed')
      lines(new$x, pred.w.plim[,3],lwd=1,col=i+1,lty='dashed')
      points(models[[i]]$model$x,models[[i]]$model$y,col=i+1)
      legend('topleft',legend=gt$Name,fill=1+(1:length(models)))
    }
    
    if (length(models)==2) {
      # Plot the proportion as well to enable comparisons
      # Primarily intended to estimate the excess donations by Oneg donors
      lines(models[[1]]$model$x, models[[1]]$model$y/models[[2]]$model$y,lwd=5,lty='dotted')
      mean.y=mean(models[[1]]$model$y/models[[2]]$model$y)
      text(x=30,y=mean.y,labels=paste('ratio =',round(mean.y,3)))
    }
  }
  
  return(mean.y)
}

# New take on counting the cumulative amounts of donations: utilise pivoting
distributionMatrix2 = function(data.group,data.full,density=FALSE,max.dt=as.Date('2024-04-16'),year.start=2000,year.end=2023) {
  # nb! should implement the remaining parameters
  # The last year should include a full year of donations
    
  # stats will contain the time in years from each donor's first donation to each subsequent donation
  # in years. Example tibble [220,092 × 3]
  stats = data.group %>%
    left_join(data.full[,c('date','ord','numid')],join_by(numid,x$ord<=y$ord)) %>%
    mutate(ydiff = as.integer(as.numeric(date.y-date.x) / 365.25) + 1,
           ydiff.dec=as.numeric(date.y-date.x) / 365.25, 
           cdon = 1 + (ord.y-ord.x)) %>%
    group_by(rowid,ydiff) %>%
    summarise(cdon=max(cdon),.groups='drop')
  
  # form a grid (donor=rowid x years since first donation), at this point as a list
  # and the average number of cumulative donations as data
  # Example: tibble [625 x 3]
  resgrid = expand.grid(rowid=unique(stats$rowid),ydiff=0:max(stats$ydiff,na.rm=TRUE)) %>%
    inner_join(stats[,c('rowid','ydiff','cdon')],join_by(rowid,closest(y$ydiff<=x$ydiff))) %>%
    inner_join(data.full[,c('rowid','date')],join_by(rowid)) %>%
    mutate(year=year(date)) %>%
    group_by(year,ydiff.x) %>%
    summarise(cdon=mean(cdon),.groups='drop')
  
  # Convert the result to matrix form
  mat=as.matrix(pivot_wider(resgrid,names_from=ydiff.x,values_from=cdon,values_fn=mean))
  rownames(mat)=mat[,1]
  mat=mat[,2:dim(mat)[2]]
  
  dmat = getDensity(mat)
  
  # nb! These override the parameters that were never actually set but the defaults were used
  year.end=max(resgrid$year)
  year.start=min(resgrid$year)
  
  # Fill in possible gaps in the matrix with zeros
  years.ahead=max((year.end-year.start)+1,dim(mat)[2])
  zerom=matrix(0,ncol=years.ahead,nrow=dim(mat)[1])
  zerom[1:dim(mat)[1],1:dim(mat)[2]]=mat
  rownames(zerom)=rownames(mat)
  colnames(zerom)=1:years.ahead
  mat=zerom
  mat.na=is.na(mat)
  mat[is.na(mat)]=0
  mat=t(apply(mat,1,FUN=function(x) cummax(x)))
  
  mat[dmat==0]=NA
  
  return(mat)
}

sumDonationAmounts = function(nres,gt,existing=FALSE,years.ahead=55,total.donations=0,print.debug=FALSE) {
  dares = computeDonationAmounts(nres,gt,by.age=FALSE,is.existing=existing,total.donations=total.donations)
  dona = matrix(0,ncol=years.ahead,nrow=length(nres))
  for (i in 1:length(nres)) {
    dona[i,] = dares[[i]]$donation.amounts
  }
  
  if (!existing) {
    m = years.ahead
    cdona = matrix(0,nrow=dim(gt)[1],ncol=2*m)
    for (i in 1:m) {
      cdona[,i:(i+(m-1))] = cdona[,i:(i+(m-1))] + dona
    }
    
    return(cdona[,1:m])
  }
  
  return(dona)
}

plotDonationAmounts = function(dona,gt) {
  dfdona=data.frame(grp=gt$Name,dona)
  pdata=pivot_longer(dfdona,cols=starts_with('X'),names_to='year',names_prefix='X',values_to='donations') %>%
    group_by(grp) %>%
    mutate(year = as.integer(year))
  
  p = data.frame(pdata) %>%
    ggplot(aes(x = year, y = donations, fill = grp)) +
    geom_area() +     
    theme(legend.position="bottom")
  
  print(p)
}

getDensity = function(distm) {
  distm.m1=cbind(matrix(0,ncol=1,nrow=dim(distm)[1]),distm[,1:(dim(distm)[2]-1)])   
  return(distm-distm.m1)
}

plotDistibutionMatrix = function(distm,diff=FALSE,skip.years=1,main='') {
  if (!diff) {
    colfunc <- colorRampPalette(c("white","green"))
    cut.lower  = min(distm,na.rm=TRUE)-0.1
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

# names(countries$fi$res[[1]])
# [1] "sizes" "distm" "dista" "index" "m"     "m.nls" "all"  
# Täällä on tarvittavia tietoja
# gtl: countries$fi$gt
# Huom! Tehtävä erikseen ennusteet myös 1. ja 2. vuodelle
# Näiden perusteella voi sitten ennustaa nykyisen luovuttajakunnan liikkeitä. Nyt ennustetussa mallissa vain 3+-vuodet mukana.
computeDonationAmounts = function(resl,gtl,years.ahead=55,first.predicted.year=2023,is.existing=FALSE,by.age=FALSE,total.donations=0,print.debug=FALSE) {
	data.sum = 0
	for (k in 1:length(resl)) {
		# cf. countries$nl$res[[1]]$sizes
		# data.sum = data.sum + table(year(resl[[k]]$data$date))[as.character(first.predicted.year-1)]
		# 2024-12-08 New version using presaved sizes
		data.sum = data.sum + resl[[k]]$sizes[as.character(first.predicted.year-1),'n']
	}
	
	for (k in 1:length(resl)) {
		group=resl[[k]]
		
		max.age=gtl[k,'MaximumAge']

		# 2025-08-13 This should be replaced with data frames produced outside this function
		m=group$m
		new=data.frame(x=1:years.ahead, sqrt.x=(1:years.ahead)^0.5)
		pred = predict(m, new, interval = "prediction")
		# pred tässä vastaa est-arvoja

		pv = as.vector(pred[,1])
		
		# There are the expected numbers of donations per year
		# This includes conversion from cumulative values to densities; could use the function as well
		# pp ~ per person
		pred.donations.pp=(c(pv,0)-c(0,pv))[1:length(pv)]
		
		# 2024-08-09 The predictions may become negative, so set them to 0 here
		# 2025-08-13 This won't be needed with the current values
		pred.donations.pp[pred.donations.pp<0] = 0
		
		group$dista[is.na(group$dista)]=1
		dista=group$dista[,'2020'] # nb hard coded
		dista=cumulativeToDensity(dista)
				
		sumn=0
		# iterate over the initial ages in the group
		donation.amounts = matrix(0,ncol=years.ahead,nrow=length(rownames(group$dista)))
		colnames(donation.amounts)=1:years.ahead
		rownames(donation.amounts)=rownames(group$dista)
		for (i in 1:length(rownames(group$dista))) {
			n=rownames(group$dista)[i]
			years.active=max.age-as.integer(n)
			
			if (years.active < 0)
				# This case occurs when the age group is over the maximum age and thus has no active years
				break
			
			# in case of old donors, must start from a later position in pred.donations.pp
			# but still place the first values in the beginning, eg. project them for 2024 etc.
			# Need to compute the dlast table here
			annual.donation.densities = pred.donations.pp
			if ((years.active+1) <= years.ahead)
				annual.donation.densities[(years.active+1):years.ahead]=0
			
			# nb! hard coded data again: use the number of donors from year 2003
			if (is.existing) {
				# ... contents has been removed from this version
			} else {
				# The case for new donors: simple and easy
				vmult = 1
				if (!is.null(total.donations) && total.donations > 0) 
					vmult = as.numeric(total.donations) / as.numeric(data.sum)
				
				# donation amounts: rows ~ groups, columns ~ years.ahead
				# Here the loop (i) is over the age distribution
				
				# 2025-04-30 debug code
				if (print.debug) {
					print(paste('vmult =',vmult))
					print('annual.donation.densities')
					print(annual.donation.densities)
					print(paste('first.predicted.year =',first.predicted.year))
					print('group$sizes column and row names')
					print(colnames(group$sizes))
					print(rownames(group$sizes))
					print('dista')
					print(dista)
				}
				
				donation.amounts[i,] = vmult * annual.donation.densities *
					as.integer(group$sizes[as.character(first.predicted.year-1),'n']) * dista[i]
				# as.integer(table(year(group$data$date))[as.character(first.predicted.year-1)]) * dista[i]
			}
		} # for (i in 1:length(rownames(group$dista))) # groups
		
		if (by.age)
			resl[[k]]$donation.amounts = donation.amounts
		else
			resl[[k]]$donation.amounts = colSums(donation.amounts)
	}
	
	return(resl)
}

plotDelayBySex = function(activity.stats.sex,country) {
  # plot(activity.stats$ord,activity.stats$delay,
  #      xlab='number of previous donations',ylab='delay until next donation',type='l',lwd=3,main=country)
  
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
