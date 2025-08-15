# 2025-07-19
# Much effort was invested to improve the exponential models. However, the results were not satisfactory.
# The sqrt-models seems to work better overall.
# These should be estimated for 

# 2025-07-01 Should try using all the years to model the shape of the curves
# Maybe the first year to estimate the trend
# Must check that the last year is a full year (cannot be for those who started in 2024).
# - There used to be a need to recruit donors on behalf of other areas.
# How much immigration there is from that are in general.

# - 2025-07-07
# Pitäisi siis muodostaa pätkistä sarjat
# Kuitenkin jotain ryhmittäisyyttä? Vastaavat ryhmittelyt kuin nyt
# Voi olla myös pelkkä maa, tai sitten maa/sukupuoli + maa/veriryhmä
# Tuloksia voisi sitten tarkastella sen mukaan, miten minkin numeron selittäminen onnistuu
# Olisiko sittenkin vain ensimmäinen ja viimeinen mukana kullakin rivillä selittäjänä?

# y=x_n=x0*r1*r2*r3*r4^(n-3) | log(·)
# Y=X0+R1+R2+R3+(n-3)R4 (R ~ parametreja, X,Y: lukuarvoja riveiltä, n rivin pituudesta)
# Mahdollisesti sekamalli niin, että samoissa suhteissa muuttuvat mutta alkuarvot ovat eri
# k~montako r-termiä otetaan alusta (r4-kerroin ei mukana)

newRegression = function(distm,k=NULL,y0=1,plots=NULL,cn=NULL,years.ahead=55) {
	if (is.null(k)) {
		k=length(which(!is.na(distm[y0,])))-4
	}
	col.names=c('year0','len','y','x0',paste0('r',1:k),'rr')
	nc=length(col.names)
	mat=matrix(0.0,nc=nc,nr=nrow(distm)^2)
	index=1

	distm.1=distm[,1]

	# normlisation of distm
	distm=distm/distm.1

	hs = cumsum(0.5^((1:nc)-1))
	for (i in y0:nrow(distm)) {
		len0=length(which(!is.na(distm[i,])))-1
		if (len0 < 2) {
			# print('breaking')
			break
			i=i-2
		}
		for (j in 2:len0) {
			len = j
			# if (len < 2)
			# 	break

			x0 = log(distm[i,1])
			y = log(distm[i,len]) # tässä pitäisi varmaankin olla -1, koska viimeinen vuosi voi olla vajaa

			predv=c(i,len,y,x0,rep(1,k),len-(k+1))
			if (len - (k+1) < 0) {
				predv[(length(predv)+(len-(k+1))):length(predv)]=0
			}

			# 2025-07-19 computing the rr value
			if (predv[length(predv)] > 0)
				predv[length(predv)]=hs[predv[length(predv)]]

			mat[index,]=predv
			index = index + 1
		}
	}
	mat=mat[apply(mat,1,FUN=function(x) !all(x==0)),]
	df=data.frame(mat)
	colnames(df)=col.names
	df$year0=as.factor(df$year0)
	frml.char=paste0('y~0+year0:x0+',paste0('r',1:k,collapse='+'),'+rr')

	# new formula without the year0-terms
	# year0:x0
	# 0 removed: need a constant term
	# cannot introduce it, because r1 is essentially a constant
	frml.char=paste0('y~0+',paste0('r',1:k,collapse='+'),'+rr')

	# ok, this seems to work: need to use the same x0 for the last year, 
	# otherwise that parameter becomes obsolete as there is only one data point for it
	max.i = levels(df$year0)[max(as.integer(df$year0))]
	df[df$year0==max.i,'year0']=as.character(as.integer(max.i)-1)
	m=lm(formula(frml.char),data=df)
	sm=summary(m)
	print(sm)

	df2=df
	df2$year0.factor=df2$year0
	df2$year0=as.integer(df2$year0)
	df2[nrow(df2),'year0']=df2[nrow(df2),'year0']+1

	resa=data.frame(cbind(df2[,c('year0','len')],res=sm$residuals))
	resa2=as.matrix(pivot_wider(resa,values_from='res',names_from='len'))
	rownames(resa2)=rownames(distm)[y0:(y0+nrow(resa2)-1)]
	resa2=resa2[,-1]
	# plotDistibutionMatrix(resa2,diff=TRUE,skip.years=0,main='the residuals')
	# intercepts=sm$coeff[grepl('^year0',rownames(sm$coeff)),'Estimate']
	# plot(intercepts,ylim=c(0,5))

	new.data.cols=c('year0.factor','x0',paste0('r',1:k),'rr')
	new.data=df2[,new.data.cols]

	# datan jatkaminen
	df.block = df2[df2$year0==1,c(new.data.cols,'len')]
	extras=do.call(rbind, replicate(20, df.block[nrow(df.block),], simplify = FALSE))
	extras$rr=extras$rr+(1:nrow(extras))
	extras$len=extras$len+(1:nrow(extras))
	df.block=rbind(df.block,extras)

	new.data.ext = do.call(rbind,
		by(df2,df2[,'year0.factor'],function(x) {
			df.block$year0.factor=unique(x$year0.factor)[1]
			df.block$x0=min(x$x0)
			df.block
			})
		)
	# new.data.ext$year0=as.factor(new)
	wh = grepl('year0.factor',colnames(new.data.ext))
	colnames(new.data.ext)[wh]='year0'

	esti=predict(m,new.data.ext,interval='confidence')
	esti=(data.frame(exp(esti)))
	dftot=cbind(new.data.ext,esti)

	resolution=150
	offset=2
if ('stage-1' %in% plots) {
	filename=paste0('../fig/',cn,'-exponential-stage-1.png')
	png(filename,res=resolution,width=9*resolution,height=7*resolution)

	plot(x=NULL,ylim=c(0,50),xlim=c(2,55))
	for (i2 in 1:length(unique(dftot$year0))) {
		y2 = unique(dftot$year0)[i2]
		dfslc=dftot[dftot$year0==y2,]
		ddf = df[df$year0==y2,]

		y2=as.integer(y2)
		lines(dfslc$len,dfslc$fit+(offset*y2),lty='dashed')
		lines(dfslc$len,dfslc$lwr+(offset*y2),lty='dotted')	
		lines(dfslc$len,dfslc$upr+(offset*y2),lty='dotted')

		points(ddf$len,exp(ddf$y)+(offset*y2),lwd=2)
	}
	dev.off()
}

bsAssign('m')
bsAssign('sm')
# bsAssign('intercepts')
bsAssign('new.data.ext')
	rs0=sm$coeff[grep('^r[0-9]+$',rownames(sm$coeff)),'Estimate']
	# plot(rs)
	# plot(1/rs)
	ind=2:length(rs0)
	rs=rs0[ind]
	rs.inv=1/rs # nb! r1 is often negative, hence skipped here
	m2=lm(rs.inv~ind)
	print(summary(m2))
	# sm=summary(m)
	# sm
	new.data=data.frame(ind=1:years.ahead)
	est=data.frame(predict(m2,new.data,interval='confidence'))

	est0=data.frame(predict(m,unique(new.data.ext[,grepl('^r[0-9r]',colnames(new.data.ext))]),interval='confidence'))

	rs.est=est # rep(0,years.ahead)
	# rs.est[1:length(rs0)]=rs0
	rs.est[1:length(rs0),]=exp(est0[1:length(rs0),])
	est2=(1/est[(length(rs0)+1):years.ahead,][,c(1,3,2)])
	a1=rs.est[length(rs0),]
	a2=apply(est2,2,FUN=function(x) exp(cumsum(x)))
	for (i in 1:ncol(a1)) {
		rs.est[(length(rs0)+1):years.ahead,i]=a2[,i]*a1[1,i]
	}
	# prd=exp(cumsum(rs.est[1:years.ahead]))
	# prd=mean(intercepts)*prd
	# prd

bsAssign('rs.est')

	filename=paste0('../fig/',cn,'-exponential-stage-3.png')
	png(filename,res=resolution,width=9*resolution,height=7*resolution)
	plot(x=NULL,xlim=c(1,years.ahead),ylim=c(0,50))
	for (i in y0:nrow(distm)) {
		lines(1:years.ahead,rs.est$fit+(i-y0)*offset,lty='dashed')
		lines(1:years.ahead,rs.est$lwr+(i-y0)*offset,lty='dotted')
		lines(1:years.ahead,rs.est$upr+(i-y0)*offset,lty='dotted')
		points(1:ncol(distm),distm[i,]+(i-y0)*offset)
	}
	dev.off()

	# sapply(1:years.ahead,FUN=function(x) {
	#		prd=exp(cumsum(rs.est[1:x]))
	#		prd=mean(intercepts)*prd
	#		prd
	#	})

	# plot(new.data$ind,est$fit,type='l')
	# points(ind,rs.inv,col='red')

if ('stage-2' %in% plots) {
	filename=paste0('../fig/',cn,'-exponential-stage-2.png')
	png(filename,res=resolution,width=9*resolution,height=7*resolution)

	plot(new.data$ind,1/est$fit,type='l')
	points(ind,1/rs.inv,col='red')
	dev.off()
}
	# sm

	return(list(coeff=sm$coeff,data=ddf,estimate=esti,r.m=m,intercepts=NULL))
}

setwd('c:/hy-version/donor-recruitment-prediction/src')

# %%% read anonymous data files
## ----setup, include=FALSE,echo=FALSE------------------------------------------
# knitr::opts_chunk$set(echo = FALSE,warning=FALSE)
library(tidyverse)
library(openxlsx)
library(ggplot2) # # heatmaps etc
library(reshape2) # melt (needed in heatmaps)

## ----parameters,echo=FALSE----------------------------------------------------
source('functions-2.r')
param=list()
param$data.directory = 'C:/Users/super/OneDrive - University of Helsinki/veripalvelu/paper-1 long-term-predictions/data'


## ----read-files,echo=FALSE----------------------------------------------------
file.names = dir(path=param$data.directory,pattern="*.xlsx")
file.names = file.names[!grepl('~',file.names)]
file.names = file.names[!grepl('^old',file.names)]
file.names = file.names[grepl('.xlsx$',file.names)]
file.paths = paste(param$data.directory,'/',file.names,sep='')

countries = list()
gt = NULL
for (file in file.paths) {
	identifier = sub('.+[/\\]([a-z]+)[^/\\]+$','\\1',file) # gsub('.*\\\\(..).*\\.xlsx$','\\1',file)
	if (nchar(identifier) > 2) 
		next
	
	curr = list()
	res = list()
	
	sheet.names = getSheetNames(file)
	for (sn in sheet.names) {
		data = read.xlsx(file,colNames=TRUE,rowNames=TRUE,sheet = sn)
		if (grepl(' def(inition)?$',sn)) {
			if (is.null(gt)) {
				gt = data
			} else
				gt = rbind(gt,data)
		} else if (sn == 'parameters') {
			curr$parameters = data
		} else if (grepl('sizes',sn)) {
				grp = list()
				grp$sizes = data
				rownames(grp$sizes) = grp$sizes$year
		} else if (grepl('distm|dista',sn)) {
			kind = sub('.+(distm|dista)','\\1',sn)
			if (kind == 'distm') {
				# There is an error in data generation: the columns start repeating
				# Remove the extra columns here
				ncols = which.max(colSums(is.na(data)))
				if (length(ncols) > 0) {
					grp[[kind]] = as.matrix(data[,1:(ncols-0)])
				} else
					grp[[kind]]=NULL
			}
			if (kind == 'dista') {
				grp[[kind]] = data
				res[[length(res)+1]] = grp
			}
		} else if (grepl('^activity-stats',sn)) {
			data$delay = as.numeric(data$delay)
			data$prop = as.numeric(data$prop) 
			if (grepl('sex',sn))
				data = data[data$Sex!="",]
			curr[[gsub('-','.',sn)]] = data
		} else {
			print(paste('not processed',sn))
		}
	}
	
	curr$gt = gt
	gt = NULL
	curr$res = res
	countries[[identifier]] = curr
}

# read anonymous data files %%% -->

# %%% compute statistics for exploratory analysis
et=NULL # main exploratory data set
activity.stats=NULL # combined activity stats
activity.stats.sex=NULL # combined activity stats + grouped by sex
for (cn in names(countries)) {
	country = countries[[cn]]
	asincr = cbind(country=cn,country$activity.stats)
	asincr.sex = cbind(country=cn,country$activity.stats.sex)
	if (is.null(activity.stats)) {
		activity.stats = asincr
		activity.stats.sex = asincr.sex
	} else {
		activity.stats = rbind(activity.stats,asincr)
		activity.stats.sex = rbind(activity.stats.sex,asincr.sex)
	}
	for (i in 1:nrow(country$gt)) {
		gt.row = cbind(country=cn,country$gt[i,])
		rese = country$res[[i]]
		cdm = rese$distm
		pdm = cdm2pdm(cdm)
		
		m_longer = function(cdm) {
			cdm = data.frame(cdm)
			cdm = cbind(year0=rownames(cdm),cdm)
			cdm= pivot_longer(cdm,colnames(cdm)[2:ncol(cdm)])
			colnames(cdm)=c('year0','ord','value')
			cdm$ord = as.integer(sub('X','',cdm$ord))
			return(cdm)
		}
		
		dista = rese$dista
		cols = colnames(dista)[-1]

		# Remove the empty columns from the dista
		# All the rows may be lacking (i==15,cn='fi'), or there might be one present (i==15,cn='nl')
		# The apply will result in Warning: no non-missing arguments to max; returning -Inf if there are empty columns
		wh = which(apply(dista,2,max,na.rm=TRUE)<0.85)
		if (length(wh) > 0) {
			dista = dista[,-wh]
			cols = colnames(dista)[-1]
		}
		
		dista[,cols] = apply(data.frame(dista[,cols]),2,FUN=cumulativeToDensity)
		
		# Add an extra column to make sure some distribution will be available when using the closest <= condition below
		max.year = max(as.integer(colnames(dista[cols]),na.rm=FALSE))
		dista[['2100']] = dista[[as.character(max.year)]]

		avgage = dista %>%
			pivot_longer(colnames(dista)[-1]) %>%
			group_by(name) %>%
			summarise(avage=sum(age*value,na.rm=TRUE)) %>%
			mutate(year=as.integer(name))
		
		lcm=m_longer(cdm)
		lpm=m_longer(pdm)
		
		huu=cross_join(gt.row,lcm)
		colnames(huu)=sub('value','cdon',colnames(huu))
		hu2=inner_join(huu,lpm,join_by(year0,ord))
		colnames(hu2)=sub('value','don',colnames(hu2))
		hu2$year0=as.integer(hu2$year0)
		
		hu2=left_join(hu2,rese$sizes,join_by(x$year0==y$year))
		
		hu2 = left_join(hu2,avgage,join_by(closest(x$year0<=y$year)))
		hu2 = hu2[,-ncol(hu2)] # remove the age column coming from avgage
		hu2$avage = (hu2$avage + hu2$ord) - 1	# compute the average age for subsequent year
		hu2 = hu2[,!colnames(hu2) %in% c('name')]
		hu2$year = (hu2$year0+hu2$ord) - 1
		int.cols = c('age.lower','age.upper','MaximumAge','year','n')
		hu2[int.cols] = lapply(hu2[int.cols],as.integer)
		if (is.null(et)) {
			et = hu2
		} else
			et =rbind(et,hu2)
	}
}

# 2025-08-13
# select and normalize (no NA's, maximum value 1) the age distributions
# Selecting the most recent year with an age distribution of reasonable length
agedist=data.frame(country=character(),gr.name=character(),age=integer(),density=numeric())
for (cn in names(countries)) {
	# print(paste('*********',cn))
	for (i in 1:length(countries[[cn]]$res)) {
		# print(countries[[cn]]$res[[i]]$dista)
		dista=countries[[cn]]$res[[i]]$dista # drop the age column here
		ages=dista[,1]
		dista=dista[,-1]
		wh = 1:ncol(dista)
		wh=which(apply(data.frame(dista[,wh]),2,FUN=function(x) max(x,na.rm=TRUE) ) > 0.70)
		wh1=wh
		wh=which(apply(data.frame(dista[,wh]),2,FUN=function(x) sum(!is.na(x)) )>4)
		if (length(wh) == 0) {
			print('made the youngness assumption')
			wh = wh1
		} else {
		}

		dista0=data.frame(dista[,max(wh)])
		colnames(dista0)='density' # colnames(dista)[max(wh)]
		if (max(dista0,na.rm=TRUE) < 1) {
			wh.na=min(which(is.na(dista0)))
			dista0[wh.na,1]=1
		}
		# print(paste('selected',colnames(dista)[max(wh)],max(dista0,na.rm=TRUE)))
		dista0=cbind(age=ages,dista0)

		dista0=dista0[!is.na(dista0[,2]),]
		dista0[,2]=cumulativeToDensity(dista0[,2])
		countries[[cn]]$res[[i]]$dista0=dista0
		agedist=rbind(agedist,data.frame(country=cn,name=countries[[cn]]$gt[[i,'Name']],dista0))
	}
	# countries[[cn]]$agedist=agedist
}

# save
save(et,file=str_c(str_replace(param$data.directory,"/data","/results"),"/et.Rdata"))
save(activity.stats,file=str_c(str_replace(param$data.directory,"/data","/results"),"/activity.stats.Rdata"))
save(activity.stats.sex,file=str_c(str_replace(param$data.directory,"/data","/results"),"/activity.stats.sex.Rdata"))

et.noage = et %>%
	ungroup() %>%
	filter(age.upper<100,BloodGroup=='-O-') %>%
	group_by(country,DonationPlaceType,Sex,BloodGroup,Multiplier,MaximumAge,year0,ord,year) %>%
	summarise(Name=min(Name),cdon=sum(n*cdon)/sum(n),don=sum(n*don)/sum(n),n=sum(n),avage=sum(n*avage)/sum(n),.groups='drop') %>%
	mutate(age.lower=0,age.upper=100)
et.noage=et.noage[,colnames(et)]
et.noage$Name=sub(' [0-9].+','',et.noage$Name)

# Combine the new -O- with 0-100 age data from above with the O- rows
# In the combined data, there are no age groups
et.noage = bind_rows(et.noage,et %>% filter(BloodGroup=='O-'))
save(et,file=str_c(str_replace(param$data.directory,"/data","/results"),"/et.noage.Rdata"))

# Cut out the last, incomplete year; these might be complete ones as well
et.ord.max = et %>%
	group_by(year0,country) %>%
	summarise(ord.max=max(ord),.groups='drop') 

et = et %>%
	inner_join(et.ord.max,join_by(year0,country,x$ord<y$ord.max)) %>%
	dplyr::select(-ord.max)

## ----echo=FALSE---------------------------------------------------------------
# These values are experimental in the data, so quick-fix them here
countries$fi$parameters$reference.year = 2003
countries$fi$parameters$last.data.year = 2023

countries$nl$parameters$reference.year = 2011
countries$nl$parameters$last.data.year = 2023

countries$fr$parameters$reference.year = 2017
countries$fr$parameters$last.data.year = 2023

countries$au$parameters$reference.year = 2013
countries$au$parameters$last.data.year = 2023

countries$nc$parameters$reference.year = 2003
countries$nc$parameters$last.data.year = 2024

reference.years=data.frame(year=integer(),country=character())
for (cn in names(countries)) {
	reference.years[cn,'country']=cn
	reference.years[cn,'year']=countries[[cn]]$param$reference.year
}

colours=list()
colours$fi='darkblue'
colours$nl='orange'
colours$fr='red3'
colours$au='#007F3B' # 'green3'
colours$nc='black' # 'green3'

colfun = function(x) {
	colours[[x]]
}

nuisance.cols = c('Multiplier','MaximumAge','Name','avage')
dim.cols = c('country','age.lower','age.upper','DonationPlaceType','Sex','BloodGroup',nuisance.cols)
dim.keep = c('country','Sex') # this is the changing part

spec.list = list()
spec.list$country = list()
spec.list$country$dim.keep = c('country')
spec.list$country$pch = function(x) {15}
spec.list$country$colours = colours
spec.list$country$col.dim = 'country'
spec.list$country$pch.dim = 'country'

spec.list$Sex = list()
spec.list$Sex$dim.keep = c('Sex')
spec.list$Sex$pch = function(x) {2}
spec.list$Sex$colours = list(Male='blue3',Female='red3')
spec.list$Sex$col.dim = 'Sex'

spec.list$country.sex = list()
spec.list$country.sex$dim.keep = c('country','Sex')
spec.list$country.sex$pch = function(x) { pchs=list(Female=2,Male=6);  return(pchs[[x]])}
spec.list$country.sex$colours = colours
spec.list$country.sex$col.dim = 'country'
spec.list$country.sex$pch.dim = 'Sex'

spec.list$country.bloodgr = list()
spec.list$country.bloodgr$dim.keep = c('country','BloodGroup')
spec.list$country.bloodgr$pch = function(x) { pchs=list(); pchs[['-O-']]=4; pchs[['O-']]=1;  return(pchs[[x]])}
spec.list$country.bloodgr$colours = colours
spec.list$country.bloodgr$col.dim = 'country'
spec.list$country.bloodgr$pch.dim = 'BloodGroup'
spec=spec.list$country

# initialise the (x,sqrt.x) plotting area
plot(x=NULL,
	xlim=c(-0.4,0.0),ylim=c(1,6),
	# xlim=c(min(plotdata[[plot.terms[1]]],na.rm=TRUE),max(plotdata[[plot.terms[1]]],na.rm=TRUE)),
	# ylim=c(min(plotdata[[plot.terms[2]]],na.rm=TRUE),max(plotdata[[plot.terms[2]]],na.rm=TRUE)),
	xlab=plot.terms[1],ylab=plot.terms[2])

# 2025-07-09 There is quite a bit of variation in these
# Should specify the parameter if used; now multiple included
boxplot(Estimate~country,data=csm)

# 2025-07-08 This is the plot presented in the meeting
# The results are formed based on spec.list and et in getGroupEstimates.
# This function does some plotting.
source('functions-2.r')
res=150
# png('../fig/estimated-parameters.png',width=8*res,height=6*res,res=res)
par(mar=c(4,4,0.1,0.1))
plot(x=NULL,
	xlim=c(1,30),ylim=c(-3,40),
	# xlim=c(min(plotdata[[plot.terms[1]]],na.rm=TRUE),max(plotdata[[plot.terms[1]]],na.rm=TRUE)),
	# ylim=c(min(plotdata[[plot.terms[2]]],na.rm=TRUE),max(plotdata[[plot.terms[2]]],na.rm=TRUE)),
	xlab='years from first donation (till half of the expected maximum)',ylab='(estimated total) number of donations')

# getGroupEstimates(et,spec.list$country,lwd=3,plot='curve')
### dfr: results the sqrt-models estimated based on each year0 separately
dfr=NULL
for (i in -2:20) {
	res=getGroupEstimates(et,spec.list$country,lwd=3,plot='alt',year.offset=i,index=i+3)
	if (is.null(dfr)) {
		dfr=res$data
	} else
		dfr=rbind(dfr,res$data)
}

csm=compute.csm(dfr,plot=FALSE)

# Plot the points based on the combined sqrt regressions
for (cn in unique(csm$country)) {
	data=csm[csm$country==cn,]
	data$Estimate=as.numeric(data$Estimate)
print(paste(data[data$var=='x.half','Estimate'],data[data$var=='y.max','Estimate']))
	points(data[data$var=='x.half','Estimate'],data[data$var=='y.max','Estimate'],col=spec$colours[[cn]],pch=11,cex=1.2)
}

# 2025-07-19 This causes an error, but does not seem to do anything
# test2 = pivot_wider(data[,c('x','y','year0')],names_from='x',values_from='y') 
if (FALSE) {
test= dfr %>%
	filter(country=='cn') %>%
	filter(!is.na(cdon)) %>%
	dplyr::select(country,year0,ord,cdon) %>%
	data.frame()
test2 = pivot_wider(test,names_from='ord',values_from='cdon',id_cols=c('year0','country')) %>%
	arrange(country,year0)

for (cn in unique(test2$country)) {
	distm = test2 %>%
		filter(country==cn) %>%
		dplyr::select(-country) %>%
		data.frame()
	rownames(distm)=distm[,1]
	colnames(distm)=1:ncol(distm)
	distm=distm[,-1]
	distm
}
}

rv=getGroupEstimates(et,spec.list$country.bloodgr,plot='alt')
rv=getGroupEstimates(et,spec.list$country.sex,plot='alt')
rv=getGroupEstimates(et,spec.list$country,lwd=7,plot='alt')
legend(x='bottom',fill=unlist(colours),legend=names(colours))
legend(x='bottomright',pch=c(1,2,6,3,4),legend=c('all','Female','Male','-O-','O-'))
# end of the plot presented in the SanguinStats meeting --> %%% 

dev.off()

# the traditional csm-plots (written to files)
rv=compute.csm(dfr,plot='all',return.fit=TRUE)

# %%% iterate over the new exponential models

et.test = et %>%
	filter(!is.na(cdon),!is.na(don)) %>%
	# inner_join(reference.years,join_by(x$year0==y$year,country)) %>%
	group_by(!!!syms(setdiff(colnames(et),setdiff(dim.cols,spec$dim.keep)))) %>%
	summarise(cdon=sum(n*cdon),don=sum(n*don),.groups='drop') %>%
	group_by(!!!syms(c(spec$dim.keep,'year0','ord','year'))) %>%
	summarise(n2=sum(n),cdon=sum(cdon)/n2,don=sum(don)/n2,.groups='drop') 

# pah = dfr %>% arrange(country,year0,x) %>% data.frame(); pah[1:30,]
# 2025-07-19 checked that the sqrt estimates are correct
for (cn in names(countries)) {
	distm = et.test %>% 
		filter(country==cn) %>% 
		dplyr::select(cdon,ord,year0) %>%
		pivot_wider(values_from='cdon',names_from=c('ord')) %>%
		data.frame()
	rownames(distm)=distm$year0
	distm=distm[,2:ncol(distm)]
	colnames(distm)=1:ncol(distm)
	distm=as.matrix(distm)

	print(cn)
	rv=newRegression(distm,y0=3,cn=cn,plots=c('stage-1','stage-2'))
print(rs.est)
}


# end of computations --> %%%

## ----explore-data-examples,echo=FALSE-----------------------------------------
et %>%
	summarise(sum(n*don,na.rm=TRUE))

et %>%
	group_by(country) %>%
	summarise(sum(n*don,na.rm=TRUE))

# nb! The annual sum for case fi,2000 includes all the donations from those who first donated
# in 2000 during the first full year since the first date of donation. Hence the sums on the rows represent
# full years but do not match calendar years; the years are provided just for reference.
# In particular, the first year has about 1.5 times the donations, and the last year only some remaining bits
# since it was cut in April anyhow.
cydata = et %>%
	group_by(country,year) %>%
	summarise(sum(n*don,na.rm=TRUE)) %>%
	filter(year<2024)
colnames(cydata)[3]='value'

cymat = pivot_wider(cydata,id_cols=c('year'),names_from='country')
cymat=cymat[cymat$year<=2024,]
ggplot(cydata,														
			 aes(x = year,
					 y = value,
					 col = country,lwd=3)) + geom_line()

## ----estimate-models----------------------------------------------------------
# Here, the data is augmented with models estimated based on distm
for (m in names(countries)) {
	for (i in 1:length(countries[[m]]$res)) {
		params = countries[[m]]$parameters
		last.data.year = params$last.data.year
		ref.year = as.character(params$reference.year)
		distm=countries[[m]]$res[[i]]$distm

		m0 = estimate.predict(distm,ref.year=ref.year,last.data.year=last.data.year,years.ahead=55,main='',try.nls=TRUE)

		countries[[m]]$res[[i]]$index=i
		countries[[m]]$res[[i]]$m = m0$m
		countries[[m]]$res[[i]]$m.nls = m0$m.nls

		res.all = list()
		year0.lengths=apply(distm,1,function(x)sum(!is.na(x)))
		years.to.use = which(year0.lengths>=3)
		years.to.use = names(year0.lengths[years.to.use])
		for (year in years.to.use) {
			m0 = estimate.predict(distm,ref.year=year,last.data.year=last.data.year,years.ahead=55,main='',try.nls=TRUE)
			res=list()
			res$index=i
			res$year=year
			res$m = m0$m
			res$m.nls = m0$m.nls
			res.all[[year]]=res
		}
		countries[[m]]$res[[i]]$all=res.all
	}
}

names(countries[[m]]$res[[i]]$all[[1]])

# extract results from the baseline only
names(countries$fi$res[[1]])
summary(countries$fi$res[[1]]$m)
df.list=list()
for (m in names(countries)) {
	lv=lapply(countries$fi$res,FUN=function(x) {
		cf = summary(x$m)$coeff
		cf.nls = summary(x$m.nls)$coeff
		return(cbind(country=m,i=x$index,term=c(rownames(cf),rownames(cf.nls)),rbind(cf,cf.nls)))
		})
	df=data.frame(do.call(rbind,lv))
	df[,4:7]=apply(df[,4:7],1,as.numeric)
	df$i=as.integer(df$i)
	df.list[[paste0(m,'.m')]]=df
}
mcf=do.call(rbind,df.list)

# use all years with enough data points from distm's
df.list=list()
for (m in names(countries)) {
	lv.all=lapply(countries[[m]]$res,FUN=function(x) {
		# cf = summary(x$m)$coeff
		# cf.nls = summary(x$m.nls)$coeff
		res.all = x$all # this will be a list
		df.all = lapply(res.all,FUN=function(y) {
			cf = summary(y$m)$coeff
			cf=data.frame(cf)
			cf['r.squared','Estimate'] = summary(y$m)$r.squared
			if (!is.null(y$m.nls)) {
				cf.nls = summary(y$m.nls)$coeff
			} else 
				cf.nls=cf[0,]
			colnames(cf)=c('estimate','std.error','t.value','p.value')
			colnames(cf.nls)=c('estimate','std.error','t.value','p.value')
			return(cbind(country=m,i=y$index,year0=y$year,term=c(rownames(cf),rownames(cf.nls)),rbind(cf,cf.nls)))
		})
		df.all2=data.frame(do.call(rbind,df.all))
		return(df.all2)
		})
	df=data.frame(do.call(rbind,lv.all))
 	df$i=as.integer(df$i)
	df.list[[paste0(m,'.m')]]=df
}
mcf=do.call(rbind,df.list)
mcf[,5:8]=apply(mcf[,5:8],2,as.numeric)
rownames(mcf)=1:nrow(mcf)

str(et)

# haluttaisiin siis piirtää käyriä esim. kokonaisuuksina maittain
# nls(y~y1*exp(-lambda*x)+y0,data=sample2

plot.terms=c('lambda','y0')
plot.terms=c('x','sqrt.x')

testdata=mcf %>%
	filter(i==15,term %in% plot.terms)

plotdata=pivot_wider(testdata[,c('country','estimate','term','year0')],values_from='estimate',names_from=c('term'))

plotdata$col=sapply(plotdata$country,FUN=colfun)
plot(plotdata[[plot.terms[2]]],plotdata[[plot.terms[1]]],col=plotdata$col)
skip0=3

plot(x=NULL,xlim=c(min(plotdata[[plot.terms[2]]],na.rm=TRUE),max(plotdata[[plot.terms[2]]],na.rm=TRUE)),
	ylim=c(min(plotdata[[plot.terms[1]]],na.rm=TRUE),max(plotdata[[plot.terms[1]]],na.rm=TRUE)),
	xlab=plot.terms[2],ylab=plot.terms[1])
for (m in names(countries)) {
	cn.data=plotdata[plotdata$country==m,]
	cn.data=cn.data[skip0:nrow(cn.data),]
	print(m)
	print(colfun(m))
	print(max(cn.data[[plot.terms[2]]]))
	lines(cn.data[[plot.terms[2]]],cn.data[[plot.terms[1]]],col=colfun(m),lwd=3)
	points(cn.data[[plot.terms[2]]],cn.data[[plot.terms[1]]],col=colfun(m))
	points(cn.data[[plot.terms[2]]][1],cn.data[[plot.terms[1]]][1],col=colfun(m),pch=3)
	points(cn.data[[plot.terms[2]]][nrow(cn.data)],cn.data[[plot.terms[1]]][nrow(cn.data)],col=colfun(m),pch=8)
}

sqrt.param=list(fi=1,fr=1.2,au=2.3,nl=4)
xrange=0:20
plot(x=NULL,xlim=c(min(xrange),max(xrange)),ylim=c(0,20))
for (m in names(countries)) {
	lines(xrange,sqrt.param[[m]]*sqrt(xrange)-0.15*xrange,col=colfun(m),lwd=3)
}

## ----echo=FALSE---------------------------------------------------------------
# This is a slighty modified version of plot.estimated in blood-donor-recruitment-prediction.R
plot.estimated.countries = function(nres,gt,bundle=FALSE,years.ahead=55,main='') {
	# Here, gt contains reference year and last.data year values (per country)
	
	models=list()
	models.nls = list()
	for (i in 1:length(nres)) {
		group = nres[[i]]
		ms = estimate.predict(group$distm,
			ref.year=gt[i,'reference.year'],last.data.year=gt[i,'last.data.year'],
			main = if (bundle) '' else gt$Name[i],
			sub=paste('n=',dim(group$data)[1],sep=''),try.nls=TRUE)
		
		models[[i]] = ms$m
		models.nls[[i]] = ms$m.nls
	}
	
	mean.y = NULL
	
	# Create a plot with all the model estimates if requested
	if (bundle) {
		# nb! should set a reasonable value for the maximum
		plot(x=NULL,type='n',xlim=c(0,55),ylim=c(0,20),main=main,ylab='cumulative number of donations',xlab='years since first donation')
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
			min.len = min(length(models[[1]]$model$y),length(models[[2]]$model$y))
			lines(models[[1]]$model$x[1:min.len], models[[1]]$model$y[1:min.len]/models[[2]]$model$y[1:min.len],lwd=5,lty='dotted')
			mean.y=mean(models[[1]]$model$y[1:min.len]/models[[2]]$model$y[1:min.len])
			text(x=30,y=mean.y,labels=paste('ratio =',round(mean.y,3)))
		}
	}
	
	return(list(models=models,models.nls=models.nls))
}


## -----------------------------------------------------------------------------
# Plot the countrywise comparisons for each group
# nb!/todo Could also make comparisons based on the estimated parameters: integrate over a given range
for (i in 1:length(countries$nl$res)) {
	local.gt = NULL
	local.res = list()
	for (m in names(countries)) {
		if (is.null(local.gt)) {
			local.gt = countries[[m]]$gt[i,]
			ncol.gt = ncol(countries[[m]]$gt)
			rownames(local.gt) = m
		} else {
			local.gt[m,1:ncol.gt] = countries[[m]]$gt[i,]
		}
		local.res[[m]] = countries[[m]]$res[[i]]
		if (m == 'nl')
			local.res[[m]]$distm = local.res[[m]]$distm[,1:14]
		
		local.gt[nrow(local.gt),'reference.year'] = countries[[m]]$parameters$reference.year
		local.gt[nrow(local.gt),'last.data.year'] = countries[[m]]$parameters$last.data.year
	}
	
	# nb! hard-coded constant; the name is missing for some reason on the first line
	group.name = local.gt[2,'Name']
	local.gt$Name = names(countries)
	plot.estimated.countries(local.res,local.gt,bundle=TRUE,main=group.name)
}
# Change in hemoglobin accross countries
# Confidence intervals for predictions
# Countrwise comparisons
# Model could be estimated using splines
# Trends over time: some mixed model to see the trends over time
# Not yet hemoglobin
# 7th Jan
# Oneg thing to be included, highlight this; plots for this as well
# Split by
# - Oneg/others
# - Sex
# - (Age)
# - (Type of place)

## ----echo=FALSE---------------------------------------------------------------
country='fi'
for (i in 1:length(countries[[country]]$res)) {
	plotDistibutionMatrix(countries[[country]]$res[[i]]$distm,diff=FALSE,skip.years=1,main=countries[[country]]$gt$Name[i])
}


## ----compute-donation-amounts,echo=FALSE--------------------------------------
dona=list() 
for (m in names(countries)) {
	dona[[m]] = sumDonationAmounts(countries[[m]]$res,countries[[m]]$gt,FALSE,total.donations=NULL) 
}


## ----plot-donation-amounts,echo=FALSE-----------------------------------------
for (m in names(countries)) {
	plotDonationAmounts(dona[[m]],countries[[m]]$gt)
}


## -----------------------------------------------------------------------------
plot(x=NULL,type='n',xlim=c(0,200),ylim=c(0,100),xlab='number of previous donations',ylab='probability of next donation (%)')
for (i in 1:length(names(countries))) {
	stats = countries[[i]]$activity.stats
	lines(stats$ord,100*stats$prop,lwd=3,col=i+1)
}
legend('bottomright',legend=names(countries),fill=1+(1:length(countries)))

plot(x=NULL,type='n',xlim=c(0,200),ylim=c(0,600),xlab='number of previous donations',ylab='time till next donation')
for (i in 1:length(names(countries))) {
	stats = countries[[i]]$activity.stats
	lines(stats$ord,stats$delay,lwd=3,col=i+1)
}
legend('bottomright',legend=names(countries),fill=1+(1:length(countries)))

for (i in 1:length(names(countries))) {
	stats = countries[[i]]$activity.stats.sex
	plotPropBySex(stats,names(countries)[i])
}

for (i in 1:length(names(countries))) {
	stats = countries[[i]]$activity.stats.sex
	plotDelayBySex(stats,names(countries)[i])
}

# Divide both by sex
# Confidence intervals


## ----echo=FALSE---------------------------------------------------------------
local.gt = countries$fi$gt
local.gt$reference.year = countries$fi$parameters$reference.year
local.gt$last.data.year = countries$fi$parameters$last.data.year
mf=plot.estimated.countries(countries$fi$res,local.gt,bundle=FALSE)
str(mf[[1]])
mf$models.nls[[1]]

source('functions-2.r')

# extract the parameter estimates
for (nm in names(countries)) {
	mf=plot.estimated.countries(countries[[nm]]$res,countries[[nm]]$gt,bundle=FALSE)
	for (j in length(mf$models)) {
		coeff.lm = summary(mf$models[[i]])$coeff
		# coeff.lm$country=nm
		coeff.nls = summary(mf$models.nls[[i]])$coeff
		coeff.nls$country=nm
	}
}

## -----------------------------------------------------------------------------
# knitr::knit_exit()

