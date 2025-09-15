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
print(file)
	identifier = sub('.+[/\\]([a-z]+)[^/\\]+$','\\1',file) # gsub('.*\\\\(..).*\\.xlsx$','\\1',file)
	if (nchar(identifier) > 2) 
		next
	
	curr = list()
	res = list()
	
	sheet.names = getSheetNames(file)
	for (sn in sheet.names) {
print(sn)
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
	print(cn)
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
		

		# 2025-09-03 the ct dista-data is broken, let's fix it with an artificial distribution
		if (is.data.frame(dista)) {
			dista[,cols] = apply(data.frame(dista[,cols]),2,FUN=cumulativeToDensity)
		} else {
			dista=data.frame(age=dista,"2012"=1/(1:length(dista)))
			colnames(dista)[2]=2011
		}
		
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

table(et$country)

# 2025-08-13
# select and normalize (no NA's, maximum value 1) the age distributions
# Selecting the most recent year with an age distribution of reasonable length
agedist=data.frame(country=character(),gr.name=character(),age=integer(),density=numeric())
for (cn in names(countries)) {
	break
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

		dista0= dista0 %>% 
			right_join(expand.grid(age=17:70,density0=0),join_by(age)) %>%
			# filter(is.na(density)) %>%
			mutate(density=max(density,density0,na.rm=TRUE))
	
		countries[[cn]]$res[[i]]$dista0=dista0
		agedist=rbind(agedist,data.frame(country=cn,name=countries[[cn]]$gt[[i,'Name']],dista0))
	}
	# countries[[cn]]$agedist=agedist
}

agedist %>% group_by(country) %>% summarise(age.max=max(age))

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
	filter(!is.na(cdon)) %>%
	group_by(year0,country) %>%
	summarise(ord.max=max(ord),.groups='drop') 

et = et %>%
	inner_join(et.ord.max,join_by(year0,country,x$ord<y$ord.max)) %>%
	dplyr::select(-ord.max)

et %>%
	filter(Name=='Male Office 0-25',country=='au') %>%
	arrange(year0,ord) %>%
	top_n(50)

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

# reference.years should no longer be used
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
colours$nc='black'
colours$ct='purple'

max.lookback=list()
max.lookback$fi=5+5
max.lookback$nl=5+5
max.lookback$fr=5+5
max.lookback$au=5+5
max.lookback$nc=5+5
max.lookback$ct=5+5

cn.names=list()
cn.names$fi='Finland'
cn.names$nl='Netherlands'
cn.names$fr='France'
cn.names$au='Australia'
cn.names$nc='Navarre'
cn.names$ct='Catalonia'

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
