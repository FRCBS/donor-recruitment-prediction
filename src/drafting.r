source('functions-2.r')
tst2=getGroupEstimates2(et,spec,year0.ord=1:100,agedist=agedist,save.years.from.end=-5,save.years.overlap=5)


# Must be moved to a better place
plotCountrySummaries(et,grps,estimates.year0.models,spec,coeff.data)

# This should be changed
shared.dir='C:/Users/super/OneDrive - University of Helsinki/veripalvelu/paper-1 long-term-predictions/long-term-predictions-manuscript/'

library(xtable)

# nb! If smaller groups are used (even the original ones) and the donation forecasts are to be 
# aggregated, must add an extra step for this
# These are written to files by default; just used for reference
# nb! should probably rename phase -> model for clarity
# nb! Something should be said about the confidence intervals
# Different groups might actually be correlated, so summing may be a necessity;
# no possibility to make them shorter. Should see if smaller groups yield more precise estimates in the first place
# Possibly extensions: 
#  - time series plots of cdon50; more readable
#  - average ord of donations: the first year will cause a problem, so this is bound to be distorted
# Must do:
#  - some confidence intervals needed

agedist=NULL

#######
# Each year separately
#######
# What are the parameters here?
#  - 
model='don-x.a+x1'
cumulative=FALSE
rlist=lapply(1:25,FUN=function(x) getGroupEstimates2(et,spec,year0.ord=x,agedist=agedist,save.years.from.end=5))
grps=rlist[[1]]$grps
tst=rlist[lengths(rlist)!=0]
estimates0=do.call(rbind,lapply(tst,FUN=function(x) predictDonations2(x,model=model,cumulative=cumulative)))
tst2=getGroupEstimates2(et,spec,year0.ord=1:100,agedist=agedist,save.years.from.end=-5,save.years.overlap=5)
estimates.tail=predictDonations2(tst2,model=model,cumulative=cumulative,multiplier=1)
table(estimates0$year0.hi)
table(estimates.tail$year0.lo)
table(estimates.tail$year0.hi)
estimates.year0.models=rbind(estimates0,estimates.tail)

# estimates.tail %>% filter(rw==3)

getCoeff = function(x) {
	df.year0=x$prdct %>% group_by(rw) %>% summarise(year0=min(year0.lo))
	inner_join(x$coeff,df.year0,join_by(rw)) 
	}

# These are used to plot the trajectories
coeff.year0.models=rbind(do.call(rbind,lapply(tst,FUN=getCoeff)),getCoeff(tst2))

# estimates = estimates.year0.models
plotEstimatesVsActual(et,estimates.year0.models,spec,ylim=c(100,2500),grps=grps)
coeff.data=plotCoeffData(coeff.year0.models,spec.list$country,grps,phase,dparam,vfun,FALSE)


plotEstimatesVsActual(et,estimates.year0.models,spec,main='Predictions with year0-specific models (5-year tail)',
	filename=paste0('../fig/estimate-vs-actual-discrete.png'))

#######
# All years (after 2nd) as a lump
#######
rv.1=getGroupEstimates2(et,spec,plot='curve',try.nls=FALSE,year0.ord=1,agedist=agedist)
rv.2=getGroupEstimates2(et,spec,plot='curve',try.nls=FALSE,year0.ord=2,agedist=agedist)
rv.3p=getGroupEstimates2(et,spec,plot='curve',try.nls=FALSE,year0.ord=3:100,agedist=agedist)
rvs=list(rv.1,rv.2,rv.3p)
estimates=do.call(rbind,lapply(rvs,FUN=function(x) predictDonations2(x,model='log-year0-log'))) # cdon.a-x-year0
# estimates=do.call(rbind,lapply(rvs,FUN=function(x) predictDonations2(x,model='don-x.a+year0+x1',cumulative=FALSE))) # cdon.a-x-year0

# plotPredictions(rv.3p,models='all')
plotEstimatesVsActual(et,estimates,spec,main='Predictions with years estimated as a lump with x1',grps=grps) # ,ylim=c(100,500))
plotEstimatesVsActual(et,estimates,spec,filename=paste0(shared.dir,'figure-d forecasted-donations.png'))

#### Plotting the coefficients
filename=paste0(shared.dir,'parameters.png')
png.res=150
png(filename,width=7*png.res,height=7*png.res,res=png.res)
par(mar=c(2.2,4.1,0.5,0.6)) # no space at the top
plot(x=NULL,xlim=c(0.30,0.72),ylim=c(1.2,3.3),xlab='exponent',ylab='multiplier')

phase='log-log'
dparam=c('log.x.','.Intercept.')
vfun=c(NA,exp)
rv=getGroupEstimates2(et,spec.list$country,plot='curve',try.nls=FALSE,year0.ord=3:100,agedist=agedist)
plotCoeffData(rv$coeff,spec.list$country,rv$grps,phase,dparam,vfun)

# points by year
# TODO The years could also be estimated separately, one line of distm per each round
# These are now excluded
dparam=c('log.x.','year0')
phase='log-year0-log'
# plotCoeffData(rv$coeff,spec,rv$grps,phase,dparam,vfun,FALSE)

rv=getGroupEstimates2(et,spec.list$country.sex,plot='curve',try.nls=FALSE)
phase='log-log'
dparam=c('log.x.','.Intercept.')
plotCoeffData(rv$coeff,spec.list$country.sex,rv$grps,phase,dparam,vfun,TRUE)

rv=getGroupEstimates2(et,spec.list$country.bloodgr,plot='curve',try.nls=FALSE)
plotCoeffData(rv$coeff,spec.list$country.bloodgr,rv$grps,phase,dparam,vfun,TRUE)

rv$coeff[rv$coeff$phase=='log-log',]
rv$coeff %>% filter(phase==phase)

# contours
b = seq(0.3,0.75,len=50)
for (u in c(5,7.5,10,15,20,25,30)) {
	# u = a·50^b -> a=u/50^b, log(u)=log(a)+b·log(50) -> b=(log(u)-log(a))/log(50)
	a = u/50^b
	lines(b,a,lty='dotted')
	b0=1.2
	text((log(u)-log(b0))/log(50),y=b0,labels=u)
}

# legends
legend('topright',pch=c(15,2,6,1,4),legend=c('all','female','male','O-','other than O-'))
legend('topleft',fill=unlist(sapply(rv.3p$grps$country,FUN=colfun)),legend=sapply(rv.3p$grps$country,FUN=function(x) cn.names[[x]]))
dev.off()
#####

#####
# trajectories of parameters (from models estimated individually for each year)
filename=paste0(shared.dir,'parameter-trajectories.png')
# png.res=100
png(filename,width=7*png.res,height=7*png.res,res=png.res)
par(mar=c(2.2,4.1,0.5,0.6)) # no space at the top
plot(x=NULL,xlim=c(0.3,0.72),ylim=c(1.2,5),xlab='exponent',ylab='multiplier')
coeff.data=plotCoeffData(coeff.year0.models,spec.list$country,grps,phase,dparam,vfun,FALSE)
# legend('topright',pch=c(15,2,6,1,4),legend=c('all','female','male','O-','other than O-'))
legend('topleft',fill=unlist(sapply(rv.3p$grps$country,FUN=colfun)),legend=sapply(rv.3p$grps$country,FUN=function(x) cn.names[[x]]))
dev.off()

# Miksi Australian vuodet ovat 2000-2010, vaikka data on vuosilta 2010-2023?
coeff.year0.models[coeff.year0.models$phase=='log-log'&coeff.year0.models$rw==1,]
table(coeff.year0.models$rw)

tst2$prdct[tst2$prdct$phase=='log-log'&tst2$prdct$rw==1,]

coeff.year0.models %>%
	group_by(rw) %>%
	summarise(min.year0=min(year0),max.year0=max(year0))

pt=getGroupEstimates2(et,spec,year0.ord=1,agedist=agedist,save.years.from.end=5)

# getStuff = function(pt,rw) pt[pt$phase=='log-log'&pt$rw==1,]

pt$prdct[pt$prdct$phase=='log-log'&pt$prdct$rw==1,]

# The numbers here are not extraorbitant
rv.3p$prdct[rv.3p$prdct$phase=='log-log'&rv.3p$prdct$rw==2,]

rv=rv.3p

options(warn=2)

tuh=predictDonations2(rv.3p,model='don-x.a+year0+x1',cumulative=FALSE)
str(tuh)
tuh[tuh$rw==2&tuh$prd.year==2003,]

tuh %>% filter(prd.year==2002)

estimates[estimates$rw==2&estimates$prd.year==2002,]

prd.data[prd.data$rw==2&prd.data$year0==2002,][1:10,]
prd.data=prd.data[prd.data$rw==2,]

str(rv.3p$prdct)
rv.1$prdct %>% filter(rw==2,phase=='don-x.a+x1',x==1)
rv.2$prdct %>% filter(rw==2,phase=='don-x.a+x1',x==1)
rv.3p$prdct %>% filter(rw==3,phase=='don-x.a+year0',x<5)

rv.3p$prdct %>% filter(rw==2,year0==2002,x<5)
table(rv.3p$prdct)

# ok, vika on siis lopulta ainakin 'don-x.a+year0+x1'-ennusteessa (olisikohan väärä power siellä)
# väärä selitettävä muuttuja, lisäksi x.pwr-arvot olivat pielessä

###############
### html-output
# table of r2-values
captions=list()

r2.data=rv.3p$coeff %>% filter(parameter=='r.squared') %>% dplyr::select(Estimate,rw,phase)
# pivot_wider(actual.don,values_from='don2',names_from=c('country'))) %>% arrange(year)
r2.2=pivot_wider(r2.data,values_from='Estimate',names_from='rw')
colnames(r2.2)=sapply(colnames(r2.2),FUN=function(x) cn.names[[grps[as.integer(x),'country']]])
colnames(r2.2)[1]='Model'
r2.2=cbind(r2.2,r2.mean=apply(r2.2[,2:ncol(r2.2)],1,mean))
r2.2 = r2.2 %>% arrange(r2.mean)
colnames(r2.2)[ncol(r2.2)]='Mean'
r2.2

html.template="<!DOCTYPE html>
<html>
<head>
<style>
html *
{
   font-size: 1em !important;
   color: #000 !important;
   font-family: Arial !important;
}
</style>
</head>
<body>
¤table¤
</body>
</html>"
html.table=paste(capture.output(print(xtable(r2.2,digits=5),type='html',include.rownames=FALSE)),collapse='\n')
caption='<b>Table P</b> Estimated coefficients of determination (R<sup>2</sup>) from different model specifications, data from the 3rd year onwards'
html.file=sub('¤table¤',paste0(caption,'\n',html.table),html.template)
cat(html.file)
cat(html.file,file=paste0(shared.dir,'table-c r2-values.html'))

# Parameter plots (these have been already written above to png files)
# <td style='text-align:center; vertical-align:middle'></td> 

html.table.parameters='<table><tr><td><img width=500 src="parameters.png"></td><td><img width=500 src="parameter-trajectories.png"></td></tr>
<tr><td style=\'text-align:center;\'>(a)</td><td style=\'text-align:center;\'>(b)</td></tr></table>'

captions$p='<b>Figure P </b>Estimated parameters for different blood services. (a) All the years taken together (3rd year onwards). 
Significant differences between blood services and betweeen groups within blood services can be observed. The dashed lines 
are contours of the estimated cumulative donations in 50 years. 
(b) Parameters for blood services estimated separately for each year in data. Dark colours represent more recent years.'

html.file=sub('¤table¤',html.table.parameters,html.template)
cat(html.file)
cat(html.file,file=paste0(shared.dir,'figure-p parameters.html'))

captions$d='<b>Figure D</b> Forecasted donations compared with the actual historical donations'
captions$e='<b>Figure E</b> An example of a distribution matrix at the top, and at the bottom, models fitted (lines) to the 
rows of the distribution matrix (lines) and the original data (circles).
The colours at the top and the bottom match each other. The predict future donations from the five final years (in yellow) 
and future years, a single model is fitted. In addition to these years themselves, additional years are included to 
achieve more accurate estimates for the paramters: these years are marked with the vertical bar between the matrix and axis.
'

list.of.legends=paste(sapply(sort(names(captions)),FUN=function(x) captions[[x]]),sep='<br><br>')
html.file=sub('¤table¤',paste0('<h2>Figure legends</h2>','\n',paste(list.of.legends,collapse='<p>')),html.template)
cat(html.file)
cat(html.file,file=paste0(shared.dir,'list of legends.html'))

# numbers of new donors
data=sizes.data %>% filter(rw==5,year0<2025) %>% dplyr::select(n2,year0) 
plot(n2~year0,data=data,ylim=c(0,max(data$n2)))

flist <- list.files("../submit/","summary*", full.names = TRUE)
file.copy(flist,shared.dir,overwrite=TRUE)

pdf('../submit/distm-sample.pdf',width=8,height=12)
par(mfrow=c(2,1))
plotDistibutionMatrix(countries$fi$res[[1]]$distm,skip=0)
plot(x=1:5)
dev.off()

### Figure E (the explanation figure)
library(RColorBrewer)
n <- 21 # this is not used
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

# palette(rainbow(n.classes))
plot.et.data = function(etd,i) {
	if (nrow(etd)==0) {
		print('returning')
		return()
	}
	print(dim(etd))
	rect(etd$ord-0.5,etd$year0.int-0.5,etd$ord-0.5+1,etd$year0.int-0.5+1,col=col_vector[i],border='white')
	text(etd$ord,etd$year0.int,labels=round(etd$cdon,1),cex=0.75)
}

plotActivityAndFit = function(tst.item,x,country='fi') {
	etd=tst.item$et.data %>% filter(country=='fi')
	fid=tst.item$prdct %>% inner_join(grps,join_by(rw)) %>% filter(country=='fi',phase=='log-log')
	points(etd$x,etd$cdon,col=col_vector[x])
	lines(fid$x,fid$fit,col=col_vector[x],lwd=3)
}

pdf('../submit/figure-e distm-and-curves.pdf',height=8,width=8)
nf <- layout(
	matrix(c(1,2),ncol=1,byrow=TRUE), 
	heights=c(2,1)
)

par(mar=c(2.2,4.1,0.5,0.6)) # no space at the top
plot(NULL,xlim=c(0.5,25),ylim=rev(c(2000,2023)),xlab='years since first donation',ylab='year of first donation')
etd0=tst2$et.data %>% filter(country=='fi')
plot.et.data(etd0,length(col_vector))
lines(c(0,0),c(min(etd0$year0.int)-0.5,max(etd0$year0.int)+0.5),lwd=3)
for (i in 1:length(tst)) {
	etd=tst[[i]]$et.data %>% filter(country=='fi')
	plot.et.data(etd,i)
}

plot(NULL,xlim=c(0.5,25),ylim=c(0,20),xlab='years since first donation',ylab='cumulative donations')
sapply(1:3,FUN=function(x) plotActivityAndFit(tst[[x]],x) )
plotActivityAndFit(tst2,length(col_vector))
dev.off()

# table1
# - luovuttajien ja luovutusten kokonaismäärät
# - luovuttajien/luovutusten sukupuolijakauma
# - office/mobile-jakauma
# - veriryhmien osuudet luovuttajissa/luovutuksissa (vain O-/muut saatavilla)
# - ikäjakaumastsa jotakin: ensiluovuttajien keskimääräinen ikä (ei haittaa,)
