source('functions-2.r')
# plotEstimatesVsActual(et,estimates.year0.models,spec,ylim=c(100,2500),grps=grps,mode='mfrow')
rv.3p=getGroupEstimates2(et,spec,plot='curve',try.nls=FALSE,year0.ord=3:100,agedist=agedist)

# Must be moved to a better place
plotCountrySummaries(et,grps,list(main=estimates.year0.models,nofuture=estimates.year0.models.nofuture),spec,coeff.data)
# plotCountrySummaries(et,grps,list(main=estimates,nofuture=estimates.nofuture),spec,coeff.data)
# plotCountrySummaries(et,grps,estimates.year0.models.nofuture,spec,coeff.data)

library(xtable)

# nb! If smaller groups are used (even the original ones) and the donation forecasts are to be 
# aggregated, must add an extra step for this
# nb! should probably rename phase -> model for clarity
# Different groups might actually be correlated, so summing may be a necessity;
agedist=NULL

#######
# Each year separately
#######
# What are the parameters here?
#  - 
# str(tst)
# unique(tst[[1]]$prdct$phase)
model.pwr='don~x.pwr+x1' # 'log(don)~log(x)+x1' # tässä year0 aiheutti virheen: yksi vuosi tuli mukaan sekä häntään että muute
model='log(don)~log(x)+x1'
table(rlist[[1]]$prdct$phase)
cumulative=FALSE

rlist=lapply(1:25,FUN=function(x) getGroupEstimates2(et,spec,year0.ord=x,agedist=agedist,save.years.from.end=5))
grps=rlist[[1]]$grps
tst=rlist[lengths(rlist)!=0]
estimates0=do.call(rbind,lapply(tst,FUN=function(x) predictDonations2(x,model=model,cumulative=cumulative)))
estimates0.pwr=do.call(rbind,lapply(tst,FUN=function(x) predictDonations2(x,model=model.pwr,cumulative=cumulative)))

rlist=lapply(1:25,FUN=function(x) getGroupEstimates2(et,spec,year0.ord=x,agedist=agedist,save.years.from.end=5,
	filter.threshold=NULL))
tst=rlist[lengths(rlist)!=0]
estimates0.nofilter=do.call(rbind,lapply(tst,FUN=function(x) predictDonations2(x,model=model,cumulative=cumulative)))

tst2=getGroupEstimates2(et,spec,year0.ord=1:100,agedist=agedist,save.years.from.end=-5,save.years.overlap=5)
tst2.nooverlap=getGroupEstimates2(et,spec,year0.ord=1:100,agedist=agedist,save.years.from.end=-5,save.years.overlap=0)
tst2.nofilter=getGroupEstimates2(et,spec,year0.ord=1:100,agedist=agedist,save.years.from.end=-5,save.years.overlap=0,
	filter.threshold=NULL)

estimates.tail=predictDonations2(tst2,model=model,cumulative=cumulative,multiplier=1)
estimates.tail.pwr=predictDonations2(tst2,model=model.pwr,cumulative=cumulative,multiplier=1)
estimates.tail.nooverlap=predictDonations2(tst2.nooverlap,model=model,cumulative=cumulative,multiplier=1)

estimates.tail.nofuture=predictDonations2(tst2,model=model,cumulative=cumulative,multiplier=0)

# +filtering, +overlap
year.test=2015 # debug
estimates0 %>% filter(rw==4,prd.year==year.test) # debug
estimates.tail %>% filter(rw==4,prd.year==year.test) # debug

estimates.year0.models=rbind(estimates0,estimates.tail)
estimates.year0.models.pwr=rbind(estimates0.pwr,estimates.tail.pwr)
estimates.year0.models.nofilter=rbind(estimates0.nofilter,estimates.tail.nooverlap)
estimates.year0.models.nooverlap=rbind(estimates0,estimates.tail.nooverlap)
estimates.year0.models.ultimate=rbind(estimates0.pwr,estimates.tail)

estimates.year0.models.nofuture=rbind(estimates0,estimates.tail.nofuture)

getCoeff = function(x) {
	df.year0=x$prdct %>% group_by(rw) %>% summarise(year0=min(year0.lo))
	inner_join(x$coeff,df.year0,join_by(rw)) 
	}

# These are used to plot the trajectories
coeff.year0.models=rbind(do.call(rbind,lapply(tst,FUN=getCoeff)),getCoeff(tst2))

# estimates = estimates.year0.models
plotEstimatesVsActual(et,estimates.year0.models,spec,ylim=c(100,2500),grps=grps,mode='mfrow')
# coeff.data=plotCoeffData(coeff.year0.models,spec.list$country,grps,phase,dparam,vfun,FALSE)

# Mitä malleja haluttaisiin mukaan?
# yhtenä kokonaisuutena
# yksittäin, ei lisämausteita
# yksittäin, overlap, ei filtteröintiä
# lopullinen, eli yksittäin, overlap ja filtteröinti mukana

#######
# All years (after 2nd) as a lump
#######
model='log(don)~log(x)+0+year0+x1'
model.pwr='don~x.pwr+0+year0+x1'
rv.1=getGroupEstimates2(et,spec,plot='curve',try.nls=FALSE,year0.ord=1,agedist=agedist)
rv.2=getGroupEstimates2(et,spec,plot='curve',try.nls=FALSE,year0.ord=2,agedist=agedist)
rv.3p=getGroupEstimates2(et,spec,plot='curve',try.nls=FALSE,year0.ord=3:100,agedist=agedist)
rvs=list(rv.1,rv.2,rv.3p)
estimates=do.call(rbind,lapply(rvs,FUN=function(x) predictDonations2(x,model=model,cumulative=FALSE)))
estimates.nofuture=do.call(rbind,lapply(rvs,FUN=function(x) predictDonations2(x,model=model,cumulative=FALSE,multiplier=0)))
estimates.pwr=do.call(rbind,lapply(rvs,FUN=function(x) predictDonations2(x,model=model.pwr,cumulative=FALSE)))
plotEstimatesVsActual(et,estimates,spec,grps=grps) 
# plotEstimatesVsActual(et,estimates.nofuture,spec,grps=grps) 
# plotEstimatesVsActual(et,estimates.pwr,spec,grps=grps) 

######
# summaries of the methods
plotEstimatesVsActual(et,estimates.year0.models.nofilter,spec,grps=grps,filename=paste0(param$shared.dir,'fig/eva-no.overlap-no.filtering.png'),mode='mfrow')
plotEstimatesVsActual(et,estimates.year0.models.nooverlap,spec,grps=grps,filename=paste0(param$shared.dir,'fig/eva-no.overlap-filtering.png'),mode='mfrow')
plotEstimatesVsActual(et,estimates.year0.models,spec,grps=grps,filename=paste0(param$shared.dir,'fig/eva-overlap-filtering.png'),mode='mfrow')
plotEstimatesVsActual(et,estimates,spec,grps=grps,filename=paste0(param$shared.dir,'fig/eva-lump.png'),mode='mfrow')
plotEstimatesVsActual(et,estimates.pwr,spec,grps=grps,filename=paste0(param$shared.dir,'fig/eva-lump-pwr.png'),mode='mfrow')
plotEstimatesVsActual(et,estimates.year0.models.pwr,spec,grps=grps,filename=paste0(param$shared.dir,'fig/eva-overlap-filtering-pwr.png'),mode='mfrow')
plotEstimatesVsActual(et,estimates.year0.models.ultimate,spec,grps=grps,filename=paste0(param$shared.dir,'fig/eva-ultimate.png'),mode='mfrow')


#### Plotting the coefficients
filename=paste0(param$shared.dir,'fig/parameters.png')
# png.res=param$png.resolution
pdfOrPng(filename,width=7,height=7)
# pdf(filename,width=7,height=7)
par(mar=c(4.1,4.1,0.5,0.6)) # no space at the top
param.xlim=c(0.30,0.72)
param.ylim=c(1.2,3)
plot(x=NULL,xlim=param.xlim,ylim=param.ylim,xlab='exponent (b)',ylab='multiplier (exp(a))')

phase='log(cdon)~log(x)'
dparam=c('log.x.','.Intercept.')
vfun=c(NA,exp)
rv=getGroupEstimates2(et,spec.list$country,plot='curve',try.nls=FALSE,year0.ord=3:100,agedist=agedist)
unique(rv$coeff$phase)
plotCoeffData(rv$coeff,spec.list$country,rv$grps,phase,dparam,vfun)

rv=getGroupEstimates2(et,spec.list$country.sex,plot='curve',try.nls=FALSE)
plotCoeffData(rv$coeff,spec.list$country.sex,rv$grps,phase,dparam,vfun,TRUE)

rv=getGroupEstimates2(et,spec.list$country.bloodgr,plot='curve',try.nls=FALSE)
plotCoeffData(rv$coeff,spec.list$country.bloodgr,rv$grps,phase,dparam,vfun,TRUE)

rv=getGroupEstimates2(et,spec.list$country.age,plot='curve',try.nls=FALSE)
plotCoeffData(rv$coeff,spec.list$country.age,rv$grps,phase,dparam,vfun,TRUE)

rv$coeff[rv$coeff$phase=='log-log',]
rv$coeff %>% filter(phase==phase)

# contours
addCountours=function(x) {
	b = seq(param.xlim[1]-0.2,param.xlim[2]+0.2,len=50)
	for (u in c(5,7.5,10,15,20,25,30,35)) {
		# u = a·50^b -> a=u/50^b, log(u)=log(a)+b·log(50) -> b=(log(u)-log(a))/log(50)
		a = u/50^b
		lines(b,a,lty='dotted')
		b0=1.2
		text((log(u)-log(b0))/log(50),y=b0,labels=u)
	}
}
addCountours()

# legends
# spec.list$country.age$pch = function(x) { pchs=list(a0=7,a25=9,a40=12);  return(pchs[[paste0('a',x)]])}
legend('topright',pch=c(15,2,6,1,4,7,9,12),legend=c('all','female','male','O-','other than O-','age < 25','25≤age<40','age≥40'),bty='s',bg='white')
legend('topleft',fill=unlist(sapply(rv.3p$grps$country,FUN=colfun)),legend=sapply(rv.3p$grps$country,FUN=function(x) cn.names[[x]]),bty='s',bg='white')
dev.off()
#####

#####
# trajectories of parameters (from models estimated individually for each year)
filename=paste0(param$shared.dir,'fig/parameter-trajectories.png')
# png.res=100
pdfOrPng(filename,width=7,height=7)
par(mar=c(4.1,4.1,0.5,0.6)) # no space at the top
plot(x=NULL,xlim=param.xlim,ylim=param.ylim,xlab='exponent (b)',ylab='multiplier (exp(a))')
coeff.data=plotCoeffData(coeff.year0.models,spec.list$country,grps,phase,dparam,vfun,FALSE)
addCountours()
# legend('topright',pch=c(15,2,6,1,4),legend=c('all','female','male','O-','other than O-'))
legend('topleft',fill=unlist(sapply(rv.3p$grps$country,FUN=colfun)),legend=sapply(rv.3p$grps$country,FUN=function(x) cn.names[[x]]),bty='s',bg='white')
dev.off()

###############
### html-output
# table of r2-values
captions=list()
include.captions=TRUE

### table or r^2-values
r2.data=rv.3p$coeff %>% filter(!grepl('y~|(sqrt)',phase)) %>% filter(parameter=='r.squared') %>% dplyr::select(Estimate,rw,phase)
r2.2=pivot_wider(r2.data,values_from='Estimate',names_from='rw')
colnames(r2.2)=sapply(colnames(r2.2),FUN=function(x) cn.names[[grps[as.integer(x),'country']]])
colnames(r2.2)[1]='Model'
r2.2=cbind(r2.2,r2.mean=apply(r2.2[,2:ncol(r2.2)],1,mean))
r2.2 = r2.2 %>% arrange(r2.mean)
colnames(r2.2)[ncol(r2.2)]='Mean'
r2.2

wrL=grep('x1',r2.2$Model,value=TRUE)
nrl=setdiff(r2.2$Model,wrL)
wrl=sub('.x1','',wrL)
setdiff(nrl,wrl)

#   # font-family: times, serif;
#   white-space: nowrap;

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

table {
  border-collapse: collapse;
}

td, th {
  border: 1px solid #ffffff;
  text-align: right;
  padding: 4px;
  font-size: 11px;
}

table td:first-child {
    text-align: left;
}​

tr:nth-child(even) {
  background-color: #ffffff;
}
</style>
</head>
<body>
¤table¤
</body>
</html>"

html.table=paste(capture.output(print(xtable(r2.2,digits=5),type='html',include.rownames=FALSE)),collapse='\n')
caption='<b>Table C</b> Estimated coefficients of determination (R<sup>2</sup>) from different model specifications, data from the 3rd year onwards'
html.file=sub('¤table¤',paste0(caption,'\n',html.table),html.template)
cat(html.file,file=paste0(param$shared.dir,'table-c r2-values.html'))

###
# Parameter plots (these have been already written above to png files)
# <td style='text-align:center; vertical-align:middle'></td> 
html.table.parameters='<table><tr><td><img width=500 src="fig/parameters.png"></td><td><img width=500 src="fig/parameter-trajectories.png"></td></tr>
<tr><td style=\'text-align:center;\'>(a)</td><td style=\'text-align:center;\'>(b)</td></tr></table>'

captions$figure3='<b>Figure 3 </b>Estimated parameters for different blood establishments from model <span>log(cdon)~a+b&middot;log(x)</span>, where 
exp(a) and b are referred to as the multiplier and exponent, respectively. 
(a) All the years taken together (3rd year onwards). 
Significant differences between blood establishments and between groups within blood establishments can be observed. The dashed lines 
are contours of the estimated cumulative donations in 50 years. 
(b) Parameters for blood establishments estimated separately for each year in data. Dark colours represent more recent years.'

html.file=sub('¤table¤',paste(html.table.parameters,if(include.captions) captions$figure3 else '',sep='\n'),html.template)
# cat(html.file)
convertOutput(html.file,file=paste0(param$shared.dir,'figure-3 parameters.html')) 

###

###
# Model specification plots
html.table.specifications='<table><tr>
<td><img width=500 src="fig/eva-lump.png"></td>
<td><img width=500 src="fig/eva-lump-pwr.png"></td> </tr><tr>
<tr><td style=\'text-align:center;\'>(a) Years together, logarithmic for, filtering applied</td><td style=\'text-align:center;\'>(b) As in (a), but power form</td></tr>
<td><img width=500 src="fig/eva-no.overlap-no.filtering.png"></td>
<td><img width=500 src="fig/eva-overlap-filtering.png"></td> </tr><tr>
<tr><td style=\'text-align:center;\'>(c) Logarithmic form, years separately, no overlap, no filtering</td><td style=\'text-align:center;\'>(d) Logarithmic form, years separately, overlap and filtering</td></tr>
<td><img width=500 src="fig/eva-overlap-filtering-pwr.png"></td>
<td><img width=500 src="fig/eva-ultimate.png"></td> </tr><tr>
<tr><td style=\'text-align:center;\'>(e) Power form, years separately, overlap and filtering</td><td style=\'text-align:center;\'>(f)&nbsp;Head of (e) and tail of (d) amalgated</td></tr>
</table>'

# Estimated models with formula log(don) ~ log(x) + x<sub>1</sub> and different structures: 
captions$figure2='<b>Figure 2</b> Forecasts with different functional forms and model structures. The illustrations for 
each blood establishment have been scaled to roughly fit the designated height while keeping the 
positions of the actual donation symbols fixed between the panels.
(a)&nbsp;All years estimated together as a single model using the logarithmic form, filtering applied. 
The forecast errors exhibit some serial correlation, but overall the forecasts are the most accurate, resulting in 
the tightest confidence intervals of all the alternatives discussed in this figure.
(b)&nbsp;Similarly, but with the power form. The confidence intervals for forecasts are significantly wider 
for this specification, although the predictions for the past values are more accurate.
(c)&nbsp;All years but the last 5 estimated separately, no filtering applied and no overlap for the tail. The lack of 
filtering causes the effect of spikes, e.g., around year 2015 for Navarre, to be distribution both before and after the spike, 
leading to overly large estimates outside the spike, and underestimation during the spike. (d)&nbsp;All years but the last 5 estimated separately, 
both filtering and overlap at the tail applied. The additional effect of overlap affects the confidence intervals at the right tails,
especially prominently for Australia and the Netherlands. (e)&nbsp;The power functional form yields better forecasts for 
the years estimated individually, but is inferior to logarithmic form at the tails; filtering and overlap applied.
(f)&nbsp;The tail from panel (d) combined with the head from panel (e). Note that the tail also includes predictions from the head.'

html.file=sub('¤table¤',paste(html.table.specifications,if(include.captions) captions$figure2 else '',sep='\n'),html.template)
# cat(html.file)
convertOutput(html.file,file=paste0(param$shared.dir,'figure-2 model specifications.html'))
###


###
# Summary plots: each country individually
html.table.summaries='<table><tr>
<td><img width=500 src="fig/summary-au.png"></td>
<td><img width=500 src="fig/summary-ct.png"></td> </tr><tr>
<tr><td style=\'text-align:center;\'>(a) Australia</td><td style=\'text-align:center;\'>(b) Catalonia</td></tr>
<td><img width=500 src="fig/summary-fi.png"></td>
<td><img width=500 src="fig/summary-fr.png"></td> </tr><tr>
<tr><td style=\'text-align:center;\'>(c) Finland</td><td style=\'text-align:center;\'>(d) France</td></tr>
<td><img width=500 src="fig/summary-nc.png"></td>
<td><img width=500 src="fig/summary-nl.png"></td> </tr><tr>
<tr><td style=\'text-align:center;\'>(e) Navarre</td><td style=\'text-align:center;\'>(f) Netherlands</td></tr>
</table>'

captions$figure4='<b>Figure 4</b> Summary of estimated models and forecasted donations by country (panels a&ndash;f). In each panel, 
the top part contains the actual donation amounts (circles), estimated donations assuming constant number of annual new donors, 
and their confidence intervals (solid and dotted lines) and bars (number of new donors); an alternative scenario with now new donors 
is shown with dashed lines with confidence interval (dotted). 
Further, the bottom part contains statistics about donor activity and maturity: the cdon<sub>50</sub>
values and their confidence intervals are shown in solid and dashed lines, and the donor maturity, computed as the weighted average of 
years since first donation, with number of donations as the weights. Finally, the line with slope y=&half;x is drawn as a reference.'

html.file=sub('¤table¤',paste(html.table.summaries,if(include.captions) captions$figure4 else '',sep='\n'),html.template)
convertOutput(html.file,file=paste0(param$shared.dir,'figure-4 summaries.html'))

# flist <- list.files("../submit/","summary*", full.names = TRUE)
# file.copy(flist,param$shared.dir,overwrite=TRUE)
###

# captions$d='<b>Figure x</b> Forecasted donations compared with the actual historical donations'

# numbers of new donors
# data=sizes.data %>% filter(rw==5,year0<2025) %>% dplyr::select(n2,year0) 
# plot(n2~year0,data=data,ylim=c(0,max(data$n2)))

### Figure E (the explanation figure)
library(RColorBrewer)
n <- 21 # this is not used: the palette has a fixed number of colours
qual_col_pals = brewer.pal.info[brewer.pal.info$category=='qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

# palette(rainbow(n.classes))
plot.et.data = function(etd,i) {
	if (nrow(etd)==0) {
		print('returning')
		return()
	}
	rect(etd$ord-0.5,etd$year0.int-0.5,etd$ord-0.5+1,etd$year0.int-0.5+1,col=col_vector[i],border='white')
	text(etd$ord,etd$year0.int,labels=round(etd$cdon,1),cex=0.75)
}

plotActivityAndFit = function(tst.item,x,country='fi') {
	etd=tst.item$et.data %>% filter(country=='fi')
	fid=tst.item$prdct %>% inner_join(grps,join_by(rw)) %>% filter(country=='fi',phase=='log(cdon)~log(x)')
	points(etd$x,etd$cdon,col=col_vector[x])
	lines(fid$x,fid$fit,col=col_vector[x],lwd=3)
}

#### 
pdfOrPng(paste0(param$shared.dir,'fig/distm-and-curves.png'),height=8,width=8)
nf <- layout(
	matrix(c(1,2),ncol=1,byrow=TRUE), 
	heights=c(2,1)
)

par(mar=c(2.2,4.1,0.5,0.6)) # no space at the top
plot(NULL,xlim=c(0.5,25),ylim=rev(c(2000,2023)),xlab='years since first donation',ylab='year of first donation')
etd0=tst2$et.data %>% filter(country=='fi')
plot.et.data(etd0,length(col_vector))
arrows(0,min(etd0$year0.int)-0.5,0,max(etd0$year0.int)+0.5,lwd=2,angle=90,code=3,length=0.025) # sidebar
for (i in 1:length(tst)) {
	etd=tst[[i]]$et.data %>% filter(country=='fi')
	plot.et.data(etd,i)
}

plot(NULL,xlim=c(0.5,25),ylim=c(0,20),xlab='years since first donation',ylab='cumulative donations')
dummy=sapply(1:3,FUN=function(x) plotActivityAndFit(tst[[x]],x) )
plotActivityAndFit(tst2,length(col_vector))

dev.off()

captions$figure1='<b>Figure 1</b> An example of a distribution matrix at the top, and at the bottom, models fitted (lines) to the 
rows of the distribution matrix (lines) and the original data (circles).
The colours at the top and the bottom match each other. The predict future donations from the five final years (in yellow) 
and future years, a single model is fitted. In addition to these years themselves, additional years are included to 
achieve more accurate estimates for the paramters: these years are marked with the vertical bar between the matrix and axis.'

html.fragment='<img width=800 src="fig/distm-and-curves.png">\n<p>'

html.file=sub('¤table¤',paste(html.fragment,if(include.captions) captions$figure1 else '',sep='\n'),html.template)
convertOutput(html.file,file=paste0(param$shared.dir,'figure-1 distm-and-curves.html'))

# flist <- list.files("../submit/","figure-1*", full.names = TRUE)
# file.copy(flist,param$shared.dir,overwrite=TRUE)

list.of.legends=paste(sapply(sort(names(captions)),FUN=function(x) captions[[x]]),sep='\n<br><br>')
html.file=sub('¤table¤',paste0('<h2>Figure legends</h2>','\n',paste(list.of.legends,collapse='\n<p>')),html.template)
# cat(html.file)
cat(html.file,file=paste0(param$shared.dir,'list of legends.html'))

# table1
# - luovuttajien ja luovutusten kokonaismäärät
# - luovuttajien/luovutusten sukupuolijakauma
# - office/mobile-jakauma
# - veriryhmien osuudet luovuttajissa/luovutuksissa (vain O-/muut saatavilla)
# - ikäjakaumastsa jotakin: ensiluovuttajien keskimääräinen ikä (ei haittaa,)

# For the abstract etc.
et0 %>% filter(ord==1) %>% summarise(donors=sum(n))
et0 %>% summarise(donations=sum(n*don,na.rm=TRUE))

donors= et0 %>%
	filter(ord==1) %>%
	group_by(country) %>%
	summarise(donors=sum(n))

donors.oneg= et0 %>%
	filter(ord==1,BloodGroup=='O-') %>%
	group_by(country) %>%
	summarise(donors=sum(n))

donors.female= et0 %>%
	filter(ord==1,Sex=='Female') %>%
	group_by(country) %>%
	summarise(donors=sum(n))

donors.young= et0 %>%
	filter(ord==1,age.lower==0) %>%
	group_by(country) %>%
	summarise(donors=sum(n))

donors.middle= et0 %>%
	filter(ord==1,age.lower==25) %>%
	group_by(country) %>%
	summarise(donors=sum(n))

donations= et0 %>%
	filter() %>%
	group_by(country) %>%
	summarise(donations=sum(n*don,na.rm=TRUE))

donations.oneg= et0 %>%
	filter(BloodGroup=='O-') %>%
	group_by(country) %>%
	summarise(donations=sum(n*don,na.rm=TRUE))

donations.female= et0 %>%
	filter(Sex=='Female') %>%
	group_by(country) %>%
	summarise(donations=sum(n*don,na.rm=TRUE))

donations.young= et0 %>%
	filter(age.lower==0) %>%
	group_by(country) %>%
	summarise(donations=sum(n*don,na.rm=TRUE))

donations.middle= et0 %>%
	filter(age.lower==25) %>%
	group_by(country) %>%
	summarise(donations=sum(n*don,na.rm=TRUE))

processNumbers=function(category) {
	suffix=c('','.oneg','.female','.young','.middle')
	tmp=lapply(suffix,FUN=function(s) {
			data=get(paste0(category,s))
			cbind(col=paste0(category,s),pivot_wider(data,names_from='country',values_from=category))
		})
	df=do.call(rbind,tmp)
	for (rw in 2:nrow(df))
		df[rw,-1]=df[rw,-1]/df[1,-1]
	df
}

category=c('donors','donations')
tmp=lapply(category,FUN=processNumbers)
df=do.call(rbind,tmp)

perc.rows=which(grepl('\\.',df$col))
df[perc.rows,-1]=df[perc.rows,-1]*100
df[-perc.rows,-1]=df[-perc.rows,-1]/1000

df$col[-perc.rows]=firstUp(df$col[-perc.rows])
df$col[-perc.rows]=paste(df$col[-perc.rows],' (1,000)')
df$col[perc.rows]=sub('^',paste0(rep('&nbsp;',5),collapse=''),df$col[perc.rows])
df[perc.rows,2:ncol(df)]=apply(df[perc.rows,2:ncol(df)],2,FUN=function(x) sprintf('%.1f%%',x))
repl=list(oneg='O-',female='Female',young='age < 25',middle='25 <= age < 40')
for (rp in names(repl)) {
	df$col[perc.rows]=sub(paste0(';[^;]+',rp),paste0(';',repl[rp]),df$col[perc.rows])
}

colnames(df)[2:ncol(df)]=sapply(colnames(df)[2:ncol(df)],FUN=function(x) cn.names[[x]])
colnames(df)[1]=' '
###

html.table=paste(capture.output(print(xtable(df,align=c('l','l',rep('r',ncol(df)-1))),type='html',include.rownames=FALSE)),collapse='\n')
html.table=gsub('&amp;','&',html.table)
caption='<b>Table 1</b> Summary statistics of donors and donations'
html.file=sub('¤table¤',paste0(caption,'\n',html.table),html.template)
cat(html.file,file=paste0(param$shared.dir,'table-1.html'))

# Notes:
# removed fragment: “What if we would be able to make our donors donate like the Dutch?” or “What if we would not like to put any extra pressure on O- donors”.  
# [järjestys ja muut kirjoittajat; osoitteet ja mahdollisesti orcid:t muistettava kysyä (katsottava lehden vaatimuksista); Australiasta eettisen luvan numero tms.; funding; spreadsheet tool status; letters vs. numbers]

# EZPS
# 1C