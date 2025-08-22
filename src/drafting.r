source('functions-2.r')

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

#######
# Each year separately
#######
# What are the parameters here?
#  - 
model='don-x.a'
cumulative=FALSE
rlist=lapply(1:25,FUN=function(x) getGroupEstimates2(et,spec,year0.ord=x,agedist=agedist,save.years.from.end=5))
tst=rlist[lengths(rlist)!=0]
estimates0=do.call(rbind,lapply(tst,FUN=function(x) predictDonations2(x,model=model,cumulative=cumulative)))
tst2=getGroupEstimates2(et,spec,year0.ord=1:100,agedist=agedist,save.years.from.end=-5)
estimates.tail=predictDonations2(tst2,model=model,cumulative=cumulative,multiplier=1)
estimates.year0.models=rbind(estimates0,estimates.tail)

coeff.year0.models=do.call(rbind,lapply(tst,FUN=function(x) cbind(x$coeff,year0=min(x$prdct$year0.lo)))) 

# estimates = estimates.year0.models
plotEstimatesVsActual(et,estimates.year0.models,spec)

plotEstimatesVsActual(et,estimates.year0.models,spec,main='Predictions with year0-specific models (5-year tail)',filename=paste0('../fig/estimate-vs-actual-lump.png'))

#######
# All years (after 2nd) as a lump
#######
rv.1=getGroupEstimates2(et,spec,plot='curve',try.nls=FALSE,year0.ord=1,agedist=agedist)
rv.2=getGroupEstimates2(et,spec,plot='curve',try.nls=FALSE,year0.ord=2,agedist=agedist)
rv.3p=getGroupEstimates2(et,spec,plot='curve',try.nls=FALSE,year0.ord=3:100,agedist=agedist)
rvs=list(rv.1,rv.2,rv.3p)
estimates=do.call(rbind,lapply(rvs,FUN=function(x) predictDonations2(x,model='cdon-x.a+year0'))) # cdon.a-x-year0
estimates=do.call(rbind,lapply(rvs,FUN=function(x) predictDonations2(x,model='don-x.a+year0',cumulative=FALSE))) # cdon.a-x-year0

#
predictDonations2(rv.2,model='cdon-x.a+year0')
which(rv.2$prdct$lrw>rv.2$prdct$upr)
which(rv.3p$prdct$lrw>rv.3p$prdct$upr)
which(estimates$est.lo>estimates$est.hi)
estimates[1:10,]
# drafting ends

plotPredictions(rv.3p,models='all')
plotEstimatesVsActual(et,estimates,spec,main='Predictions with years estimated as a lump')
plotEstimatesVsActual(et,estimates,spec,main='Predictions with years estimated as a lump',filename=paste0('../fig/estimate-vs-actual-discrete.png'))

#### Plotting the coefficients
filename=paste0('../fig/estimates-2d.png')
png.res=100
png(filename,width=1080,height=1080,res=150) # ,width=png.res*9,height=png.res*6)
plot(x=NULL,xlim=c(0.33,0.72),ylim=c(1.2,3.1),xlab='exponent',ylab='multiplier')

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
filename=paste0('../fig/estimates-2d-trajectories.png')
png.res=100
png(filename,width=1080,height=1080,res=150) # ,width=png.res*9,height=png.res*6)
plot(x=NULL,xlim=c(0.33,0.72),ylim=c(1.2,3.1),xlab='exponent',ylab='multiplier')
plotCoeffData(coeff.year0.models,spec.list$country,rv.3p$grps,phase,dparam,vfun,FALSE)
legend('topright',pch=c(15,2,6,1,4),legend=c('all','female','male','O-','other than O-'))
legend('topleft',fill=unlist(sapply(rv.3p$grps$country,FUN=colfun)),legend=rv.3p$grps$country)
dev.off()
