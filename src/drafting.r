source('functions-2.r')

#######
# Each year separately
#######
rlist=lapply(1:25,FUN=function(x) getGroupEstimates2(et,spec,year0.ord=x,agedist=agedist,save.years.from.end=5))
tst2=getGroupEstimates2(et,spec,year0.ord=1:100,agedist=agedist,save.years.from.end=-5)
tst=rlist[lengths(rlist)!=0]
# extract coefficients
coeff.year0.models=do.call(rbind,lapply(tst,FUN=function(x) cbind(x$coeff,year0=min(x$prdct$year0.lo))))

estimates0=do.call(rbind,lapply(tst,FUN=function(x) predictDonations2(x,model='cdon-x.a')))
estimates.tail=predictDonations2(tst2,model='cdon-x.a',multiplier=0)
estimates.year0.models=rbind(estimates0,estimates.tail)

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

# These are written to files by default
plotPredictions(rv.3p,models='all')

plotEstimatesVsActual(et,estimates,spec,main='Predictions with year0-factor',filename=paste0('../fig/estimate-vs-actual-discrete.png'))

estimates
# [1] 6206   18

# plotting the age distribution (by country) - roughly makes sense
pah5=pivot_wider(agedist.local[,c('age','density','country')],values_from='density',names_from=c('age'))
pah5
matplot(t(pah5),type='l')
legend(x='top',fill=1:5,legend=pah5$country)

prd.years=data.frame(prd.year=2024:(2024+10))
prd.data=pred.d

#### Kertoimien piirtäminen kuvaajaan
# Miten tämä siis pitäisi tehdä?
filename=paste0('../fig/estimates-2d.png')
png.res=100
png(filename,width=1080,height=1080,res=150) # ,width=png.res*9,height=png.res*6)
plot(x=NULL,xlim=c(0.33,0.72),ylim=c(1.2,3.1),xlab='exponent',ylab='multiplier')

# trajectories
# plotCoeffData(coeff.year0.models,spec.list$country,rv.3p$grps,phase,dparam,vfun,FALSE)

phase='log-log'
dparam=c('log.x.','.Intercept.')
vfun=c(NA,exp)
rv=getGroupEstimates2(et,spec.list$country,plot='curve',try.nls=FALSE,year0.ord=3:100,agedist=agedist)
plotCoeffData(rv$coeff,spec.list$country,rv$grps,phase,dparam,vfun)

# plotCoeffData(rv,spec,rv.1$grps,phase,dparam,vfun,error.bars=FALSE)

# points by year
# TODO The years could also be estimated separately, one line of distm per each round
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
legend('topleft',fill=unlist(sapply(rv.3p$grps$country,FUN=colfun)),legend=rv.3p$grps$country)
dev.off()
#####

#####
# trajectories
filename=paste0('../fig/estimates-2d-trajectories.png')
png.res=100
png(filename,width=1080,height=1080,res=150) # ,width=png.res*9,height=png.res*6)
plot(x=NULL,xlim=c(0.33,0.72),ylim=c(1.2,3.1),xlab='exponent',ylab='multiplier')
plotCoeffData(coeff.year0.models,spec.list$country,rv.3p$grps,phase,dparam,vfun,FALSE)
legend('topright',pch=c(15,2,6,1,4),legend=c('all','female','male','O-','other than O-'))
legend('topleft',fill=unlist(sapply(rv.3p$grps$country,FUN=colfun)),legend=rv.3p$grps$country)
dev.off()

param.log.intercept=rv$coeff[grepl('^log',rv$coeff$phase)&rv$coeff$param!='log(x)',]
rv$coeff[grepl('^log',rv$coeff$phase)&rv$coeff$param=='log(x)',]
summary(exp(param.log.intercept$Estimate))

lm.bs = function(y.max) {
	data2=data[as.integer(as.character(data$year0))>=2012,]
	y=y.max-data2$cdon
	log.y=log(y)
	m=lm(log.y~x+year0,data2)
	sm=summary(m)
	return(summary(m)$coeff[2,1])
	return(summary(m)$r.squared)
}

# ok, this works
fit.table=pivot_wider(rv$fit[rv$fit$rw==5,c('x','upr','year0')],names_from='x',values_from='upr')
ref.year=as.integer(as.character(as.data.frame(fit.table)[nrow(fit.table),1]))
fitted=predict(rv$m.year0[[5]],newdata=data.frame(x0=c(ref.year,2025)),interval='confidence')
correction=fitted[2]-fitted[1]
# forecast for the year 2025 (e.g.)
fit.table[nrow(fit.table),2:ncol(fit.table)]+correction

#####
# iterate over countries and groups; check dista's a pilot example
for (cn in names(countries)) {
	print(paste('*********',cn))
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
			# dista0=data.frame(dista[,max(wh1)])
			# print(dista0)
			wh = wh1
		} else {
		}

		dista0=data.frame(dista[,max(wh)])
		colnames(dista0)=colnames(dista)[max(wh)]
		if (max(dista0,na.rm=TRUE) < 1) {
			wh.na=min(which(is.na(dista0)))
			dista0[wh.na,1]=1
		}
		print(paste('selected',colnames(dista)[max(wh)],max(dista0,na.rm=TRUE)))
		dista0=cbind(age=ages,dista0)

		dista0=dista0[!is.na(dista0[,2]),]
		countries[[cn]]$res[[i]]$dista0=dista0
	}
}

dim(dista)
