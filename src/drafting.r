source('functions-2.r')

rlist=lapply(1:25,FUN=function(x) getGroupEstimates2(et,spec,year0.ord=x,agedist=agedist,save.years.from.end=5))
tst2=getGroupEstimates2(et,spec,year0.ord=1:100,agedist=agedist,save.years.from.end=-5)
tst=rlist[lengths(rlist)!=0]
# extract coefficients
coeff.year0.models=do.call(rbind,lapply(tst,FUN=function(x) cbind(x$coeff,year0=min(x$prdct$year0.lo))))

estimates0=do.call(rbind,lapply(tst,FUN=function(x) predictDonations2(x,model='cdon-x.a')))
estimates.tail=predictDonations2(tst2,model='cdon-x.a')
estimates.year0.models=rbind(estimates0,estimates.tail)

tst2$prdct %>%
	filter(phase=='cdon-x.a') %>%
	arrange(rw)

estimates.tail[,1:15] %>%
	arrange(rw,year) %>%
	top_n(50)

estimates %>%
	group_by(rw,year0) %>%
	summarise(n=n(),max(year0),.groups='drop') %>%
	arrange(rw,year0) %>%
	data.frame()

plotEstimatesVsActual(et,estimates,spec,main='Predictions with year0-specific models (5-year tail)')

rv.1=getGroupEstimates2(et,spec,plot='curve',try.nls=FALSE,year0.ord=1,agedist=agedist)
rv.2=getGroupEstimates2(et,spec,plot='curve',try.nls=FALSE,year0.ord=2,agedist=agedist)
rv.3p=getGroupEstimates2(et,spec,plot='curve',try.nls=FALSE,year0.ord=3:100,agedist=agedist)
rvs=list(rv.1,rv.2,rv.3p)
estimates=do.call(rbind,lapply(rvs,FUN=function(x) predictDonations2(x,model='cdon-x.a+year0'))) # cdon.a-x-year0

# These are written to files by default
plotPredictions(rv.3p)

plotEstimatesVsActual(et,estimates,spec,main='Predictions with year0-factor')


estimates
# [1] 6206   18

# plotting the age distribution (by country) - roughly makes sense
pah5=pivot_wider(agedist.local[,c('age','density','country')],values_from='density',names_from=c('age'))
pah5
matplot(t(pah5),type='l')
legend(x='top',fill=1:5,legend=pah5$country)

tmp=by(rv.1$prdct,rv.1$prdct[,c('phase','rw')],FUN=prd.cumulative2density)
pred.d=array2DF(tmp)
pred.d=pred.d[,3:ncol(pred.d)]
str(pred.d)

pred.d[1:10,]

# Tätä ei ehkä sittenkään tarvita
prdct.all = rbind(rv.1$prdct,rv.2$prdct,rv.3p$prdct)

sizes = rv.1$data %>%

prd.years=data.frame(prd.year=2024:(2024+10))
prd.data=pred.d

#### Kertoimien piirtäminen kuvaajaan
# Miten tämä siis pitäisi tehdä?
plot(x=NULL,xlim=c(0.33,0.72),ylim=c(1.2,3.1),xlab='exponent',ylab='coefficient')

phase='log-log'
dparam=c('log.x.','.Intercept.')
vfun=c(NA,exp)
plotCoeffData(rv$coeff,spec,rv$grps,phase,dparam,vfun)

plotCoeffData(rv,spec,rv.1$grps,phase,dparam,vfun,error.bars=FALSE)

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
for (u in c(5,10,15,20,25,30)) {
	# u = a·50^b -> a=u/50^b, log(u)=log(a)+b·log(50) -> b=(log(u)-log(a))/log(50)
	a = u/50^b
	lines(b,a,lty='dotted')
	b0=1.2
	text((log(u)-log(b0))/log(50),y=b0,labels=u)
}

# arrows(x, avg-sdev, x, avg+sdev, length=0.05, angle=90, code=3)
param.log.intercept=rv$coeff[grepl('^log',rv$coeff$phase)&rv$coeff$param!='log(x)',]

rv$coeff[grepl('^log',rv$coeff$phase)&rv$coeff$param=='log(x)',]
summary(exp(param.log.intercept$Estimate))

# This is not actually needed, after all (join is done within the function)
data=rv$coeff %>%
	inner_join(rv$grps,join_by(rw))


dev.off()

data$x0=data$x-1
data$y0=data$cdon-1
plot((cdon-1)^2~x0,data=data,ylim=c(0,76)) # ,xlim=c(0,15),ylim=c(0,10))

data

m=lm(log(y0)~log(x),data=data)
summary(m)

m=lm(log(cdon)~log(x),data=data)
summary(m)

# y=b·x^a
# ln(y)=ln(b·x^a)=ln(b) + a·ln(x)
# intercept ~ ln(b) => b=exp(intercept)
# slope ~ a

str(rv)

y.max=50
as.character(data$year0)

lm.bs = function(y.max) {
	data2=data[as.integer(as.character(data$year0))>=2012,]
	y=y.max-data2$cdon
	log.y=log(y)
	m=lm(log.y~x+year0,data2)
	sm=summary(m)
	return(summary(m)$coeff[2,1])
	return(summary(m)$r.squared)
}

plot(cdon^3~x,data=data2)
for(yr in unique(data2$year0)) {
	data3=data2[data2$year0==yr,]
	lines(data3$x,data3$cdon^3,col=as.integer(yr))
}

# viimeinen piste tosiaan myös otettava pois

plot(don~x,data=data2)
plot(1/log(don)~x,data=data2)

1/log(data$don)

plot(log.y~data$x)

plot(log.y~data$x)

y0=max(data$cdon)
delta=0.01
incr=10
y0.lo=y0+delta
y0.hi=y0+incr
y=seq(y0.lo,y0.hi,length.out=10)

pah=sapply(y,FUN=lm.bs)
pah
plot(pah)

while (TRUE) {
	rs.low=lm.bs()
}

rv$coeff

names(rv)

rv$grps
rv$m.year0

rv$coeff
rv$fit
names(rv)

rv$fit
# todo: pitäisi tarkistaa, onko näissä eroa sen suhteen, mikä on ennustejakson pituus

# ok, this works
fit.table=pivot_wider(rv$fit[rv$fit$rw==5,c('x','upr','year0')],names_from='x',values_from='upr')
ref.year=as.integer(as.character(as.data.frame(fit.table)[nrow(fit.table),1]))
fitted=predict(rv$m.year0[[5]],newdata=data.frame(x0=c(ref.year,2025)),interval='confidence')
correction=fitted[2]-fitted[1]
# forecast for the year 2025 (e.g.)
fit.table[nrow(fit.table),2:ncol(fit.table)]+correction

# The confidence intervals are not equal among different year0-levels
# Major differences

tt=fit.table[,2:ncol(fit.table)]
tt[10,]-tt[15,]

t.lwr=fit.table
t.upr=fit.table

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
