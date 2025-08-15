source('functions-2.r')

plot(x=NULL,xlim=c(0.33,0.72),ylim=c(1.2,3.1),xlab='exponent',ylab='coefficient')

rv.1=getGroupEstimates2(et,spec,plot='curve',try.nls=FALSE,year0.ord=1,agedist=agedist)
rv.2=getGroupEstimates2(et,spec,plot='curve',try.nls=FALSE,year0.ord=2,agedist=agedist)
rv.3p=getGroupEstimates2(et,spec,plot='curve',try.nls=FALSE,year0.ord=3:100,agedist=agedist)
rvs=list(rv.1,rv.2,rv.3p)
estimates=do.call(rbind,lapply(rvs,FUN=predictDonations2))

data.frame(rv.1$agedist)

# toteutuneet luovutusmäärät
actual.don = et %>%
	filter(!is.na(cdon),!is.na(don)) %>%
	group_by(!!!syms(c('year',spec$dim.keep))) %>%
	summarise(don2=sum(n*don),.groups='drop') %>%
	arrange(country,year)

data.frame(actual.don)
str(actual.don)
df.ad=data.frame(pivot_wider(actual.don,values_from='don2',names_from=c('country')))
df.ad = df.ad %>% arrange(year)
rownames(df.ad)=as.character(df.ad$year)
# df.ad=df.ad[,-1]
matplot(df.ad,type='l')

dev.off()
tst=predictDonations2(rv.1) %>%
	arrange(rw,prd.year,year0)

pah=estimates %>% 
	group_by(rw,prd.year) %>%
	summarise(est=sum(est),est.trnc=sum(est.trnc),.groups='drop') %>% 
	rename(year=prd.year) %>%
	inner_join(rv.1$grps,join_by(rw)) %>%
	data.frame()

df3=data.frame(pivot_wider(pah[,!colnames(pah) %in% c('rw','est')],values_from='est.trnc',names_from=c('country')))
# df3 =df3[,-1]

dev.off()
plot(NULL,xlim=c(2000,2035),ylim=c(0,3e6))
countries=grep('..',colnames(df3),value=TRUE)
for (cn in countries) {
	lines(df3$year,df3[[cn]],type='l',lwd=2,lty='solid',col=colfun(cn)) # col=unlist(sapply(colnames(df3),FUN=colfun)))
	points(df.ad$year,df.ad[[cn]],type='p',col=colfun(cn))
}
# points(x=1:20,y=rep(3e5,20)) # ok, this works

# todo 

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

str(prd.data)
str(rv.1$data)

predictDonations2 = function(rv,prd.start=2000,prd.len=55) {
	tmp=by(rv$prdct,rv$prdct[,c('phase','rw')],FUN=prd.cumulative2density)
	pred.d=array2DF(tmp)
	pred.d=pred.d[,3:ncol(pred.d)]
	prd.years=data.frame(prd.year=prd.start:(prd.start+prd.len)) # nb! hard-coded parameters
	prd.data=pred.d

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

	# 2025-08-14 tähän pitää lisätä agedist, jos saatavilla
	# liitettävä rw-perusteella oikeastaan missä tahansa vaiheessa mukaan
	# laskettava ikä vuonna prd.year=year0+x(+1) ja sovellettava tähän ehtoja between(17,65)
	# 2025-08-15 Tähän olisi lisättävä niin, että vuosiin ei varmasti jää aukkoja
	# tiheysjakaumaan voi lisätä nollat näille paikoille
	pah=cross_join(prd.years,sizes.data) %>%
		inner_join(prd.data[prd.data$phase=='cdon.a-x',],
			join_by(rw,between(x$year0,y$year0.lo,y$year0.hi))) %>%
		mutate(year=year0+x-1) %>%
		filter(year==prd.year) %>%
		mutate(est=n2*fit,est.lo=n2*lwr,est.hi=n2*upr)

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
		mutate(est.trnc=est*cumulative,est.lo=est.lo*cumulative,est.hi=est.hi*cumulative)

	return(pah2)
}

# Miten tämä siis pitäisi tehdä?

phase='log-log'
dparam=c('log.x.','.Intercept.')
vfun=c(NA,exp)
plotCoeffData(rv$coeff,spec,rv$grps,phase,dparam,vfun)

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

### --- ennustekäyrät: tuotetaan kuvat vertailua varten
plotPredictions = function(rv,xlim=c(1,55),ylim=c(1,25)) {
	for (rw in rv$grps$rw) {
		filename=paste0('../fig/',paste(grps[rw,],collapse='-'),'-predictions.png')
		resolution=150
		png(filename,res=resolution,width=9*resolution,height=7*resolution)
		plot(x=NULL,xlim=xlim,ylim=ylim)
		phases=unique(rv$prdct$phase)
		
		col=1
		for (ph in phases) {
			data5=rv$prdct[rv$prdct$phase==ph&rv$prdct$rw==rw,]
			lines(data5$x,data5$fit,lty='solid',lwd=3,col=col)
			lines(data5$x,data5$lwr,lty='dashed',lwd=1.5,col=col)
			lines(data5$x,data5$upr,lty='dashed',lwd=1.5,col=col)
			col=col+1
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

#### 

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
