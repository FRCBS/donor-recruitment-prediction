source('functions-2.r')

plot(x=NULL,xlim=c(0.33,0.72),ylim=c(1.2,3.1),xlab='exponent',ylab='coefficient')

rv=getGroupEstimates2(et,spec,plot='curve',try.nls=FALSE)

rv$prdct

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

### --- ennustekäyrät
for (rw in rv$grps$rw) {
	filename=paste0('../fig/',paste(grps[rw,],collapse='-'),'-predictions.png')
	resolution=150
	png(filename,res=resolution,width=9*resolution,height=7*resolution)
	plot(x=NULL,xlim=c(1,55),ylim=c(1,25))
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

