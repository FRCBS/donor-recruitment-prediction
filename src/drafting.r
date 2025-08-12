source('functions-2.r')
rv=getGroupEstimates2(et,spec,plot='curve',try.nls=TRUE)

data$x0=data$x-1
data$y0=data$cdon-1
plot((cdon-1)^2~x0,data=data,ylim=c(0,76)) # ,xlim=c(0,15),ylim=c(0,10))
plot

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

