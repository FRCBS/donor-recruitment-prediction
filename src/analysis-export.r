if (FALSE) {
	df=estimates0 %>% # this is an example; actually need to bind different models together and add labels
		filter(rw==rw0) %>% dplyr::select('prd.year','rw','n2','fit','lwr','upr','year0') %>%
		inner_join(grps,join_by(rw)) %>%
		dplyr::select(-rw) %>%
		mutate(blood.gr='all',id=paste0('estimate0/',year0,'/',blood.gr))
}

df = export.estimates

n2.data=estimates0 %>%
	filter(rw==rw0,x==1) %>%
	dplyr::select(year0,n2) %>%
	rename(year=year0,n=n2) %>%
	arrange(year) %>%
	mutate(estimate=NA,ci.low=NA,ci.upr=NA)
rws=(1:(2055-max(n2.data$year)))
n2.data[nrow(n2.data)+rws,'year']=max(n2.data$year,na.rm=TRUE)+rws 
data.frame(n2.data)
# n2.data=rbind(n2.data,data.frame(year=(max(n2.data$year)+1):2080,n=n2.data$n[nrow(n2.data)]))

n.wide = et %>%
	filter(ord==1) %>%
	mutate(bgr=sapply(BloodGroup,function(x) if(x=='O-') 'Oneg' else 'all')) %>%
	dplyr::select(country,bgr,n,year0) %>%
	pivot_wider(names_from=c('country','bgr'),values_from='n',values_fn=sum,names_sep='.') %>%
	arrange(year0)
rws=(1:(2055-max(n.wide$year0)))
n.wide[nrow(n.wide)+rws,'year0']=max(n.wide$year0)+rws
n.wide = fill(n.wide,colnames(n.wide)[-1],.direction='down')

# todo: must extend these to 2055 with the 
# should actually utilise the et0 structure: not that much cutting, undue

n2.col=which(colnames(df)=='n2')
colnames(df)[n2.col]='n2.value'
year0.col=which(colnames(df)=='year0')

hdr.nr=8

n.col.xlsx=ncol(df)+2+2

x=1:55
yrs=2000:(2000+55)
if (FALSE) {
	a=2.2
	b=0.52
	y=a*x^b
	plot(y~x)
	df2$fit=a*df2$x^b
} else {
	# This is what is actually needed in the exported document
	n2.new=estimates0 %>%
		filter(x==1) %>%
		inner_join(grps,join_by(x$rw==y$rw)) %>%
		dplyr::select(year0,country) %>%
		group_by(country) %>%
		summarise(year0=min(year0))

	df2=expand.grid(year0=yrs,x=x,country=(grps$country))
	df2$country=as.character(df2$country)
	df2=inner_join(df2,n2.new,join_by(country,x$year0>=y$year0),suffix=c('','.extra'))
	df2$prd.year=df2$year0+df2$x-1
	df2$fit=df2$x
	df2=df2[,-which(colnames(df2) %in% c('x','year0.extra')]
}

new.rows=nrow(df)+(1:nrow(df2))
df[new.rows,]=NA
for (col in colnames(df2)) {
	df[new.rows,col]=df2[,col]
}
df$id[new.rows]=paste0(df$country[new.rows],'/parameters/',df$prd.year[new.rows])

###
wb <- createWorkbook() 
addWorksheet(wb,"parameters") 
addWorksheet(wb,"nr of new donors") 
writeData(wb,'nr of new donors',n.wide)
writeData(wb,"parameters",x=df,startRow=hdr.nr)
writeData(wb,"parameters",n2.data,startCol=n.col.xlsx-1,startRow=hdr.nr)

n.frml=paste0('vlookup(',int2col(n.col.xlsx-1),hdr.nr+(1:nrow(n2.data)),',',
	'\'nr of new donors\'!$A$1:$',int2col(ncol(n.wide)),'$',nrow(n.wide)+1,',',
	'match(',int2col(n.col.xlsx),7,',\'nr of new donors\'!$A1:$',int2col(ncol(n.wide)),'1,0),FALSE)')
writeFormula(wb,"parameters",n.frml,startCol=n.col.xlsx,startRow=hdr.nr+1)

# references to the n-values in the estimate table
n.references=paste0(int2col(n.col.xlsx),hdr.nr+1+df$year0-2000)
writeFormula(wb,"parameters",n.references,startCol=n2.col,startRow=hdr.nr+1)

rws=hdr.nr+(1:nrow(df))
# computing the individual forecast as the product: fit*n
# this has been deprecated
for (i in 1:3) {
	frml.mult=paste0(int2col(which(colnames(df)=='fit')+i-1),rws,'*',int2col(n2.col),rws)
	# writeFormula(wb,"parameters",frml.mult,startCol=ncol(df)+i,startRow=hdr.nr+1)
}
# writeData(wb,'parameters',matrix(c('estimate','ci.low','ci.hi'),nc=3),startCol=ncol(df)+1,startRow=hdr.nr,colNames=FALSE)

writeData(wb,'parameters',c('multiplier','exponent'),startCol=n.col.xlsx+3,startRow=2,colNames=FALSE)
writeData(wb,'parameters',c(a,b),startCol=n.col.xlsx+4,startRow=2,colNames=FALSE)
a.ref=paste0('$',int2col(n.col.xlsx+4),'$',2)
b.ref=paste0('$',int2col(n.col.xlsx+4),'$',3)

styBold=createStyle(textDecoration='bold')

# parameter section
writeData(wb,'parameters',c('nl','Oneg','log.separately','fi','all'),startCol=n.col.xlsx,startRow=1)
writeData(wb,'parameters',c('Blood establishment','Blood gr','Model','Number of new donors, country','Number of new donors, blood group'),startCol=n.col.xlsx-2,startRow=1)
prm.col=int2col(n.col.xlsx)
concat.frml=paste0('if(',prm.col,'3="parameters",',
	paste0(paste(concat.frml=paste0(prm.col,c(1,3)),collapse='&"/"&'),'&"/",'),
	paste0(paste(concat.frml=paste0(prm.col,1:3),collapse='&"/"&'),'&"/"'),")")
writeFormula(wb,'parameters',concat.frml,startCol=n.col.xlsx,startRow=6)
prm.col=int2col(n.col.xlsx)
concat.frml=paste(concat.frml=paste0(prm.col,1:2),collapse='&"."&')
writeFormula(wb,'parameters',concat.frml,startCol=n.col.xlsx,startRow=7)
addStyle(wb,'parameters',styBold,cols=n.col.xlsx-2,rows=1:hdr.nr)

# Formula for the estimates computed based on parameters
rws=hdr.nr+new.rows
fit.col=which(colnames(df)=='fit')
# frml.fit=paste0(a.ref,'*',int2col(fit.col),rws,'^',b.ref)
frml.fit=paste0(a.ref,'*',df2$fit,'^',b.ref)
writeFormula(wb,'parameters',frml.fit,startCol=int2col(which(colnames(df)=='fit')),startRow=min(new.rows)+hdr.nr)

# summing the estimates for the final forecasts using sumif
# =SUMIF($C$7:$C$3679;U7;$T$7:$T$3678)
rws=hdr.nr+(1:nrow(df))
n.rws=hdr.nr+(1:nrow(n2.data))
prd.year.col=int2col(which(colnames(df)=='prd.year'))
sum.col=which(colnames(df)=='fit') # (ncol(df)+1)

st0=createStyle(numFmt='0')
# write also the condidence intervals
id.col=int2col(which(colnames(df)=='id'))
for (i in 0:2) {
	# =AB7*SUMIF($A$7:$A$898;$AA7;$E$7:E$898)
	# nb! This is old now, copied below
	frml.sumif=paste0(int2col(n.col.xlsx),n.rws,
		'*sumif($',prd.year.col,'$',rws[1],':$',prd.year.col,'$',rws[length(rws)],',',
		'$',int2col(n.col.xlsx),n.rws,',', # condition: prd.year
		'$',int2col(sum.col+i),'$',rws[1],':','$',int2col(sum.col+i),'$',rws[length(rws)],')')

	frml.sumif=paste0(int2col(n.col.xlsx),n.rws,
		'*sumif($',id.col,'$',rws[1],':$',id.col,'$',rws[length(rws)],',',
		'',int2col(n.col.xlsx),'$',hdr.nr-2,'&','$',int2col(n.col.xlsx-1),n.rws,',', # condition: /-separated string of prd.year, country, blood group, mode
		'$',int2col(sum.col+i),'$',rws[1],':','$',int2col(sum.col+i),'$',rws[length(rws)],')')

	writeFormula(wb,'parameters',frml.sumif,startCol=n.col.xlsx+1+i,startRow=hdr.nr+1)

	addStyle(wb,'parameters',st0,cols=n.col.xlsx+1+i,rows=(hdr.nr)+(1:length(frml.sumif)))
}

saveWorkbook(wb,"../tool.xlsx",overwrite=TRUE)

# sarakkeiden piilottaminen
# groupColumns(wb, sheet, cols, hidden = FALSE, level = -1)
# write.xlsx(df,'../tool.xlsx')

# getwd()
# system(paste('"C:\\Program Files\\Microsoft Office\\root\\Office16\\EXCEL.EXE"','../tool.xlsx'),intern=FALSE,ignore.stdout=FALSE,ignore.stderr=FALSE)

colToExcel=function(n) {
	res=''
	for (a in rev(1:3)) {
		val=(((n-1) %% 26))+1
		res=paste0(LETTERS[val],res)
		n=(n-val)/26
		if (n==0)
			return(res)
	}
	return(res)
}



