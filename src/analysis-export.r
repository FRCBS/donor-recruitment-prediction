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
	arrange(year)
n2.data=rbind(n2.data,data.frame(year=(max(n2.data$year)+1):2080,n=n2.data$n[nrow(n2.data)]))

n2.col=which(colnames(df)=='n2')
colnames(df)[n2.col]='n2.value'
year0.col=which(colnames(df)=='year0')

hdr.nr=6

n.col.xlsx=ncol(df)+2+1

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
	df2=expand.grid(year0=yrs,x=x)
	df2$prd.year=df2$year0+df2$x-1
}

new.rows=nrow(df)+(1:nrow(df2))
df[new.rows,]=NA
df$phase[new.rows]='parameters'
for (col in colnames(df2)) {
	df[new.rows,col]=df2[,col]
}

wb <- createWorkbook() 
addWorksheet(wb,"parameters") 
writeData(wb,"parameters",x=df,startRow=hdr.nr)
writeData(wb,"parameters",n2.data,startCol=n.col.xlsx-1,startRow=hdr.nr)

# references to the n-values in the estimate table
n.references=paste0(colToExcel(n.col.xlsx),hdr.nr+1+df$year0-2000)
writeFormula(wb,"parameters",n.references,startCol=n2.col,startRow=hdr.nr+1)

rws=hdr.nr+(1:nrow(df))
# computing the individual forecast as the product: fit*n
# this has been deprecated
for (i in 1:3) {
	frml.mult=paste0(colToExcel(which(colnames(df)=='fit')+i-1),rws,'*',colToExcel(n2.col),rws)
	# writeFormula(wb,"parameters",frml.mult,startCol=ncol(df)+i,startRow=hdr.nr+1)
}
# writeData(wb,'parameters',matrix(c('estimate','ci.low','ci.hi'),nc=3),startCol=ncol(df)+1,startRow=hdr.nr,colNames=FALSE)

writeData(wb,'parameters',c('multiplier','exponent'),startCol=n.col.xlsx+3,startRow=2,colNames=FALSE)
writeData(wb,'parameters',c(a,b),startCol=n.col.xlsx+4,startRow=2,colNames=FALSE)
a.ref=paste0('$',colToExcel(n.col.xlsx+4),'$',2)
b.ref=paste0('$',colToExcel(n.col.xlsx+4),'$',3)

# Formula for the estimates computed based on parameters
rws=hdr.nr+new.rows
x.col=which(colnames(df)=='x')
frml.fit=paste0(a.ref,'*',colToExcel(x.col),rws,'^',b.ref)
writeFormula(wb,'parameters',frml.fit,startCol=colToExcel(which(colnames(df)=='fit')),startRow=min(new.rows)+hdr.nr)

# summing the estimates for the final forecasts using sumif
# =SUMIF($C$7:$C$3679;U7;$T$7:$T$3678)
rws=hdr.nr+(1:nrow(df))
n.rws=hdr.nr+(1:nrow(n2.data))
prd.year.col=colToExcel(which(colnames(df)=='prd.year'))
sum.col=which(colnames(df)=='fit') # (ncol(df)+1)

st0=createStyle(numFmt='0')
# write also the condidence intervals
id.col=int2col(which(colnames(df)=='id'))
for (i in 0:2) {
	# =AB7*SUMIF($A$7:$A$898;$AA7;$E$7:E$898)
	# nb! This is old now, copied below
	frml.sumif=paste0(colToExcel(n.col.xlsx),n.rws,
		'*sumif($',prd.year.col,'$',rws[1],':$',prd.year.col,'$',rws[length(rws)],',',
		'$',colToExcel(n.col.xlsx-1),n.rws,',', # condition: prd.year
		'$',int2col(sum.col+i),'$',rws[1],':','$',int2col(sum.col+i),'$',rws[length(rws)],')')

	frml.sumif=paste0(colToExcel(n.col.xlsx),n.rws,
		'*sumif($',id.col,'$',rws[1],':$',id.col,'$',rws[length(rws)],',',
		'',colToExcel(n.col.xlsx-1),'$',hdr.nr-1,'&','$',colToExcel(n.col.xlsx-1),n.rws,',', # condition: /-separated string of prd.year, country, blood group, mode
		'$',int2col(sum.col+i),'$',rws[1],':','$',int2col(sum.col+i),'$',rws[length(rws)],')')

	writeFormula(wb,'parameters',frml.sumif,startCol=n.col.xlsx+1+i,startRow=hdr.nr+1)

	addStyle(wb,'parameters',st0,cols=n.col.xlsx+1+i,rows=(hdr.nr)+(1:length(frml.sumif)))
}

saveWorkbook(wb,"../tool.xlsx",overwrite=TRUE)

# sarakkeiden piilottaminen
# groupColumns(wb, sheet, cols, hidden = FALSE, level = -1)
# write.xlsx(df,'../tool.xlsx')

getwd()
system(paste('"C:\\Program Files\\Microsoft Office\\root\\Office16\\EXCEL.EXE"','../tool.xlsx'),intern=FALSE,ignore.stdout=FALSE,ignore.stderr=FALSE)

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



