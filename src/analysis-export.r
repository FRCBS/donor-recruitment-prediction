df = export.estimates
rw0=3
n2.data=estimates0 %>%
	filter(rw==rw0,x==1) %>%
	dplyr::select(year0,n2) %>%
	rename(year=year0,n=n2) %>%
	arrange(year) %>%
	mutate('activity multiplier'=NA,estimate=NA,ci.low=NA,ci.upr=NA)
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

n.col.xlsx=ncol(df)+2+5

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
	df2$fit=df2$x # This is later used as a constant in the formula for computed the fitted values
	df2=df2[,-which(colnames(df2) %in% c('x','year0.extra'))]
}

new.rows=nrow(df)+(1:nrow(df2))
df[new.rows,]=NA
for (col in colnames(df2)) {
	df[new.rows,col]=df2[,col]
}
df$id[new.rows]=paste0(df$country[new.rows],'/parameters/',df$prd.year[new.rows])

df[,c('est','est.lo','est.hi')]=NA

###
wb <- createWorkbook()
shMain='main' 
addWorksheet(wb,shMain) 
addWorksheet(wb,"nr of new donors") 
addWorksheet(wb,"coefficients") 
writeData(wb,'nr of new donors',n.wide)
writeData(wb,shMain,x=df,startRow=hdr.nr)
writeData(wb,shMain,n2.data,startCol=n.col.xlsx-1,startRow=hdr.nr)
writeData(wb,'coefficients',x=coeff.df)

n.frml=paste0('vlookup($',int2col(n.col.xlsx-1),hdr.nr+(1:nrow(n2.data)),',',
	'\'nr of new donors\'!$A$1:$',int2col(ncol(n.wide)),'$',nrow(n.wide)+1,',',
	'match($',int2col(n.col.xlsx),'$7,\'nr of new donors\'!$A1:$',int2col(ncol(n.wide)),'1,0),FALSE)')
writeFormula(wb,shMain,n.frml,startCol=n.col.xlsx,startRow=hdr.nr+1)

# references to the n-values in the estimate table
n.references=paste0(int2col(n.col.xlsx),hdr.nr+1+df$year0-2000)
writeFormula(wb,shMain,n.references,startCol=n2.col,startRow=hdr.nr+1)

styRight=createStyle(halign='right')
styBold=createStyle(textDecoration='bold')
styColour=createStyle(fgFill="#fffee0",locked=FALSE)
styDecimal=createStyle(numFmt="0,00")

rws=hdr.nr+(1:nrow(df))
est.col=which(colnames(df)=='est')
# computing the individual forecast as the product: fit*n
# this has been deprecated
for (i in 1:3) {
	frml.mult=paste0(int2col(which(colnames(df)=='fit')+i-1),rws,'*',int2col(n2.col),rws)
	writeFormula(wb,shMain,frml.mult,startCol=est.col+i-1,startRow=hdr.nr+1)
}
# writeData(wb,shMain,matrix(c('estimate','ci.low','ci.hi'),nc=3),startCol=ncol(df)+1,startRow=hdr.nr,colNames=FALSE)

a=2.2
b=0.52
writeData(wb,shMain,c('Parameters to use when model=parameters','multiplier','exponent','cdon50'),startCol=n.col.xlsx+2,startRow=1,colNames=FALSE)
writeData(wb,shMain,c(a,b),startCol=n.col.xlsx+3,startRow=2,colNames=FALSE)
writeFormula(wb,shMain,paste0(int2col(n.col.xlsx+3),2,'*50^',int2col(n.col.xlsx+3),3),startCol=n.col.xlsx+3,startRow=4)
# addStyle(wb,shMain,styDecimal,cols=n.col.xlsx+3,rows=4)
a.ref=paste0('$',int2col(n.col.xlsx+3),'$',2)
b.ref=paste0('$',int2col(n.col.xlsx+3),'$',3)

# parameter section
writeData(wb,shMain,c('nl','Oneg','log.separately','fi','all'),startCol=n.col.xlsx,startRow=1)
writeData(wb,shMain,c(paste0('Blood establishment, one of: ',paste(grps$country,collapse=', ')),'Blood group: all or Oneg',paste0('Model, one of: ',paste(c(names(models.map),'parameteres'),collapse=', ')),'Number of new donors, country','Number of new donors, blood group'),startCol=n.col.xlsx-2,startRow=1)
prm.col=int2col(n.col.xlsx)
concat.frml=paste0('if(',prm.col,'3="parameters",',
	paste0(paste(concat.frml=paste0(prm.col,c(1,3)),collapse='&"/"&'),'&"/",'),
	paste0(paste(concat.frml=paste0(prm.col,1:3),collapse='&"/"&'),'&"/"'),")")
writeFormula(wb,shMain,concat.frml,startCol=n.col.xlsx,startRow=6)
prm.col=int2col(n.col.xlsx)
concat.frml=paste(concat.frml=paste0(prm.col,1:2),collapse='&"."&')
writeFormula(wb,shMain,concat.frml,startCol=n.col.xlsx,startRow=7)
writeFormula(wb,shMain,paste0(int2col(n.col.xlsx),1:2),startCol=n.col.xlsx,startRow=4)

addStyle(wb,shMain,styColour,cols=n.col.xlsx,rows=1:5)
addStyle(wb,shMain,styBold,cols=n.col.xlsx-2,rows=1:hdr.nr)

# Formula for the estimates computed based on parameters
rws=hdr.nr+new.rows
fit.col=which(colnames(df)=='fit')
# frml.fit=paste0(a.ref,'*',int2col(fit.col),rws,'^',b.ref)
frml.fit=paste0(a.ref,'*',df2$fit,'^',b.ref)
writeFormula(wb,shMain,frml.fit,startCol=int2col(which(colnames(df)=='fit')),startRow=min(new.rows)+hdr.nr)
addStyle(wb,shMain,styColour,cols=n.col.xlsx+3,rows=2:3)
addStyle(wb,shMain,styBold,cols=n.col.xlsx+2,rows=2:3)

# formulas for the estimates in the long table
# removed this
for (i in 0:2) {
	# est.frml=n.references=paste0(int2col(n.col.xlsx),hdr.nr+1+df$year0-2000)
	# writeFormula(wb,shMain,n.references,startCol=est.col+i,startRow=hdr.nr+1)
}


# summing the estimates for the final forecasts using sumif
# =SUMIF($C$7:$C$3679;U7;$T$7:$T$3678)
rws=hdr.nr+(1:nrow(df))
n.rws=hdr.nr+(1:nrow(n2.data))
prd.year.col=int2col(which(colnames(df)=='prd.year'))
sum.col=which(colnames(df)=='est') # (ncol(df)+1)

st0=createStyle(numFmt='0')
# write also the condidence intervals
id.col=int2col(which(colnames(df)=='id'))
n.col.xlsx=n.col.xlsx+1
writeData(wb,shMain,rep(1,length(n.rws)),startCol=n.col.xlsx,startRow=hdr.nr+1) # acitivity multipliers
sapply(-1:0,FUN=function(x) addStyle(wb,shMain,styColour,cols=n.col.xlsx+x,rows=hdr.nr+(1:length(n.rws))))
addStyle(wb,shMain,styRight,cols=n.col.xlsx-2,rows=hdr.nr)
addStyle(wb,shMain,styBold,cols=n.col.xlsx+(-2:4),rows=hdr.nr)
for (i in 0:2) {
	frml.sumif=paste0(int2col(n.col.xlsx),n.rws,'*',
		'sumif($',id.col,'$',rws[1],':$',id.col,'$',rws[length(rws)],',',
		'$',int2col(n.col.xlsx-1),'$',hdr.nr-2,'&','$',int2col(n.col.xlsx-2),n.rws,',', # condition: /-separated string of prd.year, country, blood group, mode
		'$',int2col(sum.col+i),'$',rws[1],':','$',int2col(sum.col+i),'$',rws[length(rws)],')')

	writeFormula(wb,shMain,frml.sumif,startCol=n.col.xlsx+1+i,startRow=hdr.nr+1)

	addStyle(wb,shMain,st0,cols=n.col.xlsx+1+i,rows=(hdr.nr)+(1:length(frml.sumif)))
}

setColWidths(wb,shMain,cols=1:(ncol(df)+1),hidden=TRUE)
setColWidths(wb,shMain,cols=n.col.xlsx-2,width=45)
# setRowHeights(wb,shMain,rows=hdr.nr-(1:2),heights=NA) # the concatenated parameters

groupColumns(wb,'coefficients',cols=which(grepl('lo|hi',colnames(coeff.df))),hidden=TRUE,level=-1)

# groupRows(wb,shMain,rows=hdr.nr-(1:2),hidden=TRUE)
# groupColumns(wb,shMain,cols=1:(ncol(df)+1),hidden=TRUE,level=-1)

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



