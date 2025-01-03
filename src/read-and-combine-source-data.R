param=list()
param$wd = getwd()

# getwd() might behave differently depending on the environment where it is run (console vs. Rmd),
# therefore checking if working directory is set to the src folder and moving up if yes.
if (grepl('[/\\]src(data)[/\\]?',param$wd)) {
  param$wd = sub('[/\\]src(data)?([/\\]?)$','\\2',param$wd)
}

setwd(file.path(param$wd,'srcdata'))

# nb! If using Rstudio, working directory should be set to the donor-recruitment-prediction folder at this point.
# The source data files are assumed to be saved under 
# If not, please set it manually to point to the directory where the source data files can be found.
param$wd

tm <- as.POSIXlt(Sys.time(),"UTC")
timestamp = strftime(tm,"%Y-%m-%dT%H_%M_%S%z")

param$sink.file = NULL

# nb! 
# Uncomment the following line to save the log to a timestamped file in the log directory
# param$sink.file = file.path(param$wd,'log',paste0('read-and-combine-source-data_',timestamp,'.log'))

if (!is.null(param$sink.file)) {
  sink(param$sink.file)
}

dir.create(file.path(param$wd,"results"),showWarnings = FALSE)
dir.create(file.path(param$wd,"log"),showWarnings = FALSE)
# param$result.file = file.path(param$wd,"results","data.xlsx")

# This is where is donationdata will be written
datafile = file.path(param$wd,'donationdata.Rdata')

# nb! adjust the header (here included) and sep (here tab, '\t') parameters as necessary
t.donation=read.csv('donation_fake.csv',header=TRUE,colClasses=c(NA,NA,'Date',NA,NA),sep='\t')
t.deferral=read.csv('deferral_fake.csv',header=TRUE,colClasses=c(NA,'POSIXct','POSIXct',NA),sep='\t')
t.donor=read.csv('donor_fake.csv',header=TRUE,colClasses=c(NA,NA,NA,NA,'Date',NA),sep='\t')
t.contact=read.csv('contact_fake.csv',header=TRUE,colClasses=c(NA,NA,NA,'POSIXct',NA),sep='\t')

# nb! These lines are necessary to run only if the source data files did not include column names
colnames(t.donation)=c("releaseID","BloodDonationTypeKey","DonationDate","DonationPlaceType","DonationPlaceCode")
colnames(t.deferral)=c("releaseID","DeferralStartDate","DeferralEndDate","DonorAdverseReactionType")
colnames(t.donor)=c("releaseID","Sex","PostalCode","PermissionToInvite","DateOfBirth","BloodGroup")
colnames(t.contact)=c("releaseID","ContactChannel","ContactType","DateSent","DonationSiteCode")

# combine the (up to) four data frames read above into a single list called donationdata
# nb! The correct functioning of the processing in blood-donor-recruitment-predction.Rmd requires that
# an object with this name is loaded from param$data.file.
donationdata = list(donation=t.donation,deferral=t.deferral,donor=t.donor,contact=t.contact)

print('dimensions of source data frames')
for (n in names(donationdata)) {
  print(paste(n,dim(donationdata[[n]])))
}

# data frames others than the donor data frame
# (the other data frames are treated differently)
others = c('donation','deferral','contact')

duplicated.donors = which(duplicated(donationdata$donor$releaseID))
len = length(duplicated.donors)
if (len > 0) {
  print(paste(len,"releaseID's found in",o,"but not in the donor table, eg.",donationdata$donor$releaseID[min(in.other.only)]))
}

for (o in others) {
  in.other.only = setdiff(donationdata[[o]]$releaseID,donationdata$donor$releaseID)
  len = length(in.other.only)
  if (len > 0) {
    print(paste(len,"releaseID's found in",o,"but not in the donor table, eg.",min(in.other.only)))
  }
  
  in.donor.only = setdiff(donationdata$donor$releaseID,donationdata[[o]]$releaseID)
  len = length(in.donor.only)
  if (len > 0) {
    print(paste(len,"releaseID's found in the donor table but not in the",o,"table, eg.",min(in.donor.only)))
  }
}

table = 'donation'
check.na = c('releaseID','DonationDate')
# col = 'DonationPlaceCode'
for (col in check.na) {
  nas = which(is.na(donationdata[[table]][[col]]))
  if (length(nas) > 0) {
    print(paste('WARNING:',length(nas),"NA's found in",table,'column',col,'these will be removed'))
    donationdata[[table]] = donationdata[[table]][-nas,]
  }
}

for (n in names(donationdata)) {
  print(paste('summary of',n))
  print(summary(donationdata[[n]]),na.rm=FALSE)
  print(colSums(is.na(donationdata[[n]])))
}

save(donationdata,file=datafile)

if (!is.null(param$sink.file)) {
  sink()
}


str(donationdata)
