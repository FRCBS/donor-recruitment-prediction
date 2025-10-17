param=list()
param$wd = getwd()

# getwd() might behave differently depending on the environment where it is run (console vs. Rmd),
# therefore checking if working directory is set to the src folder and moving up if yes.
if (grepl('[/\\]src(data)[/\\]?',param$wd)) {
  param$wd = sub('[/\\]src(data)?([/\\]?)$','\\2',param$wd)
}

# Set the working directory to srcdata: this is assumed to exist before running the script and
# the source files saved in the directory
setwd(file.path(param$wd,'srcdata'))

# nb! If using Rstudio, working directory should be set to the donor-recruitment-prediction folder at this point.
# The source data files are assumed to be saved under 
# If not, please set it manually to point to the directory where the source data files can be found.
param$wd

tm <- as.POSIXlt(Sys.time(),"UTC")
timestamp = strftime(tm,"%Y-%m-%dT%H_%M_%S%z")

# Specify a file where the logs will be written
param$sink.file = file.path(param$wd,'log',paste0('read-and-combine-source-data_',timestamp,'.log'))

# nb! Uncomment and run the following line to skip saving the output to a log file
# and to have it printed as the lines are run.
param$sink.file = NULL

if (!is.null(param$sink.file)) {
  sink(param$sink.file)
}

dir.create(file.path(param$wd,"results"),showWarnings = FALSE)
dir.create(file.path(param$wd,"log"),showWarnings = FALSE)

# This is where is donationdata will be written
datafile = file.path(param$wd,'donationdata.Rdata')

file_dir <- '/mnt/c/Users/potha01m/data/Donaties/'#navarra/' #stored in same dir

t.donation <- read.csv(file.path(file_dir, "donations.csv"), header = TRUE, colClasses = c(NA, NA, "Date", NA, "Date", NA, NA, NA, NA))
t.donor <- read.csv(file.path(file_dir, "donor.csv"), header = TRUE, colClasses = c(NA, NA, NA, NA, "Date", NA))
#change if comes from R or python. From R has another column (index)
# deferral <- read.csv(file.path(file_dir, "deferral.csv"), header = TRUE, colClasses = c(NA, "POSIXct", "POSIXct", NA))
t.deferral <- read.csv(file.path(file_dir, "deferral.csv"), header = TRUE, colClasses = c(NA, NA, "POSIXct", "POSIXct", NA))
t.contact <- read.csv(file.path(file_dir, "contact.csv"), header = TRUE, colClasses = c(NA, NA, NA, "POSIXct", NA))
####

# print the structure for convenience at an early point
t.donationdata = list(donation=t.donation,deferral=t.deferral,donor=t.donor,contact=t.contact)
for (n in names(donationdata)) {
  print(paste('structure of',n))
  data = t.donationdata[[n]]
  str(data[0,])
  if (dim(data)[2] == 1) {
    print(paste('Warning: table has one column only. Please check that the separtor in read.csv matches the one used in the data file'))
  }
}
rm(t.donationdata)

# nb! These lines should be run only if the source data files does  not include column names
# nb! Make sure the column names match the content of the columns in case they are in different order
# nb! Adjust here your data to match the column names used in data-description.xlsx

colnames(t.donation)=c("releaseID","BloodDonationTypeKey","DonationDate","DonationPlaceType","DonationPlaceCode")
colnames(t.deferral)=c("releaseID","DeferralStartDate","DeferralEndDate","DonorAdverseReactionType")
colnames(t.donor)=c("releaseID","Sex","PostalCode","PermissionToInvite","DateOfBirth","BloodGroup")
colnames(t.contact)=c("releaseID","ContactChannel","ContactType","DateSent","DonationSiteCode")
####

# combine the (up to) four data frames read above into a single list called donationdata
# nb! The correct functioning of the processing in blood-donor-recruitment-predction.Rmd requires that
# an object with this name is loaded from param$data.file.
donationdata = list(donation=t.donation,deferral=t.deferral,donor=t.donor,contact=t.contact)

if (is.null(donationdata[['donation']])) 
    print('ERROR: donation data missing in donationdata')

if (is.null(donationdata[['donation']])) 
  print('ERROR: donor data missing in donationdata')

types = list()
types[['donation']] = c("character","character","Date","character","character")
types[['donor']] = c("character","character","character","character","Date","character")

# print('dimensions of source data frames')
for (n in names(donationdata)) {
  print(paste('dimensions of',n,paste(dim(donationdata[[n]]),collapse=',')))
  data = donationdata[[n]]
  str(data[0,])
  
  # convert releaseID to char
  donationdata[[n]]$releaseID = as.character(data$releaseID)
  
  if (n %in% names(types)) {
    classes = sapply(data, class)
    for (i in 1:length(types[[n]])) {
      spec = types[[n]][i]
      actual = classes[i]
      if (spec != 'character' && spec != actual) {
        print(paste('ERROR: type of data in table',n,'is',actual,'while expecting',spec))
      }
    }
  }
}

# data frames others than the donor data frame
# (the other data frames are treated differently)
others = c('donation','deferral','contact')

duplicated.donors = which(duplicated(donationdata$donor$releaseID))
len = length(duplicated.donors)
if (len > 0) {
  print(paste('Warning:',len,"releaseID's found in",o,"but not in the donor table"))
  # print(paste('Warning:',len,"releaseID's found in",o,"but not in the donor table, eg.",donationdata$donor$releaseID[min(in.other.only)]))
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
for (col in check.na) {
  nas = which(is.na(donationdata[[table]][[col]]))
  if (length(nas) > 0) {
    print(paste('WARNING:',length(nas),"NA's found in",table,'column',col,'these will be removed'))
    donationdata[[table]] = donationdata[[table]][-nas,]
  }
}

# Check the essential enumerations here
# Other than enumerated values are just listed but not removed
# Special conventions given below under each enumeration
enumerations = list()
enumerations[['donor,BloodGroup']] = 'A+,A-,AB+,AB-,B+,B-,O+,O-'
enumerations[['donor,Sex']] = 'Male,Female'
enumerations[['donation,BloodDonationTypeKey']] = 'Whole Blood (K)'
# Only donations/rows with this values (Whole Blood (K)) are included in the analysis
enumerations[['donation,DonationPlaceType']] = 'Office'
# Values other than Office are classified as Mobile

for (e in names(enumerations)) {
  evals = strsplit(enumerations[[e]],',')[[1]]
  tabcol = strsplit(e,',')[[1]]
  tab = tabcol[1]
  col = tabcol[2]
  data = donationdata[[tab]][[col]]
  xval = which(!data %in% evals)
  n = length(xval)
  if (n > 0) {
    print(paste(n,'values in table',tab,'column',col,'that are not in the expected values:',paste(evals,collapse=',')))
    print(table(data[xval],useNA='ifany'))
  }
}

# Save the data to the datafile (by default at the root level of the local repository)
save(donationdata,file=datafile)

if (!is.null(param$sink.file)) {
  sink()
  
  # Output the log to the console for convenience
  fileName <- param$sink.file
  cat(readChar(fileName, file.info(fileName)$size))
}

