#From the created data files with create_data.py, now make a R-list of these dataframes for use in the .Rmd file

#CHECK FILE DIR
file_dir <- "/mnt/c/Users/potha01m/data/Donaties/" #stored in same dir

donations <- read.csv(file.path(file_dir, "donations.csv"), header = TRUE, colClasses = c(NA, NA, "Date", NA, "Date", NA, NA, NA, NA))
str(donations)
donor <- read.csv(file.path(file_dir, "donor.csv"), header = TRUE, colClasses = c(NA, NA, NA, NA, "Date", NA))
str(donor)
#change if comes from R or python. From R has another column (index)
# deferral <- read.csv(file.path(file_dir, "deferral.csv"), header = TRUE, colClasses = c(NA, "POSIXct", "POSIXct", NA))
deferral <- read.csv(file.path(file_dir, "deferral.csv"), header = TRUE, colClasses = c(NA, NA, "POSIXct", "POSIXct", NA))
str(deferral)
contact <- read.csv(file.path(file_dir, "contact.csv"), header = TRUE, colClasses = c(NA, NA, NA, "POSIXct", NA))
str(contact)

donationdata <- list(donation = donations, donor = donor, deferral = deferral, contact = contact)
save(donationdata, file = file.path(file_dir, "data.rdata"))


