# extracts random samples from each data set in donation data
# Each column is sampled independently of each other to break the data

file <- param$data.file # defined in the Rmd file that should be done first
load(file = file) # replaces the modified donationdata with the original one

n = 100 # sample size

for (m in names(donationdata)) {
  data = donationdata[[m]]
  output = NULL
  for (c in colnames(data)) {
    smpl = sample(data[[c]],n)
    
    if (c == 'releaseID') {
      # Replace the releaseID's with essential random text data (both a-z and 0-9 included)
      smpl = paste0('SP_ALPHABETA',1000+(1:n))
    }
    
    if (is.null(output))
      output = data.frame(smpl)
    else
      output = cbind(output,data.frame(smpl))
  }
  colnames(output) = colnames(data)
  write.table(output,file = paste(m,'.txt',sep=''),sep='\t',col.names = FALSE,row.names = FALSE,quote = FALSE)
}

