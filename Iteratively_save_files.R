
# This function breaks a datatable into chunks and iteratively saves each chunk based on a 
# certain condition. 
# in this case years 2009-2013

# save files for all years combined, and for each individual year
setwd("")

# function to save each year
saveyears <- function(data) {for (i in 2009:2013){
  write.table (data[year==i,], file=paste("ChamberTemp_dailyMean_CiPEHR", i, ".csv", sep="_"),
               sep =',', dec='.', row.names=FALSE, col.names=TRUE) }}

# save each year
saveyears(filled.temp)


