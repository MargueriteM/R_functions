# Read in files in a directory and then name each data frame with the file name

# modified from:
# http://stackoverflow.com/questions/5319839/read-multiple-csv-files-into-separate-data-frames

##Read files named xyz1111.csv, xyz2222.csv, etc.
filenames <- list.files(path="../Data/original_data",
                        pattern="xyz+.*csv", full.names=FALSE) # full names = False will make it so the directory is not included in the name

##Create list of data frame names without the ".csv" part 
names <-substr(filenames,1,7)

# if you want to exlude just the last few characters:
names <-substr(filenames_wtd,1,nchar(filenames_wtd)-4)


###Load all files
for(i in names){
  filepath <- file.path("../Data/original_data/",paste(i,".csv",sep=""))
  assign(i, read.table(filepath,
                       sep = ",", header=TRUE))
}