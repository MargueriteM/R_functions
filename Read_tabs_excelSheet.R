
# Try to read mutliple pages of excel sheet for Kim

# 
# load library
library(readxl) 

# define the wd, this will be called later
readfiles <- "~/Desktop/macroissue/raw"
# set the name of the file you want to read
# (there may be a way to adapt this to do multiple files at once)
# using this allows you to define the file at the top instead of search your code for the right place
data_to_read <- 'GC_5.9.16_raw.xls'

# set wd
setwd(readfiles)

# trying solution from:
# http://stackoverflow.com/questions/12945687/how-to-read-all-worksheets-in-an-excel-workbook-into-an-r-list-with-data-frame-e
# https://blog.rstudio.org/2015/04/15/readxl-0-1-0/

# define function to read all data in all tabs and combine into list
read_excel_allsheets <- function(filename) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X,
                                                        col_names=c("index","gasID","time","quantity","height","area"),
                                                        skip=2))
  names(x) <- sheets
  x
}

# use do.call and rbind to compile all data into a dataframe
rawdatadf <- do.call("rbind",read_excel_allsheets(data_to_read))
# remove rows that have an NA in index
rawdatadf1 <- rawdatadf[complete.cases(rawdatadf[,c("index")]),]
# remove all rows in which index=="Index" or "Total"
rawdatadf2 <- subset(rawdatadf1, index!="Index"&index!="Total")
# create a column from the name of each list (which are the tab names)
rawdatadf2$filename <- rownames(rawdatadf2)

# use the tab name to extract information
# the only hitch is that I can only use _ as a searator. 
# some googling might yield a solution for different types of separators
# eg: info in the first position of _ separation
rawdatadf2$sampleID <- sapply(strsplit(as.character(rawdatadf2$filename),"_"),"[",1)
# info in 2nd position
rawdatadf2$rep <- sapply(strsplit(as.character(rawdatadf2$filename),"_"),"[",2)
# 3rd position, etc....
rawdatadf2$sample_time <- sapply(strsplit(as.character(rawdatadf2$filename),"_"),"[",3)





