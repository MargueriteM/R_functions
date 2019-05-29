
# Code to replicate data rows in a data table by the number of years occuring in a range


# replicate data rows: 
# https://simplmodl.wordpress.com/2014/01/31/a-simple-way-to-repeat-replicate-or-explode-data-table-rows-in-r/

library(data.table)

# create mock data
dat <- data.table(author=c("Belshe; McGuire; Virkkala", "Belsh; Virkkala"),
                  year=c("1995-1998; 2000","2000-2008"), site=c("A","B"), 
                  lat=c("1","2"), long=c("4","-5"), notes = c("what; what", "try; this"))


# split the data at the semi-colon into rows, the by = "" will preserve the semi-colons in the cell
# so list all the columns that you do not want to have unlisted by semi-colon in the by column
dat1 <- dat[,lapply(.SD, function(x) unlist(tstrsplit(x, ";", fixed=TRUE))), by="author,notes"]

# create a start and end year based on the range in the year column (after separating out the years with semi-colons)
dat1[,year_start := as.numeric(sapply(strsplit(as.character(year),"-"),"[",1))]
dat1[,year_end := as.numeric(sapply(strsplit(as.character(year),"-"),"[",2))]

# Subet data with only one year
k <- dat1[is.na(year_end)==T, .(author, year, lat, long, notes)]

# Subset data with more than one year and create sequence of year based on start and end date
t <- dat1[is.na(year_end)==F, .(year = seq(year_start, year_end, by = 1)), by = .(author, lat, long, notes)]

# Combine both data.table
all <- rbind(k, t)
