
# Code to replicate data rows in a data table by the number of years occuring in a range

# replicate data rows: 
# https://simplmodl.wordpress.com/2014/01/31/a-simple-way-to-repeat-replicate-or-explode-data-table-rows-in-r/

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

# now replicate the rows for each year range, based on the number of years in the range
# eg: 1995-1998 will create 3 replicate rows
dat_keep <- copy(dat1[is.na(year_end),])
dat2 <- copy(dat1[!is.na(year_end),])

dat3 <- dat2[rep(seq(1, nrow(dat2)), 2)]


dat3 <- dat2[,lapply(.SD, function(x) rep(x, year_end-(year_start-1))), by="author"]

# create a new year column that counts from start year to end year, by 1 so that we have a duplicate entry for 
# each year measured, but with a unique year listed. 

years_test <- data.table(year_start=2000, year_end=2004)

rep_years <- function(dt) {
  for (i in 1:(dt[,year_end]-1)-dt[,year_start]){
  year1 <- dt[,year_start]
    year <- dt[,year_start]+i
    year_final <- dt[,year_end]
    year_measured[i] <- year
  
  }
  out <- rbind(year1,do.call("rbind",year_measured),year_final)
}

test_function <- rep_years(years_test)


dat3[,year_measured:= rep_years(.SD), by="author"]

# now add the data with only one year of data together with the replicated data 
dat_keep[,year_measured := year_start]
dat_final <- rbind(dat_keep, dat3)


