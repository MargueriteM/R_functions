# convert DOY to date variable
# Gerardo Celis
# July 2014

dat$hour <- floor(dat$hourmin)
dat$minutes <- (dat$hourmin - dat$hour)*60
dat$time <- paste(dat$year, dat$doy, dat$hour, dat$minutes, sep="-")
dat$time <- as.POSIXct(dat$time, format="%Y-%j-%H-%M")

# dat$time <- ymd_hms(dat$time)

#### Create half hour variable that rounds to the closest half hour timestamp ####

hobo$half.hour <- ifelse(hobo$minutes <= 15, hobo$hour, hobo$hour + 0.5)
hobo$half.hour <- ifelse(hobo$minutes > 46, hobo$hour + 1, hobo$half.hour)
hobo$half.hour <- ifelse(hobo$half.hour==24.0, 0.0, hobo$half.hour)

# half hour time stamp from lubridate #
temps_otc [ , test := round_date(Date_time, unit="30 mins")]


########## M. Mauritz July 2014 ########

# convert hourmin into a dummy variable that will indicate hours and half hours as a minute decimal.
# in this case any value ending in .0 will be even and fall on the hour, 
# any value ending in .5 will be odd and fall on the half hour
final$check <- final$hourmin*60/30
final$min[which((final$check)%%2 == 0)] <- 0
final$min[which((final$check)%%2 != 0)] <- 30

final$hour[which((final$check)%%2 == 0)] <- final$hourmin[which((final$check)%%2 == 0)]
final$hour[which((final$check)%%2 != 0)] <- final$hourmin[which((final$check)%%2 != 0)] - 0.5


# create a date/time stamp from date (as.Date) and a half.hour in 0 and 0.5
temps_all[,check:=half.hour*60/30]
temps_all[which((temps_all$check)%%2 == 0),min:=0]
temps_all[which((temps_all$check)%%2 != 0),min:=30]

temps_all[min==0,hour:=as.integer(round(half.hour))]
temps_all[min==30,hour:=as.integer(floor(half.hour))]

temps_all[,hourmin := paste(hour,min,sep=":")]
temps_all[,date_time := as.POSIXct(paste(as.character(date),hourmin,sep=" "),"%Y-%m-%d %H:%M",tz="UTC"),]

View(temps_all[,.(half.hour,check,min,hour,date_time,date)])


# create a date variable from DOY
final$date <- as.Date(final$DOY-1, origin = "2014-01-01")
final$datetime <- paste(final$date, final$hour, final$min, sep=" ")
final$dateformat <- parse_date_time(final$datetime,"ymd, !H!M")

# create a date variable from DOY for multiple years
# adapted from: http://stackoverflow.com/questions/13442461/populating-a-data-frame-in-r-in-a-loop

mylist <- list() #create an empty list

for (i in 2014:2015){
  data1 <- subset(data,year==i)
  data1$date <- as.Date(data1$DOY-1, origin = paste(i,"01-01",sep="-"))
  mylist[[i]] <- data1}
data2 <- do.call("rbind",mylist) #combine all vectors into a matrix


