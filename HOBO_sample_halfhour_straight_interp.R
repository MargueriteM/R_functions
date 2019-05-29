# M. Mauritz 12 Nov 2015

# quick and dirty code to do a straight line interpolation between two points
# creates a half hour interval of hourly data (But this can probably be improved by a timeseries approach)

# load libraries
library(ggplot2)
library(data.table)
library(zoo)

# adapted from: 
# http://stackoverflow.com/questions/8152603/interpolating-hourly-basis
# http://stackoverflow.com/questions/15114834/interpolate-zoo-object-with-missing-dates

# import data
setwd("~/Desktop/SchuurLab/FluxData_Healy/Autochambers_2015/HOBO_data")
load("HOBO_2014-10-01_to_2015-09-30_half_hourly.Rdata")

# subset data to test the function
test <- subset(HMD2, DOY>=267 & year==2015)

# convert to datatable to apply the interpolation individually to each day
DT <- data.table(test, key=c("DOY"))
# create a data table output that has variables interpolated at desired time interval
# first make a half.hour variable at the desired interval
dt_times <- DT[,list(half.hour=seq(from=0, DT$half.hour[length(DT$half.hour)], by=0.5)),by=DOY]
# merge so that there are 'NAs' anywhere you want a value
combined <- merge(dt_times, test, by=c("DOY","half.hour"),all=TRUE)
# remove first row because it is NA. If you need that row then you have to think of something else... 
# but R has no information to interpolate it. Could carry an observation back.
combined <- combined[-1]

# interpolate missing values
# na.approx will linearly interpolate between missing values. For a missing half hour, that is appropriate!
# will not work with first row NA
# this is a little tricky because there is no unambiguous row ID, but data table preserves the row order, 
# the cbind should be safe.
interp <- combined[,list(
 # half.hour=unique(half.hour),
                    temp.interp=na.approx(Tair),
                    PAR.interp= na.approx(PAR),
                    pressure.interp=na.approx(pressure),
                    wind_sp.interp=na.approx(wind_sp))]#,by=DOY]

interp2 <- cbind(combined,interp)

# this works for filling na's by carrying last observation forward, will not work with first row NA
interp1 <- combined[,list(
  temp.interp=na.locf(Tair),
  PAR.interp= na.locf(PAR),
  pressure.interp=na.locf(pressure),
  wind_sp.interp=na.locf(wind_sp))]

interp3 <- cbind(combined,interp1)

# graph to see if it worked
ggplot()+geom_point(data=interp,aes(x=half.hour, y=temp.interp),colour="black")+
  geom_point(data=test,aes(x=half.hour, y=Tair),colour="red")+
  facet_grid(.~DOY)

ggplot()+geom_point(data=interp1[100:200],aes(y=pressure.interp),colour="black")+
  geom_point(data=test,aes(x=half.hour, y=pressure),colour="red")+
  facet_grid(.~DOY)

plot(interp2$temp.interp,interp2$Tair)
plot(interp2$PAR.interp,interp2$PAR)


ggplot()+geom_point(data=interp,aes(x=half.hour, y=PAR.interp),colour="black")+
  geom_point(data=test,aes(x=half.hour, y=PAR),colour="red")+
  facet_grid(.~DOY)

