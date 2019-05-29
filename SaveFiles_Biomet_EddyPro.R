########################################################################### 
# function to format and save external Biomet data file in EddyPro format #
#                     written by M. Mauritz, May 2019                     #
###########################################################################


# input file format:
# date_time LWin LWout Rn SWin SWout       Ta       RH       WD   MWS      PPFD     PPFDr P_rain
# 1: 2010-07-27 15:00:00   NA    NA NA   NA    NA 29.06385 37.32938 239.6936 0.830  848.2501  93.17917      0
# 2: 2010-07-27 15:30:00   NA    NA NA   NA    NA 30.02727 34.17983 277.2242 1.630 1561.1780 154.22083      0
# 3: 2010-07-27 16:00:00   NA    NA NA   NA    NA 30.71930 31.89295 260.3165 1.085 1476.7475 153.90835      0
# 4: 2010-07-27 16:30:00   NA    NA NA   NA    NA 31.00006 29.25450 232.7004 2.240 1197.0685 125.57501      0
# 5: 2010-07-27 17:00:00   NA    NA NA   NA    NA 30.89116 29.68530 254.5105 2.815  854.9121  90.95695      0
# 6: 2010-07-27 17:30:00   NA    NA NA   NA    NA 30.56567 30.84304 248.3601 2.560  578.7516  62.97083      0
# SWC_1_1_1 Ts_1_1_1 SHF_2_1_1 SHF_2_2_1 SHF_1_1_1 SHF_1_2_1       Pa
# 1: 0.07528750      NaN  74.62034  33.89665  45.17533  24.33413 86.00547
# 2: 0.07516771      NaN  41.94596  31.64099  23.14995  20.50340 85.96440
# 3: 0.07528750      NaN  36.57793  26.77249  24.29663  17.34491 85.93349
# 4: 0.07545417      NaN  47.62257  24.28238  41.50134  20.69789 85.91320
# 5: 0.07554791      NaN  47.33667  23.96258  48.84647  25.85045 85.90324
# 6: 0.07541457      NaN  40.41320  23.61629  41.30637  28.74641 85.90845

savebiomet <- function(data,startyear,endyear) {
  
  # make all NaNs be NA
  data [is.na(data)] <- NA
  
  # format the timestamps for month, day, hour, minute to have 2 digits
  # format timestamp for EddyPro
  # o	timestamp_1 yyyy
  # o	timestamp_2 mm
  # o	timestamp_3 dd
  # o	timestamp_4 MM
  # o	timestamp_5 HH
  
  data[,':=' (timestamp_1 = year(date_time),
              timestamp_2 = month(date_time),
              timestamp_3 = day(date_time),
              timestamp_4 = hour(date_time),
              timestamp_5 = minute(date_time),
              date_time = NULL)]
  
  data[,':=' (timestamp_2 = sprintf ("%02d",timestamp_2),
              timestamp_3 = sprintf ("%02d",timestamp_3),
              timestamp_4 = sprintf ("%02d",timestamp_4),
              timestamp_5 = sprintf ("%02d",timestamp_5))]
  
  for (i in startyear:endyear){
    # subset each year and convert all columns in data to characters so the unit row can be added
    data1 <- data[timestamp_1==i,lapply(.SD, as.character)]
    
    
    # add a row with units as the first row which will become the second header row for Eddy Pro
    biomet_units <- data.table("W+1m-2","W+1m-2","W+1m-2","W+1m-2","W+1m-2","C","%","degrees","m+1m-1",
                               "umol+1m-2s-1","umol+1m-2s-1","mm","m+3m-3","C","W+1m-2","W+1m-2","W+1m-2",
                               "W+1m-2","kPa","yyyy","mm","dd","HH","MM")
    
    colnames(biomet_units) <- colnames(data1)
    data1 <- rbind(biomet_units,data1)
    
    # save with columns in prescribed order
    write.table (data1[,.(timestamp_1, timestamp_2, timestamp_3,timestamp_4,timestamp_5,
                          Ta,RH,Pa,WD,MWS,PPFD,PPFDr,P_rain,SWC_1_1_1, Ts_1_1_1,
                          SHF_1_1_1, SHF_1_2_1, SHF_2_2_1, SHF_2_2_1,LWin,LWout,SWin,SWout,Rn)],
                 file=paste("Biomet_EddyPro_",i, ".csv",sep=""),
                 sep =',', dec='.', row.names=FALSE, na="-9999", quote=FALSE)
  }}


# save each year
# savebiomet(biomet,2010,2019)