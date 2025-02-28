###########################################################################################################################################################
###(1) Extracting and summarizing data from drop sensors##################################################################################################
########################################################################################################################################################
###updated 11-10-24
#R version 4.3.1

#Questions for Aidan:

#Subtract time zone? WHAT IS THE TIME ZONE OF THE DATA (local ET???)
#NEED TO ASK AIDAN
#DROP DATA ARE IN LOCAL TIME EST/EDT

#File missing for drop: 8294, from 6/18 - 6/20? 
#Only three files in folder but metadata say there should be four
# A: found it, will backup to SSD/HDD

#Also for sensor 3091, metadata says there should be one file in the folder but there are two
# A: I think this is related to sensor 3091 being removed from St. Lukes
#    which messed up how I enter collection datetimes in the metadata.
# A: I'll find a fix for this.

#What are the dates in the drop filenames - these seem to differ from dates in the
#metadata (deployed and collected dates)?
# A: The dates in the filenames are generally the datetime when I collected
#    but can also be an earlier datetime if the drop ran out of battery.
# A: Some files are from checkup dates (as opposed to collection dates), so 
#    there are some extras.


## ******************************************************************************************
## Preamble
## ******************************************************************************************

#Clears workspace
rm(list=ls())

#install.packages("lubridate", dependencies = TRUE)
library(lubridate) #lubridate_1.9.2

## ******************************************************************************************
## Load data files
## ******************************************************************************************


#Load metadata files
meta_dat <- read.csv("Data/audiomoth_data_2024/SciFaith_Drop_metadata.csv")
dim(meta_dat) #264  12
#date_out <- read.csv("data/TOMST data/SERC_downloaded_Fall 2023/SERC_TMS4_datacollection_coords_Fall2023.csv")
#sens_ids <- read.csv("data/TOMST data/SERCsensorIDs.csv")

#set wd() to SSD 
setwd("D:/Science and Faith Audio Files")


#Load TOMST data files (wrap into a function that gets filenames, checks for duplicates,
#loads files into list and names columns)


#Load all drop files-------------------------------------------------------------
#get list of filenames
serc <- paste("SERC", dir("SERC"), sep = "/")
sm <- paste("Stillmeadow", dir("Stillmeadow"), sep = "/")
sl <- paste("StLukes", dir("StLukes"), sep = "/")
sh <- paste("SweetHope", dir("SweetHope"), sep = "/")
lg <- paste("LibertyGrace", dir("LibertyGrace"), sep = "/")

sens.names <- c(serc, sm, sl, sh, lg)

# sens <- c("SERC/NEON007_N7", "SERC/NEON019_N19", "Stillmeadow/Open_SMO9", 
#           "Stillmeadow/Classroom_SMC7", "StLukes/Open1_SLO1", 
#           "StLukes/Forest3_SLR3", "SweetHope/SweetHope_SH4") 
# #Look at just a subset first

filenames <- vector()
sites.names <-vector()
drop.id <-vector()
for (i in 1:length(sens.names)) {
  temp <-
    list.files(paste(sens.names[i], "Drop", sep = "/"), pattern =
                 "*.csv$")
  temp2 <- paste(sens.names[i], "Drop", temp, sep = "/")
  temp3 <- rep(sub('.*/', '', sens.names[i]), length(temp))
  temp4 <- substr(temp, 5, 8) 
  filenames <- c(filenames, temp2)
  sites.names <- c(sites.names, temp3)
  drop.id <- c(drop.id, temp4)
}

length(unique(drop.id)) #19

#Load all files from Drop sensors
drop_raw <- lapply(filenames, function(x) {
  read.csv(x,  header = FALSE, skip = 5)
}) #big list of data files
names(drop_raw) <- drop.id #Assign sensor names to each df in list
length(drop_raw) #168


#Add column names for drops
cols <- c("date_time", "temp", "wb_temp", "rel_hum", "stat_pres", "heat_index", "dew_point", "data_type")

drop_raw <- lapply(drop_raw, setNames, cols)
head(drop_raw[[1]])


#Perform some data completeness QC checks***************************************
#wrap into a QC function that checks:
#(1) dimensions of each data file
#(2) Any files have temp values with commas instead of period for the decimal (country specifc formatting)
#(3) All end dates are correct, matching download dates, and date format is consistent across files 
#(4) Any NAs for date, temp, or moisture values
#Function should generate a summary table for each check, with a row for each sensor


#(1) Check dimensions of each sensor file for completeness/outliers
summary(sapply(drop_raw, dim)[1, ])
sapply(drop_raw, dim)
#one file has far fewer records than median of 4760 obs,
#sensor at Forest3_SLR3 has only 889 records associated with file:
#"StLukes/Forest3_SLR3/Drop/D2953091_Jun_26_2024_2_00_00_PM.csv" 
#Need to check in/out dates from metadata file

#(2) Check which files have temp values with commas instead of periods for decimal points
#This should check across all three temp columns, temp.1,temp,2 and temp.3
formt_chk <- data.frame()
for (i in 1:length(drop_raw)) {
  tmp <-
    data.frame(sensor = names(drop_raw[i]),
               is.numeric = is.numeric(drop_raw[[i]][, c("temp")]))
  formt_chk <- rbind(formt_chk, tmp)
}
#all are numeric


#(3) Check that all end dates are similar/correct (make sure data fully downloaded)
#and that date format is consistent across files
#I.e., check last timestamp versus date collected

date_chk <- data.frame()
for (i in 1:length(drop_raw)) {
  tmp <-
    data.frame(
      sensor = names(drop_raw[i]),
      data_time_strt = drop_raw[[i]][1, "date_time"],
      data_time_end = drop_raw[[i]][nrow(drop_raw[[i]]), "date_time"]
    )
  date_chk <- rbind(date_chk, tmp)
}
date_chk
#Overlapping dates for some sensor downloads, indicating data were not previously
#cleared from memory following download.
#sensor at Forest3_SLR3 has only records from only 6 days (6/20 - 6/26)
#associated with file:"StLukes/Forest3_SLR3/Drop/D2953091_Jun_26_2024_2_00_00_PM.csv" 
#Need to check in/out dates from metadata file

#Also, some start dates are from year 2000 (?) need to drop these
#Still need to trim files according to in/out dates/times from metadata file


#(4) Check if files contain NAs for any temp or moisture values
#Need a better way to check this and incorporate into a QC summary table
for (i in 1:length(drop_raw)) {
  print(i)
  print(summary(is.na(drop_raw[[i]][ , 1:6])))
}
#some sensors have added columns just full of NAS (15 rather than 8 columns, 7 with all NAs)
#7983  7983  7983 7983  7983
#Not sure why - make sure this does not impact downstream data manipulation


#see what is going on with the weird datafiles-------------------------------------
#Looks like sonmeone probably opened the files in excel and them saved them,
#which added NAs and changed the datatime format - don't do this!
#affects these items in the list: 151, 152, 153, 160, 161

str(drop_raw[[151]])
str(drop_raw[[152]])
str(drop_raw[[153]])
str(drop_raw[[160]])
str(drop_raw[[161]])

#split out dataframes with different date_time formats into two lists-------------

drop_raw1 <- drop_raw[-c(151, 152, 153, 160, 161)]
length(drop_raw1) #163

drop_raw2 <- drop_raw[c(151, 152, 153, 160, 161)]
length(drop_raw2) #5

#recheck dates and dimensions
date_chk <- data.frame()
for (i in 1:length(drop_raw2)) {
  tmp <-
    data.frame(
      sensor = names(drop_raw2[i]),
      data_time_strt = drop_raw2[[i]][1, "date_time"],
      data_time_end = drop_raw2[[i]][nrow(drop_raw2[[i]]), "date_time"]
    )
  date_chk <- rbind(date_chk, tmp)
}
date_chk

sapply(drop_raw2, dim)

#drop weird columns with all NAs
str(drop_raw2)
drop_raw2 <- lapply(drop_raw2, function(x) { x[ -c(9:15)] })


## ******************************************************************************************
## Format date and time
## ******************************************************************************************


drop_dt1 <- lapply(drop_raw1, function(x) cbind(x, datetime = parse_date_time(x$date_time, '%Y-%m-%d %H:%M:%S %p')))
str(drop_dt1)

drop_dt2 <- lapply(drop_raw2, function(x) cbind(x, datetime = parse_date_time(x$date_time, '%m/%d/%Y %H:%M')))
str(drop_dt2)

drop_dt <- c(drop_dt1, drop_dt2)
length(drop_dt) #168
str(drop_dt)

#Panama is UTC-5 timezone, as is Edgewater during standard time (EST)
#Maybe just use standard time (UTC-5) offset for all data and not worry about daylight savings
#like most of the world???
#drop_dt <- lapply(drop_dt, function(x) cbind(x, datetime_loc = x$datetime - hours(5)))

#Separate out date and time elements
drop_dt <- lapply(drop_dt, function(x) cbind(x, hour = as.POSIXlt(x$datetime)$hour))
drop_dt <- lapply(drop_dt, function(x) cbind(x, minute = as.POSIXlt(x$datetime)$min))
drop_dt <- lapply(drop_dt, function(x) cbind(x, month = as.POSIXlt(x$datetime)$mon+1))
drop_dt <- lapply(drop_dt, function(x) cbind(x, day = day(as.POSIXlt(x$datetime))))
drop_dt <- lapply(drop_dt, function(x) cbind(x, date = date(x$datetime)))
drop_dt <- lapply(drop_dt, function(x) cbind(x, time = format(as.POSIXct(x$datetime), format = "%H:%M")))


#Perform additional data completeness QC checks***************************************
#Wrap into QC check function/s

#Check if files contain NAs for any timestamps
#again as lubridate sometimes introduces NAs (not sure why)
for (i in 1:length(drop_dt)) {
  print(i)
  print(summary(is.na(drop_dt[[i]][ , "datetime"])))
}
#18 NAs in two of the dataframes


#Check that all hour intervals are 10 min
tdiff_chk <- data.frame()
for (i in 1:length(drop_dt)) {
  tmp <- as.data.frame(table(diff(drop_dt[[i]][ , "datetime"], lag = 1)))
  tmp$sensor <- names(drop_dt[i])
  tmp$units <- attributes(diff(drop_dt[[i]][ , "datetime"], lag = 1))$units
  tmp$index <- i
  colnames(tmp)[1] <- "interval" 
  tdiff_chk <- rbind(tdiff_chk, tmp)
}

which(diff(drop_dt[[11]][ , "datetime"], lag = 1) > 10)
drop_dt[[11]][1:25 , ]

#Lots of time intervals are different from 10 minutes. 
#Need to trim data to in/out date/times and recheck


#*******************************************************************************


#Trim to within in/out dates/times for each sensor---------------------------------
#cleanup deployment data
dim(meta_dat)
str(meta_dat)
meta_dat <- meta_dat[ , 1:10]
summary(as.factor(meta_dat$Deployment..))
meta_dat <- subset(meta_dat, Deployment.. != "checkup")
dim(meta_dat) #167  10


length(unique(meta_dat$Drop..)) #19 unique drops
length(unique(meta_dat$Site)) #17 Unique sites

#get earliest deployment and latest collection date for each sensor
meta_dat$deploy.date <- as.POSIXct(meta_dat$Deployment.date, format = '%m/%d/%Y')
meta_dat$collect.date <- as.POSIXct(meta_dat$Collected.date, format = '%m/%d/%Y')

ddate <- aggregate(
  list(deploy.date = meta_dat$deploy.date),
  by = list(drop = meta_dat$Drop..),
  FUN = min,
  na.rm = TRUE
) #include na.rm if NAs in data

cdate <- aggregate(
  list(collect.date = meta_dat$collect.date),
  by = list(drop = meta_dat$Drop..),
  FUN = max,
  na.rm = TRUE
) #include na.rm if NAs in data

summary(ddate$drop == cdate$drop)
ddate <- cbind(ddate, cdate[ , 2])
colnames(ddate)[3] <- "collect.date"

length(unique(names(drop_dt))) #19 unique sensors
ddate <- ddate[ddate$drop %in% unique(names(drop_dt)), ]
dim(ddate) #19 3

#Subset to data between deploy and collect dates for each sensor
drop_foc <- list()
for (i in 1:length(drop_dt)) {
  first <- ddate[which(ddate$drop == names(drop_dt[i])), "deploy.date"] + days(1)
  last <- ddate[which(ddate$drop == names(drop_dt[i])), "collect.date"] - days(1)
  
  tmp <- subset(drop_dt[[i]], date >= first & date <= last)
  
  drop_foc[[i]] <- tmp
}

names(drop_foc) <- names(drop_dt)

sapply(drop_raw, dim)
sapply(drop_foc, dim)

length(drop_foc) #168
str(drop_foc)


#Perform additional data completeness QC checks-----------------------------------
#Wrap into QC check function/s

#Check if files contain NAs for any timestamps
#again as lubridate sometimes introduces NAs (not sure why)
for (i in 1:length(drop_foc)) {
  print(i)
  print(summary(is.na(drop_foc[[i]][ , "datetime"])))
}
#No NAs here; one dataframe was empty (no data within date range?)
str(drop_foc[[136]])

#Drop empty dataframe
drop_foc <- drop_foc[-c(136)]

#Check that all hour intervals are 10 min
tdiff_chk <- data.frame()
for (i in 1:length(drop_foc)) {
  tmp <- as.data.frame(table(diff(drop_foc[[i]][ , "datetime"], lag = 1)))
  tmp$sensor <- names(drop_foc[i])
  tmp$units <- attributes(diff(drop_foc[[i]][ , "datetime"], lag = 1))$units
  tmp$index <- i
  colnames(tmp)[1] <- "interval" 
  tdiff_chk <- rbind(tdiff_chk, tmp)
}

#There are a number of timesteps > 10
which(diff(drop_foc[[59]][ , "datetime"], lag = 1) > 10)
drop_foc[[59]][3875:3880 , ] #skips from 10:20 pm to 11:30 pm 

which(diff(drop_foc[[140]][ , "datetime"], lag = 1) > 10)
drop_foc[[140]][2525:2535, ] #skips from 11:50 pm on 5/16 to 12:00 am on 5/28 
drop_foc[[140]][4640:4645, ] #skips from 4:00 pm to 4:20 pm

which(diff(drop_foc[[140]][ , "datetime"], lag = 1) < 10)
drop_foc[[140]][35:45, ] #skips backwards from 23:50 on 5/17 to 00:00 on 5/17? 
drop_foc[[140]][1580:1588, ] #skips backwars from 5/27 to 5/10!!

which(diff(drop_foc[[100]][ , "datetime"], lag = 1) > 1000)
drop_foc[[100]][1300:1305, ] #Skips forward 5 days in June - data gaps?

#Some time intervals are still different from 10 minutes. 
#Most are positive and could represent data gaps

summary(tdiff_chk$interval)
tdiff_chk[which(tdiff_chk$interval == -25290 |
                  tdiff_chk$interval == -1430), ]

which(diff(drop_foc[[149]][ , "datetime"], lag = 1) < 1)
drop_foc[[100]][1300:1305, ] #Skips forward 5 days in June - data gaps?

#Some are negative - all from sensor 7964. May need to drop this sensor
#Sensor 7964 was at the open site at St. Lukes
#seems like it is just these two time jumps repeated across downloads
which(diff(drop_foc[[140]][ , "datetime"], lag = 1) < 10)
drop_foc[[140]][35:45, ] #skips backwards from 23:50 on 5/17 to 00:00 on 5/17? 
drop_foc[[140]][1580:1588, ] #skips backwards from 5/27 to 5/10!!



#----------------------------------------------------------------------------------


#Will ultimately want to be able to generate summary stats for day and night for each month
#e.g., mean daytime temperature for june, july, etc

# #Here just subsetted to 7/15/23 to 9/20/23
# tms4_foc <- lapply(drop_dt, function(x) subset(x, datetime_loc >= "2023-07-15 00:00:00" & datetime_loc <= "2023-09-20 23:59:00"))


#Drop empty dataframe/s from list
sapply(drop_foc, dim)
# names(drop_foc)
# drop_foc[25]
# drop_foc <- drop_foc[-25]
# length(drop_foc) #34


#Combine all into a single dataframe
drop_foc.df <- do.call(rbind, unname(Map(cbind, sensor = names(drop_foc), drop_foc)))
str(drop_foc.df) #1606053 obs. of  16 variables
table(drop_foc.df$sensor) #Numbers match those expected from sum of individual 
#downloads in drop_foc list

#Subset to unique cases, as there are lots of duplicate records from consecutive
#downloads where the memory was not cleared
drop.df <- unique(drop_foc.df[ , ])
str(drop.df) #386393 obs. of  16 variables


#Merge in site info
site.dat <- data.frame(sites = sites.names, sensor = drop.id)
site.dat <- unique(site.dat[ , ])
drop.df <-
  merge(
    drop.df,
    site.dat,
    by.x = c("sensor"),
    by.y = c("sensor"),
    all.x = TRUE
  )
dim(drop.df) #386393     17


#Perform more data completeness QC checks----------------------------------------

#Check that ranges of temperature and moisture values makes sense
#Need to improve/automate this part too

#Some variables converted to character for some reason
drop.df$wb_temp <- as.numeric(drop.df$wb_temp)
drop.df$rel_hum <- as.numeric(drop.df$rel_hum)
drop.df$heat_index <- as.numeric(drop.df$heat_index)
drop.df$dew_point <- as.numeric(drop.df$dew_point)

#which observations are NAs
drop.df[is.na(drop.df$wb_temp), ]
drop.df[88275:88285, ]
#All four NAs are from sensor 7964 on 9-8-24 at open St Lukes

summary(drop.df$temp)
summary(drop.df$wb_temp) #4 NAs
summary(drop.df$rel_hum) #4 NAs
summary(drop.df$stat_pres)
summary(drop.df$heat_index) #4 NAs
summary(drop.df$dew_point) #4 NAs
summary(drop.df$datetime)
summary(as.factor(drop.df$sites))
#No extreme values!!

aggregate(
  list(min.date = drop.df$datetime),
  by = list(sites = drop.df$sites),
  FUN = min,
  na.rm = TRUE
) #include na.rm if NAs in data

#             sites            min.date
# 1       Back_LGB1 2024-06-28 00:00:00
# 2  Classroom_SMC7 2024-04-12 00:00:00
# 3    Forest2_SLF2 2024-04-13 00:00:00
# 4    Forest3_SLR3 2024-05-10 10:40:00
# 5  MuddyCreek_MC1 2024-04-11 00:00:00
# 6      NEON002_N2 2024-04-11 00:00:00
# 7      NEON007_N7 2024-04-10 00:00:00
# 8      NEON008_N8 2024-04-10 00:00:00
# 9      NEON009_N9 2024-04-11 00:00:00
# 10    NEON017_N17 2024-04-10 00:00:00
# 11    NEON018_N18 2024-04-10 00:00:00
# 12    NEON019_N19 2024-04-10 00:00:00
# 13      Open_SMO9 2024-04-12 00:00:00
# 14     Open1_SLO1 2024-04-13 00:00:00
# 15  Overlook_SMO6 2024-04-12 00:00:00
# 16     Pool_SMP11 2024-04-12 00:00:00
# 17  SweetHope_SH4 2024-04-12 00:00:00

aggregate(
  list(max.date = drop.df$datetime),
  by = list(sites = drop.df$sites),
  FUN = max,
  na.rm = TRUE
) #include na.rm if NAs in data

#             sites            max.date
# 1       Back_LGB1 2024-09-24 23:50:00
# 2  Classroom_SMC7 2024-09-24 23:50:00
# 3    Forest2_SLF2 2024-09-22 23:50:00
# 4    Forest3_SLR3 2024-07-08 23:50:00
# 5  MuddyCreek_MC1 2024-09-22 23:50:00
# 6      NEON002_N2 2024-09-22 23:50:00
# 7      NEON007_N7 2024-09-22 23:50:00
# 8      NEON008_N8 2024-09-22 23:50:00
# 9      NEON009_N9 2024-09-22 23:50:00
# 10    NEON017_N17 2024-09-22 23:50:00
# 11    NEON018_N18 2024-09-22 23:50:00
# 12    NEON019_N19 2024-09-22 23:50:00
# 13      Open_SMO9 2024-09-24 23:50:00
# 14     Open1_SLO1 2024-09-22 23:50:00
# 15  Overlook_SMO6 2024-09-24 23:50:00
# 16     Pool_SMP11 2024-09-24 23:50:00
# 17  SweetHope_SH4 2024-09-24 23:50:00


#further trim data to common min and max dates - 2024-05-10 10:40:00 to 2024-07-08 03:50:00
drop.df <- subset(drop.df, datetime >= "2024-05-15 00:00:00" & datetime <= "2024-09-20 00:00:00")
str(drop.df) #303402 obs. of  17 variables
range(drop.df$datetime) #"2024-05-15 04:00:00 UTC" "2024-09-20 04:00:00 UTC"

drop.df$record_ID <- seq.int(nrow(drop.df))


#--------------------------------------------------------------------------------

#Add hour + minute variable for plotting
drop.df$hour.min <- drop.df$hour + (drop.df$minute/60)

#convert to temp to C
head(drop.df)
drop.df$temp_c <- (drop.df$temp - 32)/1.8
drop.df$wb_temp_c <- (drop.df$wb_temp - 32)/1.8
drop.df$heat_index_c <- (drop.df$heat_index - 32)/1.8

#add habitat and site factors
drop.df$habitat <- as.factor(drop.df$sites)
drop.df$location <- as.factor(drop.df$sites)

neon <-read.csv("./NEON_plots_habitat.csv") #plot 20 is on edge of field and forest

levels(drop.df$habitat) <- c("open", "forest", "forest", "forest", "forest", 
                             "forest", "open", "open", "forest", "forest",
                             "open", "forest", "open", "open", "open",
                             "forest", "open")
levels(drop.df$location)  <- c("urban", "urban", "urban", "urban", "exurban",
                               "exurban", "exurban", "exurban", "exurban", "exurban",
                               "exurban", "exurban", "urban", "urban", "urban",
                               "urban", "urban")

#Save dataframe with all (day and night) temp measurements
dim(drop.df) #303402     24

#set wd() to local project
setwd("C:/Users/Nowak/Documents/JUSTIN - Active_Documents/SERC/Research Projects/Science and Faith/Acoustic_monitoring/urban_audiomoth_project_11.11.24")

#write.csv(drop.df, "Data/cleaned_drop_data/drop_data_may_sept_2024_24hrs_11.10.24.csv", row.names = FALSE)


## ******************************************************************************************
## Aggregate to daily metrics for each sensor
## ******************************************************************************************

#select daytime hours (based on approx times of sunrise and sunset for focal dates - check this)
# drop_day <- subset(drop.df, hour  >= 6 & hour <= 20) #for temperate zone
# dim(drop_day) #189099     24

#Create day/night factor
drop.df$day_night <- cut(drop.df$hour, breaks = c(-Inf, 5, 20, Inf), labels = c("night","day", "night"))
dim(drop.df) #303402     25

table(drop.df$hour, drop.df$day_night)
drop_day <- drop.df

length(unique(drop_day$date)) #128 days in dataset
table(drop_day$date, drop_day$sites)
#Data missing only from site Forest3_SLR3 for 6/18-6/20
#7 sites x 58 days = 406-2 = 404 expected rows in aggregated data

#Calculate daily 95th percentile temperature and a conservative measure of maximum,
#eliminating potential outlier measurements (may need to revisit after assesssing bias with thermocouple)
colnames(drop_day)
daily_95 <-
  aggregate(
    list(
      temp_95 = drop_day$temp_c,
      wb_temp_95 = drop_day$wb_temp_c,
      heat_index_95 = drop_day$heat_index_c,
      rel_hum_95 = drop_day$rel_hum
    ),
    by = list(sites = drop_day$sites,
              date = drop_day$date,
              day_night = drop_day$day_night),
    FUN = function(x)
      quantile(x, 0.95, na.rm = TRUE)
  ) #include na.rm if NAs in data

dim(daily_95) #4042    7 (2013    6)
summary(daily_95) #No NAs



#Calculate max daily temperature
colnames(drop_day)
daily_max <-
  aggregate(
    list(
      temp_max = drop_day$temp_c,
      wb_temp_max = drop_day$wb_temp_c,
      heat_index_max = drop_day$heat_index_c,
      rel_hum_max = drop_day$rel_hum
    ),
    by = list(sites = drop_day$sites,
              date = drop_day$date,
              day_night = drop_day$day_night),
    FUN = function(x) max(x, na.rm = TRUE)
  ) #include na.rm if NAs in data

dim(daily_max) #4042    7 (2013    6)
summary(daily_max) #No NAs

#Calculate mean daily temperature
colnames(drop_day)
daily_mn <-
  aggregate(
    list(
      temp_mn = drop_day$temp_c,
      wb_temp_mn = drop_day$wb_temp_c,
      heat_index_mn = drop_day$heat_index_c,
      rel_hum_mn = drop_day$rel_hum
    ),
    by = list(sites = drop_day$sites,
              date = drop_day$date,
              day_night = drop_day$day_night),
    FUN = function(x) mean(x, na.rm = TRUE)
  ) #include na.rm if NAs in data

dim(daily_mn) #4042    7  (2013    6)
summary(daily_mn) #No NAs


#Calculate daily 5th percentile temperature and a conservative measure of minimum,
colnames(drop_day)
daily_05 <-
  aggregate(
    list(
      temp_05 = drop_day$temp_c,
      wb_temp_05 = drop_day$wb_temp_c,
      heat_index_05 = drop_day$heat_index_c,
      rel_hum_05 = drop_day$rel_hum
    ),
    by = list(sites = drop_day$sites,
              date = drop_day$date,
              day_night = drop_day$day_night),
    FUN = function(x)
      quantile(x, 0.05, na.rm = TRUE)
  ) #include na.rm if NAs in data

dim(daily_05) #4042    7  (2013    6)
summary(daily_05) #No NAs

summary(daily_95$sites == daily_mn$sites) #all match
summary(daily_95$sites == daily_05$sites) #all match
summary(daily_95$date == daily_mn$date) #all match

#combine summaries
daily <-
  cbind(
    daily_95,
    daily_max[, c("temp_max", "wb_temp_max", "heat_index_max", "rel_hum_max")],
    daily_mn[, c("temp_mn", "wb_temp_mn", "heat_index_mn", "rel_hum_mn")],
    daily_05[, c("temp_05", "wb_temp_05", "heat_index_05", "rel_hum_05")])
dim(daily) #4042   19 (2013    18)
head(daily)
plot(daily$temp_max, daily$temp_95)
plot(daily$wb_temp_95, daily$temp_95)
abline(0, 1)

#add habitat and site factors
daily$habitat <- as.factor(daily$sites)
daily$location <- as.factor(daily$sites)

levels(daily$habitat) <- c("open", "forest", "forest", "forest", "forest", 
                           "forest", "open", "open", "forest", "forest",
                           "open", "forest", "open", "open", "open",
                           "forest", "open")
levels(daily$location)  <- c("urban", "urban", "urban", "urban", "exurban",
                             "exurban", "exurban", "exurban", "exurban", "exurban",
                             "exurban", "exurban", "urban", "urban", "urban",
                             "urban", "urban")

table(daily$sites, daily$habitat)
table(daily$sites, daily$location)
dim(daily) #4042   21

#Save summaries
#write.csv(daily, "Data/cleaned_drop_data/drop_data_2024_daytime_summaries_11.10.24.csv", row.names = FALSE)
#write.csv(daily, "Data/cleaned_drop_data/drop_data_2024_day_night_summaries_11.23.24.csv", row.names = FALSE)

#Compare files--------------------------------------------------------------------

day_tmp <- read.csv("Data/cleaned_drop_data/drop_data_2024_daytime_summaries_11.10.24.csv")
dim(day_tmp) #2013   20

day <- subset(daily, day_night == "day")
dim(day)

summary(day_tmp$date == day$date) #All match
plot(day_tmp$wb_temp_mn, day$wb_temp_mn) #All match
abline(0, 1)


## ******************************************************************************************
## Make plots
## ******************************************************************************************

#Reload data from file if needed (please see note on issues when reformatting timestamps)

library(lubridate)
# tms4_jul.dat <- read.csv("data/TOMST data/SERC_downloaded_Fall 2023/cleaned data and summary stats/tms4_data_072023_24hrs_11.14.23.csv")

# #Reconvert timestamp to posixct and calculate local time (saving as/loading csv messes with format)
# tms4_jul.dat$datetime = as.POSIXct(tms4_jul.dat$datetime, format='%Y-%m-%d %H:%M', tz = "UTC")
# summary(is.na(tms4_jul.dat$datetime)) 
# #NAs introduced when reformatting to posix - Jonathan, can you please figure out why?
# #We shouldn't have any NAs at this point...
# 
# #Subtract time zone
# #Edgewater, MD timezone is UTC-4 hours
# tms4_jul.dat$datetime_loc <- tms4_jul.dat$datetime - hours(5)
# summary(is.na(tms4_jul.dat$datetime_loc))
# 
# #Re-Check that all hour intervals are 15 min 
# tdiff_chk <- as.data.frame(table(diff(tms4_jul.dat[ , "datetime_loc"], lag = 1)))
# colnames(tdiff_chk)[1] <- "interval" 
# tdiff_chk



#Plot temp versus date and hour~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#install.packages("ggplot2", dependencies = TRUE)
library(ggplot2)
theme_set(theme_bw())

sens_ids <- unique(drop.df$sensor)
levels(drop.df$habitat)
levels(as.factor(drop.df$sites))

levels(daily$habitat) <- c("open", "forest", "forest", "forest", "forest", 
                           "forest", "open", "open", "forest", "forest",
                           "open", "forest", "open", "open", "open",
                           "forest", "open")

#Re-order factor levels for 24 hr data
drop.df$sites <- as.factor(drop.df$sites)
drop.df$sites <-
  factor(drop.df$sites,
         levels = c("Back_LGB1", "NEON007_N7", "NEON008_N8", "NEON018_N18",
                    "Open_SMO9", "Open1_SLO1", "Overlook_SMO6", "SweetHope_SH4",
                    "Pool_SMP11", "Classroom_SMC7", "Forest2_SLF2", "Forest3_SLR3", "MuddyCreek_MC1", 
                    "NEON002_N2", "NEON009_N9", "NEON017_N17", "NEON019_N19"))
levels(drop.df$sites)

drop.df$habitat <-
  factor(drop.df$habitat,
         levels = c("open", "forest"))

levels(drop.df$habitat)

#Re-order factor levels for daiy summaries 
daily$sites <- as.factor(daily$sites)
levels(daily$sites)
daily$sites <-
  factor(daily$sites,
         levels = c("Back_LGB1", "NEON007_N7", "NEON008_N8", "NEON018_N18",
                    "Open_SMO9", "Open1_SLO1", "Overlook_SMO6", "SweetHope_SH4",
                    "Pool_SMP11", "Classroom_SMC7", "Forest2_SLF2", "Forest3_SLR3", "MuddyCreek_MC1", 
                    "NEON002_N2", "NEON009_N9", "NEON017_N17", "NEON019_N19"))

levels(daily$habitat)
daily$habitat <-
  factor(daily$habitat,
         levels = c("open", "forest"))

table(daily$sites, daily$location, daily$habitat)

#Plot and save  - lets loop through sensors and save as pngs so we can visually
#inspect and makes sure everything looks ok

#temperature versus date and hour
png("./Figures_7-26-24/sci_faith_air_temp_2024_24hrs_07.26.24.png", width = 9.7, height = 4.3, units = 'in', res = 600)
temp.date_plot <-
  ggplot(daily, aes(date, wb_temp_95, group = factor(sites), color=factor(habitat))) +
  facet_grid(cols = vars(location)) +
  #geom_point(aes(color = factor(sites)), shape = 16, size = 1, stroke = 1, alpha = 0.55) + #color = "#2d718eff"
  geom_line(aes(color = factor(habitat)), size = 1, alpha = 0.75) + #color = "#2d718eff"
  scale_color_manual(values=c("#fba238ff", "#29af7fff")) +
  # ggplot(btree_sub.df.cov, aes(datetime_loc, temp.3, color = LAI)) +
  # geom_point(aes(color = LAI), shape = 16, size = 1, stroke = 1, alpha = 0.55) +
  # scale_color_viridis_c(option = "viridis") +
  #guides(col= guide_legend(title= "Tree diversity")) +
  theme(axis.text = element_text(size = 14)) + 
  #theme(legend.title=element_blank()) +
  #theme(legend.position = "none") +
  theme(legend.title=element_blank()) +
  theme(legend.text = element_text(size = 14)) +
  theme(strip.text = element_text(size = 12)) +
  theme(panel.grid.major = element_line(colour = "white")) + 
  theme(panel.grid.minor = element_line(colour = "white")) + 
  theme(panel.grid.major.y = element_line(colour = "white")) +
  theme(panel.border = element_rect(color = "black", size = 1)) +
  theme(axis.text = element_text(size = 14)) + 
  theme(axis.title.y=element_text(size = 14)) +
  theme(axis.title.x=element_text(size = 14)) +
  #scale_x_continuous(expand = c(0.015, 0.009)) +
  #scale_y_continuous(breaks = seq(-1.5, 1.5,by=0.5)) + 
  theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, unit = "cm")) +
  xlab("Date") + 
  #ylab("Soil moisture (pulse rate)")
  ylab("Temperature (°C)") 
#labs(y = expression("Temperature " *~degree*C))
temp.date_plot
dev.off()


#Q95 temperature versus date
png("./Figures_7-26-24/sci_faith_95_air_temp_2024_day_v2_07.26.24.png", width = 9.7, height = 4.3, units = 'in', res = 600)
temp.date_plot <-
  ggplot(daily, aes(date, temp_95, group = factor(sites), color=factor(location))) +
  facet_grid(cols = vars(habitat)) +
  #geom_point(aes(color = factor(sites)), shape = 16, size = 1, stroke = 1, alpha = 0.55) + #color = "#2d718eff"
  geom_line(aes(color = factor(location)), size = 1, alpha = 0.75) + #color = "#2d718eff"
  scale_color_manual(values=c("#fba238ff", "#29af7fff")) +
  # ggplot(btree_sub.df.cov, aes(datetime_loc, temp.3, color = LAI)) +
  # geom_point(aes(color = LAI), shape = 16, size = 1, stroke = 1, alpha = 0.55) +
  # scale_color_viridis_c(option = "viridis") +
  #guides(col= guide_legend(title= "Tree diversity")) +
  theme(axis.text = element_text(size = 14)) + 
  #theme(legend.title=element_blank()) +
  #theme(legend.position = "none") +
  theme(legend.title=element_blank()) +
  theme(legend.text = element_text(size = 14)) +
  theme(strip.text = element_text(size = 12)) +
  theme(panel.grid.major = element_line(colour = "white")) + 
  theme(panel.grid.minor = element_line(colour = "white")) + 
  theme(panel.grid.major.y = element_line(colour = "white")) +
  theme(panel.border = element_rect(color = "black", size = 1)) +
  theme(axis.text = element_text(size = 14)) + 
  theme(axis.title.y=element_text(size = 14)) +
  theme(axis.title.x=element_text(size = 14)) +
  #scale_x_continuous(expand = c(0.015, 0.009)) +
  #scale_y_continuous(breaks = seq(-1.5, 1.5,by=0.5)) + 
  theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, unit = "cm")) +
  xlab("Date") + 
  #ylab("Soil moisture (pulse rate)")
  #ylab("Relative humidity (%)")
  ylab("Q95 temperature (°C)") 
#labs(y = expression("Temperature " *~degree*C))
temp.date_plot
dev.off()


#temperature versus hour
png("./Figures_7-26-24/sci_faith_Q95_air_temp_2024_day_07.26.24.png", width = 9.7, height = 4.3, units = 'in', res = 600)
temp.hour_plot <-
  ggplot(drop.df, aes(hour.min, temp_c, group = factor(sites), color=factor(habitat))) +
  facet_grid(cols = vars(location)) +
  geom_point(aes(color = factor(habitat)), shape = 16, size = 1, stroke = 1, alpha = 0.55) + #color = "#2d718eff"
  #geom_line(aes(color = factor(habitat)), size = 1, alpha = 0.75) + #color = "#2d718eff"
  scale_color_manual(values=c("#fba238ff", "#29af7fff")) +
  # ggplot(btree_sub.df.cov, aes(datetime_loc, temp.3, color = LAI)) +
  # geom_point(aes(color = LAI), shape = 16, size = 1, stroke = 1, alpha = 0.55) +
  # scale_color_viridis_c(option = "viridis") +
  #guides(col= guide_legend(title= "Tree diversity")) +
  theme(axis.text = element_text(size = 14)) + 
  #theme(legend.title=element_blank()) +
  #theme(legend.position = "none") +
  theme(legend.title=element_blank()) +
  theme(legend.text = element_text(size = 14)) +
  theme(strip.text = element_text(size = 12)) +
  theme(panel.grid.major = element_line(colour = "white")) + 
  theme(panel.grid.minor = element_line(colour = "white")) + 
  theme(panel.grid.major.y = element_line(colour = "white")) +
  theme(panel.border = element_rect(color = "black", size = 1)) +
  theme(axis.text = element_text(size = 14)) + 
  theme(axis.title.y=element_text(size = 14)) +
  theme(axis.title.x=element_text(size = 14)) +
  #scale_x_continuous(expand = c(0.015, 0.009)) +
  #scale_y_continuous(breaks = seq(-1.5, 1.5,by=0.5)) + 
  theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, unit = "cm")) +
  xlab("Hour") + 
  #ylab("Soil moisture (pulse rate)")
  ylab("Temperature (°C)") 
#labs(y = expression("Temperature " *~degree*C))
temp.hour_plot
dev.off()


#plot data completeness-----------------------------------------------------------

site_ord <- data.frame(sites = levels(daily$sites), site.ord = 1:17)

daily$hab_loc <- paste(daily$habitat, daily$location, sep = "_")
levels(as.factor(daily$hab_loc))

#add NAs to site X dates with missing values
sitexdate <- expand.grid(unique(daily$date), unique(daily$sites))
colnames(sitexdate) <- c("date", "site")
sitexdate$site_date <- paste(sitexdate$site, sitexdate$date, sep = "_")
daily$site_date <- paste(daily$sites, daily$date, sep = "_")

daily.exp <-
  merge(
    sitexdate,
    daily,
    by.x = c("site_date"),
    by.y = c("site_date"),
    all.x = TRUE
  )
dim(daily.exp) #2176   25

daily.exp <-
  merge(
    daily.exp,
    site_ord,
    by.x = c("site"),
    by.y = c("sites"),
    all.x = TRUE
  )
dim(daily.exp) #2013   26

levels(as.factor(daily.exp$hab_loc))
lev <- levels(as.factor(daily.exp$site))

png("./Figures_11-10-24/sci_faith_drop_data_completeness_so_sites_2024_11.10.24.png", width = 9.0, height = 5.5, units = 'in', res = 600)
series <- ggplot(daily.exp, aes(date.x, site.ord.y, colour = hab_loc)) + 
  geom_line(aes(group = factor(site.ord.y)), size = 8) + #MCMCglmm estimates only
  scale_color_manual(values=c("darkgreen", "green", "darkorange3", "orange")) + 
  #annotate("text", x = daily.exp$date.x[1005], y = 1:17, label = lev) +
  theme(axis.text = element_text(size = 16)) + 
  theme(panel.grid.major = element_line(colour = "white")) + 
  theme(panel.grid.minor = element_line(colour = "white")) + 
  theme(panel.border = element_rect(color = "black", size = 1)) +
  #theme(axis.title.x=element_blank()) +
  #theme(legend.title=element_blank()) +
  guides(fill = guide_legend(title = "Location type", title.theme = element_text(size = 16))) + 
  labs(y = expression("Time series")) +
  labs(x = expression("")) +
  labs(color='Landscape type') +
  theme(axis.title.y=element_text(size = 16)) +
  theme(axis.title.x=element_text(size = 16)) +
  theme(axis.text.x=element_text(size = 16, angle=45, hjust=1)) +
  theme(legend.title=element_text(size = 14)) +
  theme(legend.text=element_text(size = 16)) +
  theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, unit = "cm"))
#coord_flip() +
#scale_y_continuous(limits = c(-0.8, 2))
series
dev.off()



