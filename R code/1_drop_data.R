###########################################################################################################################################################
###(1) Extracting and summarizing data from drop sensors##################################################################################################
########################################################################################################################################################
###updated 11-26-24
#R version 4.3.1

#Questions for Aidan:

#Subtract time zone? WHAT IS THE TIME ZONE OF THE DATA (local ET???)
#NEED TO ASK AIDAN

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

#What the raw data look like
# 0;31.10.2013 11:45;0;21.5625;22.0625;23.125;148;1;0

#What the values are
# 0 	index of the measure
# 31.10.2013 11:45 	date and time in UTC
# 0 	time zone
# 21.5625 	T1
# 22.0625 	T2
# 23.125 	T3
# 148 	soil moisture count (raw moisture data)
# 1 	shake - values 1 (shake) or 0. Only for old model TMS-3.
# 0 	errFlag (if =1 the device couldn't convert time from PCF chip)


#Load metadata files
getwd()
meta_dat <- read.csv("Data/audiomoth_data_2024/SciFaith_Drop_metadata.csv")
#date_out <- read.csv("data/TOMST data/SERC_downloaded_Fall 2023/SERC_TMS4_datacollection_coords_Fall2023.csv")
#sens_ids <- read.csv("data/TOMST data/SERCsensorIDs.csv")

# #set wd() to SSD 
# setwd("D:/Science and Faith Audio Files")

#Load TOMST data files (wrap into a function that gets filenames, checks for duplicates,
#loads files into list and names columns)


#Load all drop files-------------------------------------------------------------
#get list of filenames
serc <- paste("SERC", dir("SERC"), sep = "/")
sl <- paste("StLukes", dir("StLukes"), sep = "/")
sh <- paste("SweetHope", dir("SweetHope"), sep = "/")
sm <- paste("Stillmeadow", dir("Stillmeadow"), sep = "/")
lg <- paste("LibertyGrace", dir("LibertyGrace"), sep = "/")

sens.names <- c(serc, sm, sl, sh, lg)

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

#Load all files from Drop sensors
drop_raw <- lapply(filenames, function(x) {
                            read.csv(x,  header = FALSE, skip = 5)
                          }) #big list of data files
names(drop_raw) <- drop.id #Assign sensor names to each df in list
length(drop_raw) #68


#Add column names
cols <- c("date_time", "temp", "wb_temp", "rel_hum", "stat_pres", "heat_index", "dew_point", "data_type")

drop_raw <- lapply(drop_raw, setNames, cols)
library(tidyverse)
drop_raw <- lapply(drop_raw, select, all_of(cols))
drop_raw <- lapply(drop_raw, drop_na)
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
#No NAs

####
#Count NAs in each df in drop_raw
count_nas <- function(df) {
  colSums(is.na(df))
}

na_counts <- lapply(drop_raw, count_nas)
drop_raw_nas <- do.call(rbind, na_counts)
rm(na_counts)
####

## ******************************************************************************************
## Format date and time
## ******************************************************************************************

rm(problematic_files)

drop_dt <- lapply(drop_raw, function(x) cbind(x, datetime = parse_date_time(x$date_time, '%Y-%m-%d %H:%M:%S %p')))

drop_dt <- lapply(drop_raw, function(x) {
  x %>% 
    mutate(datetime = as.POSIXct(date_time, format = '%Y-%m-%d %I:%M:%S %p'))
})

# it won't let me look at the dfs for each individual sensor (just gives me the 7983 with 11106 rows -- the first 7983 in the list)

#Note: there is an app for downloading the sensor data. Apparently an update caused a format change to the
#timestamp (hence the contortions above). IN the app, one can specify the date format (but the change went unnoticed.
#In the post-update format, there is no hour:minute timestamp at midnight so every date/time at midnight gets
#turned into an NA when converting to posix above (ugh).
#Ideally, we will avoid all of this in the future by specifying the date/time format that must be used in the
#app when downloading data. For now, we just have to deal with it the best we can.



#Panama is UTC-5 timezone, as is Edgewater during standard time (EST)
#Maybe just use standard time (UTC-5) offset for all data and not worry about daylight savings
#like most of the world??? seriously!!
#drop_dt <- lapply(drop_dt, function(x) cbind(x, datetime_loc = x$datetime - hours(5)))

#Separate out date and time elements
drop_dt <- lapply(drop_dt, function(x) cbind(x, hour = as.POSIXlt(x$datetime)$hour))
drop_dt <- lapply(drop_dt, function(x) cbind(x, minute = as.POSIXlt(x$datetime)$min))
drop_dt <- lapply(drop_dt, function(x) cbind(x, month = as.POSIXlt(x$datetime)$mon+1))
drop_dt <- lapply(drop_dt, function(x) cbind(x, day = day(as.POSIXlt(x$datetime))))
drop_dt <- lapply(drop_dt, function(x) cbind(x, date = as.POSIXct(strftime(x$datetime, format="%Y-%m-%d"))))
drop_dt <- lapply(drop_dt, function(x) cbind(x, time = format(as.POSIXct(x$datetime), format = "%H:%M"))) 

# all the SH4 ones get turned into NA -- different datetime format -- figure out later
# SH format:      "5/18/2024 10:40"
# Correct format: "2024-05-23 09:20:00 PM"
# correct format via excel: yyyy-mm-dd h:mm:ss AM/PM 

#Perform additional data completeness QC checks***************************************
#Wrap into QC check function/s

#Check if files contain NAs for any timestamps
#again as lubridate sometimes introduces NAs (not sure why)
for (i in 1:length(drop_dt)) {
  print(i)
  print(summary(is.na(drop_dt[[i]][ , "datetime"])))
}

# NAs table
na_counts <- lapply(drop_dt, count_nas)
drop_dt_nas <- do.call(rbind, na_counts)
rm(na_counts)

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

#Two sensors have time stamps that jump back in time
which(diff(drop_dt[[4]][ , "datetime"], lag = 1) > 10)
drop_dt[[4]][1:25 , ]

#Lots of time intervals are different from 10 minutes. 
#Need to trim data to in/out date/times and recheck


#*******************************************************************************


#Trim to within in/out dates/times for each sensor---------------------------------
#cleanup deployment data
str(meta_dat)
meta_dat <- meta_dat[ , 1:10]
summary(as.factor(meta_dat$Deployment..))
meta_dat <- subset(meta_dat, Deployment.. != "Checkup")

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

length(unique(names(drop_dt))) #8 unique sensors
ddate <- ddate[ddate$drop %in% unique(names(drop_dt)), ]
dim(ddate) #8 3

#Subset to desired date and time range 
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


#Perform additional data completeness QC checks-----------------------------------
#Wrap into QC check function/s

#Check if files contain NAs for any timestamps
#again as lubridate sometimes introduces NAs (not sure why)
for (i in 1:length(drop_foc)) {
  print(i)
  print(summary(is.na(drop_foc[[i]][ , "datetime"])))
}
#No NAs here


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

#Two sensors have time stamps that jump back in time
which(diff(drop_foc[[4]][ , "datetime"], lag = 1) > 10)
drop_foc[[4]][1:25 , ]

#Some (few) time intervals are still different from 10 minutes. 
#----------------------------------------------------------------------------------


#Will ultimately want to be able to generate summary stats for day and night for each month
#e.g., mean daytime temperature for june, july, etc

# #Here just subsetted to 7/15/23 to 9/20/23
# tms4_foc <- lapply(drop_dt, function(x) subset(x, datetime_loc >= "2023-07-15 00:00:00" & datetime_loc <= "2023-09-20 23:59:00"))


#Drop empty dataframe/s from list
sapply(drop_foc, dim)
names(drop_foc)
drop_foc[25]
drop_foc <- drop_foc[-25]
length(drop_foc) #34


#Combine all into a single dataframe
#optional - Jonathan, you may prefer to keep as a list or do things differently
drop_foc.df <- do.call(rbind, unname(Map(cbind, sensor = names(drop_foc), drop_foc)))
str(drop_foc.df) #154483 obs. of  16 variables
table(drop_foc.df$sensor) #Numbers match those expected from sum of individual 
#downloads in drop_foc list

#Subset to unique cases, as there are lots of duplicate recrods from consecutive
#downloads where the memory was not cleared
drop.df <- unique(drop_foc.df[ , ])
str(drop.df) #84399 obs. of  16 variables (so nearly half of ob were duplicates?)

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
dim(drop.df) #79920   17

drop.df <- drop.df %>% mutate_at(c('wb_temp', 'rel_hum', 
                                   'heat_index', 'dew_point'), 
                                 as.numeric)

#Perform more data completeness QC checks----------------------------------------

#Check that ranges of temperature and moisture values makes sense
#Need to improve/automate this part too

summary(drop.df$temp)
summary(drop.df$wb_temp)
summary(drop.df$rel_hum)
summary(drop.df$stat_pres)
summary(drop.df$heat_index)
summary(drop.df$dew_point)
summary(drop.df$datetime)
summary(as.factor(drop.df$sites))

#No NAs or extreme values!!

aggregate(
  list(min.date = drop.df$datetime),
  by = list(sites = drop.df$sites),
  FUN = min,
  na.rm = TRUE
) #include na.rm if NAs in data

# sites            min.date
# 1 Classroom_SMC7 2024-04-12 04:00:00
# 2   Forest2_SLF2 2024-04-13 04:00:00
# 3     NEON007_N7 2024-04-10 04:00:00
# 4    NEON019_N19 2024-04-10 04:00:00
# 5      Open_SMO9 2024-04-12 04:00:00
# 6     Open1_SLO1 2024-04-13 04:00:00

aggregate(
  list(max.date = drop.df$datetime),
  by = list(sites = drop.df$sites),
  FUN = max,
  na.rm = TRUE
) #include na.rm if NAs in data

# sites            max.date
# 1 Classroom_SMC7 2024-07-09 03:50:00
# 2   Forest2_SLF2 2024-07-09 03:50:00
# 3     NEON007_N7 2024-07-08 03:50:00
# 4    NEON019_N19 2024-07-08 03:50:00
# 5      Open_SMO9 2024-07-09 03:50:00
# 6     Open1_SLO1 2024-07-09 03:50:00



#further trim data to common min and max dates - 2024-05-10 10:40:00 to 2024-07-08 03:50:00
drop.df <- subset(drop.df, datetime >= "2024-04-13 04:00:00" & datetime <= "2024-07-08 03:50:00")
str(drop.df) #78107 obs. of  17 variables

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

#levels(drop.df$habitat) <- c("forest", "forest", "open", "forest", "open", "open")
levels(drop.df$habitat) <- c("forest", "open", "open", "forest")
#levels(drop.df$location)  <- c("urban", "urban", "exurban", "exurban", "urban", "urban")
levels(drop.df$location) <- c("urban", "urban", "urban", "urban")

#Save dataframe with all (day and night) temp measurements
dim(drop.df) #78107   23

#set wd() to local project
setwd("C:/Users/Nowak/Documents/JUSTIN - Active_Documents/SERC/Research Projects/Science and Faith/Acoustic_monitoring")

#write.csv(drop.df, "Data/cleaned_drop_data/drop_data_may_july_2024_24hrs_07.26.24.csv", row.names = FALSE)
#write.csv(drop.df, "C:/Users/kirchgrabera/Smithsonian Dropbox/Aidan Kirchgraber/Science and Faith/Aidan/audio_analysis/birdnet/drop_data_april_july_2024_24hrs_10.16.24.csv", row.names = FALSE)
write.csv(drop.df, "C:/Users/kirchgrabera/Smithsonian Dropbox/Aidan Kirchgraber/Science and Faith/Aidan/audio_analysis/birdnet/sm_drop_data_april_july_2024_24hrs_11.08.24.csv", row.names = FALSE)

## ******************************************************************************************
## Aggregate to daily metrics for each sensor
## ******************************************************************************************

#select daytime hours (based on approx times of sunrise and sunset for focal dates - check this)
drop_day <- subset(drop.df, hour  >= 6 & hour <= 20) #for temperate zone
dim(drop_day) #48492    23


length(unique(drop_day$date)) #87 days in dataset
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
              date = drop_day$date),
    FUN = function(x)
      quantile(x, 0.95, na.rm = TRUE)
  ) #include na.rm if NAs in data

dim(daily_95) #520   6
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
              date = drop_day$date),
    FUN = function(x) max(x, na.rm = TRUE)
  ) #include na.rm if NAs in data

dim(daily_max) #520   6
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
              date = drop_day$date),
    FUN = function(x) mean(x, na.rm = TRUE)
  ) #include na.rm if NAs in data

dim(daily_mn) #520   6
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
              date = drop_day$date),
    FUN = function(x)
      quantile(x, 0.05, na.rm = TRUE)
  ) #include na.rm if NAs in data

dim(daily_05) #520   6
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
dim(daily) #520   18
head(daily)
plot(daily$temp_max, daily$temp_95)
abline(0, 1)

#add habitat and site factors
daily$habitat <- as.factor(daily$sites)
daily$location <- as.factor(daily$sites)

#levels(daily$habitat) <- c("forest", "forest", "open", "forest", "open", "open")
#levels(daily$location) <- c("urban", "urban", "exurban", "exurban", "urban", "urban")

#for stillmeadow
levels(daily$habitat) <- c("forest", "open", "open", "forest")
levels(daily$location) <- c("urban", "urban", "urban", "urban")


table(daily$sites, daily$habitat)
table(daily$sites, daily$location)

#Save summaries
#write.csv(daily, "Data/cleaned_drop_data/drop_data_2024_daytime_summaries_07.26.24.csv", row.names = FALSE)
#write.csv(daily, "C:/Users/kirchgrabera/Smithsonian Dropbox/Aidan Kirchgraber/Science and Faith/Aidan/audio_analysis/birdnet/drop_data_2024_daytime_summaries_10.16.24.csv", row.names = FALSE)
write.csv(daily, "C:/Users/kirchgrabera/Smithsonian Dropbox/Aidan Kirchgraber/Science and Faith/Aidan/audio_analysis/birdnet/sm_drop_data_2024_daytime_summaries_11.08.24.csv", row.names = FALSE)

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

#Re-order factor levels for 24 hr data
drop.df$sites <- as.factor(drop.df$sites)
drop.df$sites <-
  factor(drop.df$sites,
         #levels = c("Open_SMO9", "Open1_SLO1", "SweetHope_SH4",  "NEON007_N7", "Classroom_SMC7", "Forest2_SLF2", "NEON019_N19"))
         levels = c("Classroom_SMC7", "Open_SMO9", "Overlook_SMO6", "Pool_SMP11"))
levels(drop.df$sites)

drop.df$habitat <-
  factor(drop.df$habitat,
         levels = c("open", "forest"))

levels(drop.df$habitat)

#Re-order factor levels for daily summaries 
daily$sites <- as.factor(daily$sites)
levels(daily$sites)
daily$sites <-
  factor(daily$sites,
         #levels = c("Open_SMO9", "Open1_SLO1", "SweetHope_SH4",  "NEON007_N7", "Classroom_SMC7", "Forest2_SLF2", "NEON019_N19"))
         levels = c("Classroom_SMC7", "Open_SMO9", "Overlook_SMO6", "Pool_SMP11"))

levels(daily$habitat)
daily$habitat <-
  factor(daily$habitat,
         levels = c("open", "forest"))



#Plot and save  - lets loop through sensors and save as pngs so we can visually
#inspect and makes sure everything looks ok

#temperature versus date and hour
png("./Figures_7-26-24/sci_faith_air_temp_2024_24hrs_07.26.24.png", width = 9.7, height = 4.3, units = 'in', res = 600)
temp.date_plot <-
  ggplot(daily, aes(date, temp_95, group = factor(sites), color=factor(habitat))) +
  facet_grid(cols = vars(location)) +
    #geom_point(aes(color = factor(sites)), shape = 16, size = 1, stroke = 1, alpha = 0.55) + #color = "#2d718eff"
  geom_line(aes(color = factor(habitat)), linewidth = 1, alpha = 0.75) + #color = "#2d718eff"
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




