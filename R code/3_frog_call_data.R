###########################################################################################################################################################
###(3) Prepping frog call detections from BirdNET##################################################################################################
########################################################################################################################################################
###updated 12-4-24
#R version 4.3.1

#NOTE: AudioMoth timestamps are in UTC - 0 (unlike drops). Script converts to local time.
#Eastern Daylight Time (EDT) is four hours behind UTC - 0 (UTC−04:00). 
#EDT is from second Sunday in March to the first Sunday in November.

#To do: 
#(1) #Need to run timestamp QC checks on audio data too, as for drop data!!
#(2) Make data completeness figure as for drop data (check against meta data!)
#    and make sure all files have been run through birdnet too
#(3) !!Need to ultimately redo validation step for the frogs
#(4) Determine why would there be more rows than unique recording IDs in site_time df? - done
#(5) Need to drop duplicate records for bird data as done here for frogs -done
#(6) !!Need to recalculate counts and summaries for birds as done here for frogs
#    Previous code neglected to include NAs as 0s :/ - done
#(7) Need to be 100% sure whether data reflect local time or UTCs

## ******************************************************************************************
## Preamble
## ******************************************************************************************

#Clears workspace
rm(list=ls())

library(ggplot2)
theme_set(theme_bw())
library(NSNSDAcoustics)
library(seewave)
library(stringr)

## ******************************************************************************************
## Load BirdNET results files with frog detections
## ******************************************************************************************

#set wd() to SSD 
setwd("D:/Science and Faith Audio Files")



#construct file paths------------------------------------------------------------

# #get list of filenames
serc <- paste("SERC", dir("SERC"), sep = "/")
sm <- paste("Stillmeadow", dir("Stillmeadow"), sep = "/")
sl <- paste("StLukes", dir("StLukes"), sep = "/")
sh <- paste("SweetHope", dir("SweetHope"), sep = "/")
lg <- paste("LibertyGrace", dir("LibertyGrace"), sep = "/")

sens.names <- c(serc, sm, sl, sh, lg)

# sites <- c("SERC/NEON007_N7", "SERC/NEON019_N19", "Stillmeadow/Open_SMO9", 
#            "Stillmeadow/Classroom_SMC7", "StLukes/Open1_SLO1", 
#            "StLukes/Forest3_SLR3", "SweetHope/SweetHope_SH4") 

sd <- c("/SD_A", "/SD_B")

temp <- expand.grid(sens.names, sd)
sites_sd <- paste(temp$Var1, temp$Var2, sep = "")

#Get subfolder names
foldernames <- vector()
for (i in 1:length(sites_sd)) {
  temp <-
    list.files(sites_sd[i])
  temp2 <- paste(sites_sd[i], temp, sep = "/")
  foldernames <- c(foldernames, temp2)
}

foldernames <- paste(foldernames, "results", sep = "/")
length(foldernames) #152

#Get birdnet results file names
filenames <- vector()
for (i in 1:length(foldernames)) {
  temp <-
    list.files(paste(foldernames[i], sep = "/"), pattern = "*.csv$")
  temp2 <- paste(foldernames[i], temp, sep = "/")
  filenames <- c(filenames, temp2)
}

length(filenames) #344769



#get ALL time stamps and filenames for each audio sample---------------------------------------------------
#****The gather birdnet results function does not add in files with no results
#We need to account for all sample files in dataframe, including those with no detections****

#funciton to grab text at end of file names
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

site_time <- data.frame()
for (i in 1:length(filenames)) {
  
  #Parse audiomoth filenames to get timestamps and site names
  fn <- substrRight(filenames[i], 35)
  fn <- substr(fn, 1, 15)
  fn2 <- paste(fn, ".WAV", sep = "")
  
  tmp <- audiomoth(fn2, tz = "UTC")
  pars <- str_split(filenames[i], "/")
  site <- pars[[1]][2]
  
  #Add to dataframe
  tmp <-
    cbind(tmp, data.frame(
      site = site,
      filepath = filenames[i],
      recording_ID = paste(site, fn, sep = "_"),
      index = i
    ))
  
  site_time <- rbind(site_time, tmp)
  
  print(i)
  
}
dim(site_time) #344769     11
length(unique(site_time$recording_ID)) #344223??
#Why would there be more rows than unique recording IDs?

#Need to determine why would there be more rows than unique recording IDs in site_time df?
tmp <- as.data.frame(table(site_time$recording_ID))
table(tmp$Freq)
tmp[which(tmp$Freq == 2), "Var1"]
site_time[which(site_time$recording_ID == "Pool_SMP11_20240627_165000"), ]
site_time[which(site_time$recording_ID == "SweetHope_SH4_20240501_190000"), ]
site_time[which(site_time$recording_ID == "Pool_SMP11_20240701_124000"), ]

#There are duplicate recordings from Stillmeadow pool, and SweetHope
#Looks like the same files got saved to both SD_A and SD_B folders for ~500 files
#Need to drop these duplicates so that they don't bias the analyses

#Drop duplicated recordings based on recording ID
colnames(site_time)
site_time_nodups <- site_time[!duplicated(site_time[ ,c('recording_ID')]), ]
dim(site_time_nodups) #344223     10
summary(unique(site_time_nodups$recording_ID) == unique(site_time$recording_ID))
#All Match

tmp <- as.data.frame(table(site_time_nodups$recording_ID))
table(tmp$Freq)

site_time <- site_time_nodups

#write.csv(site_time, file = "./process/site_time_11.23.24", row.names = FALSE)
#write.csv(site_time, file = "./process/site_time_nodups_11.25.24", row.names = FALSE)
site_time <- read.csv("./process/site_time_nodups_11.25.24")


#Load results files from BirdNET---------------------------------------------------

#modified birdnet_gather function (more efficient)
get.bndat <- function (results.directory) 
{
  paths <- list.files(results.directory, pattern = "*.csv$", full.names = TRUE)
  dat <- suppressWarnings(rbindlist(lapply(paths, function(x) read.csv(x))))
  
  return(dat)
}

#Loop over subdirectories and combine CSVs

length(foldernames) #152
bn_results <- data.frame()
for (i in 1:length(foldernames)) {
  
  #Load and combine results CSVs
  tmp.df <- get.bndat(foldernames[i])
  
  #Add in site name to each dataframe
  pars <- str_split(foldernames[i], "/")
  tmp.df$site <- pars[[1]][2]
  
  bn_results <- rbind(bn_results, tmp.df)
  
  print(i)
  
}


#get recording ID (merge key)
bn_results$wav_file <- substrRight(bn_results$filepath, 19)
bn_results$recording_ID <- paste(bn_results$site, substr(bn_results$wav_file, 1, 15), sep = "_")

dim(bn_results) #4254087      17
length(unique(bn_results$recording_ID)) #306096


#Subset to just frog detections---------------------------------------------------

#Need to ultimately redo validation step for the frogs
#For now, will use 0.1 threshold (all records) as identified
#by Julia (but not 100% sure how this was done)

sort(unique(bn_results$scientific_name))
sort(unique(bn_results$scientific_name))[2000:3000]
bn_results[which(bn_results$scientific_name == "Lithobates sphenocephalus"), c("scientific_name", "site")]

#subset to native frogs
keep <- c("Acris crepitans", "Anaxyrus americanus", "Anaxyrus fowleri", 
          "Gastrophryne carolinensis", "Dryophytes chrysoscelis", "Dryophytes versicolor", 
          "Dryophytes cinereus", "Lithobates clamitans", "Lithobates catesbeianus",
          "Lithobates palustris", "Lithobates sylvaticus", "Pseudacris crucifer"
)
bn_frogs <- bn_results[bn_results$scientific_name %in% keep, ]
dim(bn_frogs) #91676    17

summary(as.factor(bn_frogs$scientific_name))
# Acris crepitans       Anaxyrus americanus          Anaxyrus fowleri 
# 193                       604                        12 
# Dryophytes chrysoscelis       Dryophytes cinereus     Dryophytes versicolor 
# 11847                       201                       191 
# Gastrophryne carolinensis   Lithobates catesbeianus      Lithobates clamitans 
# 37326                      3558                     12195 
# Lithobates palustris     Lithobates sylvaticus       Pseudacris crucifer 
# 7223                        91                     18235 


# #subset to grey treefrogs (both)
# keep <- c("Dryophytes chrysoscelis", "Dryophytes versicolor")
# bn_grey <- bn_results[bn_results$scientific_name %in% keep, ]
# dim(bn_grey) #1762   17


#Merge timestaps from each sample with BNET results---------------------------------------------------
#**This allows us to account for samples without any detections

colnames(bn_frogs)[1] <- "filepath_wav"
bn_frogs <- bn_frogs[ , -15] #drop site column - redundant

sites_frogs <-
  merge(
    site_time,
    bn_frogs,
    by.x = c("recording_ID"),
    by.y = c("recording_ID"),
    all.x = TRUE
  )

dim(sites_frogs) #414547     25
length(unique(sites_frogs$recording_ID)) #344223

summary(as.factor(sites_frogs$scientific_name))
# Acris crepitans       Anaxyrus americanus          Anaxyrus fowleri 
# 193                       604                        12 
# Dryophytes chrysoscelis       Dryophytes cinereus     Dryophytes versicolor 
# 11847                       201                       191 
# Gastrophryne carolinensis   Lithobates catesbeianus      Lithobates clamitans 
# 37326                      3558                     12195 
# Lithobates palustris     Lithobates sylvaticus       Pseudacris crucifer 
# 7223                        91                     18235 
# NA's 
# 322871 

table(sites_frogs$site, sites_frogs$scientific_name)
#detected at all sites

#Note: for future runs, need to convert timestamps from UTC - 0 to local time
#here, before subsetting to focal dates. See below, line 320 and beyond

#subset to dates of temp dataset
range(sites_frogs$time)
sites_frogs_s <- subset(sites_frogs, time >= "2024-05-15 00:00:00" & time <= "2024-09-20 00:00:00")
range(sites_frogs_s$time)
str(sites_frogs_s) #299353 obs. of  25 variables

summary(as.factor(sites_frogs_s$scientific_name))

table(sites_frogs_s$site, sites_frogs_s$scientific_name)

#save file------------------------------------------------------------------

#set wd() to local project
setwd("C:/Users/Nowak/Documents/JUSTIN - Active_Documents/SERC/Research Projects/Science and Faith/Acoustic_monitoring/urban_audiomoth_project_11.11.24")

# write.csv(sites_frogs_s, file = "Data/cleaned_audio_data/sites_frog_detections_11-24-24.csv", row.names = FALSE)

#To do: subset dataframe by confidence >= 0.1
summary(sites_frogs_s$confidence) #Ok, all are above 0.1 per the BNET settings


## ******************************************************************************************
## Generate daily summaries
## ******************************************************************************************


library(seewave)

#Load saved data
gdat <- read.csv("Data/cleaned_audio_data/sites_frog_detections_11-24-24.csv")
dim(gdat) #299353     25

head(gdat)

#reformat date time--------------------------------------------------------------

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

gdat$wav_file <- paste(substrRight(gdat$recording_ID, 15), ".WAV", sep = "")
tmp <- audiomoth(gdat$wav_file, tz = "UTC")
#Started at ~10:30 am

gdat$datetime <- tmp$time
summary(is.na(gdat$datetime))

gdat$date <- as.POSIXct(strftime(gdat$datetime, format="%Y-%m-%d", tz = "UTC"))

#Save intermediate process files
#write.csv(gdat, file = "Data/cleaned_audio_data/sites_frog_detections_dates_11-25-24.csv", row.names = FALSE)


#Add local timestamps converting from UTC - 0 to EDT (UTC - 4)-----------------------------
#Note: this was added in after running through the initial script. For future runs, 
#need to integrate this step earlier prior to subsetting to focal dates 
#This was a temporary fix here, to avoid lengthy re-run of file processing above
#Added 12-4-23

#Load intermediate data file
gdat <-  read.csv("Data/cleaned_audio_data/sites_frog_detections_dates_11-25-24.csv")
str(gdat) #299353 obs. of  27 variables

#reconvert time to posix
library(anytime)
summary(is.na(gdat$time)) #No NAs
gdat$datetime <- anytime(gdat$time)
summary(gdat$datetime) #No NAs; range 2024-05-15 00:00:00.0000 to 2024-09-20 00:00:00.0000
summary(gdat$datetime == gdat$time) #
gdat[ , c("datetime", "time", "site")]

#Convert to local time
#Eastern Daylight Time (EDT) is four hours behind UTC - 0 (UTC−04:00).
gdat$datetime_loc <- gdat$datetime - hours(4)
summary(gdat$datetime_loc)
gdat[ , c("datetime_loc", "datetime", "time", "site")]

#Separate out date and time elements
gdat$hour_loc <- as.POSIXlt(gdat$datetime_loc)$hour
gdat$min_loc <- as.POSIXlt(gdat$datetime_loc)$min
gdat$month_loc <- as.POSIXlt(gdat$datetime_loc)$mon+1
gdat$day_loc <- day(as.POSIXlt(gdat$datetime_loc))
gdat$date_loc <- date(as.POSIXlt(gdat$datetime_loc))
summary(gdat$date_loc) 
gdat[ , c("date_loc", "datetime_loc", "datetime", "time", "site")]

summary(gdat$date_loc == as.POSIXct(gdat$time, format="%Y-%m-%d"))
summary(gdat) #No NAs in time elements

#save full combined dataset with local time
#write.csv(gdat, file = "Data/cleaned_audio_data/sites_frog_detections_loctime_dates_12-4-24.csv", row.names = FALSE)


#Generate summaries--------------------------------------------------------------

#Make counts of species with >1000 detections
#ifelse leave NAs! - need to convert these to 0s
summary(as.factor(gdat$scientific_name))

gdat$drychr_n <- ifelse(gdat$scientific_name == "Dryophytes chrysoscelis", 1, 0)
gdat[is.na(gdat$drychr_n), "drychr_n"] <- 0
table(gdat$drychr_n)

gdat$gascar_n <- ifelse(gdat$scientific_name == "Gastrophryne carolinensis", 1, 0)
gdat[is.na(gdat$gascar_n), "gascar_n"] <- 0
table(gdat$gascar_n)

gdat$litcat_n <- ifelse(gdat$scientific_name == "Lithobates catesbeianus", 1, 0)
gdat[is.na(gdat$litcat_n), "litcat_n"] <- 0
table(gdat$litcat_n)

gdat$litcla_n <- ifelse(gdat$scientific_name == "Lithobates clamitans", 1, 0)
gdat[is.na(gdat$litcla_n), "litcla_n"] <- 0
table(gdat$litcla_n)

colnames(gdat)
summary(gdat[ , c(34:37)])
table(gdat$hour_loc, gdat$drychr_n)


#subset to hours after sunrise
unique(gdat$hour_loc)
gdat_s <- subset(gdat, hour_loc > 6)
table(gdat_s$hour_loc)
summary(as.factor(gdat_s$scientific_name))

#Calculate daily call counts
daily_ct <-
  aggregate(
    list(drychr_n = gdat$drychr_n,
         gascar_n = gdat$gascar_n,
         litcat_n = gdat$litcat_n,
         litcla_n = gdat$litcla_n),
    by = list(
      date_loc = gdat$date_loc,
      site = gdat$site
    ),
    FUN = function(x) sum(x, na.rm = TRUE)
  ) #include na.rm if NAs in data

dim(daily_ct) #1849    6

#Calculate daily call counts after sunrise
day_ct <-
  aggregate(
    list(drychr_asr = gdat_s$drychr_n,
         gascar_asr = gdat_s$gascar_n,
         litcat_asr = gdat_s$litcat_n,
         litcla_asr = gdat_s$litcla_n), #asr = after sun rise
    by = list(
      date_loc = gdat_s$date_loc,
      site = gdat_s$site
    ),
    FUN = function(x) sum(x, na.rm = TRUE)
  ) #include na.rm if NAs in data

dim(day_ct) #1839    6


#combine dataframes of daily counts-----------------------------------------------

gdat$date_site <- paste(gdat$date_loc, gdat$site, sep = "_")
daily_ct$date_site <- paste(daily_ct$date_loc, daily_ct$site, sep = "_")
day_ct$date_site <- paste(day_ct$date_loc, day_ct$site, sep = "_")

colnames(daily_ct)  #[3:4] <- c("aci_day", "ndsi_day")
colnames(day_ct)   #[3:4] <- c("aci_nt", "ndsi_nt")

daily_ct <-
  merge(
    daily_ct,
    day_ct [ , c("drychr_asr", "gascar_asr", "litcat_asr", "litcla_asr",
                 "date_site")],
    by.x = c("date_site"),
    by.y = c("date_site"),
    all.x = TRUE
  )
dim(daily_ct) #1849    11

summary(daily_ct) #10 NAs for daytime metrics only

#Examine # of recordings for each date_site-------------------------------------
setwd("D:/Science and Faith Audio Files")
site_time <- read.csv("./process/site_time_nodups_11.25.24")

#reformat time and date
site_time$datetime <- anytime(site_time$time)
site_time$datetime_loc <- site_time$datetime - hours(4)
site_time$date_loc <- date(as.POSIXlt(site_time$datetime_loc))
site_time_s <- subset(site_time, datetime >= "2024-05-15 00:00:00" & datetime <= "2024-09-20 00:00:00")
range(site_time_s$datetime_loc)
str(site_time_s) #258376 obs. of  13 variables
length(site_time_s$recording_ID) #258376

site_time_s$date_site <- paste(site_time_s$date_loc, site_time_s$site, sep = "_")
length(unique(site_time_s$date_site)) #1849
site_time_s$count <- 1

#calculate number of samples per day
daily_n <-
  aggregate(
    list(
      count = site_time_s$count
    ),
    by = list(date_loc = site_time_s$date_loc,
              site = site_time_s$site,
              date_site = site_time_s$date_site),
    FUN = function(x) sum(x, na.rm = TRUE)
  ) #include na.rm if NAs in data

dim(daily_n) #1849    4

table(daily_n$count) #Should be 6*24 = 144 recordings per site per day

#Calculate hourly counts---------------------------------------------------------
hourly_ct <-
  aggregate(
    list(drychr_n = gdat$drychr_n,
         gascar_n = gdat$gascar_n,
         litcat_n = gdat$litcat_n,
         litcla_n = gdat$litcla_n),
    by = list(
      date_loc = gdat$date_loc,
      hour_loc = gdat$hour_loc,
      site = gdat$site,
      date_site = gdat$date_site
    ),
    FUN = function(x) sum(x, na.rm = TRUE)
  ) #include na.rm if NAs in data

dim(hourly_ct) #43111     8
plot(hourly_ct$hour_loc, hourly_ct$drychr_n)

#drop sites/days with < 140 observations (avoid biasing daily counts)-------------

head(daily_n)
keep <- daily_n[which(daily_n$count >= 140), "date_site"]
length(keep) #1748

daily_ct_s <- daily_ct[daily_ct$date_site %in% keep,]
dim(daily_ct_s) #1748   11

hourly_ct_s <- hourly_ct[hourly_ct$date_site %in% keep,]
dim(hourly_ct_s) #41952   8



#save summary files---------------------------------------------------------------
setwd("C:/Users/Nowak/Documents/JUSTIN - Active_Documents/SERC/Research Projects/Science and Faith/Acoustic_monitoring/urban_audiomoth_project_11.11.24")

# write.csv(daily_ct_s, file = "Data/cleaned_audio_data/sites_frogs_det_daily_sum_12-4-24.csv", row.names = FALSE)
# write.csv(hourly_ct_s, file = "Data/cleaned_audio_data/sites_frogs_det_hourly_sum_12-4-24.csv", row.names = FALSE)


