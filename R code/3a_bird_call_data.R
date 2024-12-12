###################################################################################################################################################
###(3) Prepping bird call detections from BirdNET##################################################################################################
###################################################################################################################################################
###updated 12-4-24
#R version 4.3.1

#NOTE: AudioMoth timestamps are in UTC - 0 (unlike drops). Script converts to local time.
#Eastern Daylight Time (EDT) is four hours behind UTC - 0 (UTC−04:00). 
#EDT is from second Sunday in March to the first Sunday in November.

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

#get list of filenames (need to grab form all site folders)
serc <- paste("SERC", dir("SERC"), sep = "/")
sm <- paste("Stillmeadow", dir("Stillmeadow"), sep = "/")
sl <- paste("StLukes", dir("StLukes"), sep = "/")
sh <- paste("SweetHope", dir("SweetHope"), sep = "/")
lg <- paste("LibertyGrace", dir("LibertyGrace"), sep = "/")

sens.names <- c(serc, sm, sl, sh, lg)

# sites <- c("SERC/NEON007_N7", "SERC/NEON019_N19", "Stillmeadow/Open_SMO9", 
#            "Stillmeadow/Classroom_SMC7", "StLukes/Open1_SLO1", 
#            "StLukes/Forest2_SLF2") 

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

#foldernames <- paste(foldernames, "results", sep = "/")
foldernames <- paste(foldernames, "results_pass2", sep = "/")

length(foldernames) #152

#Get wave file names
filenames <- vector()
for (i in 1:length(foldernames)) {
  temp <-
    list.files(paste(foldernames[i], sep = "/"), pattern = "*.csv$")
  temp2 <- paste(foldernames[i], temp, sep = "/")
  filenames <- c(filenames, temp2)
}

length(filenames) #344769


# #Why do the number of filenames differ between results pass 1 and pass 2?--------
# #There are 344769 files in pass 1 and 340432 in pass 2 - diff of 4337
# filenames_pass2 <- filenames
# a <- unique(filenames) #vector of characters a
# b <- unique(filenames_pass2) #vector of characters b
# b <- str_remove(b, "_pass2")
# diff <- a[!(a %in% b)] #identifies which characters are in a but not b
# length(diff) #4338 (one more than expected?)
# 
# #From which folders are files missing?
# missfolders <- substr(diff, 1, nchar(diff)-36)
# length(unique(missfolders)) #all from one folder
# #Ugh, ok, there was an extra underscore in the subfolder name for Sweethope 
# #in.../SweetHope/SweetHope_SH4/SD_A/20240511/results__pass2 <- should be results_pass2
# #Solved here but need to change on other backup versions of the dataset
# 
# #Does pass 1 have complete data, given expectation of files form 152 results folders?
# checkfn <- substr(filenames, 1,nchar(filenames)-36)
# length(unique(checkfn)) #152 as expected


#get ALL time stamps and filenames for each audio sample---------------------------------------------------
#****The gather birdnet results function does not add in files with no results
#We need to account for all sample files in dataframe, including those with no detections****

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
dim(site_time_nodups) #344223     11
summary(unique(site_time_nodups$recording_ID) == unique(site_time$recording_ID))
#All Match

tmp <- as.data.frame(table(site_time_nodups$recording_ID))
table(tmp$Freq)

site_time <- site_time_nodups
dim(site_time) #344223     11

# write.csv(site_time, file = "./process/site_time_12.2.24.csv", row.names = FALSE)
# write.csv(site_time, file = "./process/site_time_nodups_12-3-24.csv", row.names = FALSE)


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

dim(bn_results) #1903390      17
length(unique(bn_results$recording_ID)) #222494 (fewer than number of recordings - 344223)

#Subset to just Northern Cardinal detections---------------------------------------------------

summary(bn_results$common_name)
summary(is.na(bn_results$common_name)) #No NAs
dim(bn_results[which(bn_results$common_name == "American Goldfinch"), c("scientific_name", "site")])

#subset to focal bird species
keep <- c("American Goldfinch", "American Robin", "Blue Jay", 
          "Carolina Chickadee", "Carolina Wren", "Northern Cardinal", 
          "Red-bellied Woodpecker", "Tufted Titmouse"
)

bn_birds <- bn_results[bn_results$common_name %in% keep, ]
dim(bn_birds) #544410     17

summary(as.factor(bn_birds$common_name))
# American Goldfinch    American Robin          Blue Jay             Carolina Chickadee 
# 22952                  86641                  56089                   7680 
# Carolina Wren      Northern Cardinal        Red-bellied Woodpecker  Tufted Titmouse 
# 94963                 203207                  26469                  46409 


#Merge timestaps from each sample with BNET results---------------------------------------------------
#This allows us to account for samples without any detections

colnames(bn_birds)[1] <- "filepath_wav"
bn_birds <- bn_birds[ , -15]

sites_birds <-
  merge(
    site_time,
    bn_birds,
    by.x = c("recording_ID"),
    by.y = c("recording_ID"),
    all.x = TRUE
  )

dim(sites_birds) #782470     26
length(unique(sites_birds$recording_ID)) #344223

summary(as.factor(sites_birds$common_name))
# American Goldfinch         American Robin               Blue Jay     Carolina Chickadee 
# 22952                  86641                  56089                   7680 
# Carolina Wren      Northern Cardinal Red-bellied Woodpecker        Tufted Titmouse 
# 94963                 203207                  26469                  46409 
# NA's 
# 238060 

table(sites_birds$site, sites_birds$common_name)
#detected at all sites

#Note: for future runs, need to convert timestamps from UTC - 0 to local time
#here, before subsetting to focal dates. See below, line 320 and beyond

#subset to dates of temp dataset
range(sites_birds$time)
sites_birds_s <- subset(sites_birds, time >= "2024-05-15 00:00:00" & time <= "2024-09-20 00:00:00")
range(sites_birds_s$time)
str(sites_birds_s) #536844 obs. of  26 variables


summary(as.factor(sites_birds_s$common_name))
# American Goldfinch         American Robin               Blue Jay     Carolina Chickadee 
# 16959                  56280                  44390                   4246 
# Carolina Wren      Northern Cardinal Red-bellied Woodpecker        Tufted Titmouse 
# 60040                 134678                  15196                  18177 
# NA's 
# 186878 
table(sites_birds_s$site, sites_birds_s$common_name)

#save file------------------------------------------------------------------

#set wd() to local project
setwd("C:/Users/Nowak/Documents/JUSTIN - Active_Documents/SERC/Research Projects/Science and Faith/Acoustic_monitoring/birdsong_urban_heat")

#write.csv(sites_birds_s, file = "Data/cleaned_audio_data/sites_bird_detections_12-3-24.csv", row.names = FALSE)



## ******************************************************************************************
## Generate daily summaries
## ******************************************************************************************

library(seewave)

#Load saved data
gdat <- read.csv("Data/cleaned_audio_data/sites_bird_detections_12-3-24.csv")
dim(gdat) #536844     26

head(gdat)

#reformat date time--------------------------------------------------------------

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

gdat$wav_file <- paste(substrRight(gdat$recording_ID, 15), ".WAV", sep = "")
tmp <- audiomoth(gdat$wav_file, tz = "UTC")
#Started at ~6:15 am (took 5+ hours)

gdat$datetime <- tmp$time
summary(is.na(gdat$datetime))

gdat$date <- as.POSIXct(strftime(gdat$datetime, format="%Y-%m-%d", tz = "UTC"))

#Save intermediate process files
#write.csv(gdat, file = "Data/cleaned_audio_data/sites_bird_detections_dates_12-4-24.csv", row.names = FALSE)


#Add local timestamps converting from UTC - 0 to EDT (UTC - 4)-----------------------------
#Note: this was added in after running through the initial script. For future runs, 
#need to integrate this step earlier prior to subsetting to focal dates 
#This was a temporary fix here, to avoid lengthy re-run of file processing above
#Added 12-4-23

#Load intermediate data file
gdat <-  read.csv("Data/cleaned_audio_data/sites_bird_detections_dates_12-4-24.csv")
str(gdat) #536844 obs. of  28 variables

#reconvert time to posix
library(anytime)
library(lubridate)
summary(is.na(gdat$time)) #No NAs
gdat$datetime <- anytime(gdat$time)
summary(gdat$datetime) #No NAs; range 2024-05-15 00:00:00.0000 to 2024-09-20 00:00:00.0000
summary(gdat$datetime == gdat$time) #
gdat[ , c("datetime", "time", "site", "common_name")]

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
#write.csv(gdat, file = "Data/cleaned_audio_data/sites_bird_detections_loctime_dates_12-4-24.csv", row.names = FALSE)


#Subset by confidence threshold/s------------------------------------------------
#And generate summaries--------------------------------------------------------------

#read last saved data file
gdat <- read.csv("Data/cleaned_audio_data/sites_bird_detections_loctime_dates_12-4-24.csv")
str(gdat) #536844 obs. of  34 variables


#Still need to do this - should be an ifelse statement like below
#Threshold confidence values from Aidan
# Common Name	        Threshold	    Number of Detections
# American Goldfinch	   0.25	          850
# American Robin	       0.27	          850
# Blue Jay	             0.32	          850
# Carolina Chickadee  	 0.13	          911
# Carolina Wren	         0.21	          850
# Northern Cardinal	     0.10	          876
# Red-Bellied Woodpecker	0.28	        848
# Tufted Titmouse	       0.23	          850

summary(as.factor(gdat$common_name))
# American Goldfinch         American Robin               Blue Jay     Carolina Chickadee 
# 16959                  56280                  44390                   4246 
# Carolina Wren      Northern Cardinal Red-bellied Woodpecker        Tufted Titmouse 
# 60040                 134678                  15196                  18177 
# NA's 
# 186878 

#Make counts of species with >1000 detections
#ifelse leave NAs! - need to convert these to 0s
#Then, we are converting detections below the threshold to NAs for each species,
#as these are ambiguous - so, just excluding form the summaries for now
summary(as.factor(gdat$common_name))

#American Goldfinch
gdat$amgo_n <- ifelse(gdat$common_name == "American Goldfinch", 1, 0)
gdat[is.na(gdat$amgo_n), "amgo_n"] <- 0
gdat[which(gdat$common_name == "American Goldfinch" & gdat$confidence < 0.25), "amgo_n"] <- NA
dim(gdat[which(gdat$common_name == "American Goldfinch" & gdat$confidence < 0.25), ])
summary(as.factor(gdat$amgo_n))

#American Robin
gdat$amro_n <- ifelse(gdat$common_name == "American Robin", 1, 0)
gdat[is.na(gdat$amro_n), "amro_n"] <- 0
gdat[which(gdat$common_name == "American Robin" & gdat$confidence < 0.27), "amro_n"] <- NA
dim(gdat[which(gdat$common_name == "American Robin" & gdat$confidence < 0.27), ])
summary(as.factor(gdat$amro_n))

#Blue Jay
gdat$blja_n <- ifelse(gdat$common_name == "Blue Jay", 1, 0)
gdat[is.na(gdat$blja_n), "blja_n"] <- 0
gdat[which(gdat$common_name == "Blue Jay" & gdat$confidence < 0.32), "blja_n"] <- NA
dim(gdat[which(gdat$common_name == "Blue Jay" & gdat$confidence < 0.32), ])
summary(as.factor(gdat$blja_n))

#Carolina Chickadee
gdat$cach_n <- ifelse(gdat$common_name == "Carolina Chickadee", 1, 0)
gdat[is.na(gdat$cach_n), "cach_n"] <- 0
gdat[which(gdat$common_name == "Carolina Chickadee" & gdat$confidence < 0.13), "cach_n"] <- NA
dim(gdat[which(gdat$common_name == "Carolina Chickadee" & gdat$confidence < 0.13), ])
summary(as.factor(gdat$cach_n))

#Carolina Wren
gdat$cawr_n <- ifelse(gdat$common_name == "Carolina Wren", 1, 0)
gdat[is.na(gdat$cawr_n), "cawr_n"] <- 0
gdat[which(gdat$common_name == "Carolina Wren" & gdat$confidence < 0.21), "cawr_n"] <- NA
dim(gdat[which(gdat$common_name == "Carolina Wren" & gdat$confidence < 0.21), ])
summary(as.factor(gdat$cawr_n))

#Northern Cardinal
gdat$noca_n <- ifelse(gdat$common_name == "Northern Cardinal", 1, 0)
gdat[is.na(gdat$noca_n), "noca_n"] <- 0
gdat[which(gdat$common_name == "Northern Cardinal" & gdat$confidence < 0.1), "noca_n"] <- NA
dim(gdat[which(gdat$common_name == "Northern Cardinal" & gdat$confidence < 0.1), ])
summary(as.factor(gdat$noca_n))

#Red-bellied Woodpecker
gdat$rewo_n <- ifelse(gdat$common_name == "Red-bellied Woodpecker", 1, 0)
gdat[is.na(gdat$rewo_n), "rewo_n"] <- 0
gdat[which(gdat$common_name == "Red-bellied Woodpecker" & gdat$confidence < 0.28), "rewo_n"] <- NA
dim(gdat[which(gdat$common_name == "Red-bellied Woodpecker" & gdat$confidence < 0.28), ])
summary(as.factor(gdat$rewo_n))

#Tufted Titmouse
gdat$tuti_n <- ifelse(gdat$common_name == "Tufted Titmouse", 1, 0)
gdat[is.na(gdat$tuti_n), "tuti_n"] <- 0
gdat[which(gdat$common_name == "Tufted Titmouse" & gdat$confidence < 0.23), "tuti_n"] <- NA
dim(gdat[which(gdat$common_name == "Tufted Titmouse" & gdat$confidence < 0.23), ])
summary(as.factor(gdat$tuti_n))


summary(gdat[ , c(35:42)]) #No NAs
table(gdat$hour_loc, gdat$tuti_n)

#subset to different sets of  hours after sunrise
unique(gdat$hour_loc)
gdat_asr <- subset(gdat, hour_loc > 6)
table(gdat_asr$hour_loc)
gdat_morn <- subset(gdat, hour_loc >= 6 & hour_loc <= 10)
table(gdat_morn$hour_loc)
gdat_mid <- subset(gdat, hour_loc >= 11 & hour_loc <= 16)
table(gdat_mid$hour_loc)
gdat_eve <- subset(gdat, hour_loc >= 17 & hour_loc <= 23)
table(gdat_eve$hour_loc)

#Calculate daily call counts
daily_ct <-
  aggregate(
    list(amgo_n = gdat$amgo_n,
         amro_n = gdat$amro_n,
         blja_n = gdat$blja_n,
         cach_n = gdat$cach_n,
         cawr_n = gdat$cawr_n,
         noca_n = gdat$noca_n,
         rewo_n = gdat$rewo_n,
         tuti_n = gdat$tuti_n),
    by = list(
      date_loc = gdat$date_loc,
      site = gdat$site
    ),
    FUN = function(x) sum(x, na.rm = TRUE)
  ) #include na.rm if NAs in data

dim(daily_ct) #1849    10

#Calculate daily call counts after sunrise
day_asr <-
  aggregate(
    list(amgo_asr = gdat_asr$amgo_n,
         amro_asr = gdat_asr$amro_n,
         blja_asr = gdat_asr$blja_n,
         cach_asr = gdat_asr$cach_n,
         cawr_asr = gdat_asr$cawr_n,
         noca_asr = gdat_asr$noca_n,
         rewo_asr = gdat_asr$rewo_n,
         tuti_asr = gdat_asr$tuti_n), #asr = after sun rise
    by = list(
      date_loc = gdat_asr$date_loc,
      site = gdat_asr$site
    ),
    FUN = function(x) sum(x, na.rm = TRUE)
  ) #include na.rm if NAs in data

dim(day_asr) #1839    10


#Calculate daily call counts at morning hours
day_morn <-
  aggregate(
    list(amgo_morn = gdat_morn$amgo_n,
         amro_morn = gdat_morn$amro_n,
         blja_morn = gdat_morn$blja_n,
         cach_morn = gdat_morn$cach_n,
         cawr_morn = gdat_morn$cawr_n,
         noca_morn = gdat_morn$noca_n,
         rewo_morn = gdat_morn$rewo_n,
         tuti_morn = gdat_morn$tuti_n), #asr = after sun rise
    by = list(
      date_loc = gdat_morn$date_loc,
      site = gdat_morn$site
    ),
    FUN = function(x) sum(x, na.rm = TRUE)
  ) #include na.rm if NAs in data

dim(day_morn) #1800    10


#Calculate daily call counts at midday hours
day_mid <-
  aggregate(
    list(amgo_mid = gdat_mid$amgo_n,
         amro_mid = gdat_mid$amro_n,
         blja_mid = gdat_mid$blja_n,
         cach_mid = gdat_mid$cach_n,
         cawr_mid = gdat_mid$cawr_n,
         noca_mid = gdat_mid$noca_n,
         rewo_mid = gdat_mid$rewo_n,
         tuti_mid = gdat_mid$tuti_n), #asr = after sun rise
    by = list(
      date_loc = gdat_mid$date_loc,
      site = gdat_mid$site
    ),
    FUN = function(x) sum(x, na.rm = TRUE)
  ) #include na.rm if NAs in data

dim(day_mid) #1815    10

#Calculate daily call counts at evening hours
day_eve <-
  aggregate(
    list(amgo_eve = gdat_eve$amgo_n,
         amro_eve = gdat_eve$amro_n,
         blja_eve = gdat_eve$blja_n,
         cach_eve = gdat_eve$cach_n,
         cawr_eve = gdat_eve$cawr_n,
         noca_eve = gdat_eve$noca_n,
         rewo_eve = gdat_eve$rewo_n,
         tuti_eve = gdat_eve$tuti_n), #asr = after sun rise
    by = list(
      date_loc = gdat_eve$date_loc,
      site = gdat_eve$site
    ),
    FUN = function(x) sum(x, na.rm = TRUE)
  ) #include na.rm if NAs in data

dim(day_eve) #1819    10



#combine dataframes of daily counts-----------------------------------------------

gdat$date_site <- paste(gdat$date_loc, gdat$site, sep = "_")
daily_ct$date_site <- paste(daily_ct$date_loc, daily_ct$site, sep = "_")
day_asr$date_site <- paste(day_asr$date_loc, day_asr$site, sep = "_")
day_morn$date_site <- paste(day_morn$date_loc, day_morn$site, sep = "_")
day_mid$date_site <- paste(day_mid$date_loc, day_mid$site, sep = "_")
day_eve$date_site <- paste(day_eve$date_loc, day_eve$site, sep = "_")


colnames(daily_ct)  #[3:4] <- c("aci_day", "ndsi_day")
colnames(day_asr)   #[3:4] <- c("aci_nt", "ndsi_nt")

length(unique(daily_ct$date_site)) #1849
length(unique(day_asr$date_site)) #1839

daily_ct <-
  merge(
    daily_ct,
    day_asr [ , c("amgo_asr",  "amro_asr",  "blja_asr",  "cach_asr",  
                 "cawr_asr",  "noca_asr",  "rewo_asr",  "tuti_asr", "date_site")],
    by.x = c("date_site"),
    by.y = c("date_site"),
    all.x = TRUE
  )
dim(daily_ct) #1849    19

daily_ct <-
  merge(
    daily_ct,
    day_morn [ , c("amgo_morn",  "amro_morn",  "blja_morn",  "cach_morn",  
                  "cawr_morn",  "noca_morn",  "rewo_morn",  "tuti_morn", "date_site")],
    by.x = c("date_site"),
    by.y = c("date_site"),
    all.x = TRUE
  )
dim(daily_ct) #1849    27

daily_ct <-
  merge(
    daily_ct,
    day_mid [ , c("amgo_mid",  "amro_mid",  "blja_mid",  "cach_mid",  
                  "cawr_mid",  "noca_mid",  "rewo_mid",  "tuti_mid", "date_site")],
    by.x = c("date_site"),
    by.y = c("date_site"),
    all.x = TRUE
  )
dim(daily_ct) #1849    35

daily_ct <-
  merge(
    daily_ct,
    day_eve [ , c("amgo_eve",  "amro_eve",  "blja_eve",  "cach_eve",  
                  "cawr_eve",  "noca_eve",  "rewo_eve",  "tuti_eve", "date_site")],
    by.x = c("date_site"),
    by.y = c("date_site"),
    all.x = TRUE
  )
dim(daily_ct) #1849    43

summary(daily_ct) #<= 49 NAs for daytime metrics only

#LEFT OFF HERE 12-10-24

#Examine # of recordings for each date_site-------------------------------------
setwd("D:/Science and Faith Audio Files")
site_time <- read.csv("./process/site_time_nodups_12-3-24.csv")
dim(site_time) #344223     11

#reformat time and date
site_time$datetime <- anytime(site_time$time)
site_time$datetime_loc <- site_time$datetime - hours(4)
site_time$date_loc <- date(as.POSIXlt(site_time$datetime_loc))
site_time_s <- subset(site_time, datetime >= "2024-05-15 00:00:00" & datetime <= "2024-09-20 00:00:00")
range(site_time_s$datetime_loc)
str(site_time_s) #258376 obs. of  14 variables
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
    list(amgo_n = gdat$amgo_n,
         amro_n = gdat$amro_n,
         blja_n = gdat$blja_n,
         cach_n = gdat$cach_n,
         cawr_n = gdat$cawr_n,
         noca_n = gdat$noca_n,
         rewo_n = gdat$rewo_n,
         tuti_n = gdat$tuti_n),
    by = list(
      date_loc = gdat$date_loc,
      hour_loc = gdat$hour_loc,
      site = gdat$site,
      date_site = gdat$date_site
    ),
    FUN = function(x) sum(x, na.rm = TRUE)
  ) #include na.rm if NAs in data

dim(hourly_ct) #43111     12
plot(hourly_ct$hour_loc, hourly_ct$cawr_n)

#drop sites/days with < 140 observations (avoid biasing daily counts)-------------

head(daily_n)
keep <- daily_n[which(daily_n$count >= 140), "date_site"]
length(keep) #1748

daily_ct_s <- daily_ct[daily_ct$date_site %in% keep,]
dim(daily_ct_s) #1748   43

hourly_ct_s <- hourly_ct[hourly_ct$date_site %in% keep,]
dim(hourly_ct_s) #41952   12

#stack up morning, midday, and evening summaries
colnames(daily_ct_s)
daily_morn <- daily_ct_s[ , c(1:3, 20:27)]
daily_mid <- daily_ct_s[ , c(1:3, 28:35)]
daily_eve <- daily_ct_s[ , c(1:3, 36:43)]
daily_morn$period <- "morning"
daily_mid$period <- "midday"
daily_eve$period <- "evening"

cols <- c("amgo_n", "amro_n", "blja_n", "cach_n", "cawr_n", "noca_n", "rewo_n", "tuti_n")
colnames(daily_morn)[4:11] <- cols 
colnames(daily_mid)[4:11] <- cols 
colnames(daily_eve)[4:11] <- cols 
daily_per <- rbind(daily_morn, daily_mid, daily_eve)
dim(daily_per) #5244   12

  
#save summary files---------------------------------------------------------------
setwd("C:/Users/Nowak/Documents/JUSTIN - Active_Documents/SERC/Research Projects/Science and Faith/Acoustic_monitoring/birdsong_urban_heat")

# write.csv(daily_ct_s, file = "Data/cleaned_audio_data/sites_birds_det_daily_sum_12-10-24.csv", row.names = FALSE)
# write.csv(hourly_ct_s, file = "Data/cleaned_audio_data/sites_birds_det_hourly_sum_12-10-24.csv", row.names = FALSE)
# write.csv(daily_per, file = "Data/cleaned_audio_data/sites_birds_det_asr_periods_sum_12-10-24.csv", row.names = FALSE)


