###################################################################################################################################################
###(3) Prepping bird call detections from BirdNET##################################################################################################
###################################################################################################################################################
###updated 7-27-24
#R version 4.3.1


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
setwd("F:/Science and Faith Audio Files")



#construct file paths------------------------------------------------------------

# #get list of filenames
# serc <- paste("SERC", dir("SERC"), sep = "/")
# sm <- paste("Stillmeadow", dir("Stillmeadow"), sep = "/")
# sl <- paste("StLukes", dir("StLukes"), sep = "/")
# sh <- paste("SweetHope", dir("SweetHope"), sep = "/")
# 
# sens.names <- c(serc, sm, sl, sh)


sites <- c("SERC/NEON007_N7", "SERC/NEON019_N19", "Stillmeadow/Open_SMO9", 
           "Stillmeadow/Classroom_SMC7", "StLukes/Open1_SLO1", 
           "StLukes/Forest2_SLF2") 

sd <- c("/SD_A", "/SD_B")

temp <- expand.grid(sites, sd)
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

#Get wave file names
filenames <- vector()
for (i in 1:length(foldernames)) {
  temp <-
    list.files(paste(foldernames[i], sep = "/"), pattern = "*.csv$")
  temp2 <- paste(foldernames[i], temp, sep = "/")
  filenames <- c(filenames, temp2)
}

length(filenames) #133965



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
dim(site_time) #133965    11

#Load results files from BirdNET---------------------------------------------------

#modified birdnet_gather function (more efficient)
get.bndat <- function (results.directory) 
{
  paths <- list.files(results.directory, pattern = "*.csv$", full.names = TRUE)
  dat <- suppressWarnings(rbindlist(lapply(paths, function(x) read.csv(x))))
  
  return(dat)
}

#Loop over subdirectories and combine CSVs

length(foldernames) #56
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

dim(bn_results) #1776194    17


#Subset to just Northern Cardinal detections---------------------------------------------------

sort(unique(bn_results$scientific_name))[1000:2000]

#subset to Northern Cardinal
keep <- c("Cardinalis cardinalis")
bn_noca <- bn_results[bn_results$scientific_name %in% keep, ]
dim(bn_noca) #87288   17

summary(as.factor(bn_noca$scientific_name))

#Merge timestaps from each sample with BNET results---------------------------------------------------
#This allows us to account for samples without any detections

colnames(bn_noca)[1] <- "filepath_wav"
bn_noca <- bn_noca[ , -15]

sites_noca <-
  merge(
    site_time,
    bn_noca,
    by.x = c("recording_ID"),
    by.y = c("recording_ID"),
    all.x = TRUE
  )
dim(sites_noca) #201598    26

summary(as.factor(sites_noca$scientific_name))
# Cardinalis cardinalis                  NA's 
#                 87288                114310

table(sites_noca$site, sites_noca$scientific_name)
#detected at all sites

#subset to dates of temp dataset
sites_nocas <- subset(sites_noca, time >= "2024-04-11 00:00:00" & time <= "2024-09-08 00:00:00")
str(sites_nocas) #192806 obs. of  26 variables

summary(as.factor(sites_nocas$scientific_name))
# Cardinalis cardinalis                  NA's 
#                 80054                101600 
table(sites_nocas$site, sites_nocas$scientific_name)

#save file------------------------------------------------------------------

#set wd() to local project
setwd("C:/Users/kirchgrabera/Smithsonian Dropbox/Aidan Kirchgraber/Science and Faith/Aidan/audio_analysis/birdnet")

#write.csv(sites_nocas, file = "sites_noca_detections_10-16-24.csv", row.names = FALSE)

#To do: subset dataframe by confidence >= 0.1
summary(sites_nocas$confidence) #Ok, all are above 0.1 per the BNET settings



## ******************************************************************************************
## Generate daily summaries
## ******************************************************************************************

library(tidyverse)
library(seewave)

#Load saved data
gdat <- read.csv("C:/Users/kirchgrabera/Smithsonian Dropbox/Aidan Kirchgraber/Science and Faith/Aidan/audio_analysis/birdnet/sites_noca_detections_10-16-24.csv")
dim(gdat) #181654   26

head(gdat)

#reformat date time--------------------------------------------------------------

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

gdat$wav_file <- paste(substrRight(gdat$recording_ID, 15), ".WAV", sep = "")

tmp <- audiomoth(gdat$wav_file) #, tz = "UTC") #slow, or broken?#


gdat$datetime <- tmp$time
summary(is.na(gdat$datetime))

gdat$date <- as.POSIXct(strftime(gdat$datetime, format="%Y-%m-%d", tz = "UTC"))

#Generate summaries--------------------------------------------------------------

summary(as.factor(gdat$scientific_name))
# Cardinalis cardinalis                  NA's 
#                 80054                101600 

gdat$count <- ifelse(gdat$scientific_name == "Cardinalis cardinalis", 1, 0)
table(gdat$hour, gdat$count)

#subset to hours after sunrise
unique(gdat$hour)
gdat_s <- subset(gdat, hour > 6)
summary(as.factor(gdat_s$scientific_name))

#Calculate daily call counts
daily_ct <-
  aggregate(
    list(count = gdat$count),
    by = list(
      date = gdat$date,
      site = gdat$site
    ),
    FUN = function(x) sum(x, na.rm = TRUE)
  ) #include na.rm if NAs in data

dim(daily_ct) #853   3

#Calculate daily call counts after sunrise
day_ct <-
  aggregate(
    list(count_asr = gdat_s$count), #asr = after sun rise
    by = list(
      date = gdat_s$date,
      site = gdat_s$site
    ),
    FUN = function(x) sum(x, na.rm = TRUE)
  ) #include na.rm if NAs in data

dim(day_ct) #853   3

summary(day_ct$date == daily_ct$date)
summary(day_ct$site == daily_ct$site)

daily_ct$count_asr <- day_ct$count_asr

#write.csv(daily_ct, file = "C:/Users/kirchgrabera/Smithsonian Dropbox/Aidan Kirchgraber/Science and Faith/Aidan/audio_analysis/birdnet/sites_noca_det_daily_sum_10-16-24.csv", row.names = FALSE)

