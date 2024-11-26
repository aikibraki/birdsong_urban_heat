###########################################################################################################################################################
###(3) Prepping frog call detections from BirdNET##################################################################################################
########################################################################################################################################################
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
           "StLukes/Forest3_SLR3", "SweetHope/SweetHope_SH4") 

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

length(filenames) #143905



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
dim(site_time) #143905    11

#Load results files from BirdNET---------------------------------------------------

#modified birdnet_gather function (more efficient)
get.bndat <- function (results.directory) 
{
  paths <- list.files(results.directory, pattern = "*.csv$", full.names = TRUE)
  dat <- suppressWarnings(rbindlist(lapply(paths, function(x) read.csv(x))))
  
  return(dat)
}

#Loop over subdirectories and combine CSVs

length(foldernames) #60
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

dim(bn_results) #2015906     17


#Subset to just gray treefrog detections---------------------------------------------------

sort(unique(bn_results$scientific_name))[1000:2000]

#subset to grey treefrogs and green frogs
keep <- c("Dryophytes chrysoscelis", "Dryophytes versicolor", "Lithobates clamitans")
bn_grey <- bn_results[bn_results$scientific_name %in% keep, ]
dim(bn_grey) #1775   17

summary(as.factor(bn_grey$scientific_name))
# Dryophytes chrysoscelis   Dryophytes versicolor   Lithobates clamitans 
# 1714                      48                      13 

#subset to grey treefrogs (both)
keep <- c("Dryophytes chrysoscelis", "Dryophytes versicolor")
bn_grey <- bn_results[bn_results$scientific_name %in% keep, ]
dim(bn_grey) #1762   17


#Merge timestaps from each sample with BNET results---------------------------------------------------
#This allows us to account for samples without any detections

colnames(bn_grey)[1] <- "filepath_wav"
bn_grey <- bn_grey[ , -15]

sites_grey <-
  merge(
    site_time,
    bn_grey,
    by.x = c("recording_ID"),
    by.y = c("recording_ID"),
    all.x = TRUE
  )
dim(sites_grey) #83371    26

summary(as.factor(sites_grey$scientific_name))
# Dryophytes chrysoscelis   Dryophytes versicolor                    NA's 
#                    1714                      48                   81609 

table(sites_grey$site, sites_grey$scientific_name)
#detected at all sites

#subset to dates of temp dataset
sites_greys <- subset(sites_grey, time >= "2024-05-11 00:00:00" & time <= "2024-07-08 00:00:00")
str(sites_greys) #52467 obs. of  26 variables

summary(as.factor(sites_greys$scientific_name))
# Dryophytes chrysoscelis   Dryophytes versicolor                    NA's 
#                     926                       6                   51535 
table(sites_greys$site, sites_greys$scientific_name)

#save file------------------------------------------------------------------

#set wd() to local project
setwd("C:/Users/Nowak/Documents/JUSTIN - Active_Documents/SERC/Research Projects/Science and Faith/Acoustic_monitoring")

#write.csv(sites_greys, file = "Data/cleaned_audio_data/sites_grey_treefrogs_detections_7-28-24.csv", row.names = FALSE)

#To do: subset dataframe by confidence >= 0.1
summary(sites_greys$confidence) #Ok, all are above 0.1 per the BNET settings



## ******************************************************************************************
## Generate daily summaries
## ******************************************************************************************


library(seewave)

#Load saved data
gdat <- read.csv("Data/cleaned_audio_data/sites_grey_treefrogs_detections_7-28-24.csv")
dim(gdat) #52467    26

head(gdat)

#reformat date time--------------------------------------------------------------

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

gdat$wav_file <- paste(substrRight(gdat$recording_ID, 15), ".WAV", sep = "")
tmp <- audiomoth(gdat$wav_file, tz = "UTC")

gdat$datetime <- tmp$time
summary(is.na(gdat$datetime))

gdat$date <- as.POSIXct(strftime(gdat$datetime, format="%Y-%m-%d", tz = "UTC"))

#Generate summaries--------------------------------------------------------------

summary(as.factor(gdat$scientific_name))
table(gdat$hour, gdat$count)

gdat$count <- ifelse(gdat$scientific_name == "Dryophytes chrysoscelis", 1, 0)

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

dim(daily_ct) #368   3

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

dim(day_ct) #368   3

summary(day_ct$date == daily_ct$date)
summary(day_ct$site == daily_ct$site)

daily_ct$count_asr <- day_ct$count_asr

#write.csv(daily_ct, file = "Data/cleaned_audio_data/sites_grey_treefrogs_det_daily_sum_7-28-24.csv", row.names = FALSE)

