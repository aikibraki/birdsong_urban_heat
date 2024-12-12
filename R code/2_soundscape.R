###########################################################################################################################################################
###(2) Extracting and summarizing data from drop sensors##################################################################################################
########################################################################################################################################################
###updated 12-4-24
#R version 4.3.1

#NOTE: AudioMoth timestamps are in UTC - 0 (unlike drops). Script converts to local time.
#Eastern Daylight Time (EDT) is four hours behind UTC - 0 (UTC−04:00). 
#EDT is from second Sunday in March to the first Sunday in November (entire project period).

#To do: 
#(1) #Need to run timestamp QC checks on audio data too, as for drop data!!
#(2) Make data completeness figure as for drop data

## ******************************************************************************************
## Preamble
## ******************************************************************************************

#Clears workspace
rm(list=ls())

#install.packages("seewave", dependencies = TRUE)
library(seewave)
library(tuneR)


## ******************************************************************************************
## Load wav files and calculate acoustic indices
## ******************************************************************************************


#set wd() to SSD 
setwd("D:/Science and Faith Audio Files")


#construct file paths------------------------------------------------------------

#get list of filenames
serc <- paste("SERC", dir("SERC"), sep = "/")
sm <- paste("Stillmeadow", dir("Stillmeadow"), sep = "/")
sl <- paste("StLukes", dir("StLukes"), sep = "/")
sh <- paste("SweetHope", dir("SweetHope"), sep = "/")
lg <- paste("LibertyGrace", dir("LibertyGrace"), sep = "/")

sites <- c(serc, sm, sl, sh, lg)

# sites <- c("SERC/NEON007_N7", "SERC/NEON019_N19", "Stillmeadow/Open_SMO9", 
#           "Stillmeadow/Classroom_SMC7", "StLukes/Open1_SLO1", 
#           "StLukes/Forest3_SLR3", "SweetHope/SweetHope_SH4") 

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

#Get wave file names
filenames <- vector()
for (i in 1:length(foldernames)) {
  temp <-
    list.files(paste(foldernames[i], sep = "/"), pattern =
                 "*.WAV$")
  temp2 <- paste(foldernames[i], temp, sep = "/")
  filenames <- c(filenames, temp2)
}

length(filenames) #344769


#subset files to be processed------------------------------------------------------------
#For 80000+ files, looks like it will take 4+ days to run through
#So, lets subset to dates that correspond to temp data
#and to every three hours during the day and to samples at three hour intervals

#funciton to grab text at end of file names
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

library(plyr)
library(dplyr)
library(stringr)

temp <- NULL
temp.df <- NULL
site_time <- data.frame()

#get time stamps from filenames
for (i in 1:length(filenames)) {
  
  #Parse audiomoth filenames to get timestamps and site names
  fn <- substrRight(filenames[i], 19)
  tmp <- audiomoth(fn, tz = "UTC")
  pars <- str_split(filenames[i], "/")
  
  #Add to dataframe
  tmp <-
    cbind(tmp, data.frame(
      site = pars[[1]][2],
      filenames = filenames[i],
      index = i
    ))
  
  site_time <- rbind(site_time, tmp)
  
  print(i)
  
}

dim(site_time) #344769     10

#write.csv(site_time, file = "./process/audiomoth_sites_timestamps_11.12.24.csv", row.names = FALSE)

#Note: for future runs, need to convert timestamps from UTC - 0 to local time
#here, before subsetting to focal dates. See below, line 306 and beyond

#Subset files by date and time
#subset to dates of temp dataset
site_times <- subset(site_time, time >= "2024-05-15 00:00:00" & time <= "2024-09-20 00:00:00")
str(site_times) #258920 obs. of  10 variables

#subset by hours of day
summary(as.factor(site_times$hour))
summary(as.factor(site_times$min))
table(site_times$hour, site_times$site)
table(site_times$site, site_times$min)

# hrs <- c("03", "06", "09", "12", "15", "18", "21", "00")
hrs <- c("02", "04", "06", "08", "10", "12", "14", "16", "18", "20", "22", "00")
min <- c("00", "20", "40")

#mins <- c("00", "30")


# site_times_sm <- site_times[site_times$min %in% mins, ]
# str(site_times_sm) #86317 obs. of  10 variables

site_times_h <- site_times[site_times$hour %in% hrs, ]
str(site_times_h) #129480 obs. of  10 variables

site_times_s <- site_times_h[site_times_h$min %in% min, ]
str(site_times_s) #64747 obs. of  10 variables

summary(as.factor(site_times_s$hour))
summary(as.factor(site_times_s$min))


#Load wav files and calculate soundscape metrics---------------------------------

#function to batch process wave files
get_sc <- function(x, data = site_times_s) {
  
  id <- site_times_s[which(site_times_s$index == x), "index"]
  file <- site_times_s[which(site_times_s$index == x), "filenames"]
  
  #Load wave file
  temp <- readWave(file)
  
  #Calculate soundscape indices
  aci <- ACI(temp) #150.9793
  spec <- soundscapespec(temp, plot = FALSE) #178.2607
  ndsi <- NDSI(spec) #0.5871102
  
  #Parse audiomoth filenames to get timestamps and site names
  # amname <- substrRight(x, 19)
  # temp.df <- audiomoth(amname, tz = "UTC")
  # pars <- str_split(x, "/")
  temp.df <- site_times_s[which(site_times_s$index == x), ]
  
  #Add to dataframe
  temp.df <-
    cbind(temp.df, data.frame(
      aci = aci,
      ndsi = ndsi
    ))
  
  #Return
  return(temp.df)
  
}

# Apply function 
sc_vars <- ldply(site_times_s$index[1:10], .fun = get_sc, .progress = "text")
sc_vars_tst <- sc_vars
#~1 min 15 sec to process 10 files :/


#Try a loop (this is slightly faster for some reason)----------------------------

#set wd() to SSD 
setwd("D:/Science and Faith Audio Files")

dim(site_times_s) #64747    10

sc_vars <-  data.frame()
#for (i in 1:nrow(site_times_s)) {
#for (i in 16681:nrow(site_times_s)) {
#for (i in 32986:nrow(site_times_s)) {
for (i in 48841:nrow(site_times_s)) {
  
  #Load wave file
  temp <- readWave(site_times_s[i, "filenames"])
  
  #Calculate soundscape indices
  aci <- ACI(temp) #150.9793
  spec <- soundscapespec(temp, plot = FALSE) #178.2607
  ndsi <- NDSI(spec) #0.5871102
  
  #Parse audiomoth filenames to get timestamps and site names
  # fn <- substrRight(filenames[id], 19)
  # temp.df <- audiomoth(fn, tz = "UTC")
  # pars <- str_split(filenames[id], "/")
  temp.df <- site_times_s[i, ]
  
  #Add to dataframe
  temp.df <-
    cbind(temp.df, data.frame(
      aci = aci,
      ndsi = ndsi
    ))
  
  sc_vars <- rbind(sc_vars, temp.df)
  
  print(i)
  
}

# sc_vars_1_16680 <- sc_vars
# dim(sc_vars_1_16680) #16680    12
# sc_vars_16681_32985 <- sc_vars
# dim(sc_vars_16681_32985) #16305    12
# sc_vars_32986_48840 <- sc_vars
# dim(sc_vars_32986_48840) #15855    12
sc_vars_48841_64747 <- sc_vars
dim(sc_vars_48841_64747) #15907    12


#Save in multiple pieces

#set wd() to local project
#setwd("C:/Users/Nowak/Documents/JUSTIN - Active_Documents/SERC/Research Projects/Science and Faith/Acoustic_monitoring")

#write.csv(sc_vars_1_16680, file = "process/soundscape_metrics_1_16680_11-13-24.csv", row.names = FALSE)
#write.csv(sc_vars_16681_32985, file = "process/soundscape_metrics_16681_32985_11-14-24.csv", row.names = FALSE)
#write.csv(sc_vars_32986_48840, file = "process/soundscape_metrics_32986_48840_11-14-24.csv", row.names = FALSE)
#write.csv(sc_vars_48841_64747, file = "process/soundscape_metrics_48841_64747_11-14-24.csv", row.names = FALSE)

#Load files to be assembled-----------------------------------------------------

sc_vars_1_16680 <- read.csv("process/soundscape_metrics_1_16680_11-13-24.csv")
sc_vars_16681_32985 <- read.csv("process/soundscape_metrics_16681_32985_11-14-24.csv")
sc_vars_32986_48840 <- read.csv("process/soundscape_metrics_32986_48840_11-14-24.csv")
sc_vars_48841_64747 <- read.csv("process/soundscape_metrics_48841_64747_11-14-24.csv")


#Combine data--------------------------------------------------------------------

sc_dat <- rbind(sc_vars_1_16680, sc_vars_16681_32985, sc_vars_32986_48840, sc_vars_48841_64747)
dim(sc_dat) #64747    12

plot(as.factor(sc_dat$site), sc_dat$aci)
plot(as.factor(sc_dat$site), sc_dat$ndsi)


#reconvert time to posix
library(anytime)
summary(sc_dat$time)
sc_dat$datetime = anytime(sc_dat$time)

sc_dat$date <- as.POSIXct(sc_dat$time, format="%Y-%m-%d")
summary(sc_dat$date)

str(sc_dat)
table(sc_dat$date, sc_dat$site)

#Add habitat and location variables
sc_dat$habitat <- as.factor(sc_dat$site)
sc_dat$location <- as.factor(sc_dat$site)

levels(sc_dat$habitat) <- c("open", "forest", "forest", "forest", "forest", 
                            "forest", "open", "open", "forest", "forest",
                            "open", "forest", "open", "open", "open",
                            "forest", "open")
levels(sc_dat$location)  <- c("urban", "urban", "urban", "urban", "exurban",
                              "exurban", "exurban", "exurban", "exurban", "exurban",
                              "exurban", "exurban", "urban", "urban", "urban",
                              "urban", "urban")


dim(sc_dat) #64747    16

#set wd() to project location
setwd("C:/Users/Nowak/Documents/JUSTIN - Active_Documents/SERC/Research Projects/Science and Faith/Acoustic_monitoring/urban_audiomoth_project_11.11.24")


#save full combined dataset
#write.csv(sc_dat, file = "Data/cleaned_audio_data/soundscape_metrics_24hrs_11-23-24.csv", row.names = FALSE)


#Add local timestamps converting from UTC - 0 to EDT (UTC - 4)-----------------------------
#Note: this was added in after running through the initial script. For future runs, 
#need to integrate this step earlier prior to subsetting to focal dates 
#This was a temporary fix here, to avoid lengthy re-run of WAV file processing

#Load intermediate data file
sc_dat <-  read.csv("Data/cleaned_audio_data/soundscape_metrics_24hrs_11-23-24.csv")
str(sc_dat) #64747 obs. of  16 variables

#reconvert time to posix
library(anytime)
summary(is.na(sc_dat$time)) #No NAs
sc_dat$datetime = anytime(sc_dat$time)
summary(sc_dat$datetime) #No NAs; range 2024-05-15 00:00:00.0000 to 2024-09-20 00:00:00.0000
summary(sc_dat$datetime == sc_dat$time) #
sc_dat[ , c("datetime", "time", "site")]

#Convert to local time
#Eastern Daylight Time (EDT) is four hours behind UTC - 0 (UTC−04:00).
sc_dat$datetime_loc <- sc_dat$datetime - hours(4)
summary(sc_dat$datetime_loc)
sc_dat[ , c("datetime_loc", "datetime", "time", "site")]

#Separate out date and time elements
sc_dat$hour_loc <- as.POSIXlt(sc_dat$datetime_loc)$hour
sc_dat$min_loc <- as.POSIXlt(sc_dat$datetime_loc)$min
sc_dat$month_loc <- as.POSIXlt(sc_dat$datetime_loc)$mon+1
sc_dat$day_loc <- day(as.POSIXlt(sc_dat$datetime_loc))
sc_dat$date_loc <- date(as.POSIXlt(sc_dat$datetime_loc))
summary(sc_dat$date_loc) #64747
sc_dat[ , c("date_loc", "datetime_loc", "datetime", "time", "site")]

summary(sc_dat$date_loc == as.POSIXct(sc_dat$time, format="%Y-%m-%d"))
summary(sc_dat) #No NAs

#save full combined dataset with local time
#write.csv(sc_dat, file = "Data/cleaned_audio_data/soundscape_metrics_24hrs_loc_12-4-24.csv", row.names = FALSE)


## ******************************************************************************************
## Calculate daily summaries of soundscape metrics
## ******************************************************************************************

sc_dat$count <- 1
str(sc_dat) #64747 obs. of  23 variables

colnames(sc_dat)

#Calcualte daily mean sc metrics
daily_mn <-
  aggregate(
    list(
      aci = sc_dat$aci,
      ndsi = sc_dat$ndsi
    ),
    by = list(date_loc = sc_dat$date_loc,
              site = sc_dat$site),
    FUN = function(x) mean(x, na.rm = TRUE)
  ) #include na.rm if NAs in data

dim(daily_mn) #1849    4
summary(daily_mn) #No NAs

#calculate metrics for just daytime hours
hrs <- c("6", "8", "10", "12", "14", "16", "18", "20")

sc_dat_d <- sc_dat[sc_dat$hour_loc %in% hrs, ]
str(sc_dat_d) #43197 obs. of  23 variables
table(sc_dat_d$hour_loc)

day_mn <-
  aggregate(
    list(
      aci = sc_dat_d$aci,
      ndsi = sc_dat_d$ndsi
    ),
    by = list(date_loc = sc_dat_d$date_loc,
              site = sc_dat_d$site),
    FUN = function(x) mean(x, na.rm = TRUE)
  ) #include na.rm if NAs in data

dim(day_mn) #1841    4
summary(day_mn) #No NAs

#calculate metrics for just nighttime hours
hrs <- c("0", "2", "4", "22")

sc_dat_n <- sc_dat[sc_dat$hour_loc %in% hrs, ]
str(sc_dat_n) #21550 obs. of  23 variables
table(sc_dat_n$hour_loc)

might_mn <-
  aggregate(
    list(
      aci = sc_dat_n$aci,
      ndsi = sc_dat_n$ndsi
    ),
    by = list(date_loc = sc_dat_n$date_loc,
              site = sc_dat_n$site),
    FUN = function(x) mean(x, na.rm = TRUE)
  ) #include na.rm if NAs in data

dim(might_mn) #1843    4
summary(might_mn) #No NAs


#calculate number of samples per day
daily_n <-
  aggregate(
    list(
      count = sc_dat$count
    ),
    by = list(date_loc = sc_dat$date_loc,
              site = sc_dat$site),
    FUN = function(x) sum(x, na.rm = TRUE)
  ) #include na.rm if NAs in data

dim(daily_n) #1849    3

#combine dataframes---------------------------------------------------------------

daily_mn$date_site <- paste(daily_mn$date_loc, daily_mn$site, sep = "_")
day_mn$date_site <- paste(day_mn$date_loc, day_mn$site, sep = "_")
might_mn$date_site <- paste(might_mn$date_loc, might_mn$site, sep = "_")

summary(daily_mn$date_loc == daily_n$date_loc) #All match
summary(daily_mn$site == daily_n$site) #All match

colnames(day_mn)[3:4] <- c("aci_day", "ndsi_day")
colnames(might_mn)[3:4] <- c("aci_nt", "ndsi_nt")

daily_mn$count <- daily_n$count

daily_mn <-
  merge(
    daily_mn,
    day_mn [ , c("date_site", "aci_day", "ndsi_day")],
    by.x = c("date_site"),
    by.y = c("date_site"),
    all.x = TRUE
  )
dim(daily_mn) #1849    8

daily_mn <-
  merge(
    daily_mn,
    might_mn [ , c("date_site", "aci_nt", "ndsi_nt")],
    by.x = c("date_site"),
    by.y = c("date_site"),
    all.x = TRUE
  )
dim(daily_mn) #1849   10

summary(daily_mn) #6 NAs for nighttime and 8 for daytime metrics only

#drop sites/days with < 36 observations (avoid biasing daily means)
table(daily_mn$count) 
#Should be 36 per date/site (12*3). Weird - some are >36
#All are from site Pool_SMP11 - 
#sc_dat[which(sc_dat$date == "2024-06-27" & sc_dat$site == "Pool_SMP11"), ]
#Looks like timestamp issue -Need to run timestamp QC checks on audio data too!!
daily_mn_s <- subset(daily_mn, count == 36)
dim(daily_mn_s) #1741   10

#Add habitat and location variables
daily_mn_s$habitat <- as.factor(daily_mn_s$site)
daily_mn_s$location <- as.factor(daily_mn_s$site)

neon <-read.csv("./Data/NEON_plots_habitat.csv") #plot 20 is on edge of field and forest

levels(daily_mn_s$habitat) <- c("open", "forest", "forest", "forest", "forest", 
                                "forest", "open", "open", "forest", "forest",
                                "open", "forest", "open", "open", "open",
                                "forest", "open")
levels(daily_mn_s$location)  <- c("urban", "urban", "urban", "urban", "exurban",
                                  "exurban", "exurban", "exurban", "exurban", "exurban",
                                  "exurban", "exurban", "urban", "urban", "urban",
                                  "urban", "urban")

dim(daily_mn_s) #1741   12

#write.csv(daily_mn_s, file = "Data/cleaned_audio_data/soundscape_metrics_loc_daily_means_12-4-24.csv", row.names = FALSE)


## ******************************************************************************************
## Make exploratory plots
## ******************************************************************************************

head(daily_mn_s)

daily_mn_s$habitat <-
  factor(daily_mn_s$habitat,
         levels = c("open", "forest"))
levels(daily_mn_s$habitat)

levels(daily_mn_s$location)
ddat_sum$location <-
  factor(ddat_sum$location,
         levels = c("urban", "exurban"))


#Plot mean soundscape metrics by habitat and location
png("./Figures_7-26-24/sci_faith_ndsi_day_urban_habitat_col2_07.28.24.png", width = 4.3, height = 3.7, units = 'in', res = 600)
boxplot <- ggplot(daily_mn_s, aes(location, ndsi_day, color=factor(habitat), fill = factor(habitat))) + 
  #geom_hline(yintercept = 0, linetype="dashed", color = "grey") +
  geom_boxplot(aes(color=factor(habitat), fill = factor(habitat)), alpha = 0.40, size=1.25) + 
  # scale_color_manual(values=c("#c0bebf", "#32c32f")) + #"#fefe03",
  # scale_fill_manual(values=c("#c0bebf", "#32c32f")) +  #"#fefe03"
  scale_color_manual(values=c("#fba238ff", "#29af7fff")) +
  scale_fill_manual(values=c("#fba238ff", "#29af7fff")) +
  #geom_point(aes(y = temp.3_q95, fill = factor(div)), color = "grey45", shape = 21, position = position_jitterdodge(dodge.width = 0.75, jitter.width = 0.2), size = 1.2, stroke = 0.5) +  #
  #geom_jitter(height = 0, width = 0.15, alpha = 0.9) +
  #geom_errorbar(aes(y = Means, ymin = Means-CI95, ymax = Means+CI95), color = "black", size = 1, width = 0.1, data = model_sum) +
  #geom_point(aes(y = Means), shape = 21, color = "black", fill = "grey", size = 3, stroke = 3, data = model_sum) + 
  theme(panel.grid.major = element_line(colour = "white")) +
  theme(panel.grid.minor = element_line(colour = "white")) +
  theme(panel.grid.major.y = element_line(colour = "white")) +
  theme(panel.border = element_rect(color = "black", size = 1)) +
  theme(legend.title=element_blank()) +
  theme(axis.title.x = element_blank()) +
  #theme(legend.position = "none") +
  labs(x = expression("")) +
  theme(axis.text.y = element_text(size = 14)) +
  theme(axis.text.x = element_text(size = 14)) +
  theme(axis.title.y = element_text(size = 14)) +
  #theme(axis.title.x = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  theme(plot.margin = margin(0.5, 0.5, 0.5, 0.75, unit = "cm")) +
  #theme(axis.text.x = element_text(angle = 45, hjust=1)) + #vjust = 1, 
  #labs(y = expression("Temperature")) +
  #ylab("Acoustic complexity index (day)") +
  ylab("NDSI (day)") +
  #labs(y = expression("Plot-level leaf area index")) +
  #labs(y = expression("Plot-level vertical complexity index")) +
  labs(x = expression("Location"))
boxplot
dev.off()
