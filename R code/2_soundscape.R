###########################################################################################################################################################
###(2) Extracting and summarizing data from drop sensors##################################################################################################
########################################################################################################################################################
###updated 7-25-24
#R version 4.3.1


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
setwd("F:/Science and Faith Audio Files")


#construct file paths------------------------------------------------------------

# #get list of filenames
# serc <- paste("SERC", dir("SERC"), sep = "/")
# sm <- paste("Stillmeadow", dir("Stillmeadow"), sep = "/")
# sl <- paste("StLukes", dir("StLukes"), sep = "/")
# sh <- paste("SweetHope", dir("SweetHope"), sep = "/")
# 
# sens.names <- c(serc, sm, sl, sh)


sites <- c("SERC/NEON007_N7", "SERC/NEON019_N19", 
           "Stillmeadow/Open_SMO9", "Stillmeadow/Classroom_SMC7", 
           "StLukes/Open1_SLO1", "StLukes/Forest2_SLF2") 

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

length(filenames) #133965


#subset files to be processed------------------------------------------------------------
#For 80000+ files, looks like it will take 4+ days to run through
#So, lets subset to dates that correspond to temp data
#and to every three hours during the day and to samples at three hour intervals

#function to grab text at end of file names
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

library(dplyr)
library(plyr)
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

dim(site_time) # 133965     10

#Subset files by date and time
#subset to dates of temp dataset
site_times <- subset(site_time, time >= "2024-04-11 00:00:00" & time <= "2024-07-08 00:00:00")
str(site_times) # 71243 obs. of  10 variables

#subset by hours of day
summary(as.factor(site_times$hour))
summary(as.factor(site_times$min))
table(site_times$hour, site_times$site)
table(site_times$site, site_times$min)

hrs <- c("03", "06", "09", "12", "15", "18", "21", "00")
min <- c("00", "20", "40")

site_times <- site_times[site_times$hour %in% hrs, ]
str(site_times) #23736 obs. of  10 variables

site_times_s <- site_times[site_times$min %in% min, ]
str(site_times_s) #11871 obs. of  10 variables


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
setwd("F:/Science and Faith Audio Files")

dim(site_times_s) #11871   10

sc_vars <-  data.frame()
#for (i in 1:nrow(site_times_s[1:4000, ])) {
for (i in 4001:nrow(site_times_s)) {
  
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

#### Running it in parallel to go faster ---------------------------------------

library(parallel)

# Function to process each file
process_files <- function(i) {
  library(tuneR)
  # Load wave file
  temp <- readWave(site_times_s[i, "filenames"])
  
  # Calculate soundscape indices
  aci <- ACI(temp)
  spec <- soundscapespec(temp, plot = FALSE)
  ndsi <- NDSI(spec)
  
  # Get the corresponding dataframe row
  temp.df <- site_times_s[i, ]
  
  # Add calculated indices to dataframe
  temp.df <- cbind(temp.df, data.frame(
    aci = aci,
    ndsi = ndsi
  ))
  
  return(temp.df)
}

# Number of cores to use
num_cores <- detectCores() - 1
cl <- makeCluster(num_cores)

# Export necessary data and functions to the cluster
clusterExport(cl, varlist = c("site_times_s", "ACI", "soundscapespec", "NDSI"))

# Run the process in parallel using parLapply
sc_vars_list <- parLapply(cl, 1:nrow(site_times_s), process_files)

# Stop the cluster
stopCluster(cl)

# Combine results into a single data frame
sc_vars <- do.call(rbind, sc_vars_list)



####----------------------------------------------------------------------------

#sc_vars_1_4000 <- sc_vars
dim(sc_vars_1_4000) #4000   12
sc_vars_4001_8639 <- sc_vars
dim(sc_vars_4001_8639) #4639   12

#Save in two pieces

#set wd() to local project
setwd("C:/Users/Nowak/Documents/JUSTIN - Active_Documents/SERC/Research Projects/Science and Faith/Acoustic_monitoring")

#write.csv(sc_vars_1_4000, file = "Data/cleaned_audio_data/soundscape_metrics_1_4000_7-28-24.csv", row.names = FALSE)
#write.csv(sc_vars_4001_8639, file = "Data/cleaned_audio_data/soundscape_metrics_4001_8639_7-28-24.csv", row.names = FALSE)
write.csv(sc_vars, file = "C:/Users/kirchgrabera/Smithsonian Dropbox/Aidan Kirchgraber/Science and Faith/Aidan/audio_analysis/birdnet/soundscape_metrics_10-17-24.csv", row.names = FALSE)

#Combine data--------------------------------------------------------------------

#sc_dat <- rbind(sc_vars_1_4000, sc_vars_4001_8639)
sc_dat <- sc_vars
dim(sc_dat) #11871  12

plot(as.factor(sc_dat$site), sc_dat$aci)
plot(as.factor(sc_dat$site), sc_dat$ndsi)

sc_dat$date <- as.POSIXct(strftime(sc_dat$time, format="%Y-%m-%d"))
summary(sc_dat$time)

str(sc_dat)
table(sc_dat$date, sc_dat$site)

#Add habitat and location variables
sc_dat$habitat <- as.factor(sc_dat$site)
sc_dat$location <- as.factor(sc_dat$site)

levels(sc_dat$habitat) <- c("forest", "forest", "open", "forest", "open", "open")
levels(sc_dat$location)  <- c("urban", "urban", "exurban", "exurban", "urban", "urban")

dim(sc_dat) #11871  16

#save full combined dataset
#write.csv(sc_dat, file = "Data/cleaned_audio_data/soundscape_metrics_24hrs_7-28-24.csv", row.names = FALSE)
write.csv(sc_vars, file = "C:/Users/kirchgrabera/Smithsonian Dropbox/Aidan Kirchgraber/Science and Faith/Aidan/audio_analysis/birdnet/soundscape_metrics_24hrs_10-17-24.csv", row.names = FALSE)

## ******************************************************************************************
## Calculate daily summaries of soundscape metrics
## ******************************************************************************************

sc_dat$count <- 1
str(sc_dat) #8639 obs. of  16 variables

colnames(sc_dat)

#Calculate daily mean sc metrics
daily_mn <-
  aggregate(
    list(
      aci = sc_dat$aci,
      ndsi = sc_dat$ndsi
    ),
    by = list(date = sc_dat$date,
              site = sc_dat$site),
    FUN = function(x) mean(x, na.rm = TRUE)
  ) #include na.rm if NAs in data

dim(daily_mn) #509   4
summary(daily_mn) #No NAs

#calculate metrics for just daytime hours
hrs <- c("06", "09", "12", "15", "18")

sc_dat_d <- sc_dat[sc_dat$hour %in% hrs, ]
str(sc_dat_d) #5401 obs. of  16 variables

day_mn <-
  aggregate(
    list(
      aci = sc_dat_d$aci,
      ndsi = sc_dat_d$ndsi
    ),
    by = list(date = sc_dat_d$date,
              site = sc_dat_d$site),
    FUN = function(x) mean(x, na.rm = TRUE)
  ) #include na.rm if NAs in data

dim(day_mn) #501   4
summary(day_mn) #No NAs

#calculate metrics for just nighttime hours
hrs <- c("03", "21", "00")

sc_dat_n <- sc_dat[sc_dat$hour %in% hrs, ]
str(sc_dat_n) #3238 obs. of  16 variables

might_mn <-
  aggregate(
    list(
      aci = sc_dat_n$aci,
      ndsi = sc_dat_n$ndsi
    ),
    by = list(date = sc_dat_n$date,
              site = sc_dat_n$site),
    FUN = function(x) mean(x, na.rm = TRUE)
  ) #include na.rm if NAs in data

dim(might_mn) #509   4
summary(might_mn) #No NAs

#calculate number of samples per day
daily_n <-
  aggregate(
    list(
      count = sc_dat$count
    ),
    by = list(date = sc_dat$date,
              site = sc_dat$site),
    FUN = function(x) sum(x, na.rm = TRUE)
  ) #include na.rm if NAs in data

dim(daily_n) #509   3

#combine dataframes---------------------------------------------------------------

daily_mn$date_site <- paste(daily_mn$date, daily_mn$site, sep = "_")
day_mn$date_site <- paste(day_mn$date, day_mn$site, sep = "_")

summary(daily_mn$date == daily_n$date) #All match
summary(daily_mn$site == daily_n$site) #All match

summary(daily_mn$date == might_mn$date) #All match
summary(daily_mn$site == might_mn$site) #All match

colnames(day_mn)[3:4] <- c("aci_day", "ndsi_day")
colnames(might_mn)[3:4] <- c("aci_nt", "ndsi_nt")

daily_mn$count <- daily_n$count
daily_mn$aci_nt <- might_mn$aci_nt
daily_mn$ndsi_nt <- might_mn$ndsi_nt

daily_mn <-
  merge(
    daily_mn,
    day_mn [ , c("date_site", "aci_day", "ndsi_day")],
    by.x = c("date_site"),
    by.y = c("date_site"),
    all.x = TRUE
  )
dim(daily_mn) #509  10

summary(daily_mn) #8 NAs for daytime metrics only

#drop sites/days with < 24 observations (avoid biasing daily means)
table(daily_mn$count)
daily_mn_s <- subset(daily_mn, count == 24)

#Add habitat and location variables
daily_mn_s$habitat <- as.factor(daily_mn_s$site)
daily_mn_s$location <- as.factor(daily_mn_s$site)

levels(daily_mn_s$habitat) <- c("forest", "forest", "open", "forest", "open", "open")
levels(daily_mn_s$location) <- c("urban", "urban", "exurban", "exurban", "urban", "urban")

dim(daily_mn_s) #488  12

#write.csv(daily_mn_s, file = "Data/cleaned_audio_data/soundscape_metrics_daily_means_7-28-24.csv", row.names = FALSE)
write.csv(sc_vars, file = "C:/Users/kirchgrabera/Smithsonian Dropbox/Aidan Kirchgraber/Science and Faith/Aidan/audio_analysis/birdnet/soundscape_metrics_daily_means_10-17-24.csv", row.names = FALSE)
write.csv(daily_mn_s, file = "C:/Users/kirchgrabera/Smithsonian Dropbox/Aidan Kirchgraber/Science and Faith/Aidan/audio_analysis/birdnet/soundscape_metrics_real_daily_means_10-17-24.csv", row.names = FALSE)


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
#png("./Figures_7-26-24/sci_faith_ndsi_day_urban_habitat_col2_07.28.24.png", width = 4.3, height = 3.7, units = 'in', res = 600)
png("C:/Users/kirchgrabera/Downloads/sci_faith_aci_day_urban_habitat_col2_10.17.24.png", width = 4.3, height = 3.7, units = 'in', res = 600)

boxplot1 <- ggplot(daily_mn_s, aes(location, aci, color=factor(habitat), fill = factor(habitat))) + 
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
  ylab("ACI (day)") +
  #labs(y = expression("Plot-level leaf area index")) +
  #labs(y = expression("Plot-level vertical complexity index")) +
  labs(x = expression("Location"))
boxplot1
dev.off()


