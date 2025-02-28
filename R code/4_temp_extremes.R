###########################################################################################################################################################
### (4) Quantify temperature extremes and offsets##########################################################################################
########################################################################################################################################################
###updated 12-10-24


## ***************************************************************************************************
## Preamble
## ***************************************************************************************************

rm(list=ls())

library(ggplot2)
theme_set(theme_bw())

#Load all microclimate data (24 hr time series)
ddat <- read.csv("Data/cleaned_drop_data/drop_data_may_sept_2024_24hrs_11.10.24.csv")
dim(ddat) #303402     24

#Load microclimate summary data (means, max, min, etc)
# ddat_sum <- read.csv("Data/cleaned_drop_data/drop_data_2024_daytime_summaries_11.10.24.csv")
# dim(ddat_sum) #2013   20

ddat_sum <- read.csv("Data/cleaned_drop_data/drop_data_2024_day_night_summaries_11.23.24.csv")
ddat_sum <- ddat_sum %>% 
  filter(day_night == "day")
dim(ddat_sum) #2013  29

# dndat_sum <- read.csv("Data/cleaned_drop_data/drop_data_2024_day_night_summaries_11.23.24.csv")
# dim(dndat_sum) #4042   21

#Load weather station data
#Data downloaded from here (selected Daily summaries): https://www.ncei.noaa.gov/cdo-web/
#Go to data tools -> Find a station and search for city: https://www.ncei.noaa.gov/cdo-web/datatools/findstation
#Data from Annapolis (closest to both SERC and Annapolis)

#Data from Baltimore
wdat_bmore <- read.csv("Data/weather_station_data/baltimore_md_sci_ctr_2000_2024_daily_11.22.24.csv")
dim(wdat_bmore) #8245  24
#Data from Annapolis
wdat_annap <- read.csv("Data/weather_station_data/annapolis_naval_acad_2000_2024_daily_11.22.24.csv")
dim(wdat_annap) #8408  25


## ******************************************************************************************
## Format/reformat time stamps
## ******************************************************************************************

library(lubridate)
library(anytime)

colnames(ddat)
str(ddat)

#24 hr Microclimate data------------------------------------------------------------
#Reconvert timestamp to posixct and calculate local time (saving as/loading csv messes with format)

ddat$datetime2 <- ddat$datetime
summary(is.na(ddat$datetime)) #No NAs
summary(is.na(ddat$date_time)) #No NAs

ddat$datetime <- anytime(ddat$datetime)
#ddat$datetime = as.POSIXct(ddat$datetime2, format='%Y-%m-%d %H:%M:%S')
#ddat$datetime <- parse_date_time(ddat$date_time, '%Y-%m-%d %H:%M:%S %p')
summary(is.na(ddat$datetime)) #No NAs

summary(as.character(ddat$datetime) == ddat$datetime2) #All match
ddat[10000:11000 , c("datetime2", "datetime", "date_time")]
str(ddat)


#Re-Check that all hour intervals are 15 min 
tdiff_chk <- as.data.frame(table(diff(ddat[ , "datetime"], lag = 1)))
colnames(tdiff_chk)[1] <- "interval" 

#Check that all hour intervals are 15 min
sites <- unique(ddat$sites)
tdiff_chk <- data.frame()
for (i in sites) {
  tmp <- as.data.frame(table(diff(ddat[which(ddat$sites == i) , "datetime"], lag = 1)))
  tmp$sites <- unique(ddat[which(ddat$sites == i) , "sites"])
  tmp$sites2 <- i
  colnames(tmp)[1] <- "interval" 
  tdiff_chk <- rbind(tdiff_chk, tmp)
}
#most look ok... a few weird intervals to look into (may be just data gaps)
summary(ddat$temp_c) #No NAs
range(ddat$datetime) #"2024-05-15 04:00:00 EDT" "2024-09-20 04:00:00 EDT"


#Microclimate summary data--------------------------------------------------------***********
#Reconvert timestamp to posixct and calculate local time (saving as/loading csv messes with format)

ddat_sum$date_orig <- ddat_sum$date
ddat_sum$date <- as.POSIXct(ddat_sum$date, format='%Y-%m-%d', tz = "UTC")
ddat_sum$month <- as.POSIXlt(ddat_sum$date)$mon+1
ddat_sum$day <- day(as.POSIXlt(ddat_sum$date))
ddat_sum$year <- year(as.POSIXlt(ddat_sum$date))

summary(as.character(ddat_sum$date) == ddat_sum$date_orig) #All Match
summary(ddat_sum$date)
summary(is.na(ddat_sum$date))
range(ddat_sum$date) #"2024-05-15 UTC" "2024-09-19 UTC"
colnames(ddat_sum)
ddat_sum[ , c("date_orig", "date", "month")]


#Weather station data------------------------------------------------------------

colnames(wdat_annap)
colnames(wdat_bmore)
head(wdat_annap)
str(wdat)

met.format <- function(wdat) {
  
  wdat$date <- as.POSIXct(wdat$DATE, format = '%Y-%m-%d', tz = "UTC")
  wdat$month <- as.POSIXlt(wdat$date)$mon + 1
  wdat$day <- day(as.POSIXlt(wdat$date))
  wdat$year <- year(as.POSIXlt(wdat$date))
  
  wdat_sub <- subset(wdat, month >= 5 & month <= 9)
  wdat_sub <- subset(wdat_sub, year >= 2002)
  
  return(wdat_sub)
  
}

w_annap <- met.format(wdat_annap)
w_bmore <- met.format(wdat_bmore)

summary(is.na(w_annap$date))
summary(w_annap$month)
range(w_annap$date) #"2002-05-01 UTC" "2024-09-30 UTC"
dim(w_annap) #3513   29

summary(is.na(w_bmore$date))
summary(w_bmore$month)
range(w_bmore$date) #"2002-05-01 UTC" "2024-09-30 UTC"
dim(w_bmore) #3132   28



#---------------------------------------------------------------------------------------------
#Calculate mean and SD of maximum daily temperatures during summer months of 2000-2022

met.thresholds <- function(wdat) {
  #Calculate max daily temperature
  mean_max <-
    aggregate(
      list(TMAX_mn = wdat$TMAX),
      by = list(month = wdat$month),
      FUN = function(x)
        mean(x, na.rm = TRUE)
    ) #include na.rm if NAs in data
  
  sd_max <-
    aggregate(
      list(TMAX_sd = wdat$TMAX),
      by = list(month = wdat$month),
      FUN = function(x)
        sd(x, na.rm = TRUE)
    ) #include na.rm if NAs in data
  
  mean_max$TMAX_sd <- sd_max$TMAX_sd
  mean_max$sd2 <- (mean_max$TMAX_sd * 2) + mean_max$TMAX_mn
  
  return(mean_max)
  
}

annap_stats <- met.thresholds(w_annap)
bmore_stats <- met.thresholds(w_bmore)


#Identify extremes in drop dataset------------------------------------------------
#Using Annapolis station-derived stats as conservative and centrally located
#set of measurements

dim(ddat_sum) #2013   24
length(unique(ddat_sum$date)) #128 days 

# ddat_sum$ext_95_test <- ifelse(
#   ddat_sum$month == 5 &
#     ddat_sum$temp_95 >= annap_stats[which(annap_stats$month == 5), "sd2"],
#   1,
#   ifelse(
#     ddat_sum$month == 6 &
#       ddat_sum$temp_95 >= annap_stats[which(annap_stats$month == 6), "sd2"],
#     1,
#     ifelse(
#       ddat_sum$month == 7 &
#         ddat_sum$temp_95 >= annap_stats[which(annap_stats$month == 7), "sd2"],
#       1,
#       ifelse(
#         ddat_sum$month == 8 &
#           ddat_sum$temp_95 >= annap_stats[which(annap_stats$month == 8), "sd2"],
#         1,
#         ifelse(ddat_sum$month == 9 &
#                  ddat_sum$temp_95 >= annap_stats[which(annap_stats$month == 9), "sd2"], 1,
#                0)
#       )
#     )
#   )
# )

# table(ddat_sum$month, ddat_sum$ext_95_test, ddat_sum$habitat)          
# table(ddat_sum$month, ddat_sum$ext_95_test, ddat_sum$location)          
# table(ddat_sum$month, ddat_sum$ext_95_test, ddat_sum$habitat, ddat_sum$location) 

ddat_sum$ext_95 <- 0
ddat_sum[which(ddat_sum$month == 5 & ddat_sum$temp_95 >= annap_stats[which(annap_stats$month == 5), "sd2"]), "ext_95"] <- 1
ddat_sum[which(ddat_sum$month == 6 & ddat_sum$temp_95 >= annap_stats[which(annap_stats$month == 6), "sd2"]), "ext_95"] <- 1
ddat_sum[which(ddat_sum$month == 7 & ddat_sum$temp_95 >= annap_stats[which(annap_stats$month == 7), "sd2"]), "ext_95"] <- 1
ddat_sum[which(ddat_sum$month == 8 & ddat_sum$temp_95 >= annap_stats[which(annap_stats$month == 8), "sd2"]), "ext_95"] <- 1
ddat_sum[which(ddat_sum$month == 9 & ddat_sum$temp_95 >= annap_stats[which(annap_stats$month == 9), "sd2"]), "ext_95"] <- 1

table(ddat_sum$month, ddat_sum$ext_95, ddat_sum$habitat)          


# ddat_sum$ext_max_test <- ifelse(
#   ddat_sum$month == 5 &
#     ddat_sum$temp_max >= annap_stats[which(annap_stats$month == 5), "sd2"],
#   1,
#   ifelse(
#     ddat_sum$month == 6 &
#       ddat_sum$temp_max >= annap_stats[which(annap_stats$month == 6), "sd2"],
#     1,
#     ifelse(
#       ddat_sum$month == 7 &
#         ddat_sum$temp_max >= annap_stats[which(annap_stats$month == 7), "sd2"],
#       1,
#       ifelse(
#         ddat_sum$month == 8 &
#           ddat_sum$temp_max >= annap_stats[which(annap_stats$month == 8), "sd2"],
#         1,
#         ifelse(ddat_sum$month == 9 &
#                  ddat_sum$temp_max >= annap_stats[which(annap_stats$month == 9), "sd2"], 1,
#                0)
#       )
#     )
#   )
# )
# 
# table(ddat_sum$month, ddat_sum$ext_max_test, ddat_sum$habitat)          
# table(ddat_sum$month, ddat_sum$ext_max, ddat_sum$location)          
# table(ddat_sum$month, ddat_sum$ext_max, ddat_sum$habitat, ddat_sum$location)

ddat_sum$ext_max <- 0
ddat_sum[which(ddat_sum$month == 5 & ddat_sum$temp_max >= annap_stats[which(annap_stats$month == 5), "sd2"]), "ext_max"] <- 1
ddat_sum[which(ddat_sum$month == 6 & ddat_sum$temp_max >= annap_stats[which(annap_stats$month == 6), "sd2"]), "ext_max"] <- 1
ddat_sum[which(ddat_sum$month == 7 & ddat_sum$temp_max >= annap_stats[which(annap_stats$month == 7), "sd2"]), "ext_max"] <- 1
ddat_sum[which(ddat_sum$month == 8 & ddat_sum$temp_max >= annap_stats[which(annap_stats$month == 8), "sd2"]), "ext_max"] <- 1
ddat_sum[which(ddat_sum$month == 9 & ddat_sum$temp_max >= annap_stats[which(annap_stats$month == 9), "sd2"]), "ext_max"] <- 1

table(ddat_sum$month, ddat_sum$ext_max, ddat_sum$habitat)          


#Create variable indicating extreme temp days anywhere on the landscapes------------

length(unique(ddat_sum$date)) #128 days 
str(ddat_sum)

ext_count <-
  aggregate(
    list(ext_95_n = ddat_sum$ext_95),
    by = list(date = ddat_sum$date),
    FUN = function(x)
      sum(x, na.rm = TRUE)
  ) #include na.rm if NAs in data
dim(ext_count) #128   2

ext_mx_count <-
  aggregate(
    list(ext_max_n = ddat_sum$ext_max),
    by = list(date = ddat_sum$date),
    FUN = function(x)
      sum(x, na.rm = TRUE)
  ) #include na.rm if NAs in data
dim(ext_mx_count) #128   2

length(ext_count[which(ext_count$ext_95_n > 0), "date"]) #27/128 days
length(ext_mx_count[which(ext_mx_count$ext_max_n > 0), "date"]) #36/128 days

ext_count$ext_95_day <- ifelse(ext_count$ext_95_n > 0, 1, 0)
ext_mx_count$ext_max_day <- ifelse(ext_mx_count$ext_max_n > 0, 1, 0)

summary(ext_count$date == ext_mx_count$date)

ext_count$ext_max_day <- ext_mx_count$ext_max_day

#Merge ext temp day vars into ddat_summ
ddat_sum <-
  merge(
    ddat_sum,
    ext_count[ , c("date", "ext_95_day", "ext_max_day")],
    by.x = c("date"),
    by.y = c("date"),
    all.x = TRUE
  )
dim(ddat_sum) #2013   28



# #Temp summaries for Stillmeadow--------------------------------------------------
# 
# #Based on max temps from drops and 22 year mean max daily temps from Baltimore weather station
# #there were eight extreme heat days outside the forest in June-Aug and zero inside the forest
# 
# #Mean daytime temp for June-Aug
# mean(ddat_sum$temp_mn) #26.39013
# #Mean max daytime temp for June-Aug
# mean(ddat_sum$temp_max) #30.54104
# 
# mean(ddat_sum[which(ddat_sum$habitat == "forest"), "temp_max"]) #28.92266; 84 F
# mean(ddat_sum[which(ddat_sum$habitat == "open"), "temp_max"]) #32.15941; 89.9 F
# mean(wdat_sub$TMAX_c, na.rm = TRUE) #30.8005; 87.4
# 
# 
# max(ddat_sum[which(ddat_sum$habitat == "forest"), "temp_max"]) #37.66667; 99.8 F
# max(ddat_sum[which(ddat_sum$habitat == "open"), "temp_max"]) #39.61111; 103.3
# max(wdat_sub$TMAX_c, na.rm = TRUE) #42.22222; 108
# 
# ddat_fors <- subset(ddat_sum, habitat == "forest")
# ddat_sum[which(ddat_sum$temp_max == max(ddat_sum$temp_max)), ] #July 16
# ddat_fors[which(ddat_fors$temp_max == max(ddat_fors$temp_max)), ] #July 16
# wdat_sub[which(wdat_sub$TMAX_c == max(wdat_sub$TMAX_c, na.rm = TRUE)), ] #July 7, July 22


#Expand dataset to fill in missing dates with NAs------------------------------------------------
#Really should do this in script 1 for both 24 hrs and daily summmaries
#table(ddat_sum$date, ddat_sum$sites)

length(unique(ddat_sum$date))*length(unique(ddat_sum$sites))
#should be 2176 records total (currently 2013)

grd <- expand.grid(unique(ddat_sum$date), unique(ddat_sum$sites))
colnames(grd) <- c("date", "sites")
grd$date_site <- paste(grd$date, grd$sites, sep = "_")

ddat_sum$date_site <- paste(ddat_sum$date, ddat_sum$sites, sep = "_")

#merge daily summaries into dateXsite grid
ddat_sum_c <-
  merge(
    grd,
    ddat_sum,
    by.x = c("date_site"),
    by.y = c("date_site"),
    all.x = TRUE
  )
dim(ddat_sum_c) #2176   33

summary(ddat_sum_c$date.x == ddat_sum_c$date.y)
summary(ddat_sum_c$sites.x == ddat_sum_c$sites.y)

colnames(ddat_sum_c)
ddat_sum_c <-  ddat_sum_c[ , -4]
ddat_sum_c <-  ddat_sum_c[ , -4]

colnames(ddat_sum_c)[2:3] <- c("date", "sites")
#table(ddat_sum_c$date, ddat_sum_c$sites)

summary(ddat_sum_c$temp_95) # 163 NAs
summary(ddat_sum_c$date) # 0 NAs

#Add habitat and location variables (again)
ddat_sum_c$habitat <- as.factor(ddat_sum_c$site)
ddat_sum_c$location <- as.factor(ddat_sum_c$site)

neon <-read.csv("./Data/NEON_plots_habitat.csv") #plot 20 is on edge of field and forest

levels(ddat_sum_c$habitat) <- c("forest", "forest", "forest", "forest", "forest",
                                "open", "open", "forest", "forest", "open",
                                "forest", "open", "open", "open", "forest",
                                "open", "open")
levels(ddat_sum_c$location)  <- c("urban", "urban", "urban", "exurban", "exurban",
                                  "exurban", "exurban", "exurban", "exurban", "exurban",
                                  "exurban", "urban", "urban", "urban", "urban",
                                  "urban", "urban")

#save file
#write.csv(ddat_sum_c, "Data/cleaned_drop_data/drop_data_2024_daytime_summaries_extrm_12.10.24.csv", row.names = FALSE)



## ******************************************************************************************
## Create frequency, duration, and intensity variables
## ******************************************************************************************

#see here: https://link.springer.com/article/10.1007/s10584-023-03622-0
#for review of extreme weather metrics and concepts

# #count frequency of extreme temperature days
# daily$ext_95 <- ifelse(daily$temp.3_95 > 35.36665, 1, 0)
# daily$ext_max <- ifelse(daily$temp.3_max > 35.36665, 1, 0)
# table(daily$ext_max, daily$ext_95)
# 
# extrm_freq <-
#   aggregate(
#     list(frq_ext_95 = daily$ext_95, frq_ext_max = daily$ext_max),
#     by = list(
#       btree_plot = daily$btree_plot,
#       sensor = daily$sensor
#     ),
#     FUN = sum, na.rm=TRUE
#   ) #include na.rm if NAs in data
# 
# dim(extrm_freq) #75  4
# plot(extrm_freq$frq_ext_95, extrm_freq$frq_ext_max)
# abline(0, 1)
# 
# #count longest duration of extreme temperature days per plot
# library(omnibus)
# 
# extrm_dur <-
#   aggregate(
#     list(dur_ext_95 = daily$ext_95, dur_ext_max = daily$ext_max),
#     by = list(
#       btree_plot = daily$btree_plot,
#       sensor = daily$sensor
#     ),
#     FUN = function(x) longRun(x, 1)
#   ) #include na.rm if NAs in data
# 
# dim(extrm_dur) #75  4
# 
# #spot check results
# daily[which(daily$sensor == "95250011"), ]
# daily[which(daily$sensor == "95250018"), ]
# daily[which(daily$sensor == "95252160"), ]
# #Looks good
# 
# #quantify intensity of extreme temperatures as the sum of deviations from long term mean
# #Calculate daily deviations from long term mean
# dim(daily) #3675   11
# colnames(daily)
# 
# daily$temp.3_95_anom <- (daily$temp.3_95 - 28.99152)/3.187565
# daily$temp.3_max_anom <- (daily$temp.3_max - 28.99152)/3.187565
# 
# #calculate average (or sum? )deviations across study period for each plot
# extrm_int <-
#   aggregate(
#     list(int_ext_95 = daily$temp.3_95_anom, int_ext_max = daily$temp.3_max_anom),
#     by = list(
#       btree_plot = daily$btree_plot,
#       sensor = daily$sensor
#     ),
#     FUN = mean, na.rm=TRUE
#   ) #include na.rm if NAs in data
# 
# dim(extrm_int) #75  4
# summary(extrm_int$int_ext_95)
# plot(extrm_int$int_ext_95, extrm_int$int_ext_max)
# abline(0, 1)
# 
# summary(extrm_int$btree_plot == extrm_dur$btree_plot) #all match!
# summary(extrm_int$btree_plot == extrm_freq$btree_plot) #all match!
# summary(extrm_int$sensor == extrm_dur$sensor) #all match!
# summary(extrm_int$sensor == extrm_freq$sensor) #all match!
# 
# #combine metrics into one dataframe
# ext_metrics <- cbind(extrm_freq, extrm_dur[ , 3:4], extrm_int[ , 3:4])
# plot(ext_metrics$frq_ext_95, ext_metrics$dur_ext_95)
# plot(ext_metrics$frq_ext_95, ext_metrics$int_ext_95)



## ******************************************************************************************
## Plot extreme temperature events
## ******************************************************************************************

library(ggplot2)
theme_set(theme_bw())

ddat_sum_old <- ddat_sum
ddat_sum <- ddat_sum_c

levels(as.factor(ddat_sum$habitat))
levels(as.factor(ddat_sum$sites))

#Re-order factor levels for summary data
ddat_sum$habitat <- as.factor(ddat_sum$habitat)
ddat_sum$sites <- as.factor(ddat_sum$sites)
ddat_sum$location <- as.factor(ddat_sum$location)

ddat_sum$sites <-
  factor(ddat_sum$sites,
         levels = c("Open_SMO9", "Open1_SLO1", "SweetHope_SH4",  "NEON007_N7", "Classroom_SMC7", "Forest3_SLR3", "NEON019_N19"))
levels(ddat_sum$sites)

ddat_sum$habitat <-
  factor(ddat_sum$habitat,
         levels = c("open", "forest"))
levels(ddat_sum$habitat)

levels(ddat_sum$location)
ddat_sum$location <-
  factor(ddat_sum$location,
         levels = c("urban", "exurban"))


#create extreme variable for plotting
ddat_sum$ext_95_plt <- ddat_sum$ext_95
ddat_sum[which(ddat_sum$ext_95_plt == 0), "ext_95_plt"] <- NA

#Plot and save  

#temperature versus date and hour
png("./Figures_11-10-24/sci_faith_Q95_air_temp_2024_day_extrm_red_12.5.24.png", width = 9.7, height = 4.3, units = 'in', res = 600)
temp.date_plot <-
  ggplot(ddat_sum, aes(date, temp_95, group = factor(sites), color=factor(habitat))) +
  facet_grid(cols = vars(location)) +
  geom_line(aes(color = factor(habitat)), size = 1, alpha = 0.75) + #color = "#2d718eff"
  geom_point(data = ddat_sum[which(ddat_sum$ext_95_plt == 1), ], aes(date, temp_95), color = "red", shape = 17, size = 2, stroke = 2) + ##8305a7ff; #color = "#2d718eff"
  scale_color_manual(values=c("#fba238ff", "#29af7fff")) +
  theme(axis.text = element_text(size = 14)) + 
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
  ylab("Q95 temperature (°C)") 
#labs(y = expression("Temperature " *~degree*C))
temp.date_plot
dev.off()





















# testing -- AK -----------------------------------------------------------------

ddat_sum_sl <- ddat_sum %>%
  filter(sites %in% c("Open1_SLO1", "Forest2_SLF2", "Forest3_SLR3"))
dim(ddat_sum_sl) #309  29

ddat_ext_sum_sl <- ddat_sum_sl %>%
  filter(ext_95 == 1 | ext_max == 1 | ext_95_day == 1 | ext_max_day == 1) %>%
  select(date_orig, habitat, ext_95, ext_max, ext_95_day, ext_max_day) %>%
  arrange(date_orig, habitat)

ddat_summary <- ddat_ext_sum_sl %>%
  group_by(habitat) %>%
  summarize(
    ext_95_count = sum(ext_95),
    ext_max_count = sum(ext_max),
    ext_95_day_count = sum(ext_95_day),
    ext_max_day_count = sum(ext_max_day)
  ) %>%
  filter(ext_95_count > 0 | ext_max_count > 0 | 
           ext_95_day_count > 0 | ext_max_day_count > 0)
