#################################################################################################################################
### (4) Quantify temperature extremes and offsets################################################################################
#################################################################################################################################
###updated 11-26-24


## ***************************************************************************************************
## Preamble
## ***************************************************************************************************

rm(list=ls())

library(ggplot2)
theme_set(theme_bw())

#Load all microclimate data (24 hr time series)
# ddat <- read.csv("Data/cleaned_drop_data/drop_data_may_july_2024_24hrs_07.26.24.csv")
ddat <- read.csv("Data/cleaned_drop_data/drop_data_may_sept_2024_24hrs_11.10.24.csv")
dim(ddat) #303402  24

#Load microclimate summary data (means, max, min, etc)
#ddat_sum <- read.csv("Data/cleaned_drop_data/drop_data_2024_daytime_summaries_07.26.24.csv")
ddat_sum <- read.csv("Data/cleaned_drop_data/drop_data_2024_day_night_summaries_11.23.24.csv")
dim(ddat_sum) #4042  21

#Load weather station data
#Data downloaded from here (selected Daily summaries): https://www.ncei.noaa.gov/cdo-web/
#wdat <- read.csv("./Data/weather_station_data/annapolis_naval_acad_2000_2023_daily_11.6.23.csv")

#Data from Baltimore
wdat_bmore <- read.csv("Data/weather_station_data/baltimore_md_sci_ctr_2000_2024_daily_11.22.24.csv")
dim(wdat_bmore) #8425  24
#Data from Annapolis
wdat_annap <- read.csv("Data/weather_station_data/annapolis_naval_acad_2000_2024_daily_11.22.24.csv")
dim(wdat_annap) #8408  25

## ******************************************************************************************
## Format/reformat time stamps
## ******************************************************************************************

library(lubridate)

colnames(ddat)
str(ddat)

#24 hr Microclimate data*********************************************************************
#Reconvert timestamp to posixct and calculate local time (saving as/loading csv messes with format)

ddat$datetime2 <- ddat$datetime
summary(is.na(ddat$datetime2)) #No NAs
#ddat$datetime = as.POSIXct(ddat$datetime2, format='%Y-%m-%d %H:%M:%S')
#ddat$datetime <- parse_date_time(ddat$date_time, '%Y-%m-%d %H:%M:%S %p') -- old solution, creates NAs
ddat$datetime <- parse_date_time(ddat$date_time, 
                                 orders = c('mdy HM', 
                                            'Ymd HMS p'))
summary(is.na(ddat$datetime)) #No NAs

summary(as.character(ddat$datetime) == ddat$datetime2) #All match
head(ddat)

# #reformat time elements
# drop_dt <- lapply(drop_dt, function(x) cbind(x, hour = as.POSIXlt(x$datetime)$hour))
# drop_dt <- lapply(drop_dt, function(x) cbind(x, minute = as.POSIXlt(x$datetime)$min))
# drop_dt <- lapply(drop_dt, function(x) cbind(x, month = as.POSIXlt(x$datetime)$mon+1))
# drop_dt <- lapply(drop_dt, function(x) cbind(x, day = day(as.POSIXlt(x$datetime))))
# drop_dt <- lapply(drop_dt, function(x) cbind(x, date = as.POSIXct(strftime(x$datetime, format="%Y-%m-%d"))))
# drop_dt <- lapply(drop_dt, function(x) cbind(x, time = format(as.POSIXct(x$datetime), format = "%H:%M")))

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

#Microclimate summary data--------------------------------------------------------***********
#Reconvert timestamp to posixct and calculate local time (saving as/loading csv messes with format)

ddat_sum$date_orig <- ddat_sum$date
ddat_sum$date      <- as.POSIXct(ddat_sum$date, format='%Y-%m-%d', tz = "UTC")
ddat_sum$month     <- as.POSIXlt(ddat_sum$date)$mon+1
ddat_sum$day       <- day(as.POSIXlt(ddat_sum$date))
ddat_sum$year      <- year(as.POSIXlt(ddat_sum$date))

summary(ddat_sum$date)
summary(is.na(ddat_sum$date))
range(ddat_sum$date) #"2024-05-15 UTC" "2024-09-20 UTC"


#Weather station data-------------------------------------------------------------

##Annapolis Weather Station ------------------------------------------------------

colnames(wdat_annap)
head(wdat_annap)
str(wdat_annap)

wdat_annap$date  <- as.POSIXct(wdat_annap$DATE, format='%Y-%m-%d', tz = "UTC")
wdat_annap$month <- as.POSIXlt(wdat_annap$date)$mon+1
wdat_annap$day   <- day(as.POSIXlt(wdat_annap$date))
wdat_annap$year  <- year(as.POSIXlt(wdat_annap$date))

summary(wdat_annap$date)
summary(is.na(wdat_annap$date))
range(wdat_annap$date) #"2001-10-11 UTC" "2024-11-01 UTC"

#Subset to desired date and time range 
summary(ddat$month)
#For study months from 2002 to 2022 (May, June, July - so far. Need to update later)
#Based on meteorological summer - June, July, August: https://www.ncei.noaa.gov/news/meteorological-versus-astronomical-seasons
wdat_annap_sub <- subset(wdat_annap, month >= 5 & month <= 9)
wdat_annap_24  <- subset(wdat_annap_sub, year == 2024) #data from 2024
wdat_annap_sub <- subset(wdat_annap_sub, year < 2024) #data pre-2024
summary(wdat_annap_sub$month)
range(wdat_annap_sub$date) #"2002-05-01 UTC" "2023-09-30 UTC"
dim(wdat_annap_sub) #3361  29


#Calculate mean and SD of maximum daily temperatures during summer months of 2000-2023

#Calculate max daily temperature
mean_max_annap <-
  aggregate(
    list(TMAX_mn = wdat_annap_sub$TMAX),
    by = list(
      month = wdat_annap_sub$month
    ),
    FUN = function(x) mean(x, na.rm = TRUE)
  ) #include na.rm if NAs in data

sd_max_annap <-
  aggregate(
    list(TMAX_mn = wdat_annap_sub$TMAX),
    by = list(
      month = wdat_annap_sub$month
    ),
    FUN = function(x) sd(x, na.rm = TRUE)
  ) #include na.rm if NAs in data


#### Annapolis Mean & SD max temps ####
# > mean_max
# month  TMAX_mn
# 5      22.70675
# 6      27.55544
# 7      30.16283
# 8      29.13087
# 9      25.74682
# > sd_max
# month  TMAX_mn
# 5      4.624714 * 1.5 = 6.937071
# 6      3.464658 * 1.5 = 5.196987
# 7      2.756060 * 1.5 = 4.134090
# 8      2.709802 * 1.5 = 4.064703
# 9      3.442496 * 1.5 = 5.163744

# May
#Upper threshold: 22.70675 + 6.937071 = 29.64382
# 2SD:31.95618

# June
#Upper threshold: 27.55544 + 5.196987 = 32.75243
# 2SD:34.48476

# July
#Upper threshold: 30.16283 + 4.134090 = 34.29692
# 2SD:35.67495

# August
#Upper threshold: 29.13087 + 4.064703 = 33.19557
# 2SD:34.55047

# September
#Upper threshold: 25.74682 + 5.163744 = 30.91056
# 2SD:32.63181

##Baltimore Weather Station ------------------------------------------------------

colnames(wdat_bmore)
head(wdat_bmore)
str(wdat_bmore)

wdat_bmore$date  <- as.POSIXct(wdat_bmore$DATE, format='%Y-%m-%d', tz = "UTC")
wdat_bmore$month <- as.POSIXlt(wdat_bmore$date)$mon+1
wdat_bmore$day   <- day(as.POSIXlt(wdat_bmore$date))
wdat_bmore$year  <- year(as.POSIXlt(wdat_bmore$date))

summary(wdat_bmore$date)
summary(is.na(wdat_bmore$date))
range(wdat_bmore$date) #"2000-01-01 UTC" "2024-11-01 UTC"

#Subset to desired date and time range 
summary(ddat$month)
wdat_bmore_sub <- subset(wdat_bmore, month >= 5 & month <= 9)
wdat_bmore_24  <- subset(wdat_bmore_sub, year == 2024) #data from 2024
wdat_bmore_sub <- subset(wdat_bmore_sub, year < 2024) #data pre-2024
summary(wdat_bmore_sub$month)
range(wdat_bmore_sub$date) #"2000-05-01 UTC" "2023-09-30 UTC"
dim(wdat_bmore_sub) #3285   28


#Calculate mean and SD of maximum daily temperatures during summer months of 2000-2023

#Calculate max daily temperature
mean_max_bmore <-
  aggregate(
    list(TMAX_mn = wdat_bmore_sub$TMAX),
    by = list(
      month = wdat_bmore_sub$month
    ),
    FUN = function(x) mean(x, na.rm = TRUE)
  ) #include na.rm if NAs in data

sd_max_bmore <-
  aggregate(
    list(TMAX_mn = wdat_bmore_sub$TMAX),
    by = list(
      month = wdat_bmore_sub$month
    ),
    FUN = function(x) sd(x, na.rm = TRUE)
  ) #include na.rm if NAs in data


#### Baltimore Mean & SD max temps ####
# > mean_max
# month  TMAX_mn
# 5      24.36041
# 6      29.55497
# 7      32.01782
# 8      30.82417
# 9      27.10977
# > sd_max
# month  TMAX_mn
# 5      5.278262 * 1.5 = 7.917393
# 6      4.074362 * 1.5 = 6.111543
# 7      3.372170 * 1.5 = 5.058255
# 8      3.230699 * 1.5 = 4.846049
# 9      4.142979 * 1.5 = 6.214469

# May
#Upper threshold: 24.36041 + 7.917393 = 32.27780
# 2SD:34.91693

# June
#Upper threshold: 29.55497 + 6.111543 = 35.66651
# 2SD:37.70369

# July
#Upper threshold: 32.01782 + 5.058255 = 37.07608
# 2SD:38.76216

# August
#Upper threshold: 30.82417 + 4.846049 = 35.67022
# 2SD:37.28557

# September
#Upper threshold: 27.10977 + 6.214469 = 33.32424
# 2SD:35.39573


#Identify extremes in weather station dataset [TABLED] ---------------------------

# Annapolis weather station extremes (1.5 SD)
wdat_annap_24$ext_tmax <- ifelse(
  wdat_annap_24$month == 5 & wdat_annap_24$TMAX >= 29.64382, 1,
  ifelse(wdat_annap_24$month == 6 & wdat_annap_24$TMAX >= 32.75243, 1,
    ifelse(wdat_annap_24$month == 7 & wdat_annap_24$TMAX >= 34.29692, 1,
      ifelse(wdat_annap_24$month == 8 & wdat_annap_24$TMAX >= 33.19557, 1,
        ifelse(wdat_annap_24$month == 9 & wdat_annap_24$TMAX >= 30.91056, 1,
               0)
        )
      )
    )
  )

# Baltimore weather station extremes (1.5 SD)
wdat_bmore_24$ext_tmax <- ifelse(
  wdat_bmore_24$month == 5 & wdat_bmore_24$TMAX >= 32.27780, 1,
  ifelse(wdat_bmore_24$month == 6 & wdat_bmore_24$TMAX >= 35.66651, 1,
    ifelse(wdat_bmore_24$month == 7 & wdat_bmore_24$TMAX >= 37.07608, 1,
      ifelse(wdat_bmore_24$month == 8 & wdat_bmore_24$TMAX >= 35.67022, 1,
        ifelse(wdat_bmore_24$month == 9 & wdat_bmore_24$TMAX >= 33.32424, 1,
               0)
        )
      )
    )
  )

#Identify extremes in drop dataset -----------------------------------------------

dim(ddat_sum) #4042   25

# subset ddat to Annapolis sites
ddat_sum_annap <- subset(ddat_sum, 
                         sites %in% c("NEON002_N2", "NEON007_N7",
                                      "NEON008_N8", "NEON009_N9",
                                      "NEON017_N17", "NEON018_N18",
                                      "NEON019_N19", "MuddyCreek_MC1",
                                      "Open1_SLO1", "Forest2_SLF2",
                                      "Forest3_SLR3")
                         )
# find extreme heat days in Annapolis
ddat_sum_annap$ext_95 <- ifelse(
  ddat_sum_annap$month == 5 & ddat_sum_annap$temp_95 >= 29.64382, 1,
  ifelse(ddat_sum_annap$month == 6 & ddat_sum_annap$temp_95 >= 32.75243, 1,
    ifelse(ddat_sum_annap$month == 7 & ddat_sum_annap$temp_95 >= 34.29692, 1,
      ifelse(ddat_sum_annap$month == 8 & ddat_sum_annap$temp_95 >= 33.19557, 1,
        ifelse(ddat_sum_annap$month == 9 & ddat_sum_annap$temp_95 >= 30.91056, 1,
               0)
        )
      )
    )
  )

# subset ddat to Baltimore sites
ddat_sum_bmore <- subset(ddat_sum,
                         sites %in% c("Open_SMO9", "Classroom_SMC7",
                                      "Overlook_SMO6", "Pool_SMP11", 
                                      "SweetHope_SH4", "Back_LGB1")
                         )
# find extreme heat days in Baltimore
ddat_sum_bmore$ext_95 <- ifelse(
  ddat_sum_bmore$month == 5 & ddat_sum_bmore$temp_95 >= 32.27780, 1,
  ifelse(ddat_sum_bmore$month == 6 & ddat_sum_bmore$temp_95 >= 35.66651, 1,
    ifelse(ddat_sum_bmore$month == 7 & ddat_sum_bmore$temp_95 >= 37.07608, 1,
      ifelse(ddat_sum_bmore$month == 8 & ddat_sum_bmore$temp_95 >= 35.67022, 1,
        ifelse(ddat_sum_bmore$month == 9 & ddat_sum_bmore$temp_95 >= 33.32424, 1,
               0)
        )
      )
    )
  )

# # old template
# ddat_sum$ext_95 <- ifelse(
#   ddat_sum$month == 4 & ddat_sum$temp_95 >= 31.091148, 1,
#   ifelse(ddat_sum$month == 5 & ddat_sum$temp_95 >= 34.539190, 1,
#     ifelse(ddat_sum$month == 6 & ddat_sum$temp_95 >= 36.853982, 1,
#       ifelse(ddat_sum$month == 7 & ddat_sum$temp_95 >= 37.916988, 1,
#              0)
#       )
#     )
#   )

table(ddat_sum$month, ddat_sum$ext_95, ddat_sum$habitat)          
table(ddat_sum$month, ddat_sum$ext_95, ddat_sum$location)          
table(ddat_sum$month, ddat_sum$ext_95, ddat_sum$habitat, ddat_sum$location)          

table(ddat_sum_annap$month, ddat_sum_annap$ext_95, ddat_sum_annap$habitat, ddat_sum_annap$location)
table(ddat_sum_bmore$month, ddat_sum_bmore$ext_95, ddat_sum_bmore$habitat, ddat_sum_bmore$location)

#Expand dataset to fill in missing dates with NAs --------------------------------
#Really should do this in script 1 for both 24 hrs and daily summmaries
table(ddat_sum$date, ddat_sum$sites)

length(unique(ddat_sum$date))*length(unique(ddat_sum$sites))
#should be 406 records total (currently 404)

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
dim(ddat_sum_c) #522  28

colnames(ddat_sum_c)
ddat_sum_c <-  ddat_sum_c[ , -4]
ddat_sum_c <-  ddat_sum_c[ , -4]
ddat_sum_c <-  ddat_sum_c[ , -27]

colnames(ddat_sum_c)[2:3] <- c("date", "sites")
table(ddat_sum_c$date, ddat_sum_c$sites)

summary(ddat_sum_c$temp_95) # 2 NAs
summary(ddat_sum_c$date) # 0 NAs

#save file
#write.csv(ddat_sum_c, "Data/cleaned_drop_data/drop_data_2024_daytime_summaries_extrm_07.28.24.csv", row.names = FALSE)
write.csv(ddat_sum_c, "Data/cleaned_drop_data/drop_data_2024_daytime_summaries_extrm_10.16.24.csv", row.names = FALSE)


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
         levels = c("Open_SMO9", "Open1_SLO1", "NEON007_N7", "Classroom_SMC7", "Forest2_SLF2", "NEON019_N19"))
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
# png("./Figures_7-26-24/sci_faith_Q95_air_temp_2024_day_extrm_purple_07.28.24.png", width = 9.7, height = 4.3, units = 'in', res = 600)
png("C:/Users/kirchgrabera/Downloads/sci_faith_Q95_air_temp_2024_day_extrm_purple_10.16.24.png", 
    width = 9.7, height = 4.3, units = 'in', res = 600)

temp.date_plot <-
  ggplot(ddat_sum, aes(date, temp_95, group = factor(sites), color=factor(habitat))) +
  facet_grid(cols = vars(location)) +
  geom_line(aes(color = factor(habitat)), linewidth = 1, alpha = 0.75) + #color = "#2d718eff"
  geom_point(data = ddat_sum[which(ddat_sum$ext_95_plt == 1), ], aes(date, temp_95), color = "#8305a7ff", shape = 17, size = 2, stroke = 2) + ##8305a7ff; #color = "#2d718eff"
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



















