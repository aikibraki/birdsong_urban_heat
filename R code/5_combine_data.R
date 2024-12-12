###########################################################################################################################################
### (5) Combine temperature and audio data##########################################################################################
###########################################################################################################################################
###updated 12-10-24


## ***************************************************************************************************
## Preamble
## ***************************************************************************************************

rm(list=ls())

library(ggplot2)
theme_set(theme_bw())

#Load microclimate summaries
tdat <- read.csv("Data/cleaned_drop_data/drop_data_2024_daytime_summaries_extrm_12.10.24.csv")
dim(tdat) #2176   29
summary(is.na(tdat$date)) #No NAs

#Load daily mean soundscape metrics
scdat <- read.csv("Data/cleaned_audio_data/soundscape_metrics_loc_daily_means_12-4-24.csv")
dim(scdat) #1741   12
summary(is.na(scdat$date_loc)) #No NAs

#Load bird detection summaries
bdat <- read.csv("Data/cleaned_audio_data/sites_birds_det_daily_sum_12-10-24.csv")
bdat_per <- read.csv("Data/cleaned_audio_data/sites_birds_det_asr_periods_sum_12-10-24.csv")
bdat_hr <- read.csv("Data/cleaned_audio_data/sites_birds_det_hourly_sum_12-10-24.csv")

dim(bdat) #1748   43
summary(is.na(bdat$date_loc)) #No NAs
dim(bdat_per) #5244   12
summary(is.na(bdat_per$date_loc)) #No NAs


#reformat date asPosixct
tdat$date <- as.POSIXct(tdat$date, format='%Y-%m-%d')
scdat$date <- as.POSIXct(scdat$date_loc, format='%Y-%m-%d')
bdat$date <- as.POSIXct(bdat$date_loc, format='%Y-%m-%d')
bdat_per$date <- as.POSIXct(bdat_per$date_loc, format='%Y-%m-%d')

summary(as.character(tdat$date) == tdat$date_orig)
summary(tdat$date)
summary(is.na(as.character(tdat$date)))
summary(scdat$date)
summary(as.character(scdat$date) == scdat$date_loc)
summary(is.na(as.character(scdat$date)))
summary(is.na(as.character(bdat$date)))
summary(as.character(bdat$date) == bdat$date_loc)
summary(is.na(as.character(bdat_per$date)))
summary(as.character(bdat_per$date) == bdat_per$date_loc)



## ******************************************************************************************
## Merge drop data and soundscape metrics
## ******************************************************************************************

str(tdat)
str(scdat)
table(tdat$sites, tdat$habitat)
table(tdat$sites, tdat$location)

t <- unique(tdat$date_site) #vector of characters a 
s <- unique(scdat$date_site) #vector of characters b 
t[!(t %in% s)] #435 dates_sites in temp data but not in audiomoth data
s[!(s %in% t)] #all dates_sites in audiomoth data are in temp data

t_sc_dat <-
  merge(
    tdat,
    scdat[ , c("date_site", "aci", "ndsi", "aci_nt", "ndsi_nt", "aci_day", "ndsi_day", "count")],
    by.x = c("date_site"),
    by.y = c("date_site"),
    all.x = TRUE
  )
dim(t_sc_dat) #2176   36

t_sc_dat2 <- t_sc_dat

## ******************************************************************************************
## Merge drop (and soundscape metrics) with bird detections 
## ******************************************************************************************

str(t_sc_dat)
colnames(t_sc_dat)[36] <- "sc_count"
colnames(t_sc_dat2)[36] <- "sc_count"
str(bdat)

#Subtracting a day from bird data dates allows us to look at potential next day/lagged
#responses, including those of early morning vocalizations (e.g., after midnight follwoing extreme heat)
#Lines up responses with previous day temps
bdat$date_min1 <- bdat$date - days(1)
bdat$date_site_1 <- paste(bdat$date_min1, bdat$site, sep = "_")

t <- unique(t_sc_dat$date_site) #vector of characters a 
b <- unique(bdat$date_site) #vector of characters b
g <- unique(bdat$date_site_1) #vector of characters b
t[!(t %in% b)] #428 dates_sites in temp data but not in audiomoth data
b[!(b %in% t)] #0 dates_sites in audiomoth data are not in temp data
t[!(t %in% g)] #444 dates_sites in temp data but not in audiomoth data
g[!(g %in% t)] #16 dates_sites in audiomoth data are not in temp data

#First merge daily drop and calling summaries
t_sc_dat <-
  merge(
    t_sc_dat,
    bdat[ , c("date_site", "amgo_n", "amro_n", "blja_n", "cach_n", "cawr_n",
              "noca_n", "rewo_n", "tuti_n", "amgo_asr", "amro_asr", "blja_asr",
              "cach_asr", "cawr_asr", "noca_asr", "rewo_asr", "tuti_asr", 
              "amgo_morn", "amro_morn", "blja_morn",
              "cach_morn", "cawr_morn", "noca_morn", "rewo_morn", "tuti_morn",
              "amgo_mid", "amro_mid", "blja_mid",
              "cach_mid", "cawr_mid", "noca_mid", "rewo_mid", "tuti_mid",
              "amgo_eve", "amro_eve", "blja_eve",
              "cach_eve", "cawr_eve", "noca_eve", "rewo_eve", "tuti_eve")],
    by.x = c("date_site"),
    by.y = c("date_site"),
    all.x = TRUE
  )
dim(t_sc_dat) #2176   76

bdat_1 <- bdat
colnames(bdat_1)[4:43]  <- c("amgo_n_1", "amro_n_1", "blja_n_1", "cach_n_1", "cawr_n_1",
        "noca_n_1", "rewo_n_1", "tuti_n_1", "amgo_asr_1", "amro_asr_1", "blja_asr_1",
        "cach_asr_1", "cawr_asr_1", "noca_asr_1", "rewo_asr_1", "tuti_asr_1",
        "amgo_morn_1", "amro_morn_1", "blja_morn_1",
        "cach_morn_1", "cawr_morn_1", "noca_morn_1", "rewo_morn_1", "tuti_morn_1",
        "amgo_mid_1", "amro_mid_1", "blja_mid_1",
        "cach_mid_1", "cawr_mid_1", "noca_mid_1", "rewo_mid_1", "tuti_mid_1",
        "amgo_eve_1", "amro_eve_1", "blja_eve_1",
        "cach_eve_1", "cawr_eve_1", "noca_eve_1", "rewo_eve_1", "tuti_eve_1")

t_sc_dat <-
  merge(
    t_sc_dat,
    bdat_1[ , c("date_site_1", "amgo_n_1", "amro_n_1", "blja_n_1", "cach_n_1", "cawr_n_1",
                "noca_n_1", "rewo_n_1", "tuti_n_1", "amgo_asr_1", "amro_asr_1", "blja_asr_1",
                "cach_asr_1", "cawr_asr_1", "noca_asr_1", "rewo_asr_1", "tuti_asr_1",
                "amgo_morn_1", "amro_morn_1", "blja_morn_1",
                "cach_morn_1", "cawr_morn_1", "noca_morn_1", "rewo_morn_1", "tuti_morn_1",
                "amgo_mid_1", "amro_mid_1", "blja_mid_1",
                "cach_mid_1", "cawr_mid_1", "noca_mid_1", "rewo_mid_1", "tuti_mid_1",
                "amgo_eve_1", "amro_eve_1", "blja_eve_1",
                "cach_eve_1", "cawr_eve_1", "noca_eve_1", "rewo_eve_1", "tuti_eve_1")],
    by.x = c("date_site"),
    by.y = c("date_site_1"),
    all.x = TRUE
  )
dim(t_sc_dat) #2176   116

str(t_sc_dat)

#Second merge daily drop (extremes) and morn, mid, eve summaries
dim(bdat_per) #5244   13
t_dat_per <-
  merge(
    bdat_per[ , c("date_site", "date_loc", "amgo_n", "amro_n", "blja_n", "cach_n", "cawr_n",
                  "noca_n", "rewo_n", "tuti_n", "period")],
    t_sc_dat2,
    by.x = c("date_site"),
    by.y = c("date_site"),
    all.x = TRUE
  )
dim(t_dat_per) #5244   46


#Calculate VPD------------------------------------------------------------------

# install.packages("pvldcurve")
# install.packages("plantecophys")

library(pvldcurve)
library(plantecophys)

#compare output from two functions
vpd1 <- VaporPressureDeficit(t_sc_dat, humidity = "rel_hum_mn",
                             temperature = "temp_mn", atmospheric.pressure = 101.325)  #default = 101.325 (atmospheric pressure at sea level)
vpd2 <- RHtoVPD(t_sc_dat$rel_hum_mn, t_sc_dat$temp_mn, Pa = 101)

summary(vpd1$vapor.pressure.deficit)
summary(vpd2)

plot(vpd1$vapor.pressure.deficit, vpd2) #identical except for units
abline(0, 1)

t_sc_dat$vpd_kPa <- RHtoVPD(t_sc_dat$rel_hum_mn, t_sc_dat$temp_mn, Pa = 101)
dim(t_sc_dat) #2176  117

t_dat_per$vpd_kPa <- RHtoVPD(t_dat_per$rel_hum_mn, t_dat_per$temp_mn, Pa = 101)
dim(t_dat_per) #5244   47


#Are there NAs for soundscape metrics during extreme temp events?
summary(t_sc_dat$aci)
sum(t_sc_dat[is.na(t_sc_dat$aci), "ext_95"]) #Yes, during 4 extreme heat obs :/
sum(t_sc_dat[, "ext_95"], na.rm = TRUE) # 22 total extreme heat events
table(t_sc_dat[is.na(t_sc_dat$aci), c("ext_95", "habitat", "location")])
table(t_sc_dat[ , c("ext_95", "habitat", "location")])


#create extreme event factors for modeling----------------------------------------

# t_sc_dat <- read.csv("Data/analysis_data/temp_soundscape_bird_data_2024_summaries_12.5.24.csv")
# str(t_sc_dat) #2176   69

#create extreme temp factor 
t_sc_dat$ext_fact <- as.factor(t_sc_dat$ext_95)
t_sc_dat$ext_day_fact <- as.factor(t_sc_dat$ext_95_day)

levels(t_sc_dat$ext_fact) <- c("not extreme", "extreme")
levels(t_sc_dat$ext_day_fact) <- c("not extreme", "extreme")

t_dat_per$ext_fact <- as.factor(t_dat_per$ext_95)
t_dat_per$ext_day_fact <- as.factor(t_dat_per$ext_95_day)

levels(t_dat_per$ext_fact) <- c("not extreme", "extreme")
levels(t_dat_per$ext_day_fact) <- c("not extreme", "extreme")


levels(t_sc_dat$ext_fact)
levels(t_sc_dat$ext_day_fact)
summary(t_sc_dat$ext_fact) #163 NAs
summary(t_sc_dat$ext_day_fact) #163 NAs

#create extreme variable w/high VPD--------------------------------------------- 

#For daily summary data
summary(t_sc_dat$vpd_kPa)
summary(t_sc_dat[which(t_sc_dat$ext_fact == "extreme"), "vpd_kPa"]) #median = 1.83
t_sc_dat$ext_vpd <- as.character(t_sc_dat$ext_fact)
t_sc_dat$ext_vpd <- ifelse(t_sc_dat$ext_vpd == "extreme" & t_sc_dat$vpd_kPa > 1.83, "extreme high vpd", t_sc_dat$ext_vpd)
table(t_sc_dat$ext_vpd, t_sc_dat$ext_fact)
plot(as.factor(t_sc_dat$ext_vpd), t_sc_dat$vpd_kPa)

t_sc_dat$ext_vpd <- as.factor(t_sc_dat$ext_vpd)
levels(as.factor(t_sc_dat$ext_vpd))
t_sc_dat$ext_vpd <-
  factor(t_sc_dat$ext_vpd,
         levels = c("not extreme", "extreme", "extreme high vpd"))
summary(t_sc_dat$ext_vpd)

summary(t_sc_dat[which(t_sc_dat$ext_day_fact == "extreme"), "vpd_kPa"]) #median = 1.26; lets use same threshold as above
t_sc_dat$ext_day_vpd <- as.character(t_sc_dat$ext_day_fact)
t_sc_dat$ext_day_vpd <- ifelse(t_sc_dat$ext_day_vpd == "extreme" & t_sc_dat$vpd_kPa > 1.3, "extreme high vpd", t_sc_dat$ext_day_vpd)
table(t_sc_dat$ext_day_vpd, t_sc_dat$ext_day_fact)
plot(as.factor(t_sc_dat$ext_day_vpd), t_sc_dat$vpd_kPa)

t_sc_dat$ext_day_vpd <- as.factor(t_sc_dat$ext_day_vpd)
levels(as.factor(t_sc_dat$ext_day_vpd))
t_sc_dat$ext_day_vpd <-
  factor(t_sc_dat$ext_day_vpd,
         levels = c("not extreme", "extreme", "extreme high vpd"))
summary(t_sc_dat$ext_day_vpd)

#For data with daytime summaries for morn, mid, and eve
t_dat_per$ext_vpd <- as.character(t_dat_per$ext_fact)
t_dat_per$ext_vpd <- ifelse(t_dat_per$ext_vpd == "extreme" & t_dat_per$vpd_kPa > 1.83, "extreme high vpd", t_dat_per$ext_vpd)
table(t_dat_per$ext_vpd, t_dat_per$ext_fact)
plot(as.factor(t_dat_per$ext_vpd), t_dat_per$vpd_kPa)

t_dat_per$ext_vpd <- as.factor(t_dat_per$ext_vpd)
levels(as.factor(t_dat_per$ext_vpd))
t_dat_per$ext_vpd <-
  factor(t_dat_per$ext_vpd,
         levels = c("not extreme", "extreme", "extreme high vpd"))
summary(t_dat_per$ext_vpd)

summary(t_dat_per[which(t_dat_per$ext_day_fact == "extreme"), "vpd_kPa"]) #median = 1.26; lets use same threshold as above
t_dat_per$ext_day_vpd <- as.character(t_dat_per$ext_day_fact)
t_dat_per$ext_day_vpd <- ifelse(t_dat_per$ext_day_vpd == "extreme" & t_dat_per$vpd_kPa > 1.3, "extreme high vpd", t_dat_per$ext_day_vpd)
table(t_dat_per$ext_day_vpd, t_dat_per$ext_day_fact)
plot(as.factor(t_dat_per$ext_day_vpd), t_dat_per$vpd_kPa)

t_dat_per$ext_day_vpd <- as.factor(t_dat_per$ext_day_vpd)
levels(as.factor(t_dat_per$ext_day_vpd))
t_dat_per$ext_day_vpd <-
  factor(t_dat_per$ext_day_vpd,
         levels = c("not extreme", "extreme", "extreme high vpd"))
summary(t_dat_per$ext_day_vpd)

#create extreme variable w/high WGT---------------------------------------------- 
#For daily summary data
summary(t_sc_dat$wb_temp_95)
summary(t_sc_dat[which(t_sc_dat$ext_fact == "extreme"), "wb_temp_95"]) #median = 26.0
t_sc_dat$ext_wbt <- as.character(t_sc_dat$ext_fact)
t_sc_dat$ext_wbt <- ifelse(t_sc_dat$ext_wbt == "extreme" & t_sc_dat$wb_temp_95 > 26.0, "extreme high wbt", t_sc_dat$ext_wbt)
table(t_sc_dat$ext_wbt, t_sc_dat$ext_fact)
plot(as.factor(t_sc_dat$ext_wbt), t_sc_dat$wb_temp_95)

t_sc_dat$ext_wbt <- as.factor(t_sc_dat$ext_wbt)
levels(as.factor(t_sc_dat$ext_wbt))
t_sc_dat$ext_wbt <-
  factor(t_sc_dat$ext_wbt,
         levels = c("not extreme", "extreme", "extreme high wbt"))
summary(t_sc_dat$ext_wbt)

summary(t_sc_dat[which(t_sc_dat$ext_day_fact == "extreme"), "wb_temp_95"]) #median = 25.7;  lets use same threshold as above
t_sc_dat$ext_day_wbt <- as.character(t_sc_dat$ext_day_fact)
t_sc_dat$ext_day_wbt <- ifelse(t_sc_dat$ext_day_wbt == "extreme" & t_sc_dat$wb_temp_95 > 26.0, "extreme high wbt", t_sc_dat$ext_day_wbt)
table(t_sc_dat$ext_day_wbt, t_sc_dat$ext_day_fact)
plot(as.factor(t_sc_dat$ext_day_wbt), t_sc_dat$wb_temp_95)

t_sc_dat$ext_day_wbt <- as.factor(t_sc_dat$ext_day_wbt)
levels(as.factor(t_sc_dat$ext_day_wbt))
t_sc_dat$ext_day_wbt <-
  factor(t_sc_dat$ext_day_wbt,
         levels = c("not extreme", "extreme", "extreme high wbt"))
summary(t_sc_dat$ext_day_wbt)


#For data with daytime summaries for morn, mid, and eve
summary(t_dat_per[which(t_dat_per$ext_fact == "extreme"), "wb_temp_95"]) #median = 26.0
t_dat_per$ext_wbt <- as.character(t_dat_per$ext_fact)
t_dat_per$ext_wbt <- ifelse(t_dat_per$ext_wbt == "extreme" & t_dat_per$wb_temp_95 > 26.0, "extreme high wbt", t_dat_per$ext_wbt)
table(t_dat_per$ext_wbt, t_dat_per$ext_fact)
plot(as.factor(t_dat_per$ext_wbt), t_dat_per$wb_temp_95)

t_dat_per$ext_wbt <- as.factor(t_dat_per$ext_wbt)
levels(as.factor(t_dat_per$ext_wbt))
t_dat_per$ext_wbt <-
  factor(t_dat_per$ext_wbt,
         levels = c("not extreme", "extreme", "extreme high wbt"))
summary(t_dat_per$ext_wbt)

summary(t_dat_per[which(t_dat_per$ext_day_fact == "extreme"), "wb_temp_95"]) #median = 25.7;  lets use same threshold as above
t_dat_per$ext_day_wbt <- as.character(t_dat_per$ext_day_fact)
t_dat_per$ext_day_wbt <- ifelse(t_dat_per$ext_day_wbt == "extreme" & t_dat_per$wb_temp_95 > 26.0, "extreme high wbt", t_dat_per$ext_day_wbt)
table(t_dat_per$ext_day_wbt, t_dat_per$ext_day_fact)
plot(as.factor(t_dat_per$ext_day_wbt), t_dat_per$wb_temp_95)

t_dat_per$ext_day_wbt <- as.factor(t_dat_per$ext_day_wbt)
levels(as.factor(t_dat_per$ext_day_wbt))
t_dat_per$ext_day_wbt <-
  factor(t_dat_per$ext_day_wbt,
         levels = c("not extreme", "extreme", "extreme high wbt"))
summary(t_dat_per$ext_day_wbt)



#concatenate location and habitat variable
t_sc_dat$loc_hab <- paste(t_sc_dat$location, t_sc_dat$habitat, sep = " ")
summary(as.factor(t_sc_dat$loc_hab))

t_sc_dat$loc_hab <- as.factor(t_sc_dat$loc_hab)
levels(t_sc_dat$loc_hab)
t_sc_dat$loc_hab <-
  factor(t_sc_dat$loc_hab,
         levels = c("urban open", "urban forest", "exurban open", "exurban forest"))
summary(t_sc_dat$loc_hab)


t_dat_per$loc_hab <- paste(t_dat_per$location, t_dat_per$habitat, sep = " ")
summary(as.factor(t_dat_per$loc_hab))

t_dat_per$loc_hab <- as.factor(t_dat_per$loc_hab)
levels(t_dat_per$loc_hab)
t_dat_per$loc_hab <-
  factor(t_dat_per$loc_hab,
         levels = c("urban open", "urban forest", "exurban open", "exurban forest"))
summary(t_dat_per$loc_hab)


str(t_sc_dat) #2176 obs. of  124 variables
str(t_dat_per) #2176 obs. of  52 variables

table(t_sc_dat$ext_day_vpd, t_sc_dat$month, t_sc_dat$loc_hab)

#Write data file
# write.csv(t_sc_dat, "Data/analysis_data/temp_soundscape_bird_data_2024_summaries_12.11.24.csv", row.names = FALSE)
#write.csv(t_dat_per, "Data/analysis_data/temp_soundscape_bird_data_2024_dial_summaries_12.11.24.csv", row.names = FALSE)



#Explore data---------------------------------------------------------------------

plot(t_sc_dat$temp_95, t_sc_dat$amgo_n_1)
plot(t_sc_dat$vpd_kPa, t_sc_dat$amgo_n_1)
plot(t_sc_dat$wb_temp_95, t_sc_dat$amgo_n_1)
plot(t_sc_dat$aci_nt, t_sc_dat$count)
plot(t_sc_dat$ndsi_nt, t_sc_dat$count)
plot(as.factor(t_sc_dat$ext_fact_vpd), t_sc_dat$count_1)

## ******************************************************************************************
## Make exploratory plots
## ******************************************************************************************

head(t_sc_dat)

#re-order factor levels for plottting
t_sc_dat$habitat <-  as.factor(t_sc_dat$habitat)
t_sc_dat$location <-  as.factor(t_sc_dat$location)

levels(t_sc_dat$habitat)
t_sc_dat$habitat <-
  factor(t_sc_dat$habitat,
         levels = c("open", "forest"))
t_sc_dat$habitat<-droplevels(t_sc_dat$habitat)

levels(t_sc_dat$location)
t_sc_dat$location <-
  factor(t_sc_dat$location,
         levels = c("urban", "exurban"))
t_sc_dat$location<-droplevels(t_sc_dat$location)
summary(t_sc_dat$location)


#create extreme variable for plotting
t_sc_dat$ext_95_plt <- t_sc_dat$ext_95
t_sc_dat[which(t_sc_dat$ext_95_plt == 0), "ext_95_plt"] <- NA


#get rid of NAs for plotting
summary(t_sc_dat)
dim(t_sc_dat) #406  36
t_sc_datc <- t_sc_dat[complete.cases(t_sc_dat[ , 1:35]),]
dim(t_sc_datc)
summary(t_sc_datc)

#Subset data to after June 15
t_sc_dat_j15 <- subset(t_sc_dat, date >= "2024-06-15 00:00:00")
dim(t_sc_dat_j15) #161  37 (144 41)
summary(t_sc_dat_j15)



#Plot soundscape metrics verus temperature by location and habitat
#png("./Figures_7-26-24/ndsi_day_v_vpd_2024_7-29-24.png", width = 5.3, height = 4.1, units = 'in', res = 600)
#png("./Figures_7-26-24/aci_day_v_wb_temp_2024_7-29-24.png", width = 5.3, height = 4.1, units = 'in', res = 600)

#png("C:/Users/kirchgrabera/Downloads/aci_day_v_temp95_2024_10-17-24.png", width = 4.3, height = 3.7, units = 'in', res = 600)
png("C:/Users/kirchgrabera/Downloads/ndsi_day_v_temp95_2024_10-17-24_B.png", width = 5.3, height = 3.7, units = 'in', res = 1200)

effect_plot <- ggplot(t_sc_dat, aes(temp_95, ndsi_day)) + 
  geom_point(aes(fill = factor(loc_hab), color = factor(loc_hab)), shape = 21, size = 2.5, stroke = 0.5, alpha = 0.75) + 
  stat_smooth(aes(color = factor(loc_hab)), method = lm, formula = y ~ x, se = TRUE, alpha = 0.25, size = 1.5) + 
  #stat_smooth(method = lm, formula = y ~ x, se = TRUE, alpha = 0.25, size = 1.5, color = "grey35") + 
  #stat_smooth(method = lm, formula = y ~ poly(x, 2), se = FALSE, size = 1.5, color = "grey35") +
  #stat_smooth(method = loess, span = 0.75, se = FALSE, size = 1.5) +
  #stat_smooth(aes(color = factor(loc_hab)), method = loess, span = 0.75, se = FALSE, size = 1.5) +
  #geom_abline(intercept = 0, slope = 1, size = 1.1, lty = "dashed") +
  #scale_fill_manual(values=c("#c0bebf", "#fefe03", "#32c32f")) +
  #scale_color_manual(values=c("#c0bebf", "#fefe03", "#32c32f")) +
  scale_color_manual(values=c("#fBB91FFF", "#29af7fff", "#f8870eff","#2d718eff"), na.translate = F) +
  scale_fill_manual(values=c("#fBB91FFF", "#29af7fff", "#f8870eff","#2d718eff"), na.translate = F) +
  # scale_colour_discrete(na.translate = F) +
  # scale_fill_discrete(na.translate = F) +
  # scale_fill_manual(values=c("#FEB37Bff", "#AB337Cff", "#1F968Bff", "#453781FF", "grey")) +
  # scale_color_manual(values=c("#FEB37Bff", "#AB337Cff", "#1F968Bff", "#453781FF", "grey")) +
  theme(panel.grid.major = element_line(colour = "white")) + 
  theme(panel.grid.minor = element_line(colour = "white")) + 
  theme(panel.grid.major.y = element_line(colour = "white")) +
  theme(panel.border = element_rect(color = "black", size = 1)) +
  theme(legend.title=element_blank()) +
  #theme(legend.position = "none") +
  #theme(legend.position=c(0.25, 0.88)) +
  #theme(legend.text = element_text(size = 12)) +
  theme(axis.text = element_text(size = 14)) + 
  theme(axis.title.y=element_text(size = 14)) +
  theme(axis.title.x=element_text(size = 14)) +
  #scale_x_continuous(expand = c(0.015, 0.009)) +
  #scale_y_continuous(limits = c(-0.2, 8.8), breaks = seq(0 , 8, by = 2)) +
  theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, unit = "cm")) +
  #ylab("Acoustic complexity index") +
  ylab("NDSI (day)") +
  #xlab("Vapor pressure deficit (kPa)")
  xlab("Q95 temperature (°C)")
  #xlab("Wet bulb temperature (°C)")
effect_plot
dev.off()


#Plot temperature and sc metrics versus date------------------------------------------
#png("./Figures_7-26-24/sci_faith_vpd_2024_day_extrm_red_07.28.24.png", width = 9.7, height = 4.3, units = 'in', res = 600)
png("C:/Users/kirchgrabera/Downloads/sci_faith_aci_2024_day_extrm_red_10.17.24.png", width = 9.7, height = 4.3, units = 'in', res = 600)

temp.date_plot <-
  ggplot(t_sc_dat[complete.cases(t_sc_dat[ , "location"]),], aes(date, count, group = factor(sites), color=factor(habitat))) +
  facet_grid(~location, drop = TRUE) +
  #geom_line(aes(color = factor(habitat)), size = 1, alpha = 0.75) + #color = "#2d718eff"
  #geom_point(data = t_sc_dat[which(t_sc_dat$ext_95_plt == 1), ], aes(date, vpd_kPa), color = "red", shape = 17, size = 2, stroke = 2) + ##8305a7ff; #color = "#2d718eff"
  geom_point(aes(date, count), shape = 17, size = 2, stroke = 2) + ##8305a7ff; #color = "#2d718eff"
  scale_color_manual(values=c("#fba238ff", "#29af7fff"), na.translate = F) +
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
    theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, unit = "cm")) +
  xlab("Date") + 
  #ylab("Soil moisture (pulse rate)")
  #ylab("Acoustic complexity index") 
  #ylab("NDSI")
  ylab("VPD (kPA)")
#labs(y = expression("Temperature " *~degree*C))
temp.date_plot
dev.off()



#Plot mean soundscape metrics by habitat. location, and extrem temp events----------------------------------
#png("./Figures_7-26-24/sci_faith_aci_day_extremes_vpd_07.28.24.png", width = 5.3, height = 3.7, units = 'in', res = 600)
png("C:/Users/kirchgrabera/Downloads/sci_faith_aci_day_extremes_vpd_10.17.24.png", width = 5.3, height = 3.7, units = 'in', res = 600)

boxplot <- ggplot(t_sc_dat[complete.cases(t_sc_dat[ , "location"]),], aes(location, aci_day, color=factor(ext_fact_vpd), fill = factor(ext_fact_vpd))) + 
  #geom_hline(yintercept = 0, linetype="dashed", color = "grey") +
  geom_boxplot(aes(color=factor(ext_fact_vpd), fill = factor(ext_fact_vpd)), alpha = 0.40, size=1.25) + 
  # scale_color_manual(values=c("#c0bebf", "#32c32f")) + #"#fefe03",
  # scale_fill_manual(values=c("#c0bebf", "#32c32f")) +  #"#fefe03"
  scale_color_manual(values=c("grey55", "red", "purple"), na.translate = F) +
  scale_fill_manual(values=c("grey55", "red", "purple"), na.translate = F) +
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
  ylab("Acoustic complexity index (day)") +
  #ylab("NDSI (day)") +
  #labs(y = expression("Plot-level leaf area index")) +
  #labs(y = expression("Plot-level vertical complexity index")) +
  labs(x = expression("Location"))
boxplot
dev.off()


plot(t_sc_dat$aci_day, t_sc_dat$aci_nt)
abline(0, 1)

plot(t_sc_dat$ndsi_day, t_sc_dat$ndsi_nt)
abline(0, 1)

plot(t_sc_dat$temp_95, t_sc_dat$vpd_kPa)
plot(t_sc_dat$rel_hum_mn, t_sc_dat$vpd_kPa)
plot(t_sc_dat$temp_95, t_sc_dat$rel_hum_mn)
