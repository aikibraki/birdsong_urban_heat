###########################################################################################################################################
### (4) Quantify temperature extremes and offsets##########################################################################################
###########################################################################################################################################
###updated 7-28-24


## ***************************************************************************************************
## Preamble
## ***************************************************************************************************

rm(list=ls())

library(ggplot2)
theme_set(theme_bw())

#Load microclimate summaries
#tdat <- read.csv("Data/cleaned_drop_data/drop_data_2024_daytime_summaries_extrm_07.28.24.csv")
tdat <- read.csv("C:/Users/kirchgrabera/Smithsonian Dropbox/Aidan Kirchgraber/Science and Faith/Aidan/audio_analysis/birdnet/drop_data_2024_daytime_summaries_extrm_10.16.24.csv")
dim(tdat) #522  26

#Load daily mean soundscape metrics
scdat <- read.csv("C:/Users/kirchgrabera/Smithsonian Dropbox/Aidan Kirchgraber/Science and Faith/Aidan/audio_analysis/birdnet/soundscape_metrics_real_daily_means_10-17-24.csv")
dim(scdat) #354  12

#Load grey treefrog detection summaries
#gdat <- read.csv("Data/cleaned_audio_data/sites_grey_treefrogs_det_daily_sum_7-28-24.csv")
gdat <- read.csv("C:/Users/kirchgrabera/Smithsonian Dropbox/Aidan Kirchgraber/Science and Faith/Aidan/audio_analysis/birdnet/sites_noca_det_daily_sum_10-16-24.csv")
dim(gdat) #368   4


#reformat date asPosixct
tdat$date <- as.POSIXct(tdat$date, format='%Y-%m-%d')
scdat$date <- as.POSIXct(scdat$date, format='%Y-%m-%d')
gdat$date <- as.POSIXct(gdat$date, format='%Y-%m-%d')
summary(as.character(tdat$date) == tdat$date_orig)
summary(tdat$date)
summary(is.na(as.character(tdat$date)))
summary(gdat$date)
summary(is.na(as.character(gdat$date)))


## ******************************************************************************************
## Merge drop data and soundscape metrics
## ******************************************************************************************

str(tdat)
str(scdat)

t <- unique(tdat$date_site) #vector of characters a 
s <- unique(scdat$date_site) #vector of characters b 
t[!(t %in% s)] #52 dates_sites in temp data but not in audiomoth data
s[!(s %in% t)] #all dates_sites in audiomoth data are in temp data

t_sc_dat <-
  merge(
    tdat,
    scdat[ , c("date_site", "aci", "ndsi", "aci_nt", "ndsi_nt", "aci_day", "ndsi_day")],
    by.x = c("date_site"),
    by.y = c("date_site"),
    all.x = TRUE
  )
dim(t_sc_dat) #522  32

#Calculate VPD------------------------------------------------------------------

install.packages("pvldcurve")
install.packages("plantecophys")

library(pvldcurve)
library(plantecophys)

#compare output from two functions
vpd1 <- VaporPressureDeficit(t_sc_dat, humidity = "rel_hum_mn",
                              temperature = "temp_mn", atmospheric.pressure = 101.325)  #default = 101.325 (atmospheric pressure at sea level)
vpd2 <- RHtoVPD(t_sc_dat$rel_hum_mn, t_sc_dat$temp_mn, Pa = 101)

summary(vpd1$vapor.pressure.deficit) # 2 NAs
summary(vpd2) # 2 NAs

plot(vpd1$vapor.pressure.deficit, vpd2) #identical except for units
abline(0, 1)

t_sc_dat$vpd_kPa <- RHtoVPD(t_sc_dat$rel_hum_mn, t_sc_dat$temp_mn, Pa = 101)
dim(t_sc_dat) #522  33

#Write data file
#write.csv(t_sc_dat, "Data/analysis_data/temp_soundscape_data_2024_daytime_summaries_07.28.24.csv", row.names = FALSE)
write.csv(t_sc_dat, "C:/Users/kirchgrabera/Smithsonian Dropbox/Aidan Kirchgraber/Science and Faith/Aidan/audio_analysis/birdnet/temp_soundscape_data_2024_daytime_summaries_10.17.24.csv", row.names = FALSE)

#Are there NAs for soundscape metrics during extreme temp events?
summary(t_sc_dat$aci)
sum(t_sc_dat[is.na(t_sc_dat$aci), "ext_95"]) #Yes, during 4 extreme heat obs :/
sum(t_sc_dat[, "ext_95"], na.rm = TRUE) # 22 total extreme heat events
table(t_sc_dat[is.na(t_sc_dat$aci), c("ext_95", "habitat", "location")])
table(t_sc_dat[ , c("ext_95", "habitat", "location")])


## ******************************************************************************************
## Merge drop and grey treefrog detections (and soundscape metrics?)
## ******************************************************************************************

str(t_sc_dat)
str(gdat)

gdat$date_min1 <- gdat$date - days(1)
gdat$date_site <- paste(gdat$date, gdat$site, sep = "_")
gdat$date_site_1 <- paste(gdat$date_min1, gdat$site, sep = "_")

t <- unique(t_sc_dat$date_site) #vector of characters a 
g <- unique(gdat$date_site_1) #vector of characters b 
t[!(t %in% g)] #21 dates_sites in temp data but not in audiomoth data
g[!(g %in% t)] #352 dates_sites in audiomoth data are not in temp data

t_sc_dat <-
  merge(
    t_sc_dat,
    gdat[ , c("date_site", "count", "count_asr")],
    by.x = c("date_site"),
    by.y = c("date_site"),
    all.x = TRUE
  )
dim(t_sc_dat) #522 35

colnames(gdat)[3:4] <- c("count_1", "count_asr_1")

t_sc_dat <-
  merge(
    t_sc_dat,
    gdat[ , c("date_site_1", "count_1", "count_asr_1")],
    by.x = c("date_site"),
    by.y = c("date_site_1"),
    all.x = TRUE
  )
dim(t_sc_dat) #522 37

summary(t_sc_dat)

#Write data file
#write.csv(t_sc_dat, "Data/analysis_data/temp_soundscape_frog_data_2024_summaries_07.28.24.csv", row.names = FALSE)
write.csv(t_sc_dat, "C:/Users/kirchgrabera/Smithsonian Dropbox/Aidan Kirchgraber/Science and Faith/Aidan/audio_analysis/birdnet/temp_soundscape_bird_data_2024_summaries_10.17.24.csv", row.names = FALSE)

#Explore data---------------------------------------------------------------------

plot(t_sc_dat$temp_95, t_sc_dat$count_1)
plot(t_sc_dat$vpd_kPa, t_sc_dat$count_1)
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

#create extreme temp factor 
t_sc_dat$ext_fact <- as.factor(t_sc_dat$ext_95)
levels(t_sc_dat$ext_fact) <- c("not extreme", "extreme")
levels(t_sc_dat$ext_fact)
summary(t_sc_dat$ext_fact)

#create extreme variable w/high VPD 
t_sc_dat$ext_fact_vpd <- as.character(t_sc_dat$ext_fact)
t_sc_dat$ext_fact_vpd <- ifelse(t_sc_dat$ext_fact_vpd == "extreme" & t_sc_dat$vpd_kPa > 2, "extreme high vpd", t_sc_dat$ext_fact_vpd)
table(t_sc_dat$ext_fact_vpd, t_sc_dat$ext_fact)
plot(as.factor(t_sc_dat$ext_fact_vpd), t_sc_dat$vpd_kPa)

t_sc_dat$ext_fact_vpd <- as.factor(t_sc_dat$ext_fact_vpd)
levels(t_sc_dat$ext_fact_vpd)
t_sc_dat$ext_fact_vpd <-
  factor(t_sc_dat$ext_fact_vpd,
         levels = c("not extreme", "extreme", "extreme high vpd"))
t_sc_dat$ext_fact_vpd <- droplevels(t_sc_dat$ext_fact_vpd)
summary(t_sc_dat$ext_fact_vpd)


#concatenate location and habitat variable
t_sc_dat$loc_hab <- paste(t_sc_dat$location, t_sc_dat$habitat, sep = " ")
t_sc_dat[which(t_sc_dat$loc_hab == "NA NA"), "loc_hab"] <- NA

levels(as.factor(t_sc_dat$loc_hab))
t_sc_dat$loc_hab <-
  factor(t_sc_dat$loc_hab,
         levels = c("urban open", "urban forest", "exurban open", "exurban forest"))
t_sc_dat$loc_hab <- droplevels(t_sc_dat$loc_hab)


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
