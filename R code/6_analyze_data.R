###########################################################################################################################################################
### (6) Analyses of bird detection data after sunrise (asr)########################################################################################################
########################################################################################################################################################
###updated 12-18-24

#To do:
#(1) Need to go back to script 3 and subset by confidence level there - done
#(2) Analyze with high VPD/WBT
#(2) Roll into functions - mostly done
#(3) Calculate posterior contrasts among groups and add to functions

## ***************************************************************************************************
## Load packages, data, and saved models
## ***************************************************************************************************

rm(list=ls())

library(brms)
library(ggplot2)
theme_set(theme_bw())

# load custom helper functions
source("R code_working/bird_mod_functions.R")

#Load temperature and soundscape data
dat <- read.csv("Data/analysis_data/temp_soundscape_bird_data_2024_summaries_12.11.24.csv") #bird detections filtered by confidence thresholds

dim(dat) #2176   124

#get breakdown of extreme heat days by location and habitat
table(dat$ext_day_fact, dat$loc_hab)

## ***************************************************************************************************
## Make pair plots and prep data
## ***************************************************************************************************

#Examine pair plots-------------------------------------------------------------

library(GGally)

colnames(dat)
vars <-
  dat[, c(
    "temp_95",
    "wb_temp_95",
    "vpd_kPa",
    "aci",
    "ndsi",
    "ndsi_day",
    "amgo_n",
    "ext_fact",
    "ext_day_fact",
    "ext_day_vpd",
    "ext_day_wbt",
    "loc_hab"
  )]

ggpairs(vars) #Nothing too concerning

#---------------------------------------------------------------------------------
#Standardize variables-------------------------------------------------------------

#center and scale continuous covariates 
dat$z.temp_95 <- as.numeric(scale(dat$temp_95))
dat$z.wb_temp_95 <- as.numeric(scale(dat$wb_temp_95))
dat$z.vpd_kPa <- as.numeric(scale(dat$vpd_kPa))
dat$z.ndsi <- as.numeric(scale(dat$ndsi))

#---------------------------------------------------------------------------------
#Generate binary response variables----------------------------------------------

# hist(dat$amgo_asr)
# hist(dat$noca_asr)
# 
# dat$amgo_asr_bin <- ifelse(dat$amgo_asr > 0, 1, 0)
# dat$noca_asr_bin <- ifelse(dat$noca_asr > 0, 1, 0)
# dat$cach_asr_bin <- ifelse(dat$cach_asr > 0, 1, 0)


#---------------------------------------------------------------------------------
#order factor levels--------------------------------------------------------------

dat$ext_day_fact <- as.factor(dat$ext_day_fact)
levels(dat$ext_day_fact)
dat$ext_day_fact <-
  factor(dat$ext_day_fact,
         levels = c("not extreme", "extreme"))
summary(dat$ext_day_fact)


dat$ext_fact <- as.factor(dat$ext_fact)
levels(dat$ext_fact)
dat$ext_fact <-
  factor(dat$ext_fact,
         levels = c("not extreme", "extreme"))
summary(dat$ext_fact)


dat$ext_day_vpd <- as.factor(dat$ext_day_vpd)
levels(dat$ext_day_vpd)
dat$ext_day_vpd <-
  factor(dat$ext_day_vpd,
         levels = c("not extreme", "extreme", "extreme high vpd"))
summary(dat$ext_day_vpd)

dat$ext_vpd <- as.factor(dat$ext_vpd)
levels(dat$ext_vpd)
dat$ext_vpd <-
  factor(dat$ext_vpd,
         levels = c("not extreme", "extreme", "extreme high vpd"))
summary(dat$ext_vpd)

dat$ext_day_wbt <- as.factor(dat$ext_day_wbt)
levels(dat$ext_day_wbt)
dat$ext_day_wbt <-
  factor(dat$ext_day_wbt,
         levels = c("not extreme", "extreme", "extreme high wbt"))
summary(dat$ext_day_wbt)

#location and habitat variable
dat$loc_hab <- as.factor(dat$loc_hab)
levels(dat$loc_hab)
dat$loc_hab <-
  factor(dat$loc_hab,
         levels = c("urban open", "urban forest", "exurban open", "exurban forest"))
summary(dat$loc_hab)

dat$month_fact <- as.factor(dat$month)
levels(dat$month_fact) #looks good
summary(dat$month_fact)


#---------------------------------------------------------------------------------
#drop data from May and Sept----------------------------------------------------
#Not enough extreme days to support estimates when fitting three way interactions
table(dat$month_fact, dat$loc_hab, dat$ext_day_fact)

dat_1 <- subset(dat, month != 5)
dat_1 <- subset(dat_1, month != 9)
dat_1 <- droplevels(dat_1)
dim(dat_1) #1476  129

table(dat_1$month_fact, dat_1$loc_hab, dat_1$ext_day_fact)
table(dat_1$month_fact, dat_1$loc_hab, dat_1$ext_day_vpd)
table(dat_1$month_fact, dat_1$loc_hab, dat_1$ext_day_wbt)

#---------------------------------------------------------------------------------
#Examine VIF as indicator of potential multicollinearity------------------------

library(car)
#First look at VIF of intended covariate strctures here

vif(glm(noca_asr ~ ext_day_fact * loc_hab * month_fact + z.ndsi,
        family = poisson, data  = dat_1), type="predictor")

vif(glm(noca_asr ~ z.ndsi + ext_day_vpd * loc_hab,
        family = poisson, data  = dat), type="predictor")

plot(as.factor(dat$loc_hab), as.factor(dat$ext_day_fact))
plot(as.factor(dat$loc_hab), as.factor(dat$ext_day_vpd))



## ***************************************************************************************************
## Fit and plot models with RESPONSE var = bird detections after sunrise (asr)
## ***************************************************************************************************

#For each of three response types (vocalizations after sunrise, midday hours, and across times of day)
#(1) Fit models with (a) additive structure, (b) two-interactions of extreme*hab
#    and (c) three-way interactions of extreme*hab*month 

#For each species:
#(1) Fit model
#(2) Get estimates
#(3) Make and save plots (in organized subfolders)


#Function to fit and save models (arguments and defaults)
# bird.model(response,
#            main,
#            data,
#            date,
#            month_main = TRUE,
#            iter = 4000,
#            family = negbinomial(),
#            path = "./saved_models/")


#-------------------------------------------------------------------------------------
##FIRST, fit models of vocalization responses to additive effects-----------------------
#or 2-WAY interactions of:
#Main interactive effects = ext_day_fact * loc_hab + month_fact
#Main interactive effects = z.vpd * loc_hab + month_fact

#Arguments and defaults for plotting function
# ext_add_plot(
#   mod,
#   response,
#   var1,
#   col = "#8305A7FF",
#   save = FALSE,
#   date = "12-11-24",
#   path = "./figures_12-17-24/"
# )


#Red bellied woodpecker-----------------------------------------------------------

#additive model
rewo_ext_day_add <-
  bird.model("rewo_asr",
             "ext_day_fact+loc_hab+month_fact",
             dat_1,
             "12-18-24")

rewo_ext_plt <- bird_plot(rewo_ext_day_add, "rewo_asr", "ext_day_fact")
rewo_hab_plt <- bird_plot(rewo_ext_day_add, "rewo_asr", "loc_hab")
rewo_mon_plt <- bird_plot(rewo_ext_day_add, "rewo_asr", "month_fact")
rewo_ndsi_plt <- bird_plot(rewo_ext_day_add, "rewo_asr", "z.ndsi")

#2-way interaction between extreme*habitat
rewo_ext_day_hab <-
  bird.model("rewo_asr",
             "ext_day_fact*loc_hab+month_fact",
             dat_1,
             "12-18-24")

rewo_ext_hab_plt <-
  bird_plot(
    rewo_ext_day_hab,
    "rewo_asr",
    "ext_day_fact:loc_hab",
    save = TRUE,
    date = "12-18-24",
    path = "./figures_12-4-24/asr/two-way interactions/ext_day_fact"
  )

#2-way interaction between extreme_vpd*habitat
rewo_ext_vbd_hab <-
  bird.model("rewo_asr",
             "ext_day_vpd*loc_hab+month_fact",
             dat_1,
             "12-18-24")

rewo_extvpd_hab_plt <-
  bird_plot(
    rewo_ext_vbd_hab,
    "rewo_asr",
    "ext_day_vpd:loc_hab",
    save = TRUE,
    date = "12-18-24",
    path = "./figures_12-4-24/asr/two-way interactions/ext_day_vpd"
  )

#2-way interaction between extreme_wbt*habitat
rewo_ext_wbt_hab <-
  bird.model("rewo_asr",
             "ext_day_wbt*loc_hab+month_fact",
             dat_1,
             "12-18-24")

rewo_extwbt_hab_plt <-
  bird_plot(
    rewo_ext_wbt_hab,
    "rewo_asr",
    "ext_day_wbt:loc_hab",
    save = TRUE,
    date = "12-18-24",
    path = "./figures_12-4-24/asr/two-way interactions/ext_day_wbt"
  )


#-----------------------------------------------------------------------------------
##SECOND, fit models of vocalization responses to 3-WAY interaction of----------------------
#Main interactive effects = ext_day_fact * loc_hab * month_fact

#For each species:
#(1) Fit model
#(2) Get estimates
#(3) Make and save plots (in organized subfolders)

#carolina chickadees----------------------------------------------------------------
#Fit model
cach_ext_day <- 
  bird.model("cach_asr",
             "ext_day_fact*loc_hab*month_fact",
             dat_1,
             "12-18-24")
#get estimates
cach_est_ext <- ext_day.3int.est(cach_ext_day, "ext_day_fact")

#Make plot
cach_ext_plt <-
  bird_plot_3way(
    cach_est_ext[[1]],
    "cach_asr",
    "ext_day_fact",
    save = TRUE,
    "12-18-24",
    path = "./Figures_12-4-24/asr/three-way interactions/ext_day_fact"
  )
#View contrasts
cach_ext_plt
cach_est_ext$contrasts


#cardinals🐦-----------------------------------------------------------------------
#Fit model
noca_ext_day <-
  bird.model("noca_asr",
             "ext_day_fact*loc_hab*month_fact",
             dat_1,
             "12-18-24")

#get estimates
noca_est_ext <- ext_day.3int.est(noca_ext_day, "ext_day_fact")

#Make plot
noca_ext_plt <-
  bird_plot_3way(
    noca_est_ext[[1]],
    "noca_asr",
    "ext_day_fact",
    save = TRUE,
    "12-19-24",
    path = "./Figures_12-4-24/asr/three-way interactions/ext_day_fact"
  )

#View contrasts
noca_ext_plt
noca_est_ext$contrasts

#Red-bellied woodpeckers----------------------------------------------------------
#Fit model
rewo_ext_day <-
  bird.model("rewo_asr",
             "ext_day_fact*loc_hab*month_fact",
             dat_1,
             "12-19-24")

#get estimates
rewo_est_ext <- ext_day.3int.est(rewo_ext_day, "ext_day_fact")
#Make plot
rewo_ext_plt <-
  bird_plot_3way(
    rewo_est_ext[[1]],
    "rewo_asr",
    "ext_day_fact",
    save = TRUE,
    "12-19-24",
    path = "./Figures_12-4-24/asr/three-way interactions/ext_day_fact"
  )
#View contrasts
rewo_ext_plt
rewo_est_ext$contrasts

#-----------------------------------------------------------------------------------
#THIRD, fit models of vocalization reponses to 3-WAY interaction of----------------------
#Main interactive effects = ext_day_vpd * loc_hab * month_fact

#cardinals🐦-----------------------------------------------------------------------


#Red-bellied woodpeckers----------------------------------------------------------
#Fit model
rewo_ext_vpd <-
  bird.model("rewo_asr",
             "ext_day_vpd*loc_hab*month_fact",
             dat_1,
             "12-11-24")

rewo_est_extvpd <- ext_day.3int.est(rewo_ext_vpd , "ext_day_vpd")
rewo_extvpd_plt <-
  bird_plot_3way(
    rewo_est_extvpd,
    "rewo_asr",
    "ext_day_vpd",
    save = TRUE,
    "12-11-24",
    path = "./Figures_12-4-24/asr/three-way interactions/ext_day_vpd"
  )

png("./Figures_12-4-24/asr/three-way interactions/ext_day_vpd/ext_day_vpd_hab_month_rewo_asr_yadj_12-11-24.png", width = 8.8, height = 6.0, units = 'in', res = 600)
rewo_extvpd_plt + ylim(0, 20)
dev.off()

#View contrasts
rewo_ext_plt
rewo_est_ext$contrasts

#Look at the actual data underlying estimates - does it make sense?---------
#Estimates with very large CIs may reflect categories with few or no observations!
#Need to interpret carefully
table(dat_1$month_fact, dat_1$loc_hab, dat_1$ext_day_fact)
dat_1[which(dat_1$month_fact == "6" & dat_1$loc_hab == "urban open" & dat_1$ext_day_fact == "not extreme"), "cach_asr"]
dat_1[which(dat_1$month_fact == "6" & dat_1$loc_hab == "urban open" & dat_1$ext_day_fact == "extreme"), "cach_asr"]
#No obs for urbanforest_ext_5_cnt and exurbanopen_ext_5_cnt categories



#-----------------------------------------------------------------------------------
#FOURTH, Fit models of vocalization reponses to 3-WAY interaction of----------------------
#Main interactive effects = ext_day_wbt * loc_hab * month_fact
# noca_ext_wbt <-
#   bird.model("noca_asr",
#              "ext_day_wbt*loc_hab*month_fact",
#              dat_1,
#              "12-11-24")
# 
# rewo_ext_wbt <-
#   bird.model("rewo_asr",
#              "ext_day_wbt*loc_hab*month_fact",
#              dat_1,
#              "12-11-24")



#Look at the actual data underlying estimates - does it make sense?---------
table(dat_1$month_fact, dat_1$loc_hab, dat_1$ext_day_fact)

dat_1[which(dat_1$month_fact == "6" & dat_1$loc_hab == "urban open" & dat_1$ext_day_fact == "not extreme"), "cach_asr"]
dat_1[which(dat_1$month_fact == "6" & dat_1$loc_hab == "urban open" & dat_1$ext_day_fact == "extreme"), "cach_asr"]
#No obs for urbanforest_ext_5_cnt and exurbanopen_ext_5_cnt categories



## ***************************************************************************************************
## Compare models with different covar structure for a given species
## ***************************************************************************************************


#Red-bellied woodpeckers----------------------------------------------------------
loo(rewo_ext_day_add) #3859.9
loo(rewo_ext_day_hab) #3847.9 
loo(rewo_ext_vbd_hab) #3851.9
loo(rewo_ext_wbt_hab) #3851.0
loo(rewo_ext_day) #3759.1  <- for this species, the ext_day_fact:loc_hab:month_fact is best supported








## ***************************************************************************************************
## Scrapyard
## ***************************************************************************************************

#----------------------------------------------------------------------------------
#Make other plots----------------------------------------------------------------------

#This plot setup is for examining effects of continuous covariates (e.g., vpd)
ce <- conditional_effects(vpdmod, plot = FALSE)
str(ce)
ce_temp <- ce$'vpd_kPa'
str(ce_temp)

#Plasma
col2rgb("#44039eff")
col2rgb("#8305a7ff")
col2rgb("#dd5e66ff")
col2rgb("#fba238ff")
col2rgb("#5901a5ff")
col2rgb("#8305a7ff")
col2rgb("#c5407eff")
col2rgb("#dd5e66ff")
col2rgb("#f79044ff")
col2rgb("#fba238ff")

#png("./Figures_7-26-24/frog_calling_temp_07.29.24.png", width = 4.5, height = 3.5, units = 'in', res = 600)
png("./Figures_7-26-24/frog_calling_vpd_07.29.24.png", width = 4.5, height = 3.5, units = 'in', res = 600)
effect_plot <- ggplot() + 
  #geom_vline(xintercept = min(abun_jne[which(abun_jne$D_elongatus_count > 0), "D_elongatus_wt"]), color =  "grey55", size = 1, lty = "dashed") +
  #geom_point(data = abun_jne, aes(D_elongatus_wt, D_elongatus_count), fill = "grey55", color = "white", shape = 21, size = 4, stroke = 1.5, alpha = 0.25) + 
  geom_ribbon(data = ce_temp, aes(vpd_kPa, estimate__, ymin=lower__, ymax=upper__), fill = "#dd5e66ff", alpha=0.25) +
  geom_line(data = ce_temp, aes(vpd_kPa, estimate__,), color = "#dd5e66ff", size = 1.15) +
  theme(legend.title=element_blank()) +
  theme(legend.text = element_text(size = 12)) +
  theme(panel.grid.major = element_line(colour = "white")) + 
  theme(panel.grid.minor = element_line(colour = "white")) + 
  theme(panel.grid.major.y = element_line(colour = "white")) +
  theme(panel.border = element_rect(color = "black", size = 1)) +
  theme(axis.text = element_text(size = 14)) + 
  theme(axis.title.y=element_text(size = 14)) +
  theme(axis.title.x=element_text(size = 14)) +
  scale_x_continuous(expand = c(0, 0)) +
  theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, unit = "cm")) +
  #labs(x = expression("Prior day temperature (°C)")) +
  labs(x = expression("Vapor pressure deficit (kPa)")) +
  #labs(y = expression("Counts of C. aestivus"))
  #labs(y = expression("Counts of P. stygicus"))
  labs(y = expression("Probability of calling"))
effect_plot
dev.off()


#This 
#extract effects
ce <- conditional_effects(extmod2, plot = FALSE)
str(ce)
ce_ext <- ce$'ext_vpd:month_fact'
levels(as.factor(ce_ext$ext_vpd))
levels(as.factor(ce_ext$month_fact))

# ce_ext$ext_fact_vpd <-
#   factor(ce_ext$ext_fact_vpd,
#          levels = c("not extreme", "extreme", "extreme high vpd"))
# #iplc_est$pa_cat <- c("outside_pa", "pa_edge", "in_pa", "outside_pa", "pa_edge", "in_pa")

png("./Figures_7-26-24/frog_calling_extreme_temp_vpd_07.29.24.png", width = 4.75, height = 3.75, units = 'in', res = 600)
effect_plot_plus <- ggplot(ce_ext, aes(factor(ext_vpd), estimate__)) + 
  facet_grid(cols = vars(month_fact)) +
  #geom_errorbar(aes(y = log(estimate__), ymin = log(lower__), ymax = log(upper__), color = factor(iplc_pa)), size = 2, width = 0) +
  #geom_point(aes(y = log(estimate__), color = factor(iplc_pa)), fill = "grey85", shape = 21, size = 3.5, stroke = 3.0) + 
  geom_errorbar(aes(y = estimate__, ymin = lower__, ymax = upper__, color = factor(ext_vpd)), size = 2, width = 0) +
  geom_point(aes(y = estimate__, color = factor(ext_vpd)), fill = "grey85", shape = 21, size = 3.5, stroke = 3.0) + 
  scale_color_manual(values=c("grey55", "red", "purple"), na.translate = F) +
  scale_fill_manual(values=c("grey55", "red", "purple"), na.translate = F) +
  theme(panel.grid.minor = element_line(colour = "white")) + 
  theme(panel.grid.major = element_line(colour = "white")) + 
  theme(panel.border = element_rect(color = "black", size = 1)) +
  theme(axis.title.x=element_blank()) +
  theme(legend.title=element_blank()) +
  theme(axis.text = element_text(size = 18)) + 
  theme(legend.position="none") + 
  #labs(y = expression("Forest loss (ln ha/yr)")) +
  labs(y = expression("Calling activity")) +
  scale_x_discrete(labels=c("Not extreme", "Extreme",
                            "Extreme \n+ high VPD")) +
  theme(axis.text = element_text(size = 14)) + 
  theme(axis.title.y=element_text(size = 14)) +
  theme(legend.text=element_text(size = 14)) +#+ 
  scale_y_continuous(limits = c(0, 5)) +
  #theme(axis.text.x=element_text(size = 14, angle=45, hjust=1)) +
  theme(plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"))
effect_plot_plus
dev.off()




