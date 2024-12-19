###########################################################################################################################################################
### (7) Analyses of bird detection data during midday hours########################################################################################################
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

dim(dat) #2176   124; 5244   52

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

# hist(dat$amgo_mid)
# hist(dat$noca_mid)
# 
# dat$amgo_mid_bin <- ifelse(dat$amgo_mid > 0, 1, 0)
# dat$noca_mid_bin <- ifelse(dat$noca_mid > 0, 1, 0)
# dat$cach_mid_bin <- ifelse(dat$cach_mid > 0, 1, 0)


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
dim(dat_1) #1476  129; 3738   57

table(dat_1$month_fact, dat_1$loc_hab, dat_1$ext_day_fact)
table(dat_1$month_fact, dat_1$loc_hab, dat_1$ext_day_vpd)
table(dat_1$month_fact, dat_1$loc_hab, dat_1$ext_day_wbt)

#---------------------------------------------------------------------------------
#Examine VIF as indicator of potential multicollinearity------------------------

library(car)
#First look at VIF of intended covariate strctures here

vif(glm(noca_mid ~ ext_day_fact * loc_hab * month_fact + z.ndsi,
        family = poisson, data  = dat_1), type="predictor")

vif(glm(noca_mid ~ z.ndsi + ext_day_vpd * loc_hab,
        family = poisson, data  = dat), type="predictor")

plot(as.factor(dat$loc_hab), as.factor(dat$ext_day_fact))
plot(as.factor(dat$loc_hab), as.factor(dat$ext_day_vpd))



## ***************************************************************************************************
## Fit and plot models with RESPONSE var = bird detections durring midday hours
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
  bird.model("rewo_mid",
             "ext_day_fact+loc_hab+month_fact",
             dat_1,
             "12-18-24")

rewo_ext_plt <- bird_plot(rewo_ext_day_add, "rewo_mid", "ext_day_fact")
rewo_hab_plt <- bird_plot(rewo_ext_day_add, "rewo_mid", "loc_hab")
rewo_mon_plt <- bird_plot(rewo_ext_day_add, "rewo_mid", "month_fact")
rewo_ndsi_plt <- bird_plot(rewo_ext_day_add, "rewo_mid", "z.ndsi")

#2-way interaction between extreme*habitat
rewo_ext_day_hab <-
  bird.model("rewo_mid",
             "ext_day_fact*loc_hab+month_fact",
             dat_1,
             "12-18-24")

rewo_ext_hab_plt <-
  bird_plot(
    rewo_ext_day_hab,
    "rewo_mid",
    "ext_day_fact:loc_hab",
    save = TRUE,
    date = "12-18-24",
    path = "./figures_12-4-24/midday/two-way interactions/ext_day_fact"
  )

#2-way interaction between extreme_vpd*habitat
rewo_ext_vbd_hab <-
  bird.model("rewo_mid",
             "ext_day_vpd*loc_hab+month_fact",
             dat_1,
             "12-18-24")

rewo_extvpd_hab_plt <-
  bird_plot(
    rewo_ext_vbd_hab,
    "rewo_mid",
    "ext_day_vpd:loc_hab",
    save = TRUE,
    date = "12-18-24",
    path = "./figures_12-4-24/midday/two-way interactions/ext_day_vpd"
  )

#2-way interaction between extreme_wbt*habitat
rewo_ext_wbt_hab <-
  bird.model("rewo_mid",
             "ext_day_wbt*loc_hab+month_fact",
             dat_1,
             "12-18-24")

rewo_extwbt_hab_plt <-
  bird_plot(
    rewo_ext_wbt_hab,
    "rewo_mid",
    "ext_day_wbt:loc_hab",
    save = TRUE,
    date = "12-18-24",
    path = "./figures_12-4-24/midday/two-way interactions/ext_day_wbt"
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
  bird.model("cach_mid",
             "ext_day_fact*loc_hab*month_fact",
             dat_1,
             "12-18-24")
#get estimates
cach_est_ext <- ext_day.3int.est(cach_ext_day, "ext_day_fact")

#Make plot
cach_ext_plt <-
  bird_plot_3way(
    cach_est_ext[[1]],
    "cach_mid",
    "ext_day_fact",
    save = TRUE,
    "12-18-24",
    path = "./Figures_12-4-24/midday/three-way interactions/ext_day_fact"
  )
#View contrasts
cach_ext_plt
cach_est_ext$contrasts


#cardinals🐦-----------------------------------------------------------------------
#Fit model
noca_ext_day <-
  bird.model("noca_mid",
             "ext_day_fact*loc_hab*month_fact",
             dat_1,
             "12-18-24")

#get estimates
noca_est_ext <- ext_day.3int.est(noca_ext_day, "ext_day_fact")

#Make plot
noca_ext_plt <-
  bird_plot_3way(
    noca_est_ext[[1]],
    "noca_mid",
    "ext_day_fact",
    save = TRUE,
    "12-19-24",
    path = "./Figures_12-4-24/midday/three-way interactions/ext_day_fact"
  )

#View contrasts
noca_ext_plt
noca_est_ext$contrasts

#Red-bellied woodpeckers----------------------------------------------------------
#Fit model
rewo_ext_day <-
  bird.model("rewo_mid",
             "ext_day_fact*loc_hab*month_fact",
             dat_1,
             "12-19-24")

#get estimates
rewo_est_ext <- ext_day.3int.est(rewo_ext_day, "ext_day_fact")
#Make plot
rewo_ext_plt <-
  bird_plot_3way(
    rewo_est_ext[[1]],
    "rewo_mid",
    "ext_day_fact",
    save = TRUE,
    "12-19-24",
    path = "./Figures_12-4-24/midday/three-way interactions/ext_day_fact"
  )
#View contrasts
rewo_ext_plt
rewo_est_ext$contrasts


#-----------------------------------------------------------------------------------
#THIRD, fit models of vocalization reponses to 3-WAY interaction of----------------------
#Main interactive effects = ext_day_vpd * loc_hab * month_fact




#Red-bellied woodpeckers----------------------------------------------------------
#Fit model
rewo_ext_vpd <-
  bird.model("rewo_mid",
             "ext_day_vpd*loc_hab*month_fact",
             dat_1,
             "12-19-24")

#get estimates
rewo_est_extvpd <- ext_day.3int.est(rewo_ext_vpd, "ext_day_fact")
#Make plot
rewo_extvpd_plt <-
  bird_plot_3way(
    rewo_est_extvpd[[1]],
    "rewo_mid",
    "ext_day_vpd",
    save = TRUE,
    "12-19-24",
    path = "./Figures_12-4-24/midday/three-way interactions/ext_day_fact"
  )
#View contrasts
rewo_extvpd_plt
rewo_est_extvpd$contrasts






#-----------------------------------------------------------------------------------
#FOURTH, Fit models of vocalization reponses to 3-WAY interaction of----------------------
#Main interactive effects = ext_day_wbt * loc_hab * month_fact






## ***************************************************************************************************
## Compare models with different covar structure for a given species
## ***************************************************************************************************


#Red-bellied woodpeckers----------------------------------------------------------
loo(rewo_ext_day_add) #3859.9
loo(rewo_ext_day_hab) #3847.9 
loo(rewo_ext_vbd_hab) #3851.9
loo(rewo_ext_wbt_hab) #3851.0
loo(rewo_ext_day) #3759.1  <- for this species, the ext_day_fact:loc_hab:month_fact is best supported



