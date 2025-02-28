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
library(viridis)
library(dplyr)
theme_set(theme_bw())

# load custom helper functions
source("R code/bird_mod_functions.R")

# load temperature and soundscape data
dat <- read.csv("Data/analysis_data/temp_soundscape_bird_data_2024_summaries_1.7.25.csv") #bird detections filtered by confidence thresholds
dim(dat) #2176   154

# get breakdown of extreme heat days by location and habitat
table(dat$ext_day_fact, dat$loc_hab)

# load trait data
bdata <- read.csv("./Data/analysis_data/trait_table_birds_1.16.25.csv")
dim(bdata) #11  8
colnames(bdata)

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
dim(dat_1) #1476  159

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
        family = poisson, data  = dat_1), type="predictor")

plot(as.factor(dat_1$loc_hab), as.factor(dat_1$ext_day_fact))
plot(as.factor(dat_1$loc_hab), as.factor(dat_1$ext_day_vpd))


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


#---------------------------------------------------------------------------------
##FIRST, fit models of vocalization responses to additive effects-----------------
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

#American Goldfinch---------------------------------------------------------------
#additive model
amgo_ext_day_add <-
  bird.model("amgo_asr",
             "ext_day_fact+loc_hab+month_fact",
             dat_1,
             "12-23-24")

amgo_ext_plt <- bird_plot(amgo_ext_day_add, "amgo_asr", "ext_day_fact")
amgo_hab_plt <- bird_plot(amgo_ext_day_add, "amgo_asr", "loc_hab")
amgo_mon_plt <- bird_plot(amgo_ext_day_add, "amgo_asr", "month_fact")
amgo_ndsi_plt <- bird_plot(amgo_ext_day_add, "amgo_asr", "z.ndsi")

#2-way interaction between extreme*habitat
amgo_ext_day_hab <-
  bird.model("amgo_asr",
             "ext_day_fact*loc_hab+month_fact",
             dat_1,
             "12-23-24")

amgo_ext_hab_plt <-
  bird_plot(
    amgo_ext_day_hab,
    "amgo_asr",
    "ext_day_fact:loc_hab",
    save = TRUE,
    date = "12-23-24",
    path = "./Figures_12-20-24/asr/twoway/ext_day_fact"
  )

#2-way interaction between extreme_vpd*habitat
amgo_ext_vbd_hab <-
  bird.model("amgo_asr",
             "ext_day_vpd*loc_hab+month_fact",
             dat_1,
             "12-23-24")

amgo_extvpd_hab_plt <-
  bird_plot(
    amgo_ext_vbd_hab,
    "amgo_asr",
    "ext_day_vpd:loc_hab",
    save = TRUE,
    date = "12-23-24",
    path = "./Figures_12-20-24/asr/twoway/ext_day_vpd"
  )

#2-way interaction between extreme_wbt*habitat
amgo_ext_wbt_hab <-
  bird.model("amgo_asr",
             "ext_day_wbt*loc_hab+month_fact",
             dat_1,
             "12-23-24")

amgo_extwbt_hab_plt <-
  bird_plot(
    amgo_ext_wbt_hab,
    "amgo_asr",
    "ext_day_wbt:loc_hab",
    save = TRUE,
    date = "12-23-24",
    path = "./Figures_12-20-24/asr/twoway/ext_day_wbt"
  )

#American Robin-------------------------------------------------------------------
#additive model
amro_ext_day_add <-
  bird.model("amro_asr",
             "ext_day_fact+loc_hab+month_fact",
             dat_1,
             "12-23-24")

amro_ext_plt <- bird_plot(amro_ext_day_add, "amro_asr", "ext_day_fact")
amro_hab_plt <- bird_plot(amro_ext_day_add, "amro_asr", "loc_hab")
amro_mon_plt <- bird_plot(amro_ext_day_add, "amro_asr", "month_fact")
amro_ndsi_plt <- bird_plot(amro_ext_day_add, "amro_asr", "z.ndsi")

#2-way interaction between extreme*habitat
amro_ext_day_hab <-
  bird.model("amro_asr",
             "ext_day_fact*loc_hab+month_fact",
             dat_1,
             "12-23-24")

amro_ext_hab_plt <-
  bird_plot(
    amro_ext_day_hab,
    "amro_asr",
    "ext_day_fact:loc_hab",
    save = TRUE,
    date = "12-23-24",
    path = "./Figures_12-20-24/asr/twoway/ext_day_fact"
  )

#2-way interaction between extreme_vpd*habitat
amro_ext_vbd_hab <-
  bird.model("amro_asr",
             "ext_day_vpd*loc_hab+month_fact",
             dat_1,
             "12-23-24")

amro_extvpd_hab_plt <-
  bird_plot(
    amro_ext_vbd_hab,
    "amro_asr",
    "ext_day_vpd:loc_hab",
    save = TRUE,
    date = "12-23-24",
    path = "./Figures_12-20-24/asr/twoway/ext_day_vpd"
  )

#2-way interaction between extreme_wbt*habitat
amro_ext_wbt_hab <-
  bird.model("amro_asr",
             "ext_day_wbt*loc_hab+month_fact",
             dat_1,
             "12-23-24")

amro_extwbt_hab_plt <-
  bird_plot(
    amro_ext_wbt_hab,
    "amro_asr",
    "ext_day_wbt:loc_hab",
    save = TRUE,
    date = "12-23-24",
    path = "./Figures_12-20-24/asr/twoway/ext_day_wbt"
  )

#Blue Jay-------------------------------------------------------------------------
#additive model
blja_ext_day_add <-
  bird.model("blja_asr",
             "ext_day_fact+loc_hab+month_fact",
             dat_1,
             "12-23-24")

blja_ext_plt <- bird_plot(blja_ext_day_add, "blja_asr", "ext_day_fact")
blja_hab_plt <- bird_plot(blja_ext_day_add, "blja_asr", "loc_hab")
blja_mon_plt <- bird_plot(blja_ext_day_add, "blja_asr", "month_fact")
blja_ndsi_plt <- bird_plot(blja_ext_day_add, "blja_asr", "z.ndsi")

#2-way interaction between extreme*habitat
blja_ext_day_hab <-
  bird.model("blja_asr",
             "ext_day_fact*loc_hab+month_fact",
             dat_1,
             "12-23-24")

blja_ext_hab_plt <-
  bird_plot(
    blja_ext_day_hab,
    "blja_asr",
    "ext_day_fact:loc_hab",
    save = TRUE,
    date = "12-23-24",
    path = "./Figures_12-20-24/asr/twoway/ext_day_fact"
  )

#2-way interaction between extreme_vpd*habitat
blja_ext_vbd_hab <-
  bird.model("blja_asr",
             "ext_day_vpd*loc_hab+month_fact",
             dat_1,
             "12-23-24")

blja_extvpd_hab_plt <-
  bird_plot(
    blja_ext_vbd_hab,
    "blja_asr",
    "ext_day_vpd:loc_hab",
    save = TRUE,
    date = "12-23-24",
    path = "./Figures_12-20-24/asr/twoway/ext_day_vpd"
  )

#2-way interaction between extreme_wbt*habitat
blja_ext_wbt_hab <-
  bird.model("blja_asr",
             "ext_day_wbt*loc_hab+month_fact",
             dat_1,
             "12-23-24")

blja_extwbt_hab_plt <-
  bird_plot(
    blja_ext_wbt_hab,
    "blja_asr",
    "ext_day_wbt:loc_hab",
    save = TRUE,
    date = "12-23-24",
    path = "./Figures_12-20-24/asr/twoway/ext_day_wbt"
  )

#Carolina Chickadee---------------------------------------------------------------
#additive model
cach_ext_day_add <-
  bird.model("cach_asr",
             "ext_day_fact+loc_hab+month_fact",
             dat_1,
             "12-23-24")

cach_ext_plt <- bird_plot(cach_ext_day_add, "cach_asr", "ext_day_fact")
cach_hab_plt <- bird_plot(cach_ext_day_add, "cach_asr", "loc_hab")
cach_mon_plt <- bird_plot(cach_ext_day_add, "cach_asr", "month_fact")
cach_ndsi_plt <- bird_plot(cach_ext_day_add, "cach_asr", "z.ndsi")

#2-way interaction between extreme*habitat
cach_ext_day_hab <-
  bird.model("cach_asr",
             "ext_day_fact*loc_hab+month_fact",
             dat_1,
             "12-23-24")

cach_ext_hab_plt <-
  bird_plot(
    cach_ext_day_hab,
    "cach_asr",
    "ext_day_fact:loc_hab",
    save = TRUE,
    date = "12-23-24",
    path = "./Figures_12-20-24/asr/twoway/ext_day_fact"
  )

#2-way interaction between extreme_vpd*habitat
cach_ext_vbd_hab <-
  bird.model("cach_asr",
             "ext_day_vpd*loc_hab+month_fact",
             dat_1,
             "12-23-24")

cach_extvpd_hab_plt <-
  bird_plot(
    cach_ext_vbd_hab,
    "cach_asr",
    "ext_day_vpd:loc_hab",
    save = TRUE,
    date = "12-23-24",
    path = "./Figures_12-20-24/asr/twoway/ext_day_vpd"
  )

#2-way interaction between extreme_wbt*habitat
cach_ext_wbt_hab <-
  bird.model("cach_asr",
             "ext_day_wbt*loc_hab+month_fact",
             dat_1,
             "12-23-24")

cach_extwbt_hab_plt <-
  bird_plot(
    cach_ext_wbt_hab,
    "cach_asr",
    "ext_day_wbt:loc_hab",
    save = TRUE,
    date = "12-23-24",
    path = "./Figures_12-20-24/asr/twoway/ext_day_wbt"
  )

#Carolina Wren--------------------------------------------------------------------
#additive model
cawr_ext_day_add <-
  bird.model("cawr_asr",
             "ext_day_fact+loc_hab+month_fact",
             dat_1,
             "12-23-24")

cawr_ext_plt <- bird_plot(cawr_ext_day_add, "cawr_asr", "ext_day_fact")
cawr_hab_plt <- bird_plot(cawr_ext_day_add, "cawr_asr", "loc_hab")
cawr_mon_plt <- bird_plot(cawr_ext_day_add, "cawr_asr", "month_fact")
cawr_ndsi_plt <- bird_plot(cawr_ext_day_add, "cawr_asr", "z.ndsi")

#2-way interaction between extreme*habitat
cawr_ext_day_hab <-
  bird.model("cawr_asr",
             "ext_day_fact*loc_hab+month_fact",
             dat_1,
             "12-23-24")

cawr_ext_hab_plt <-
  bird_plot(
    cawr_ext_day_hab,
    "cawr_asr",
    "ext_day_fact:loc_hab",
    save = TRUE,
    date = "12-23-24",
    path = "./Figures_12-20-24/asr/twoway/ext_day_fact"
  )

#2-way interaction between extreme_vpd*habitat
cawr_ext_vbd_hab <-
  bird.model("cawr_asr",
             "ext_day_vpd*loc_hab+month_fact",
             dat_1,
             "12-23-24")

cawr_extvpd_hab_plt <-
  bird_plot(
    cawr_ext_vbd_hab,
    "cawr_asr",
    "ext_day_vpd:loc_hab",
    save = TRUE,
    date = "12-23-24",
    path = "./Figures_12-20-24/asr/twoway/ext_day_vpd"
  )

#2-way interaction between extreme_wbt*habitat
cawr_ext_wbt_hab <-
  bird.model("cawr_asr",
             "ext_day_wbt*loc_hab+month_fact",
             dat_1,
             "12-23-24")

cawr_extwbt_hab_plt <-
  bird_plot(
    cawr_ext_wbt_hab,
    "cawr_asr",
    "ext_day_wbt:loc_hab",
    save = TRUE,
    date = "12-23-24",
    path = "./Figures_12-20-24/asr/twoway/ext_day_wbt"
  )

#Eastern Towhee-------------------------------------------------------------------
#additive model
eato_ext_day_add <-
  bird.model("eato_asr",
             "ext_day_fact+loc_hab+month_fact",
             dat_1,
             "1-14-25")

eato_ext_plt <- bird_plot(eato_ext_day_add, "eato_asr", "ext_day_fact")
eato_hab_plt <- bird_plot(eato_ext_day_add, "eato_asr", "loc_hab")
eato_mon_plt <- bird_plot(eato_ext_day_add, "eato_asr", "month_fact")
eato_ndsi_plt <- bird_plot(eato_ext_day_add, "eato_asr", "z.ndsi")

#2-way interaction between extreme*habitat
eato_ext_day_hab <-
  bird.model("eato_asr",
             "ext_day_fact*loc_hab+month_fact",
             dat_1,
             "1-14-25")

eato_ext_hab_plt <-
  bird_plot(
    eato_ext_day_hab,
    "eato_asr",
    "ext_day_fact:loc_hab",
    save = TRUE,
    date = "1-14-25",
    path = "./Figures_12-20-24/asr/twoway/ext_day_fact"
  )

#2-way interaction between extreme_vpd*habitat
eato_ext_vbd_hab <-
  bird.model("eato_asr",
             "ext_day_vpd*loc_hab+month_fact",
             dat_1,
             "1-14-25")

eato_extvpd_hab_plt <-
  bird_plot(
    eato_ext_vbd_hab,
    "eato_asr",
    "ext_day_vpd:loc_hab",
    save = TRUE,
    date = "1-14-25",
    path = "./Figures_12-20-24/asr/twoway/ext_day_vpd"
  )

#2-way interaction between extreme_wbt*habitat
eato_ext_wbt_hab <-
  bird.model("eato_asr",
             "ext_day_wbt*loc_hab+month_fact",
             dat_1,
             "1-14-25")

eato_extwbt_hab_plt <-
  bird_plot(
    eato_ext_wbt_hab,
    "eato_asr",
    "ext_day_wbt:loc_hab",
    save = TRUE,
    date = "1-14-25",
    path = "./Figures_12-20-24/asr/twoway/ext_day_wbt"
  )

#Hairy Woodpecker-----------------------------------------------------------------
#additive model
hawo_ext_day_add <-
  bird.model("hawo_asr",
             "ext_day_fact+loc_hab+month_fact",
             dat_1,
             "1-14-25")

hawo_ext_plt <- bird_plot(hawo_ext_day_add, "hawo_asr", "ext_day_fact")
hawo_hab_plt <- bird_plot(hawo_ext_day_add, "hawo_asr", "loc_hab")
hawo_mon_plt <- bird_plot(hawo_ext_day_add, "hawo_asr", "month_fact")
hawo_ndsi_plt <- bird_plot(hawo_ext_day_add, "hawo_asr", "z.ndsi")

#2-way interaction between extreme*habitat
hawo_ext_day_hab <-
  bird.model("hawo_asr",
             "ext_day_fact*loc_hab+month_fact",
             dat_1,
             "1-14-25")

hawo_ext_hab_plt <-
  bird_plot(
    hawo_ext_day_hab,
    "hawo_asr",
    "ext_day_fact:loc_hab",
    save = TRUE,
    date = "1-14-25",
    path = "./Figures_12-20-24/asr/twoway/ext_day_fact"
  )

#2-way interaction between extreme_vpd*habitat
hawo_ext_vbd_hab <-
  bird.model("hawo_asr",
             "ext_day_vpd*loc_hab+month_fact",
             dat_1,
             "1-14-25")

hawo_extvpd_hab_plt <-
  bird_plot(
    hawo_ext_vbd_hab,
    "hawo_asr",
    "ext_day_vpd:loc_hab",
    save = TRUE,
    date = "1-14-25",
    path = "./Figures_12-20-24/asr/twoway/ext_day_vpd"
  )

#2-way interaction between extreme_wbt*habitat
hawo_ext_wbt_hab <-
  bird.model("hawo_asr",
             "ext_day_wbt*loc_hab+month_fact",
             dat_1,
             "1-14-25")

hawo_extwbt_hab_plt <-
  bird_plot(
    hawo_ext_wbt_hab,
    "hawo_asr",
    "ext_day_wbt:loc_hab",
    save = TRUE,
    date = "1-14-25",
    path = "./Figures_12-20-24/asr/twoway/ext_day_wbt"
  )

#Northern Cardinal----------------------------------------------------------------
#additive model
noca_ext_day_add <-
  bird.model("noca_asr",
             "ext_day_fact+loc_hab+month_fact",
             dat_1,
             "12-23-24")

noca_ext_plt <- bird_plot(noca_ext_day_add, "noca_asr", "ext_day_fact")
noca_hab_plt <- bird_plot(noca_ext_day_add, "noca_asr", "loc_hab")
noca_mon_plt <- bird_plot(noca_ext_day_add, "noca_asr", "month_fact")
noca_ndsi_plt <- bird_plot(noca_ext_day_add, "noca_asr", "z.ndsi")

#2-way interaction between extreme*habitat
noca_ext_day_hab <-
  bird.model("noca_asr",
             "ext_day_fact*loc_hab+month_fact",
             dat_1,
             "12-23-24")

noca_ext_hab_plt <-
  bird_plot(
    noca_ext_day_hab,
    "noca_asr",
    "ext_day_fact:loc_hab",
    save = TRUE,
    date = "12-23-24",
    path = "./Figures_12-20-24/asr/twoway/ext_day_fact"
  )

#2-way interaction between extreme_vpd*habitat
noca_ext_vbd_hab <-
  bird.model("noca_asr",
             "ext_day_vpd*loc_hab+month_fact",
             dat_1,
             "12-23-24")

noca_extvpd_hab_plt <-
  bird_plot(
    noca_ext_vbd_hab,
    "noca_asr",
    "ext_day_vpd:loc_hab",
    save = TRUE,
    date = "12-23-24",
    path = "./Figures_12-20-24/asr/twoway/ext_day_vpd"
  )

#2-way interaction between extreme_wbt*habitat
noca_ext_wbt_hab <-
  bird.model("noca_asr",
             "ext_day_wbt*loc_hab+month_fact",
             dat_1,
             "12-23-24")

noca_extwbt_hab_plt <-
  bird_plot(
    noca_ext_wbt_hab,
    "noca_asr",
    "ext_day_wbt:loc_hab",
    save = TRUE,
    date = "12-23-24",
    path = "./Figures_12-20-24/asr/twoway/ext_day_wbt"
  )

#Red-bellied Woodpecker-----------------------------------------------------------
#additive model
rewo_ext_day_add <-
  bird.model("rewo_asr",
             "ext_day_fact+loc_hab+month_fact",
             dat_1,
             "12-20-24")

rewo_ext_plt <- bird_plot(rewo_ext_day_add, "rewo_asr", "ext_day_fact")
rewo_hab_plt <- bird_plot(rewo_ext_day_add, "rewo_asr", "loc_hab")
rewo_mon_plt <- bird_plot(rewo_ext_day_add, "rewo_asr", "month_fact")
rewo_ndsi_plt <- bird_plot(rewo_ext_day_add, "rewo_asr", "z.ndsi")

#2-way interaction between extreme*habitat
rewo_ext_day_hab <-
  bird.model("rewo_asr",
             "ext_day_fact*loc_hab+month_fact",
             dat_1,
             "12-20-24")

rewo_ext_hab_plt <-
  bird_plot(
    rewo_ext_day_hab,
    "rewo_asr",
    "ext_day_fact:loc_hab",
    save = TRUE,
    date = "12-20-24",
    path = "./Figures_12-20-24/asr/twoway/ext_day_fact"
  )

#2-way interaction between extreme_vpd*habitat
rewo_ext_vbd_hab <-
  bird.model("rewo_asr",
             "ext_day_vpd*loc_hab+month_fact",
             dat_1,
             "12-20-24")

rewo_extvpd_hab_plt <-
  bird_plot(
    rewo_ext_vbd_hab,
    "rewo_asr",
    "ext_day_vpd:loc_hab",
    save = TRUE,
    date = "12-20-24",
    path = "./Figures_12-20-24/asr/twoway/ext_day_vpd"
  )

#2-way interaction between extreme_wbt*habitat
rewo_ext_wbt_hab <-
  bird.model("rewo_asr",
             "ext_day_wbt*loc_hab+month_fact",
             dat_1,
             "12-20-24")

rewo_extwbt_hab_plt <-
  bird_plot(
    rewo_ext_wbt_hab,
    "rewo_asr",
    "ext_day_wbt:loc_hab",
    save = TRUE,
    date = "12-20-24",
    path = "./Figures_12-20-24/asr/twoway/ext_day_wbt"
  )


#Tufted Titmouse------------------------------------------------------------------
#additive model
tuti_ext_day_add <-
  bird.model("tuti_asr",
             "ext_day_fact+loc_hab+month_fact",
             dat_1,
             "12-27-24")

tuti_ext_plt <- bird_plot(tuti_ext_day_add, "tuti_asr", "ext_day_fact")
tuti_hab_plt <- bird_plot(tuti_ext_day_add, "tuti_asr", "loc_hab")
tuti_mon_plt <- bird_plot(tuti_ext_day_add, "tuti_asr", "month_fact")
tuti_ndsi_plt <- bird_plot(tuti_ext_day_add, "tuti_asr", "z.ndsi")

#2-way interaction between extreme*habitat
tuti_ext_day_hab <-
  bird.model("tuti_asr",
             "ext_day_fact*loc_hab+month_fact",
             dat_1,
             "12-27-24")

tuti_ext_hab_plt <-
  bird_plot(
    tuti_ext_day_hab,
    "tuti_asr",
    "ext_day_fact:loc_hab",
    save = TRUE,
    date = "12-27-24",
    path = "./Figures_12-20-24/asr/twoway/ext_day_fact"
  )

#2-way interaction between extreme_vpd*habitat
tuti_ext_vbd_hab <-
  bird.model("tuti_asr",
             "ext_day_vpd*loc_hab+month_fact",
             dat_1,
             "12-27-24")

tuti_extvpd_hab_plt <-
  bird_plot(
    tuti_ext_vbd_hab,
    "tuti_asr",
    "ext_day_vpd:loc_hab",
    save = TRUE,
    date = "12-27-24",
    path = "./Figures_12-20-24/asr/twoway/ext_day_vpd"
  )

#2-way interaction between extreme_wbt*habitat
tuti_ext_wbt_hab <-
  bird.model("tuti_asr",
             "ext_day_wbt*loc_hab+month_fact",
             dat_1,
             "12-27-24")

tuti_extwbt_hab_plt <-
  bird_plot(
    tuti_ext_wbt_hab,
    "tuti_asr",
    "ext_day_wbt:loc_hab",
    save = TRUE,
    date = "12-27-24",
    path = "./Figures_12-20-24/asr/twoway/ext_day_wbt"
  )

#Wood Thrush----------------------------------------------------------------------
#additive model
woth_ext_day_add <-
  bird.model("woth_asr",
             "ext_day_fact+loc_hab+month_fact",
             dat_1,
             "1-14-25")

woth_ext_plt <- bird_plot(woth_ext_day_add, "woth_asr", "ext_day_fact")
woth_hab_plt <- bird_plot(woth_ext_day_add2, "woth_asr", "loc_hab")
woth_mon_plt <- bird_plot(woth_ext_day_add, "woth_asr", "month_fact")
woth_ndsi_plt <- bird_plot(woth_ext_day_add, "woth_asr", "z.ndsi")

#2-way interaction between extreme*habitat
woth_ext_day_hab <-
  bird.model("woth_asr",
             "ext_day_fact*loc_hab+month_fact",
             dat_1,
             "1-14-25")

woth_ext_hab_plt <-
  bird_plot(
    woth_ext_day_hab,
    "woth_asr",
    "ext_day_fact:loc_hab",
    save = TRUE,
    date = "1-14-25",
    path = "./Figures_12-20-24/asr/twoway/ext_day_fact"
  )

#2-way interaction between extreme_vpd*habitat
woth_ext_vbd_hab <-
  bird.model("woth_asr",
             "ext_day_vpd*loc_hab+month_fact",
             dat_1,
             "1-14-25")

woth_extvpd_hab_plt <-
  bird_plot(
    woth_ext_vbd_hab,
    "woth_asr",
    "ext_day_vpd:loc_hab",
    save = TRUE,
    date = "1-14-25",
    path = "./Figures_12-20-24/asr/twoway/ext_day_vpd"
  )

#2-way interaction between extreme_wbt*habitat
woth_ext_wbt_hab <-
  bird.model("woth_asr",
             "ext_day_wbt*loc_hab+month_fact",
             dat_1,
             "1-14-25")

woth_extwbt_hab_plt <-
  bird_plot(
    woth_ext_wbt_hab,
    "woth_asr",
    "ext_day_wbt:loc_hab",
    save = TRUE,
    date = "1-14-25",
    path = "./Figures_12-20-24/asr/twoway/ext_day_wbt"
  )

#---------------------------------------------------------------------------------
##SECOND, fit models of vocalization responses to 3-WAY interaction of------------
#Main interactive effects = ext_day_fact * loc_hab * month_fact

#For each species:
#(1) Fit model
#(2) Get estimates
#(3) Make and save plots (in organized subfolders)

#American Goldfinch--------------------------------------------------------------
#Fit model
amgo_ext_day <-
  bird.model("amgo_asr",
             "ext_day_fact*loc_hab*month_fact",
             dat_1,
             "2-27-25")
#get estimates
amgo_est_ext <- ext_day.3int.est(amgo_ext_day, "ext_day_fact")
#Make plot
amgo_ext_plt <-
  bird_plot_3way(
    amgo_est_ext[[1]],
    "amgo_asr",
    "ext_day_fact",
    save = TRUE,
    "2-27-25",
    path = "./Figures_12-20-24/asr/threeway/ext_day_fact"
  )

#View contrasts
amgo_ext_plt
amgo_est_ext$contrasts

#American Robin-------------------------------------------------------------------
#Fit model
amro_ext_day <-
  bird.model("amro_asr",
             "ext_day_fact*loc_hab*month_fact",
             dat_1,
             "12-23-24")
#get estimates
amro_est_ext <- ext_day.3int.est(amro_ext_day, "ext_day_fact")
#Make plot
amro_ext_plt <-
  bird_plot_3way(
    amro_est_ext[[1]],
    "amro_asr",
    "ext_day_fact",
    save = TRUE,
    "12-23-24",
    path = "./Figures_12-20-24/asr/threeway/ext_day_fact"
  )

#View contrasts
amro_ext_plt
amro_est_ext$contrasts

#Blue Jay-------------------------------------------------------------------------
#Fit model
blja_ext_day <-
  bird.model("blja_asr",
             "ext_day_fact*loc_hab*month_fact",
             dat_1,
             "12-23-24")
#get estimates
blja_est_ext <- ext_day.3int.est(blja_ext_day, "ext_day_fact")
#Make plot
blja_ext_plt <-
  bird_plot_3way(
    blja_est_ext[[1]],
    "blja_asr",
    "ext_day_fact",
    save = TRUE,
    "12-23-24",
    path = "./Figures_12-20-24/asr/threeway/ext_day_fact"
  )

#View contrasts
blja_ext_plt
blja_est_ext$contrasts

#Carolina Chickadee---------------------------------------------------------------
#Fit model
cach_ext_day <-
  bird.model("cach_asr",
             "ext_day_fact*loc_hab*month_fact",
             dat_1,
             "12-23-24")
#get estimates
cach_est_ext <- ext_day.3int.est(cach_ext_day, "ext_day_fact")
#Make plot
cach_ext_plt <-
  bird_plot_3way(
    cach_est_ext[[1]],
    "cach_asr",
    "ext_day_fact",
    save = TRUE,
    "12-23-24",
    path = "./Figures_12-20-24/asr/threeway/ext_day_fact"
  )

#View contrasts
cach_ext_plt
cach_est_ext$contrasts

#Carolina Wren--------------------------------------------------------------------
#Fit model
cawr_ext_day <-
  bird.model("cawr_asr",
             "ext_day_fact*loc_hab*month_fact",
             dat_1,
             "12-23-24")
#get estimates
cawr_est_ext <- ext_day.3int.est(cawr_ext_day, "ext_day_fact")
#Make plot
cawr_ext_plt <-
  bird_plot_3way(
    cawr_est_ext[[1]],
    "cawr_asr",
    "ext_day_fact",
    save = TRUE,
    "12-23-24",
    path = "./Figures_12-20-24/asr/threeway/ext_day_fact"
  )

#View contrasts
cawr_ext_plt
cawr_est_ext$contrasts

#Eastern Towhee-------------------------------------------------------------------
#Fit model
eato_ext_day <-
  bird.model("eato_asr",
             "ext_day_fact*loc_hab*month_fact",
             dat_1,
             "1-14-25")
#get estimates
eato_est_ext <- ext_day.3int.est(eato_ext_day, "ext_day_fact")
#Make plot
eato_ext_plt <-
  bird_plot_3way(
    eato_est_ext[[1]],
    "eato_asr",
    "ext_day_fact",
    save = TRUE,
    "1-14-25",
    path = "./Figures_12-20-24/asr/threeway/ext_day_fact"
  )

#View contrasts
eato_ext_plt
eato_est_ext$contrasts

#Hairy Woodpecker-----------------------------------------------------------------
#Fit model
hawo_ext_day <-
  bird.model("hawo_asr",
             "ext_day_fact*loc_hab*month_fact",
             dat_1,
             "1-14-25")
#get estimates
hawo_est_ext <- ext_day.3int.est(hawo_ext_day, "ext_day_fact")
#Make plot
hawo_ext_plt <-
  bird_plot_3way(
    hawo_est_ext[[1]],
    "hawo_asr",
    "ext_day_fact",
    save = TRUE,
    "1-14-25",
    path = "./Figures_12-20-24/asr/threeway/ext_day_fact"
  )

#View contrasts
hawo_ext_plt
hawo_est_ext$contrasts

#Northern Cardinal🐦--------------------------------------------------------------
#Fit model
noca_ext_day <-
  bird.model("noca_asr",
             "ext_day_fact*loc_hab*month_fact",
             dat_1,
             "12-23-24")
#get estimates
noca_est_ext <- ext_day.3int.est(noca_ext_day, "ext_day_fact")
#Make plot
noca_ext_plt <-
  bird_plot_3way(
    noca_est_ext[[1]],
    "noca_asr",
    "ext_day_fact",
    save = TRUE,
    "12-23-24",
    path = "./Figures_12-20-24/asr/threeway/ext_day_fact"
  )

#View contrasts
noca_ext_plt
noca_est_ext$contrasts

#Red-bellied Woodpecker-----------------------------------------------------------
#Fit model
rewo_ext_day <-
  bird.model("rewo_asr",
             "ext_day_fact*loc_hab*month_fact",
             dat_1,
             "12-20-24")

#get estimates
rewo_est_ext <- ext_day.3int.est(rewo_ext_day, "ext_day_fact")
#Make plot
rewo_ext_plt <-
  bird_plot_3way(
    rewo_est_ext[[1]],
    "rewo_asr",
    "ext_day_fact",
    save = TRUE,
    "12-20-24",
    path = "./Figures_12-20-24/asr/threeway/ext_day_fact"
  )
#View contrasts
rewo_ext_plt
rewo_est_ext$contrasts

#Tufted Titmouse------------------------------------------------------------------
#Fit model
tuti_ext_day <-
  bird.model("tuti_asr",
             "ext_day_fact*loc_hab*month_fact",
             dat_1,
             "12-27-24")
#get estimates
tuti_est_ext <- ext_day.3int.est(tuti_ext_day, "ext_day_fact")
#Make plot
tuti_ext_plt <-
  bird_plot_3way(
    tuti_est_ext[[1]],
    "tuti_asr",
    "ext_day_fact",
    save = TRUE,
    "12-27-24",
    path = "./Figures_12-20-24/asr/threeway/ext_day_fact"
  )

#View contrasts
tuti_ext_plt
tuti_est_ext$contrasts

#Wood Thrush----------------------------------------------------------------------
#Fit model
woth_ext_day <-
  bird.model("woth_asr",
             "ext_day_fact*loc_hab*month_fact",
             dat_1,
             "1-14-25")
#get estimates
woth_est_ext <- ext_day.3int.est(woth_ext_day, "ext_day_fact")
#Make plot
woth_ext_plt <-
  bird_plot_3way(
    woth_est_ext[[1]],
    "woth_asr",
    "ext_day_fact",
    save = TRUE,
    "1-14-25",
    path = "./Figures_12-20-24/asr/threeway/ext_day_fact"
  )

#View contrasts
woth_ext_plt
woth_est_ext$contrasts

#---------------------------------------------------------------------------------
#THIRD, fit models of vocalization responses to 3-WAY interaction of---------------
#Main interactive effects = ext_day_vpd * loc_hab * month_fact

#American Goldfinch---------------------------------------------------------------
amgo_ext_vpd <-
  bird.model("amgo_asr",
             "ext_day_vpd*loc_hab*month_fact",
             dat_1,
             "12-23-24")
#Get estimates
amgo_est_extvpd <- ext_day.3int.est(amgo_ext_vpd , "ext_day_vpd")
#Plot
amgo_extvpd_plt <-
  bird_plot_3way(
    amgo_est_extvpd[[1]],
    "amgo_asr",
    "ext_day_vpd",
    save = TRUE,
    "12-23-24",
    path = "./Figures_12-20-24/asr/threeway/ext_day_vpd"
  )
#View contrasts
amgo_extvpd_plt
amgo_est_extvpd$contrasts

#American Robin-------------------------------------------------------------------
amro_ext_vpd <-
  bird.model("amro_asr",
             "ext_day_vpd*loc_hab*month_fact",
             dat_1,
             "12-23-24")
#Get estimates
amro_est_extvpd <- ext_day.3int.est(amro_ext_vpd , "ext_day_vpd")
#Plot
amro_extvpd_plt <-
  bird_plot_3way(
    amro_est_extvpd[[1]],
    "amro_asr",
    "ext_day_vpd",
    save = TRUE,
    "12-23-24",
    path = "./Figures_12-20-24/asr/threeway/ext_day_vpd"
  )
#View contrasts
amro_extvpd_plt
amro_est_extvpd$contrasts

#Blue Jay-------------------------------------------------------------------------
blja_ext_vpd <-
  bird.model("blja_asr",
             "ext_day_vpd*loc_hab*month_fact",
             dat_1,
             "12-23-24")
#Get estimates
blja_est_extvpd <- ext_day.3int.est(blja_ext_vpd , "ext_day_vpd")
#Plot
blja_extvpd_plt <-
  bird_plot_3way(
    blja_est_extvpd[[1]],
    "blja_asr",
    "ext_day_vpd",
    save = TRUE,
    "12-23-24",
    path = "./Figures_12-20-24/asr/threeway/ext_day_vpd"
  )
#View contrasts
blja_extvpd_plt
blja_est_extvpd$contrasts

#Carolina Chickadee---------------------------------------------------------------
cach_ext_vpd <-
  bird.model("cach_asr",
             "ext_day_vpd*loc_hab*month_fact",
             dat_1,
             "12-23-24")
#Get estimates
cach_est_extvpd <- ext_day.3int.est(cach_ext_vpd , "ext_day_vpd")
#Plot
cach_extvpd_plt <-
  bird_plot_3way(
    cach_est_extvpd[[1]],
    "cach_asr",
    "ext_day_vpd",
    save = TRUE,
    "12-23-24",
    path = "./Figures_12-20-24/asr/threeway/ext_day_vpd"
  )
#View contrasts
cach_extvpd_plt
cach_est_extvpd$contrasts

#Carolina Wren--------------------------------------------------------------------
cawr_ext_vpd <-
  bird.model("cawr_asr",
             "ext_day_vpd*loc_hab*month_fact",
             dat_1,
             "12-23-24")
#Get estimates
cawr_est_extvpd <- ext_day.3int.est(cawr_ext_vpd , "ext_day_vpd")
#Plot
cawr_extvpd_plt <-
  bird_plot_3way(
    cawr_est_extvpd[[1]],
    "cawr_asr",
    "ext_day_vpd",
    save = TRUE,
    "12-23-24",
    path = "./Figures_12-20-24/asr/threeway/ext_day_vpd"
  )
#View contrasts
cawr_extvpd_plt
cawr_est_extvpd$contrasts

#Eastern Towhee-------------------------------------------------------------------
eato_ext_vpd <-
  bird.model("eato_asr",
             "ext_day_vpd*loc_hab*month_fact",
             dat_1,
             "1-14-25")
#Get estimates
eato_est_extvpd <- ext_day.3int.est(eato_ext_vpd , "ext_day_vpd")
#Plot
eato_extvpd_plt <-
  bird_plot_3way(
    eato_est_extvpd[[1]],
    "eato_asr",
    "ext_day_vpd",
    save = TRUE,
    "1-14-25",
    path = "./Figures_12-20-24/asr/threeway/ext_day_vpd"
  )
#View contrasts
eato_extvpd_plt
eato_est_extvpd$contrasts

#Hairy Woodpecker-----------------------------------------------------------------
hawo_ext_vpd <-
  bird.model("hawo_asr",
             "ext_day_vpd*loc_hab*month_fact",
             dat_1,
             "1-14-25")
#Get estimates
hawo_est_extvpd <- ext_day.3int.est(hawo_ext_vpd , "ext_day_vpd")
#Plot
hawo_extvpd_plt <-
  bird_plot_3way(
    hawo_est_extvpd[[1]],
    "hawo_asr",
    "ext_day_vpd",
    save = TRUE,
    "1-14-25",
    path = "./Figures_12-20-24/asr/threeway/ext_day_vpd"
  )
#View contrasts
hawo_extvpd_plt
hawo_est_extvpd$contrasts

#Northern Cardinal🐦--------------------------------------------------------------
noca_ext_vpd <-
  bird.model("noca_asr",
             "ext_day_vpd*loc_hab*month_fact",
             dat_1,
             "12-23-24")
#Get estimates
noca_est_extvpd <- ext_day.3int.est(noca_ext_vpd , "ext_day_vpd")
#Plot
noca_extvpd_plt <-
  bird_plot_3way(
    noca_est_extvpd[[1]],
    "noca_asr",
    "ext_day_vpd",
    save = TRUE,
    "12-23-24",
    path = "./Figures_12-20-24/asr/threeway/ext_day_vpd"
  )
#View contrasts
noca_extvpd_plt
noca_est_extvpd$contrasts

#Red-bellied Woodpecker-----------------------------------------------------------
#Fit model
rewo_ext_vpd <-
  bird.model("rewo_asr",
             "ext_day_vpd*loc_hab*month_fact",
             dat_1,
             "12-20-24")
#Get estimates
rewo_est_extvpd <- ext_day.3int.est(rewo_ext_vpd , "ext_day_vpd")
#Plot
rewo_extvpd_plt <-
  bird_plot_3way(
    rewo_est_extvpd[[1]],
    "rewo_asr",
    "ext_day_vpd",
    save = TRUE,
    "12-20-24",
    path = "./Figures_12-20-24/asr/threeway/ext_day_vpd"
  )

# png("./Figures_12-4-24/asr/three-way interactions/ext_day_vpd/ext_day_vpd_hab_month_rewo_asr_yadj_12-11-24.png", width = 8.8, height = 6.0, units = 'in', res = 600)
# rewo_extvpd_plt + ylim(0, 20)
# dev.off()

#View contrasts
rewo_extvpd_plt
rewo_est_extvpd$contrasts

#Tufted Titmouse------------------------------------------------------------------
tuti_ext_vpd <-
  bird.model("tuti_asr",
             "ext_day_vpd*loc_hab*month_fact",
             dat_1,
             "12-27-24")
#Get estimates
tuti_est_extvpd <- ext_day.3int.est(tuti_ext_vpd , "ext_day_vpd")
#Plot
tuti_extvpd_plt <-
  bird_plot_3way(
    tuti_est_extvpd[[1]],
    "tuti_asr",
    "ext_day_vpd",
    save = TRUE,
    "12-27-24",
    path = "./Figures_12-20-24/asr/threeway/ext_day_vpd"
  )
#View contrasts
tuti_extvpd_plt
tuti_est_extvpd$contrasts

#Wood Thrush----------------------------------------------------------------------
woth_ext_vpd <-
  bird.model("woth_asr",
             "ext_day_vpd*loc_hab*month_fact",
             dat_1,
             "1-14-25")
#Get estimates
woth_est_extvpd <- ext_day.3int.est(woth_ext_vpd , "ext_day_vpd")
#Plot
woth_extvpd_plt <-
  bird_plot_3way(
    woth_est_extvpd[[1]],
    "woth_asr",
    "ext_day_vpd",
    save = TRUE,
    "1-14-25",
    path = "./Figures_12-20-24/asr/threeway/ext_day_vpd"
  )
#View contrasts
woth_extvpd_plt
woth_est_extvpd$contrasts

#Look at the actual data underlying estimates - does it make sense?---------------
#Estimates with very large CIs may reflect categories with few or no observations!
#Need to interpret carefully
table(dat_1$month_fact, dat_1$loc_hab, dat_1$ext_day_fact)
dat_1[which(dat_1$month_fact == "6" & dat_1$loc_hab == "urban open" & dat_1$ext_day_fact == "not extreme"), "cach_asr"]
dat_1[which(dat_1$month_fact == "6" & dat_1$loc_hab == "urban open" & dat_1$ext_day_fact == "extreme"), "cach_asr"]
#No obs for urbanforest_ext_5_cnt and exurbanopen_ext_5_cnt categories



#---------------------------------------------------------------------------------
#FOURTH, Fit models of vocalization responses to 3-WAY interaction of--------------
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

#American Goldfinch---------------------------------------------------------------
amgo_ext_wbt <-
  bird.model("amgo_asr",
             "ext_day_wbt*loc_hab*month_fact",
             dat_1,
             "12-23-24")
#Get estimates
amgo_est_extwbt <- ext_day.3int.est(amgo_ext_wbt , "ext_day_wbt")
#Plot
amgo_extwbt_plt <-
  bird_plot_3way(
    amgo_est_extwbt[[1]],
    "amgo_asr",
    "ext_day_wbt",
    save = TRUE,
    "12-23-24",
    path = "./Figures_12-20-24/asr/threeway/ext_day_wbt"
  )
#View contrasts
amgo_extwbt_plt
amgo_est_extwbt$contrasts

#American Robin-------------------------------------------------------------------
amro_ext_wbt <-
  bird.model("amro_asr",
             "ext_day_wbt*loc_hab*month_fact",
             dat_1,
             "12-23-24")
#Get estimates
amro_est_extwbt <- ext_day.3int.est(amro_ext_wbt , "ext_day_wbt")
#Plot
amro_extwbt_plt <-
  bird_plot_3way(
    amro_est_extwbt[[1]],
    "amro_asr",
    "ext_day_wbt",
    save = TRUE,
    "12-23-24",
    path = "./Figures_12-20-24/asr/threeway/ext_day_wbt"
  )
#View contrasts
amro_extwbt_plt
amro_est_extwbt$contrasts

#Blue Jay-------------------------------------------------------------------------
blja_ext_wbt <-
  bird.model("blja_asr",
             "ext_day_wbt*loc_hab*month_fact",
             dat_1,
             "12-23-24")
#Get estimates
blja_est_extwbt <- ext_day.3int.est(blja_ext_wbt , "ext_day_wbt")
#Plot
blja_extwbt_plt <-
  bird_plot_3way(
    blja_est_extwbt[[1]],
    "blja_asr",
    "ext_day_wbt",
    save = TRUE,
    "12-23-24",
    path = "./Figures_12-20-24/asr/threeway/ext_day_wbt"
  )
#View contrasts
blja_extwbt_plt
blja_est_extwbt$contrasts

#Carolina Chickadee---------------------------------------------------------------
cach_ext_wbt <-
  bird.model("cach_asr",
             "ext_day_wbt*loc_hab*month_fact",
             dat_1,
             "12-23-24")
#Get estimates
cach_est_extwbt <- ext_day.3int.est(cach_ext_wbt , "ext_day_wbt")
#Plot
cach_extwbt_plt <-
  bird_plot_3way(
    cach_est_extwbt[[1]],
    "cach_asr",
    "ext_day_wbt",
    save = TRUE,
    "12-23-24",
    path = "./Figures_12-20-24/asr/threeway/ext_day_wbt"
  )
#View contrasts
cach_extwbt_plt
cach_est_extwbt$contrasts

#Carolina Wren--------------------------------------------------------------------
cawr_ext_wbt <-
  bird.model("cawr_asr",
             "ext_day_wbt*loc_hab*month_fact",
             dat_1,
             "12-23-24")
#Get estimates
cawr_est_extwbt <- ext_day.3int.est(cawr_ext_wbt , "ext_day_wbt")
#Plot
cawr_extwbt_plt <-
  bird_plot_3way(
    cawr_est_extwbt[[1]],
    "cawr_asr",
    "ext_day_wbt",
    save = TRUE,
    "12-23-24",
    path = "./Figures_12-20-24/asr/threeway/ext_day_wbt"
  )
#View contrasts
cawr_extwbt_plt
cawr_est_extwbt$contrasts

#Eastern Towhee-------------------------------------------------------------------
eato_ext_wbt <-
  bird.model("eato_asr",
             "ext_day_wbt*loc_hab*month_fact",
             dat_1,
             "1-14-25")
#Get estimates
eato_est_extwbt <- ext_day.3int.est(eato_ext_wbt , "ext_day_wbt")
#Plot
eato_extwbt_plt <-
  bird_plot_3way(
    eato_est_extwbt[[1]],
    "eato_asr",
    "ext_day_wbt",
    save = TRUE,
    "1-14-25",
    path = "./Figures_12-20-24/asr/threeway/ext_day_wbt"
  )
#View contrasts
eato_extwbt_plt
eato_est_extwbt$contrasts

#Northern Cardinal🐦----------------------------------------------------------------------uo
noca_ext_wbt <-
  bird.model("noca_asr",
             "ext_day_wbt*loc_hab*month_fact",
             dat_1,
             "12-23-24")
#Get estimates
noca_est_extwbt <- ext_day.3int.est(noca_ext_wbt , "ext_day_wbt")
#Plot
noca_extwbt_plt <-
  bird_plot_3way(
    noca_est_extwbt[[1]],
    "noca_asr",
    "ext_day_wbt",
    save = TRUE,
    "12-23-24",
    path = "./Figures_12-20-24/asr/threeway/ext_day_wbt"
  )
#View contrasts
noca_extwbt_plt
noca_est_extwbt$contrasts

#Hairy Woodpecker-----------------------------------------------------------------
hawo_ext_wbt <-
  bird.model("hawo_asr",
             "ext_day_wbt*loc_hab*month_fact",
             dat_1,
             "1-14-25")
#Get estimates
hawo_est_extwbt <- ext_day.3int.est(hawo_ext_wbt , "ext_day_wbt")
#Plot
hawo_extwbt_plt <-
  bird_plot_3way(
    hawo_est_extwbt[[1]],
    "hawo_asr",
    "ext_day_wbt",
    save = TRUE,
    "1-14-25",
    path = "./Figures_12-20-24/asr/threeway/ext_day_wbt"
  )
#View contrasts
hawo_extwbt_plt
hawo_est_extwbt$contrasts

#Northern Cardinal🐦--------------------------------------------------------------
noca_ext_wbt <-
  bird.model("noca_asr",
             "ext_day_wbt*loc_hab*month_fact",
             dat_1,
             "12-23-24")
#Get estimates
noca_est_extwbt <- ext_day.3int.est(noca_ext_wbt , "ext_day_wbt")
#Plot
noca_extwbt_plt <-
  bird_plot_3way(
    noca_est_extwbt[[1]],
    "noca_asr",
    "ext_day_wbt",
    save = TRUE,
    "12-23-24",
    path = "./Figures_12-20-24/asr/threeway/ext_day_wbt"
  )
#View contrasts
noca_extwbt_plt
noca_est_extwbt$contrasts

#Red-bellied Woodpecker-----------------------------------------------------------
#Fit model
rewo_ext_wbt <-
  bird.model("rewo_asr",
             "ext_day_wbt*loc_hab*month_fact",
             dat_1,
             "12-23-24")
#Get estimates
rewo_est_extwbt <- ext_day.3int.est(rewo_ext_wbt, "ext_day_wbt")
#Plot
rewo_extwbt_plt <-
  bird_plot_3way(
    rewo_est_extwbt[[1]],
    "rewo_asr",
    "ext_day_wbt",
    save = TRUE,
    "12-23-24",
    path = "./Figures_12-20-24/asr/threeway/ext_day_wbt"
  )

#View contrasts
rewo_ext_plt
rewo_est_extwbt$contrasts

#Tufted Titmouse------------------------------------------------------------------
tuti_ext_wbt <-
  bird.model("tuti_asr",
             "ext_day_wbt*loc_hab*month_fact",
             dat_1,
             "12-27-24")
#Get estimates
tuti_est_extwbt <- ext_day.3int.est(tuti_ext_wbt , "ext_day_wbt")
#Plot
tuti_extwbt_plt <-
  bird_plot_3way(
    tuti_est_extwbt[[1]],
    "tuti_asr",
    "ext_day_wbt",
    save = TRUE,
    "12-27-24",
    path = "./Figures_12-20-24/asr/threeway/ext_day_wbt"
  )
#View contrasts
tuti_extwbt_plt
tuti_est_extwbt$contrasts

#Wood Thrush----------------------------------------------------------------------
woth_ext_wbt <-
  bird.model("woth_asr",
             "ext_day_wbt*loc_hab*month_fact",
             dat_1,
             "1-14-25")
#Get estimates
woth_est_extwbt <- ext_day.3int.est(woth_ext_wbt , "ext_day_wbt")
#Plot
woth_extwbt_plt <-
  bird_plot_3way(
    woth_est_extwbt[[1]],
    "woth_asr",
    "ext_day_wbt",
    save = TRUE,
    "1-14-25",
    path = "./Figures_12-20-24/asr/threeway/ext_day_wbt"
  )
#View contrasts
woth_extwbt_plt
woth_est_extwbt$contrasts

#Look at the actual data underlying estimates - does it make sense?---------------
table(dat_1$month_fact, dat_1$loc_hab, dat_1$ext_day_fact)

dat_1[which(dat_1$month_fact == "6" & dat_1$loc_hab == "urban open" & dat_1$ext_day_fact == "not extreme"), "cach_asr"]
dat_1[which(dat_1$month_fact == "6" & dat_1$loc_hab == "urban open" & dat_1$ext_day_fact == "extreme"), "cach_asr"]
#No obs for urbanforest_ext_5_cnt and exurbanopen_ext_5_cnt categories



## ***************************************************************************************************
## Compare models with different covar structure for a given species
## ***************************************************************************************************


##Red-bellied woodpeckers---------------------------------------------------------
loo(rewo_ext_day_add2) #3858.6
loo(rewo_ext_day_hab) #3847.9 
loo(rewo_ext_vbd_hab) #3851.9
loo(rewo_ext_wbt_hab) #3851.0
loo(rewo_ext_day) #3759.1  <- for this species, the ext_day_fact:loc_hab:month_fact is best supported



#---------------------------------------------------------------------------------
#ASR HEATMAPS & MODEL AVERAGING---------------------------------------------------
##Two-way Heatmap Prep------------------------------------------------------------

files <- list.files(path = "C:/Users/kirchgrabera/Smithsonian Dropbox/Aidan Kirchgraber/Science and Faith/Aidan/birdsong_heat_project_Aidan/saved_models/asr/twoway/fact", 
                    pattern = "\\.rds$",
                    full.names = TRUE)

for (file in files) {
  oldname <- tools::file_path_sans_ext(basename(file))
  newname <- paste0(substr(oldname, 1, 4), "_ext_day_hab")
  assign(newname, readRDS(file)$mod)
}
rm(file, files, newname, oldname)

#get contrasts again
amgo_asr_2way_est <- ext_day.2int.est(amgo_ext_day_hab, "ext_day_fact")
amro_asr_2way_est <- ext_day.2int.est(amro_ext_day_hab, "ext_day_fact")
blja_asr_2way_est <- ext_day.2int.est(blja_ext_day_hab, "ext_day_fact")
cach_asr_2way_est <- ext_day.2int.est(cach_ext_day_hab, "ext_day_fact")
cawr_asr_2way_est <- ext_day.2int.est(cawr_ext_day_hab, "ext_day_fact")
hawo_asr_2way_est <- ext_day.2int.est(hawo_ext_day_hab, "ext_day_fact")
noca_asr_2way_est <- ext_day.2int.est(noca_ext_day_hab, "ext_day_fact")
rewo_asr_2way_est <- ext_day.2int.est(rewo_ext_day_hab, "ext_day_fact")
tuti_asr_2way_est <- ext_day.2int.est(tuti_ext_day_hab, "ext_day_fact")
woth_asr_2way_est <- ext_day.2int.est(woth_ext_day_hab, "ext_day_fact")
#make species columns
amgo_asr_2way_est$species <- "American Goldfinch"
amro_asr_2way_est$species <- "American Robin"
blja_asr_2way_est$species <- "Blue Jay"
cach_asr_2way_est$species <- "Carolina Chickadee"
cawr_asr_2way_est$species <- "Carolina Wren"
hawo_asr_2way_est$species <-  "Hairy Woodpecker"
noca_asr_2way_est$species <- "Northern Cardinal"
rewo_asr_2way_est$species <- "Red-bellied Woodpecker"
tuti_asr_2way_est$species <- "Tufted Titmouse"
woth_asr_2way_est$species <-  "Wood Thrush"
#combine contrasts across models into df
spp_asr_2way_est <- rbind(amgo_asr_2way_est, amro_asr_2way_est, blja_asr_2way_est,
                          cach_asr_2way_est, cawr_asr_2way_est, hawo_asr_2way_est, 
                          noca_asr_2way_est, rewo_asr_2way_est, tuti_asr_2way_est, 
                          woth_asr_2way_est)

spp_asr_2way_est$habitat <- as.factor(spp_asr_2way_est$contrasts)
levels(spp_asr_2way_est$habitat) <- c("Exurban forest", "Exurban open", "Urban forest", "Urban open")

#rescale contrasts to limit influence of very large values on visualization
hist(spp_asr_2way_est$mean)
summary(spp_asr_2way_est$mean)
spp_asr_2way_est$est <- ifelse(spp_asr_2way_est$mean >= 2, 2, spp_asr_2way_est$mean)
hist(spp_asr_2way_est$est)

library(readr)

#write_csv(spp_asr_2way_est, "C:/Users/kirchgrabera/Smithsonian Dropbox/Aidan Kirchgraber/Science and Faith/Aidan/birdsong_heat_project_Aidan/Data/model_estimates/spp_asr_2way_est.csv")

##Two-way Heatmap-----------------------------------------------------------------

png("./Figures_12-20-24/asr/heatmaps/spp10_twoway_heatmap_plasma_test_1-27-25.png",
    width = 6.25, height = 5.25, units = 'in', res = 600)
heatplot <- ggplot(data = spp_asr_2way_est, aes(x=habitat, y=species)) +
  geom_tile(aes(fill =  est)) +
  geom_text(aes(label = ifelse(sig == "Yes", "*", "")),
            size = 6) +
  scale_fill_viridis(discrete=FALSE, option = "C") +
  #scale_x_discrete(labels= c("Exurban forest", "Exurban open", "Urban forest", "Urban open")) +
  guides(alpha = FALSE) + #, fill = guide_legend(title = "Response to extremes")
  labs(fill = "Response to \n extremes") +
  theme(axis.text = element_text(size = 14),
        axis.text.x=element_text(size = 14, angle=45, hjust=1),
        legend.text=element_text(size = 12),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0))
#theme(axis.ticks.x = element_blank())+
heatplot
dev.off()

library(shadowtext)

png("./Figures_12-20-24/asr/heatmaps/spp10_twoway_heatmap_plasma_testt_1-27-25.png", 
    width = 6.25, height = 5.25, units = 'in', res = 600)
heatplott <- ggplot(data = spp_asr_2way_est, aes(x=habitat, y=species)) + 
  geom_tile(aes(fill =  est)) + 
  shadowtext::geom_shadowtext(
    aes(label = ifelse(sig == "Yes", "*", "")),
    size = 6,
    color = "black",
    bg.color = "white",
    bg.r = 0.1) +
  scale_fill_viridis(discrete=FALSE, option = "C") +
  labs(fill = "Response to \n extremes") +
  theme(axis.text = element_text(size = 14),
        axis.text.x=element_text(size = 14, angle=45, hjust=1),
        legend.text=element_text(size = 12),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0)) 
#theme(axis.ticks.x = element_blank())+
heatplott
dev.off()

#Analyze trait responses - asr 2-way ---------------------------------------------
spp_asr_2way_traits <- merge(spp_asr_2way_est, bdata,
                             by = "species", all.x = TRUE)
dim(spp_asr_2way_traits) #40  16
colnames(spp_asr_2way_traits)

##Standardize variables-----------------------------------------------------------
#center and scale continuous covariates 
spp_asr_2way_traits$z.body_mass_g <- as.numeric(scale(spp_asr_2way_traits$body_mass_g))
spp_asr_2way_traits$z.range_size_km2 <- as.numeric(scale(spp_asr_2way_traits$range_size_km2))

##Order factor levels-------------------------------------------------------------

spp_asr_2way_traits$sig <- as.factor(spp_asr_2way_traits$sig)
levels(spp_asr_2way_traits$sig)
spp_asr_2way_traits$sig <-
  factor(spp_asr_2way_traits$sig,
         levels = c("No", "Yes"))
summary(spp_asr_2way_traits$sig) #29N 11Y

spp_asr_2way_traits$habitat.y <- as.factor(spp_asr_2way_traits$habitat.y)
levels(spp_asr_2way_traits$habitat.y)
spp_asr_2way_traits$habitat.y <-
  factor(spp_asr_2way_traits$habitat.y,
         levels = c("Forest", "Open woodland"))
summary(spp_asr_2way_traits$habitat.y) #24Forest   16Openwoodland

spp_asr_2way_traits$nest_loc <- as.factor(spp_asr_2way_traits$nest_loc)
levels(spp_asr_2way_traits$nest_loc)
spp_asr_2way_traits$nest_loc <-
  factor(spp_asr_2way_traits$nest_loc,
         levels = c("Cavity", "Shrub", "Tree"))
summary(spp_asr_2way_traits$nest_loc) #20Cavity  8Shrub  12Tree

spp_asr_2way_traits$migratory_habit <- as.factor(spp_asr_2way_traits$migratory_habit)
levels(spp_asr_2way_traits$migratory_habit)
spp_asr_2way_traits$migratory_habit <-
  factor(spp_asr_2way_traits$migratory_habit,
         levels = c("Resident", "Shortdist", "Neotrop"))
summary(spp_asr_2way_traits$migratory_habit) #24Resident  12Shortdist  4Neotrop

spp_asr_2way_traits$feeding_behavior <- as.factor(spp_asr_2way_traits$feeding_behavior)
levels(spp_asr_2way_traits$feeding_behavior)
spp_asr_2way_traits$feeding_behavior <-
  factor(spp_asr_2way_traits$feeding_behavior,
         levels = c("Ground forager", "Bark forager", "Foliage gleaner"))
summary(spp_asr_2way_traits$feeding_behavior) #20Ground  8Bark  12Foliage

spp_asr_2way_traits$trophic_niche <- as.factor(spp_asr_2way_traits$trophic_niche)
levels(spp_asr_2way_traits$trophic_niche)
spp_asr_2way_traits$trophic_niche <-
  factor(spp_asr_2way_traits$trophic_niche,
         levels = c("Granivore", "Invertivore", "Omnivore"))
summary(spp_asr_2way_traits$trophic_niche) #4Granivore  24Invertivore  12Omnivore

##Dredge & find most important variables------------------------------------------

#making binary versions of some variables
spp_asr_2way_traits <- spp_asr_2way_traits %>%
  mutate(omnivore_IO = ifelse(trophic_niche == "Omnivore", "Omnivore", "Specialist"),
         resident_IO = ifelse(migratory_habit == "Resident", "Resident", "Migratory"),
         ground_eat_IO = ifelse(feeding_behavior == "Ground forager", "Ground forager", "Non-ground forager"),
         cavity_nest_IO = ifelse(nest_loc == "Cavity", "Cavity", "Non-cavity"))

#Examine pair plots
vars <-
  spp_asr_2way_traits[, c("est", "z.body_mass_g", "z.range_size_km2", "habitat.y", 
                          "nest_loc", "resident_IO", "feeding_behavior", "omnivore_IO")]

pairplot2 <- ggpairs(vars)
ggsave("asr_2way_globalmodel_condense_20250117.png", plot = pairplot2, dpi = 300, width = 10, height = 10)


#Make a global model
traits_global_asr_2way <- glm(est ~ z.body_mass_g +
                                z.range_size_km2 +
                                omnivore_IO +
                                resident_IO +
                                habitat.y,
                              family = gaussian,
                              data = spp_asr_2way_traits,
                              na.action = na.pass)

plot(traits_global_asr_2way)
vif(traits_global_asr_2way)

model.set.trait_asr2 <- dredge(traits_global_asr_2way, extra = "adjR^2")
importance.trait_asr2 <- sw(model.set.trait_asr2)
top.models.trait_asr2 <- get.models(model.set.trait_asr2, cumsum(weight) <= 0.95)
model.avg.trait_asr2 <- model.avg(top.models.trait_asr2)

summary(model.avg.trait_asr2)
# model.avg(object = top.models.trait_asr2)
# 
# Component model call: 
# glm(formula = est ~ <12 unique rhs>, family = gaussian, data = spp_asr_2way_traits, na.action = na.pass)
# 
# Component models: 
#       df logLik  AICc delta weight
# 345    5 -23.94 59.64  0.00   0.47
# 1345   6 -23.67 61.88  2.23   0.15
# 2345   6 -23.94 62.42  2.77   0.12
# 45     4 -27.32 63.79  4.15   0.06
# 12345  7 -23.48 64.46  4.82   0.04
# 235    5 -26.35 64.47  4.83   0.04
# 145    5 -26.78 65.32  5.68   0.03
# 245    5 -27.17 66.10  6.46   0.02
# 23     4 -28.53 66.20  6.56   0.02
# 1245   6 -25.92 66.38  6.74   0.02
# 35     4 -28.65 66.43  6.79   0.02
# 1235   6 -25.99 66.53  6.89   0.02
# 
# Term codes: 
# habitat.y      omnivore_IO      resident_IO    z.body_mass_g z.range_size_km2 
#         1                2                3                4                5 
# 
# Model-averaged coefficients:  
# (full average) 
#                        Estimate Std. Error Adjusted SE z value Pr(>|z|)   
# (Intercept)             0.16320    0.22591     0.23016   0.709  0.47828   
# resident_IOResident    -0.37033    0.20991     0.21402   1.730  0.08356 . 
# z.body_mass_g           0.27075    0.14059     0.14327   1.890  0.05880 . 
# z.range_size_km2       -0.32714    0.12327     0.12622   2.592  0.00955 **
# habitat.yOpen woodland  0.03674    0.12448     0.12726   0.289  0.77281   
# omnivore_IOSpecialist  -0.01052    0.17283     0.17610   0.060  0.95234   
# 
# (conditional average) 
#                        Estimate Std. Error Adjusted SE z value Pr(>|z|)   
# (Intercept)             0.16320    0.22591     0.23016   0.709  0.47828   
# resident_IOResident    -0.42176    0.16879     0.17456   2.416  0.01569 * 
# z.body_mass_g           0.29782    0.11695     0.12048   2.472  0.01344 * 
# z.range_size_km2       -0.33305    0.11620     0.11938   2.790  0.00527 **
# habitat.yOpen woodland  0.14366    0.21267     0.21901   0.656  0.51186   
# omnivore_IOSpecialist  -0.03893    0.33071     0.33704   0.115  0.90805   
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

importance.trait_asr2
#                      z.range_size_km2 z.body_mass_g resident_IO omnivore_IO habitat.y
# Sum of weights:      0.94             0.88          0.87        0.29        0.27     
# N containing models:   16               16            16          16          16  

###LMER version-------------------------------------------------------------------
#Make a global model
traits_global_asr_2way_lmer <- 
  lmer(est ~ z.body_mass_g +
         z.range_size_km2 +
         omnivore_IO +
         resident_IO +
         habitat.y + 
         (1|species),
       data = spp_asr_2way_traits,
       na.action = na.pass)
plot(traits_global_asr_2way_lmer)
qqnorm(resid(traits_global_asr_2way_lmer))
vif(traits_global_asr_2way_lmer)

# Compare to a model without random effects
traits_global_asr_2way_lm <- 
  lm(est ~ z.body_mass_g +
       z.range_size_km2 +
       omnivore_IO +
       resident_IO +
       habitat.y,
     data = spp_asr_2way_traits,
     na.action = na.pass)

AIC(traits_global_asr_2way_lmer, traits_global_asr_2way_lm)

##asr2 RVI Plot-------------------------------------------------------------------

variable_asr2 <- c("z.range_size_km2",
                   "z.body_mass_g", 
                   "resident_IO",
                   "omnivore_IO", 
                   "habitat.y")

varimport_asr2 <- as.data.frame(importance.trait_asr2)
varimport_asr2$variable_asr2 <- as.factor(variable_asr2)
levels(varimport_asr2$variable_asr2)
varimport_asr2$variable_asr2 <- factor(varimport_asr2$variable_asr2, 
                                       levels = c("z.range_size_km2",
                                                  "z.body_mass_g", 
                                                  "resident_IO",
                                                  "omnivore_IO", 
                                                  "habitat.y"))

varimport_asr2$response <- "activity"
colnames(varimport_asr2)[1] <- "importance"
#define labels for x-axis ticks
labs_asr2 <- c("Range Size", 
               "Body Mass", 
               "Migratory Habit", 
               "Trophic Niche",
               "Habitat")

png("./Figures_12-20-24/rvi/voc_rvi_asr2_glm_nice_1.21.25.png", width = 6.0, height = 5.0, units = 'in', res = 600)
rvi_asr2 <- ggplot(varimport_asr2, aes(y=importance, 
                                       x=variable_asr2,
                                       fill = factor(response))) +
  geom_bar(stat="identity", 
           size = 1, width = 0.8, 
           position = position_dodge(width = 0.9)) +
  scale_fill_manual(values=c("#cc4778")) + #31688e
  scale_x_discrete(labels = labs_asr2) +
  theme(axis.text = element_text(size = 18)) + 
  theme(panel.grid.major = element_line(colour = "white")) + 
  theme(panel.border = element_rect(color = "black", size = 1)) +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_text(size = 16)) +
  theme(axis.text.x = element_text(size = 16, 
                                   angle = 45, 
                                   vjust = 1, 
                                   hjust=1)) +
  theme(legend.position = "none") +
  labs(y = expression("Relative variable importance"),
       title = expression("Two-way GLM After Sunrise"))
#scale_y_continuous(limits = c(0, 1), expand = c(0, 0))
rvi_asr2
dev.off()
1+1
##asr2 Scatter Plots--------------------------------------------------------------

#RANGE SIZE SCATTER
png("./Figures_12-20-24/scatter/asr2_rangesize_1.21.25.png", width = 5.0, height = 5.0, units = 'in', res = 600)
scatter_rangesize_asr2 <- ggplot(spp_asr_2way_traits, 
                                 aes(y = est, 
                                     x = range_size_km2/1000000)) +
  geom_point() +
  geom_smooth(method = lm, 
              color = "#0d0887", 
              se = TRUE) +
  scale_x_continuous(labels = scales::comma) +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        panel.grid.major = element_line(color = "white"),
        panel.border = element_rect(color = "black", 
                                    size = 1),
        legend.position = "none") +
  labs(y = expression("Estimate"),
       x = expression("Range Size (millions of km²)"),
       title = expression("2-way ASR Range Size vs. Estimate"))
scatter_rangesize_asr2
dev.off()

#BODY MASS SCATTER
png("./Figures_12-20-24/scatter/asr2_bodymass_1.21.25.png", width = 5.0, height = 5.0, units = 'in', res = 600)
scatter_bodymass_asr2 <- ggplot(spp_asr_2way_traits, 
                                 aes(y = est, 
                                     x = body_mass_g)) +
  geom_point() +
  geom_smooth(method = lm, 
              color = "#0d0887", 
              se = TRUE) +
  scale_x_continuous(labels = scales::comma) +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        panel.grid.major = element_line(color = "white"),
        panel.border = element_rect(color = "black", 
                                    size = 1),
        legend.position = "none") +
  labs(y = expression("Estimate"),
       x = expression("Body Mass (g)"),
       title = expression("2-way ASR Body Mass vs. Estimate"))
scatter_bodymass_asr2
dev.off()

#---------------------------------------------------------------------------------
##Three-way Heatmap Prep----------------------------------------------------------
files <- list.files(path = "C:/Users/kirchgrabera/Smithsonian Dropbox/Aidan Kirchgraber/Science and Faith/Aidan/birdsong_heat_project_Aidan/saved_models/asr/threeway/fact", 
                    pattern = "\\.rds$",
                    full.names = TRUE)

for (file in files) {
  oldname <- tools::file_path_sans_ext(basename(file))
  newname <- paste0(substr(oldname, 1, 4), "_ext_day_hab3")
  assign(newname, readRDS(file)$mod)
}
rm(file, files, newname, oldname)

#get contrasts again
amgo_asr_3way_est <- ext_day.3int.est(amgo_ext_day_hab3, "ext_day_fact")
amro_asr_3way_est <- ext_day.3int.est(amro_ext_day_hab3, "ext_day_fact")
blja_asr_3way_est <- ext_day.3int.est(blja_ext_day_hab3, "ext_day_fact")
cach_asr_3way_est <- ext_day.3int.est(cach_ext_day_hab3, "ext_day_fact")
cawr_asr_3way_est <- ext_day.3int.est(cawr_ext_day_hab3, "ext_day_fact")
hawo_asr_3way_est <- ext_day.3int.est(hawo_ext_day_hab3, "ext_day_fact")
noca_asr_3way_est <- ext_day.3int.est(noca_ext_day_hab3, "ext_day_fact")
rewo_asr_3way_est <- ext_day.3int.est(rewo_ext_day_hab3, "ext_day_fact")
tuti_asr_3way_est <- ext_day.3int.est(tuti_ext_day_hab3, "ext_day_fact")
woth_asr_3way_est <- ext_day.3int.est(woth_ext_day_hab3, "ext_day_fact")
#make species columns
amgo_asr_3way_est$contrasts$species <- "American Goldfinch"
amro_asr_3way_est$contrasts$species <- "American Robin"
blja_asr_3way_est$contrasts$species <- "Blue Jay"
cach_asr_3way_est$contrasts$species <- "Carolina Chickadee"
cawr_asr_3way_est$contrasts$species <- "Carolina Wren"
hawo_asr_3way_est$contrasts$species <-  "Hairy Woodpecker"
noca_asr_3way_est$contrasts$species <- "Northern Cardinal"
rewo_asr_3way_est$contrasts$species <- "Red-bellied Woodpecker"
tuti_asr_3way_est$contrasts$species <- "Tufted Titmouse"
woth_asr_3way_est$contrasts$species <-  "Wood Thrush"

#combine contrasts across models into df
spp_asr_3way_est <- rbind(amgo_asr_3way_est$contrasts, amro_asr_3way_est$contrasts, 
                          blja_asr_3way_est$contrasts, cach_asr_3way_est$contrasts, 
                          cawr_asr_3way_est$contrasts, hawo_asr_3way_est$contrasts, 
                          noca_asr_3way_est$contrasts, rewo_asr_3way_est$contrasts, 
                          tuti_asr_3way_est$contrasts, woth_asr_3way_est$contrasts)

spp_asr_3way_est$habitat <- as.factor(spp_asr_3way_est$contrasts)
spp_asr_3way_est$month <- as.factor(spp_asr_3way_est$contrasts)
levels(spp_asr_3way_est$habitat) <- c("Exurban forest", "Exurban forest", "Exurban forest",
                                      "Exurban open", "Exurban open", "Exurban open",
                                      "Urban forest", "Urban forest", "Urban forest",
                                      "Urban open", "Urban open", "Urban open")

levels(spp_asr_3way_est$month) <- c("June", "July", "August",
                                    "June", "July", "August",
                                    "June", "July", "August",
                                    "June", "July", "August")

#rescale contrasts to limit influence of very large values on visualization
hist(spp_asr_3way_est$mean)
summary(spp_asr_3way_est$mean)
spp_asr_3way_est$est <- ifelse(spp_asr_3way_est$mean >= 2, 2, spp_asr_3way_est$mean)
hist(spp_asr_3way_est$est)

write_csv(spp_asr_3way_est, "C:/Users/kirchgrabera/Smithsonian Dropbox/Aidan Kirchgraber/Science and Faith/Aidan/birdsong_heat_project_Aidan/Data/model_estimates/spp_asr_3way_est_20250227.csv")

##Three-way Heatmap---------------------------------------------------------------

# png("./Figures_12-20-24/asr/heatmaps/spp10_threeway_heatmap_plasma_1-27-25.png", 
#     width = 9.4, height = 5.25, units = 'in', res = 600)
# heatplot2 <- ggplot(data = spp_asr_3way_est, aes(x=habitat, y=species, fill= est)) + 
#   facet_grid(cols = vars(month)) + #, scales = "free"
#   scale_alpha_discrete(range = c(0.60, 1)) +
#   geom_tile(aes(alpha =  sig)) + 
#   #geom_text(aes(label = sig), color = "black") +
#   scale_fill_viridis(discrete=FALSE, option = "C") +
#   #scale_x_discrete(labels= c("Exurban forest", "Exurban open", "Urban forest", "Urban open")) + 
#   guides(alpha = FALSE) + #, fill = guide_legend(title = "Response to extremes")
#   labs(fill = "Response to \n extremes") +
#   theme(strip.text = element_text(
#     size = 12)) + 
#   theme(axis.text = element_text(size = 14),
#         axis.text.x=element_text(size = 14, angle=45, hjust=1),
#         legend.text=element_text(size = 12),
#         axis.title.x=element_blank(),
#         axis.title.y=element_blank()) +
#   scale_y_discrete(expand = c(0, 0)) +
#   scale_x_discrete(expand = c(0, 0)) 
# #theme(axis.ticks.x = element_blank())+
# heatplot2
# dev.off()

png("./Figures_12-20-24/asr/heatmaps/spp10_asr3_heatmap_plasma_2-27-25.png", 
    width = 9.4, height = 5.25, units = 'in', res = 600)
heatplott2 <- ggplot(data = spp_asr_3way_est, aes(x=habitat, y=species)) + 
  facet_grid(cols = vars(month)) + #, scales = "free"
  geom_tile(aes(fill=est)) + 
  shadowtext::geom_shadowtext(
    aes(label = ifelse(sig == "Yes", "*", "")),
    size = 6,
    color = "black",
    bg.color = "white",
    bg.r = 0.1) +
  scale_fill_viridis(discrete=FALSE, option = "C") +
  labs(fill = "Response to \n extremes") +
  theme(strip.text = element_text(size = 12)) + 
  theme(axis.text = element_text(size = 14),
        axis.text.x=element_text(size = 14, angle=45, hjust=1),
        legend.text=element_text(size = 12),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0)) 
#theme(axis.ticks.x = element_blank())+
heatplott2
dev.off()

#Analyze trait responses - asr 3-way ---------------------------------------------

spp_asr_3way_traits <- merge(spp_asr_3way_est, bdata,
                         by = "species", all.x = TRUE)
dim(spp_asr_3way_traits) #120  18
colnames(spp_asr_3way_traits)

##Standardize variables-----------------------------------------------------------
#center and scale continuous covariates 
spp_asr_3way_traits$z.body_mass_g <- as.numeric(scale(spp_asr_3way_traits$body_mass_g))
spp_asr_3way_traits$z.range_size_km2 <- as.numeric(scale(spp_asr_3way_traits$range_size_km2))

##Order factor levels-------------------------------------------------------------

spp_asr_3way_traits$sig <- as.factor(spp_asr_3way_traits$sig)
levels(spp_asr_3way_traits$sig)
spp_asr_3way_traits$sig <-
  factor(spp_asr_3way_traits$sig,
         levels = c("No", "Yes"))
summary(spp_asr_3way_traits$sig) #93N  27Y

spp_asr_3way_traits$habitat.y <- as.factor(spp_asr_3way_traits$habitat.y)
levels(spp_asr_3way_traits$habitat.y)
spp_asr_3way_traits$habitat.y <-
  factor(spp_asr_3way_traits$habitat.y,
         levels = c("Forest", "Open woodland"))
summary(spp_asr_3way_traits$habitat.y) #72Forest  48Open Woodland

spp_asr_3way_traits$nest_loc <- as.factor(spp_asr_3way_traits$nest_loc)
levels(spp_asr_3way_traits$nest_loc)
spp_asr_3way_traits$nest_loc <-
  factor(spp_asr_3way_traits$nest_loc,
         levels = c("Cavity", "Shrub", "Tree"))
summary(spp_asr_3way_traits$nest_loc) #60Cavity  24Shrub  36Tree

spp_asr_3way_traits$migratory_habit <- as.factor(spp_asr_3way_traits$migratory_habit)
levels(spp_asr_3way_traits$migratory_habit)
spp_asr_3way_traits$migratory_habit <-
  factor(spp_asr_3way_traits$migratory_habit,
         levels = c("Resident", "Shortdist", "Neotrop"))
summary(spp_asr_3way_traits$migratory_habit) #72Resident  36Shortdist  12Neotrop

spp_asr_3way_traits$feeding_behavior <- as.factor(spp_asr_3way_traits$feeding_behavior)
levels(spp_asr_3way_traits$feeding_behavior)
spp_asr_3way_traits$feeding_behavior <-
  factor(spp_asr_3way_traits$feeding_behavior,
         levels = c("Ground forager", "Bark forager", "Foliage gleaner"))
summary(spp_asr_3way_traits$feeding_behavior) #60Ground  24Bark  36Foliage

spp_asr_3way_traits$trophic_niche <- as.factor(spp_asr_3way_traits$trophic_niche)
levels(spp_asr_3way_traits$trophic_niche)
spp_asr_3way_traits$trophic_niche <-
  factor(spp_asr_3way_traits$trophic_niche,
         levels = c("Granivore", "Invertivore", "Omnivore"))
summary(spp_asr_3way_traits$trophic_niche) #12Granivore  72Invertivore  36Omnivore


##Dredge & find most important variables------------------------------------------

#making binary versions of some variables
spp_asr_3way_traits <- spp_asr_3way_traits %>%
  mutate(omnivore_IO = ifelse(trophic_niche == "Omnivore", "Omnivore", "Specialist"),
         resident_IO = ifelse(migratory_habit == "Resident", "Resident", "Migratory"),
         ground_eat_IO = ifelse(feeding_behavior == "Ground forager", 1, 0),
         cavity_nest_IO = ifelse(nest_loc == "Cavity", 1, 0))

#Examine pair plots
vars2 <-
  spp_asr_3way_traits[, c("est", "z.body_mass_g", "z.range_size_km2", "habitat.y", 
                          "nest_loc", "resident_IO", "feeding_behavior", "omnivore_IO")]

pairplot2 <- ggpairs(vars2)
ggsave("asr_3way_globalmodel_condense_20250227.png", plot = pairplot2, dpi = 300, width = 10, height = 10)


#Make a global model
traits_global_asr_3way <- glm(est ~ z.body_mass_g +
                                z.range_size_km2 +
                                omnivore_IO +
                                resident_IO +
                                habitat.y,
                              family = gaussian,
                              data = spp_asr_3way_traits,
                              na.action = na.pass)
plot(traits_global_asr_3way)
vif(traits_global_asr_3way)

library(MuMIn)

model.set.trait_asr3 <- dredge(traits_global_asr_3way, extra = "adjR^2")
importance.trait_asr3 <- sw(model.set.trait_asr3)
top.models.trait_asr3 <- get.models(model.set.trait_asr3, cumsum(weight) <= 0.95)
model.avg.trait_asr3 <- model.avg(top.models.trait_asr3)

summary(model.avg.trait_asr3)
# model.avg(object = top.models.trait_asr3)
# 
# Component model call: 
# glm(formula = est ~ <16 unique rhs>, family = gaussian, data = spp_3way_traits, na.action = na.pass)
# 
# Component models: 
# df  logLik   AICc delta weight
# 34     4 -146.62 301.58  0.00   0.17
# 1345   6 -144.65 302.04  0.46   0.14
# 134    5 -146.05 302.63  1.05   0.10
# 345    5 -146.19 302.90  1.31   0.09
# 23     4 -147.36 303.06  1.48   0.08
# 234    5 -146.38 303.29  1.71   0.07
# 12345  7 -144.16 303.32  1.73   0.07
# 3      3 -148.91 304.03  2.45   0.05
# 1234   6 -145.89 304.52  2.94   0.04
# 123    5 -147.22 304.96  3.38   0.03
# 235    5 -147.24 305.00  3.42   0.03
# 2345   6 -146.17 305.08  3.50   0.03
# 1245   6 -146.49 305.72  4.13   0.02
# 14     4 -148.80 305.95  4.37   0.02
# 13     4 -148.84 306.03  4.44   0.02
# 35     4 -148.90 306.14  4.56   0.02
# 
# Term codes: 
# habitat.y      omnivore_IO      resident_IO    z.body_mass_g z.range_size_km2 
# 1                2                3                4                5 
# 
# Model-averaged coefficients:  
# (full average) 
#                        Estimate Std. Error Adjusted SE z value Pr(>|z|)  
# (Intercept)             0.32671    0.28233     0.28358   1.152   0.2493  
# resident_IOResident    -0.44670    0.19767     0.19911   2.244   0.0249 *
# z.body_mass_g           0.18413    0.16755     0.16813   1.095   0.2734  
# habitat.yOpen woodland  0.12332    0.20576     0.20655   0.597   0.5505  
# z.range_size_km2       -0.06082    0.12353     0.12400   0.491   0.6238  
# omnivore_IOSpecialist  -0.02765    0.21167     0.21263   0.130   0.8966  
# 
# (conditional average) 
#                        Estimate Std. Error Adjusted SE z value Pr(>|z|)   
# (Intercept)             0.32671    0.28233     0.28358   1.152  0.24928   
# resident_IOResident    -0.46612    0.17811     0.17977   2.593  0.00952 **
# z.body_mass_g           0.24065    0.15195     0.15278   1.575  0.11522   
# habitat.yOpen woodland  0.27569    0.22943     0.23101   1.193  0.23272   
# z.range_size_km2       -0.15073    0.15577     0.15669   0.962  0.33608   
# omnivore_IOSpecialist  -0.07151    0.33580     0.33737   0.212  0.83212   
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# appears that large-bodied spp and migratory spp are more sensitive

importance.trait_asr3
#                      resident_IO z.body_mass_g habitat.y z.range_size_km2 omnivore_IO
# Sum of weights:      0.91        0.76          0.46      0.42             0.39       
# N containing models:   16          16            16        16               16     
###LMER version-------------------------------------------------------------------

library(lme4)
#Make a global model
traits_global_asr_3way_lmer <- 
  lmer(est ~ z.body_mass_g +
         z.range_size_km2 +
         omnivore_IO +
         resident_IO +
         habitat.y + 
         (1|species),
       data = spp_asr_3way_traits,
       na.action = na.pass)
plot(traits_global_asr_3way_lmer)
qqnorm(resid(traits_global_asr_3way_lmer))
vif(traits_global_asr_3way_lmer)
ranef(traits_global_asr_3way_lmer)

# Compare to a model without random effects
traits_global_asr_3way_lm <- 
  lm(est ~ z.body_mass_g +
       z.range_size_km2 +
       omnivore_IO +
       resident_IO +
       habitat.y,
     data = spp_asr_3way_traits,
     na.action = na.pass)

AIC(traits_global_asr_3way_lmer, traits_global_asr_3way_lm)

##asr3 RVI Plot-------------------------------------------------------------------

variable_asr3 <- c("resident_IO",
                   "z.body_mass_g",
                   "z.range_size_km2", 
                   "omnivore_IO", 
                   "habitat.y")
varimport_asr3 <- as.data.frame(importance.trait_asr3)
varimport_asr3$variable_asr3 <- as.factor(variable_asr3)
levels(varimport_asr3$variable_asr3)
varimport_asr3$variable_asr3 <- factor(varimport_asr3$variable_asr3, 
                                       levels = c("resident_IO",
                                                  "z.body_mass_g",
                                                  "z.range_size_km2", 
                                                  "omnivore_IO",
                                                  "habitat.y"))

varimport_asr3$response <- "activity"
colnames(varimport_asr3)[1] <- "importance"

#define labels for x-axis ticks
labs_asr3 <- c("Migratory Habit", 
               "Body Mass", 
               "Range Size",
               "Trophic Niche",
               "Habitat")

png("./Figures_12-20-24/rvi/voc_rvi_asr3_glm_nice_2.27.25.png", 
    width = 6.0, height = 5.0, units = 'in', res = 600)
rvi_asr3 <- ggplot(varimport_asr3, aes(y=importance, 
                                       x=variable_asr3,
                                       fill = factor(response))) +
  geom_bar(stat="identity", 
           size = 1, width = 0.8, 
           position = position_dodge(width = 0.9)) +
  scale_fill_manual(values=c("#cc4778")) + #31688e
  scale_x_discrete(labels = labs_asr3) +
  theme(axis.text = element_text(size = 18)) + 
  theme(panel.grid.major = element_line(colour = "white")) + 
  theme(panel.border = element_rect(color = "black", size = 1)) +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_text(size = 16)) +
  theme(axis.text.x = element_text(size = 16, 
                                   angle = 45, 
                                   vjust = 1, 
                                   hjust=1)) +
  theme(legend.position = "none") +
  labs(y = expression("Relative variable importance"),
       title = expression("Three-way GLM After Sunrise"))
#scale_y_continuous(limits = c(0, 1), expand = c(0, 0))
rvi_asr3
dev.off()
1+1
##asr3 Scatter/Box Plots----------------------------------------------------------

#MIGRATORY HABIT BOXPLOT
png("./Figures_12-20-24/scatter/asr3_migratory_box_2.27.25.png", width = 5.0, height = 5.0, units = 'in', res = 600)
box_migratory_asr3 <- ggplot(spp_asr_3way_traits, 
                             aes(y = est, 
                                 x = resident_IO,
                                 fill = "#ed7953")) +
  geom_boxplot() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        panel.grid.major = element_line(color = "white"),
        panel.border = element_rect(color = "black", 
                                    size = 1),
        legend.position = "none") +
  labs(y = expression("Estimate"),
       x = expression(""),
       title = expression("3-way ASR Migratory Habit vs. Estimate"))
box_migratory_asr3
dev.off()

##################
png("./Figures_12-20-24/scatter/asr3_migratory_box_2.27.25test.png", width = 5.0, height = 5.0, units = 'in', res = 600)
boxplot <- ggplot(spp_asr_3way_traits, aes(y = est, x = resident_IO, color = "#0d0887", fill = "#ed7953")) + 
  geom_boxplot(aes(color="#0d0887", fill = "#ed7953"), alpha = 0.80, size=1.25) + 
  #geom_point(aes(y = temp.3_q95, fill = factor(div)), color = "grey45", shape = 21, position = position_jitterdodge(dodge.width = 0.75, jitter.width = 0.2), size = 1.2, stroke = 0.5) +  #
  #geom_jitter(height = 0, width = 0.15, alpha = 0.9) +
  #geom_errorbar(aes(y = Means, ymin = Means-CI95, ymax = Means+CI95), color = "black", size = 1, width = 0.1, data = model_sum) +
  #geom_point(aes(y = Means), shape = 21, color = "black", fill = "grey", size = 3, stroke = 3, data = model_sum) + 
  theme(panel.grid.major = element_line(colour = "white")) +
  theme(panel.grid.minor = element_line(colour = "white")) +
  theme(panel.grid.major.y = element_line(colour = "white")) +
  theme(panel.border = element_rect(color = "black", size = 1)) +
  theme(legend.title=element_blank()) +
  theme(legend.position = "none") +
  labs(x = expression("")) +
  theme(axis.text.y = element_text(size = 14)) +
  theme(axis.text.x = element_text(size = 14)) +
  theme(axis.title.y = element_text(size = 14)) +
  theme(axis.title.x = element_text(size = 14)) +
  theme(legend.text = element_text(size = 14)) +
  theme(plot.margin = margin(0.5, 0.5, 0.5, 0.75, unit = "cm")) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) + #vjust = 1, 
  #labs(y = expression("Temperature")) +
  ylab("Estimate") +
  #labs(y = expression("Plot-level leaf area index")) +
  #labs(y = expression("Plot-level vertical complexity index")) +
  labs(x = expression(""))
boxplot
dev.off()

##################

#BODY MASS SCATTER
png("./Figures_12-20-24/scatter/asr3_bodymass_2.27.25.png", width = 5.0, height = 5.0, units = 'in', res = 600)
scatter_bodymass_asr3 <- ggplot(spp_asr_3way_traits, 
                                aes(y = est, 
                                    x = body_mass_g)) +
  geom_point() +
  geom_smooth(method = lm, 
              color = "#0d0887", 
              se = TRUE) +
  scale_x_continuous(labels = scales::comma) +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        panel.grid.major = element_line(color = "white"),
        panel.border = element_rect(color = "black", 
                                    size = 1),
        legend.position = "none") +
  labs(y = expression("Estimate"),
       x = expression("Body Mass (g)"),
       title = expression("3-way ASR Body Mass vs. Estimate"))
scatter_bodymass_asr3
dev.off()

##################
#Scatterplot with linear or loess fit lines
png("./Figures_12-20-24/scatter/asr3_bodymass_2.27.25test.png", width = 5.0, height = 5.0, units = 'in', res = 600)
effect_plot <- ggplot(spp_asr_3way_traits, aes(y = est, 
                                               x = body_mass_g)) + 
  geom_point(aes(fill = "#57a595", color = "#57a595"), shape = 21, size = 2.5, stroke = 0.4, alpha = 0.80) + 
  #stat_smooth(aes(color = factor(species)), method = lm, formula = y ~ x, se = TRUE, alpha = 0.25, size = 1.5) + 
  stat_smooth(method = lm, formula = y ~ x, se = TRUE, alpha = 0.55, size = 1.5, color = "#0d0887") + 
  #stat_smooth(method = lm, formula = y ~ poly(x, 2), se = FALSE, size = 1.5, color = "grey35") +
  #stat_smooth(method = loess, span = 0.75, se = FALSE, size = 1.5) +
  #geom_abline(intercept = 0, slope = 1, size = 1.1, lty = "dashed") +
  scale_fill_manual(values=c("#57a595")) +
  scale_color_manual(values=c("#57a595")) +
  theme(panel.grid.major = element_line(colour = "white")) + 
  theme(panel.grid.minor = element_line(colour = "white")) + 
  theme(panel.grid.major.y = element_line(colour = "white")) +
  theme(panel.border = element_rect(color = "black", size = 1)) +
  theme(legend.title=element_blank()) +
  theme(legend.position = "none") +
  #theme(legend.position=c(0.25, 0.88)) +
  #theme(legend.text = element_text(size = 12)) +
  theme(axis.text = element_text(size = 14)) + 
  theme(axis.title.y=element_text(size = 14)) +
  theme(axis.title.x=element_text(size = 14)) +
  #scale_x_continuous(expand = c(0.015, 0.009)) +
  #scale_y_continuous(limits = c(-0.2, 8.8), breaks = seq(0 , 8, by = 2)) +
  theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, unit = "cm")) +
  ylab("Estimate") +
  xlab("Body Mass (g)")
effect_plot
dev.off()
##################


#MIGRATORY HABIT simple model
migratory_asr3_lm <- 
  lm(est ~ resident_IO,
     data = spp_asr_3way_traits,
     na.action = na.pass)

plot(migratory_asr3_lm)
summary(migratory_asr3_lm)

#BODY MASS simple model
bodymass_asr3_lm <- 
  lm(est ~ z.body_mass_g,
     data = spp_asr_3way_traits,
     na.action = na.pass)

plot(bodymass_asr3_lm)
summary(bodymass_asr3_lm)

##Examine collinearity------------------------------------------------------------

# TODO: Fit global models with species responses as the dependent (y) 
# and traits and the independent (x) vars in the model. 

#First look at VIF of intended covariate structures here
vif(glm(z.est ~ z.body_mass_g + nest_loc + migratory_habit + habitat.y,
        family = gaussian, data  = spp_asr_3way_traits))

vif(glm(z.est ~ z.range_size_km2 + z.body_mass_g + migratory_habit + habitat.y,
        family = gaussian, data  = spp_asr_3way_traits))

vif(glm(z.est ~ z.range_size_km2 * z.body_mass_g + migratory_habit + habitat.y,
        family = gaussian, data  = spp_asr_3way_traits))

hist(spp_asr_3way_traits$z.est)
plot(as.factor(spp_asr_3way_traits$trophic_niche), 
     as.factor(spp_asr_3way_traits$feeding_behavior))

plot(as.factor(spp_asr_3way_traits$nest_loc), 
     spp_asr_3way_traits$z.est)

plot(as.factor(spp_asr_3way_traits$migratory_habit), 
     spp_asr_3way_traits$z.range_size_km2)

##LOOIC---------------------------------------------------------------------------

#Import ext_vpd_hab3 models
files <- list.files(path = "C:/Users/kirchgrabera/Smithsonian Dropbox/Aidan Kirchgraber/Science and Faith/Aidan/birdsong_heat_project_Aidan/saved_models/asr/threeway/vpd", 
                    pattern = "\\.rds$",
                    full.names = TRUE)

for (file in files) {
  oldname <- tools::file_path_sans_ext(basename(file))
  newname <- paste0(substr(oldname, 1, 4), "_ext_vpd_hab3")
  assign(newname, readRDS(file)$mod)
}
rm(file, files, newname, oldname)

#Import ext_vpd_hab3 models
files <- list.files(path = "C:/Users/kirchgrabera/Smithsonian Dropbox/Aidan Kirchgraber/Science and Faith/Aidan/birdsong_heat_project_Aidan/saved_models/asr/threeway/wbt", 
                    pattern = "\\.rds$",
                    full.names = TRUE)

for (file in files) {
  oldname <- tools::file_path_sans_ext(basename(file))
  newname <- paste0(substr(oldname, 1, 4), "_ext_wbt_hab3")
  assign(newname, readRDS(file)$mod)
}
rm(file, files, newname, oldname)

loo(woth_ext_day_hab3)
loo(woth_ext_vpd_hab3)
loo(woth_ext_wbt_hab3)

#---------------------------------------------------------------------------------
#MIDDAY HEATMAPS & MODEL AVERAGING------------------------------------------------
##Two-way Heatmap Prep------------------------------------------------------------

files <- list.files(path = "C:/Users/kirchgrabera/Smithsonian Dropbox/Aidan Kirchgraber/Science and Faith/Aidan/birdsong_heat_project_Aidan/saved_models/midday/twoway/fact", 
                    pattern = "\\.rds$",
                    full.names = TRUE)

for (file in files) {
  oldname <- tools::file_path_sans_ext(basename(file))
  newname <- paste0(substr(oldname, 1, 4), "_ext_midday_hab")
  assign(newname, readRDS(file)$mod)
}
rm(file, files, newname, oldname)

#get contrasts again
amgo_midday_2way_est <- ext_day.2int.est(amgo_ext_midday_hab, "ext_day_fact")
amro_midday_2way_est <- ext_day.2int.est(amro_ext_midday_hab, "ext_day_fact")
blja_midday_2way_est <- ext_day.2int.est(blja_ext_midday_hab, "ext_day_fact")
cach_midday_2way_est <- ext_day.2int.est(cach_ext_midday_hab, "ext_day_fact")
cawr_midday_2way_est <- ext_day.2int.est(cawr_ext_midday_hab, "ext_day_fact")
hawo_midday_2way_est <- ext_day.2int.est(hawo_ext_midday_hab, "ext_day_fact")
noca_midday_2way_est <- ext_day.2int.est(noca_ext_midday_hab, "ext_day_fact")
rewo_midday_2way_est <- ext_day.2int.est(rewo_ext_midday_hab, "ext_day_fact")
tuti_midday_2way_est <- ext_day.2int.est(tuti_ext_midday_hab, "ext_day_fact")
woth_midday_2way_est <- ext_day.2int.est(woth_ext_midday_hab, "ext_day_fact")
#make species columns
amgo_midday_2way_est$species <- "American Goldfinch"
amro_midday_2way_est$species <- "American Robin"
blja_midday_2way_est$species <- "Blue Jay"
cach_midday_2way_est$species <- "Carolina Chickadee"
cawr_midday_2way_est$species <- "Carolina Wren"
hawo_midday_2way_est$species <-  "Hairy Woodpecker"
noca_midday_2way_est$species <- "Northern Cardinal"
rewo_midday_2way_est$species <- "Red-bellied Woodpecker"
tuti_midday_2way_est$species <- "Tufted Titmouse"
woth_midday_2way_est$species <-  "Wood Thrush"
#combine contrasts across models into df
spp_midday_2way_est <- rbind(amgo_midday_2way_est, amro_midday_2way_est, blja_midday_2way_est,
                             cach_midday_2way_est, cawr_midday_2way_est, hawo_midday_2way_est, 
                             noca_midday_2way_est, rewo_midday_2way_est, tuti_midday_2way_est, 
                             woth_midday_2way_est)

spp_midday_2way_est$habitat <- as.factor(spp_midday_2way_est$contrasts)
levels(spp_midday_2way_est$habitat) <- c("Exurban forest", "Exurban open", "Urban forest", "Urban open")

#rescale contrasts to limit influence of very large values on visualization
hist(spp_midday_2way_est$mean)
summary(spp_midday_2way_est$mean)
spp_midday_2way_est$est <- ifelse(spp_midday_2way_est$mean >= 2, 2, spp_midday_2way_est$mean)
hist(spp_midday_2way_est$est)

#write_csv(spp_midday_2way_est, "C:/Users/kirchgrabera/Smithsonian Dropbox/Aidan Kirchgraber/Science and Faith/Aidan/birdsong_heat_project_Aidan/Data/model_estimates/spp_midday_2way_est.csv")

##Two-way Heatmap-----------------------------------------------------------------

# png("./Figures_12-20-24/midday/heatmaps/spp10_midday_twoway_heatmap_plasma_1-17-25.png", 
#     width = 6.25, height = 5.25, units = 'in', res = 600)
# heatplot <- ggplot(data = spp_midday_2way_est, aes(x=habitat, y=species, fill= est)) + 
#   scale_alpha_discrete(range = c(0.60, 1)) +
#   geom_tile(aes(alpha =  sig)) + 
#   #geom_text(aes(label = sig), color = "black") +
#   scale_fill_viridis(discrete=FALSE, option = "C") +
#   #scale_x_discrete(labels= c("Exurban forest", "Exurban open", "Urban forest", "Urban open")) + 
#   guides(alpha = FALSE) + #, fill = guide_legend(title = "Response to extremes")
#   labs(fill = "Response to \n extremes") +
#   theme(axis.text = element_text(size = 14)) + 
#   theme(axis.text.x=element_text(size = 14, angle=45, hjust=1)) +
#   theme(legend.text=element_text(size = 12)) +
#   theme(axis.title.x=element_blank()) +
#   theme(axis.title.y=element_blank()) +
#   scale_y_discrete(expand = c(0, 0)) +
#   scale_x_discrete(expand = c(0, 0)) 
# #theme(axis.ticks.x = element_blank())+
# heatplot
# dev.off()
library(viridis)
png("./Figures_12-20-24/midday/heatmaps/spp10_midday_twoway_heatmap_plasma_1-17-25.png", 
    width = 6.25, height = 5.25, units = 'in', res = 600)
heatplotmid2 <- ggplot(data = spp_midday_2way_est, aes(x=habitat, y=species)) + 
  geom_tile(aes(fill =  est)) + 
  shadowtext::geom_shadowtext(
    aes(label = ifelse(sig == "Yes", "*", "")),
    size = 6,
    color = "black",
    bg.color = "white",
    bg.r = 0.1) +
  scale_fill_viridis(discrete=FALSE, option = "C") +
  labs(fill = "Response to \n extremes") +
  theme(axis.text = element_text(size = 14),
        axis.text.x=element_text(size = 14, angle=45, hjust=1),
        legend.text=element_text(size = 12),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0)) 
#theme(axis.ticks.x = element_blank())+
heatplotmid2
dev.off()

#Analyze trait responses - midday 2-way-------------------------------------------
spp_midday_2way_traits <- merge(spp_midday_2way_est, bdata,
                                by = "species", all.x = TRUE)
dim(spp_midday_2way_traits) #40  16
colnames(spp_midday_2way_traits)

##Standardize variables-----------------------------------------------------------
#center and scale continuous covariates 
spp_midday_2way_traits$z.body_mass_g <- as.numeric(scale(spp_midday_2way_traits$body_mass_g))
spp_midday_2way_traits$z.range_size_km2 <- as.numeric(scale(spp_midday_2way_traits$range_size_km2))

##Order factor levels-------------------------------------------------------------

spp_midday_2way_traits$sig <- as.factor(spp_midday_2way_traits$sig)
levels(spp_midday_2way_traits$sig)
spp_midday_2way_traits$sig <-
  factor(spp_midday_2way_traits$sig,
         levels = c("No", "Yes"))
summary(spp_midday_2way_traits$sig)

spp_midday_2way_traits$habitat.y <- as.factor(spp_midday_2way_traits$habitat.y)
levels(spp_midday_2way_traits$habitat.y)
spp_midday_2way_traits$habitat.y <-
  factor(spp_midday_2way_traits$habitat.y,
         levels = c("Forest", "Open woodland"))
summary(spp_midday_2way_traits$habitat.y)

spp_midday_2way_traits$nest_loc <- as.factor(spp_midday_2way_traits$nest_loc)
levels(spp_midday_2way_traits$nest_loc)
spp_midday_2way_traits$nest_loc <-
  factor(spp_midday_2way_traits$nest_loc,
         levels = c("Cavity", "Shrub", "Tree"))
summary(spp_midday_2way_traits$nest_loc)

spp_midday_2way_traits$migratory_habit <- as.factor(spp_midday_2way_traits$migratory_habit)
levels(spp_midday_2way_traits$migratory_habit)
spp_midday_2way_traits$migratory_habit <-
  factor(spp_midday_2way_traits$migratory_habit,
         levels = c("Resident", "Shortdist", "Neotrop"))
summary(spp_midday_2way_traits$migratory_habit)

spp_midday_2way_traits$feeding_behavior <- as.factor(spp_midday_2way_traits$feeding_behavior)
levels(spp_midday_2way_traits$feeding_behavior)
spp_midday_2way_traits$feeding_behavior <-
  factor(spp_midday_2way_traits$feeding_behavior,
         levels = c("Ground forager", "Bark forager", "Foliage gleaner"))
summary(spp_midday_2way_traits$feeding_behavior)

spp_midday_2way_traits$trophic_niche <- as.factor(spp_midday_2way_traits$trophic_niche)
levels(spp_midday_2way_traits$trophic_niche)
spp_midday_2way_traits$trophic_niche <-
  factor(spp_midday_2way_traits$trophic_niche,
         levels = c("Granivore", "Invertivore", "Omnivore"))
summary(spp_midday_2way_traits$trophic_niche) #

##Dredge & find most important variables------------------------------------------

#making binary versions of some variables
spp_midday_2way_traits <- spp_midday_2way_traits %>%
  mutate(omnivore_IO = ifelse(trophic_niche == "Omnivore", "Omnivore", "Specialist"),
         resident_IO = ifelse(migratory_habit == "Resident", "Resident", "Migratory"),
         ground_eat_IO = ifelse(feeding_behavior == "Ground forager", "Ground forager", "Non-ground forager"),
         cavity_nest_IO = ifelse(nest_loc == "Cavity", "Cavity", "Non-cavity"))

#Examine pair plots
vars3 <-
  spp_midday_2way_traits[, c("est", "z.body_mass_g", "z.range_size_km2", 
                             "habitat.y", "nest_loc", "resident_IO", 
                             "feeding_behavior", "omnivore_IO")]

pairplot3 <- ggpairs(vars3)
ggsave("midday_2way_globalmodel_condense_20250117.png", plot = pairplot3, dpi = 300, width = 10, height = 10)


#Make a global model
traits_global_midday_2way <- glm(est ~ z.body_mass_g +
                                z.range_size_km2 +
                                omnivore_IO +
                                resident_IO +
                                habitat.y,
                              family = gaussian,
                              data = spp_midday_2way_traits,
                              na.action = na.pass)

plot(traits_global_midday_2way)
vif(traits_global_midday_2way)

model.set.trait_midday2 <- dredge(traits_global_midday_2way, extra = "adjR^2")
importance.trait_midday2 <- sw(model.set.trait_midday2)
top.models.trait_midday2 <- get.models(model.set.trait_midday2, cumsum(weight) <= 0.95)
model.avg.trait_midday2 <- model.avg(top.models.trait_midday2)

summary(model.avg.trait_midday2)
# model.avg(object = top.models.trait_midday2)
# 
# Component model call: 
# glm(formula = est ~ <23 unique rhs>, family = gaussian, data = spp_midday_2way_traits, na.action = 
#       na.pass)
# 
# Component models: 
#        df logLik  AICc delta weight
# 345     5 -41.30 94.36  0.00   0.21
# 35      4 -43.33 95.80  1.45   0.10
# 2345    6 -41.01 96.57  2.21   0.07
# 3       3 -45.05 96.76  2.40   0.06
# 45      4 -43.92 96.98  2.62   0.06
# 1345    6 -41.30 97.14  2.78   0.05
# 13      4 -44.07 97.29  2.93   0.05
# 235     5 -42.86 97.49  3.13   0.04
# 135     5 -42.88 97.52  3.16   0.04
# 23      4 -44.30 97.74  3.39   0.04
# (Null)  2 -46.84 98.00  3.64   0.03
# 245     5 -43.28 98.32  3.96   0.03
# 4       3 -45.92 98.51  4.15   0.03
# 34      4 -44.70 98.55  4.19   0.03
# 123     5 -43.42 98.60  4.24   0.03
# 12345   7 -40.86 99.22  4.87   0.02
# 1       3 -46.34 99.34  4.99   0.02
# 2       3 -46.34 99.35  5.00   0.02
# 1235    6 -42.42 99.39  5.04   0.02
# 145     5 -43.86 99.48  5.12   0.02
# 5       3 -46.41 99.50  5.14   0.02
# 134     5 -43.97 99.70  5.34   0.01
# 1245    6 -42.66 99.86  5.50   0.01
# 
# Term codes: 
# habitat.y      omnivore_IO      resident_IO    z.body_mass_g z.range_size_km2 
#         1                2                3                4                5 
# 
# Model-averaged coefficients:  
# (full average) 
#                        Estimate Std. Error Adjusted SE z value Pr(>|z|)
# (Intercept)             0.33412    0.32838     0.33425   1.000    0.317
# resident_IOResident    -0.43327    0.32981     0.33515   1.293    0.196
# z.body_mass_g           0.15962    0.20773     0.21023   0.759    0.448
# z.range_size_km2       -0.22200    0.21040     0.21307   1.042    0.297
# omnivore_IOSpecialist   0.00802    0.24395     0.24840   0.032    0.974
# habitat.yOpen woodland -0.03703    0.18628     0.19007   0.195    0.846
# 
# (conditional average) 
#                        Estimate Std. Error Adjusted SE z value Pr(>|z|)  
# (Intercept)             0.33412    0.32839     0.33425   1.000   0.3175  
# resident_IOResident    -0.56032    0.26361     0.27217   2.059   0.0395 *
# z.body_mass_g           0.29948    0.19768     0.20257   1.478   0.1393  
# z.range_size_km2       -0.32239    0.17868     0.18320   1.760   0.0785 .
# omnivore_IOSpecialist   0.02941    0.46649     0.47501   0.062   0.9506  
# habitat.yOpen woodland -0.13887    0.34056     0.34833   0.399   0.6901  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

importance.trait_midday2
#                      resident_IO z.range_size_km2 z.body_mass_g omnivore_IO habitat.y
# Sum of weights:      0.74        0.66             0.54          0.30        0.28     
# N containing models:   16          16               16            16          16   

###LMER version-------------------------------------------------------------------
#Make a global model
traits_global_mid_2way_lmer <- 
  lmer(est ~ z.body_mass_g +
         z.range_size_km2 +
         omnivore_IO +
         resident_IO +
         habitat.y + 
         (1|species),
       data = spp_midday_2way_traits,
       na.action = na.pass)
plot(traits_global_mid_2way_lmer)
qqnorm(resid(traits_global_mid_2way_lmer))
vif(traits_global_mid_2way_lmer)
ranef(traits_global_mid_2way_lmer)

# Compare to a model without random effects
traits_global_mid_2way_lm <- 
  lm(est ~ z.body_mass_g +
       z.range_size_km2 +
       omnivore_IO +
       resident_IO +
       habitat.y,
     data = spp_midday_2way_traits,
     na.action = na.pass)

AIC(traits_global_mid_2way_lmer, traits_global_mid_2way_lm)

##mid2 RVI Plot-------------------------------------------------------------------

variable_mid2 <- c("resident_IO", 
                   "z.range_size_km2", 
                   "z.body_mass_g", 
                   "omnivore_IO", 
                   "habitat.y")
varimport_mid2 <- as.data.frame(importance.trait_midday2)
varimport_mid2$variable_mid2 <- as.factor(variable_mid2)
levels(varimport_mid2$variable_mid2)
varimport_mid2$variable_mid2 <- factor(varimport_mid2$variable_mid2, 
                                       levels = c("resident_IO", 
                                                  "z.range_size_km2", 
                                                  "z.body_mass_g", 
                                                  "omnivore_IO", 
                                                  "habitat.y"))

varimport_mid2$response <- "activity"
colnames(varimport_mid2)[1] <- "importance"

#define labels for x-axis ticks
labs_mid2 <- c("Migratory Habit", 
               "Range Size", 
               "Body Mass", 
               "Trophic Niche",
               "Habitat")

png("./Figures_12-20-24/rvi/voc_rvi_mid2_glm_nice_1.21.25.png", width = 6.0, height = 5.0, units = 'in', res = 600)
rvi_plot <- ggplot(varimport_mid2, aes(y=importance, 
                                       x=variable_mid2,
                                       fill = factor(response))) +
  geom_bar(stat="identity", 
           size = 1, width = 0.8, 
           position = position_dodge(width = 0.9)) +
  scale_fill_manual(values=c("#cc4778")) + #31688e
  scale_x_discrete(labels = labs_asr3) +
  theme(axis.text = element_text(size = 18)) + 
  theme(panel.grid.major = element_line(colour = "white")) + 
  theme(panel.border = element_rect(color = "black", size = 1)) +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_text(size = 16)) +
  theme(axis.text.x = element_text(size = 16, 
                                   angle = 45, 
                                   vjust = 1, 
                                   hjust=1)) +
  theme(legend.position = "none") +
  labs(y = expression("Relative variable importance"),
       title = expression("Two-way GLM Midday"))
#scale_y_continuous(limits = c(0, 1), expand = c(0, 0))
rvi_plot
dev.off()
1+1
##mid2 Scatter/Box Plots----------------------------------------------------------

#MIGRATORY HABIT BOXPLOT
png("./Figures_12-20-24/scatter/mid2_migratory_box_1.21.25.png", width = 5.0, height = 5.0, units = 'in', res = 600)
box_migratory_mid2 <- ggplot(spp_midday_2way_traits, 
                             aes(y = est, 
                                 x = resident_IO,
                                 fill = "#ed7953")) +
  geom_boxplot() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        panel.grid.major = element_line(color = "white"),
        panel.border = element_rect(color = "black", 
                                    size = 1),
        legend.position = "none") +
  labs(y = expression("Estimate"),
       x = expression(""),
       title = expression("2-way Midday Migratory Habit vs. Estimate"))
box_migratory_mid2
dev.off()

#RANGE SIZE SCATTER
png("./Figures_12-20-24/scatter/mid2_rangesize_1.21.25.png", width = 5.0, height = 5.0, units = 'in', res = 600)
scatter_rangesize_mid2 <- ggplot(spp_midday_2way_traits, 
                                 aes(y = est, 
                                     x = range_size_km2/1000000)) +
  geom_point() +
  geom_smooth(method = lm, 
              color = "#0d0887", 
              se = TRUE) +
  scale_x_continuous(labels = scales::comma) +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        panel.grid.major = element_line(color = "white"),
        panel.border = element_rect(color = "black", 
                                    size = 1),
        legend.position = "none") +
  labs(y = expression("Estimate"),
       x = expression("Range Size (millions of km²)"),
       title = expression("2-way Midday Range Size vs. Estimate"))
scatter_rangesize_mid2
dev.off()

#---------------------------------------------------------------------------------
##Three-way Heatmap Prep----------------------------------------------------------
files <- list.files(path = "C:/Users/kirchgrabera/Smithsonian Dropbox/Aidan Kirchgraber/Science and Faith/Aidan/birdsong_heat_project_Aidan/saved_models/midday/threeway/fact", 
                    pattern = "\\.rds$",
                    full.names = TRUE)

for (file in files) {
  oldname <- tools::file_path_sans_ext(basename(file))
  newname <- paste0(substr(oldname, 1, 4), "_ext_midday_hab3")
  assign(newname, readRDS(file)$mod)
}
rm(file, files, newname, oldname)

#get contrasts again
amgo_midday_3way_est <- ext_day.3int.est(amgo_ext_midday_hab3, "ext_day_fact")
amro_midday_3way_est <- ext_day.3int.est(amro_ext_midday_hab3, "ext_day_fact")
blja_midday_3way_est <- ext_day.3int.est(blja_ext_midday_hab3, "ext_day_fact")
cach_midday_3way_est <- ext_day.3int.est(cach_ext_midday_hab3, "ext_day_fact")
cawr_midday_3way_est <- ext_day.3int.est(cawr_ext_midday_hab3, "ext_day_fact")
noca_midday_3way_est <- ext_day.3int.est(noca_ext_midday_hab3, "ext_day_fact")
rewo_midday_3way_est <- ext_day.3int.est(rewo_ext_midday_hab3, "ext_day_fact")
tuti_midday_3way_est <- ext_day.3int.est(tuti_ext_midday_hab3, "ext_day_fact")
#make species columns
amgo_midday_3way_est$contrasts$species <- "American Goldfinch"
amro_midday_3way_est$contrasts$species <- "American Robin"
blja_midday_3way_est$contrasts$species <- "Blue Jay"
cach_midday_3way_est$contrasts$species <- "Carolina Chickadee"
cawr_midday_3way_est$contrasts$species <- "Carolina Wren"
noca_midday_3way_est$contrasts$species <- "Northern Cardinal"
rewo_midday_3way_est$contrasts$species <- "Red-bellied Woodpecker"
tuti_midday_3way_est$contrasts$species <- "Tufted Titmouse"

#combine contrasts across models into df
spp_midday_3way_est <- rbind(amgo_midday_3way_est$contrasts, amro_midday_3way_est$contrasts, 
                             blja_midday_3way_est$contrasts, cach_midday_3way_est$contrasts, 
                             cawr_midday_3way_est$contrasts, noca_midday_3way_est$contrasts, 
                             rewo_midday_3way_est$contrasts, tuti_midday_3way_est$contrasts)

spp_midday_3way_est$habitat <- as.factor(spp_midday_3way_est$contrasts)
spp_midday_3way_est$month <- as.factor(spp_midday_3way_est$contrasts)
levels(spp_midday_3way_est$habitat) <- c("Exurban forest", "Exurban forest", "Exurban forest",
                                         "Exurban open", "Exurban open", "Exurban open",
                                         "Urban forest", "Urban forest", "Urban forest",
                                         "Urban open", "Urban open", "Urban open")

levels(spp_midday_3way_est$month) <- c("June", "July", "August",
                                       "June", "July", "August",
                                       "June", "July", "August",
                                       "June", "July", "August")

#rescale contrasts to limit influence of very large values on visualization
hist(spp_midday_3way_est$mean)
summary(spp_midday_3way_est$mean)
spp_midday_3way_est$est <- ifelse(spp_midday_3way_est$mean >= 2, 2, spp_midday_3way_est$mean)
hist(spp_midday_3way_est$est)

write_csv(spp_midday_3way_est, "C:/Users/kirchgrabera/Smithsonian Dropbox/Aidan Kirchgraber/Science and Faith/Aidan/birdsong_heat_project_Aidan/Data/model_estimates/spp_midday_3way_est.csv")

##Three-way Heatmap---------------------------------------------------------------

png("./Figures_12-20-24/midday/heatmaps/spp_midday_threeway_heatmap_plasma_1-17-25.png", 
    width = 9.4, height = 5.25, units = 'in', res = 600)
heatplot <- ggplot(data = spp_midday_3way_est, aes(x=habitat, y=species, fill= est)) + 
  facet_grid(cols = vars(month)) + #, scales = "free"
  scale_alpha_discrete(range = c(0.60, 1)) +
  geom_tile(aes(alpha =  sig)) + 
  #geom_text(aes(label = sig), color = "black") +
  scale_fill_viridis(discrete=FALSE, option = "C") +
  #scale_x_discrete(labels= c("Exurban forest", "Exurban open", "Urban forest", "Urban open")) + 
  guides(alpha = FALSE) + #, fill = guide_legend(title = "Response to extremes")
  labs(fill = "Response to \n extremes") +
  theme(strip.text = element_text(
    size = 12)) + 
  theme(axis.text = element_text(size = 14)) + 
  theme(axis.text.x=element_text(size = 14, angle=45, hjust=1)) +
  theme(legend.text=element_text(size = 12)) +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank()) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0)) 
#theme(axis.ticks.x = element_blank())+
heatplot
dev.off()

#Analyze trait responses - midday 3-way ------------------------------------------

spp_midday_3way_traits <- merge(spp_midday_3way_est, bdata,
                                by = "species", all.x = TRUE)
dim(spp_midday_3way_traits) #96  17
colnames(spp_midday_3way_traits)

##Standardize variables-----------------------------------------------------------
#center and scale continuous covariates 
spp_midday_3way_traits$z.body_mass_g <- as.numeric(scale(spp_midday_3way_traits$body_mass_g))
spp_midday_3way_traits$z.range_size_km2 <- as.numeric(scale(spp_midday_3way_traits$range_size_km2))

##Order factor levels-------------------------------------------------------------

spp_midday_3way_traits$sig <- as.factor(spp_midday_3way_traits$sig)
levels(spp_midday_3way_traits$sig)
spp_midday_3way_traits$sig <-
  factor(spp_midday_3way_traits$sig,
         levels = c("No", "Yes")) #79N  17Y
summary(spp_midday_3way_traits$sig)

spp_midday_3way_traits$habitat.y <- as.factor(spp_midday_3way_traits$habitat.y)
levels(spp_midday_3way_traits$habitat.y)
spp_midday_3way_traits$habitat.y <-
  factor(spp_midday_3way_traits$habitat.y,
         levels = c("Forest", "Open woodland")) #48Forest  48Open woodland
summary(spp_midday_3way_traits$habitat.y)

spp_midday_3way_traits$nest_loc <- as.factor(spp_midday_3way_traits$nest_loc)
levels(spp_midday_3way_traits$nest_loc)
spp_midday_3way_traits$nest_loc <-
  factor(spp_midday_3way_traits$nest_loc,
         levels = c("Cavity", "Shrub", "Tree"))
summary(spp_midday_3way_traits$nest_loc) #48Cavity  24Shrub  24Tree

spp_midday_3way_traits$migratory_habit <- as.factor(spp_midday_3way_traits$migratory_habit)
levels(spp_midday_3way_traits$migratory_habit)
spp_midday_3way_traits$migratory_habit <-
  factor(spp_midday_3way_traits$migratory_habit,
         levels = c("Resident", "Shortdist"))
summary(spp_midday_3way_traits$migratory_habit) #60Resident  36Shortdist

spp_midday_3way_traits$feeding_behavior <- as.factor(spp_midday_3way_traits$feeding_behavior)
levels(spp_midday_3way_traits$feeding_behavior)
spp_midday_3way_traits$feeding_behavior <-
  factor(spp_midday_3way_traits$feeding_behavior,
         levels = c("Ground forager", "Bark forager", "Foliage gleaner"))
summary(spp_midday_3way_traits$feeding_behavior) #48Ground  12Bark  36Foliage

spp_midday_3way_traits$trophic_niche <- as.factor(spp_midday_3way_traits$trophic_niche)
levels(spp_midday_3way_traits$trophic_niche)
spp_midday_3way_traits$trophic_niche <-
  factor(spp_midday_3way_traits$trophic_niche,
         levels = c("Granivore", "Invertivore", "Omnivore"))
summary(spp_midday_3way_traits$trophic_niche) #12Granivore  48Invertivore  36Omnivore


##Dredge & find most important variables------------------------------------------

#making binary versions of some variables
spp_midday_3way_traits <- spp_midday_3way_traits %>%
  mutate(omnivore_IO = ifelse(trophic_niche == "Omnivore", "Omnivore", "Specialist"),
         resident_IO = ifelse(migratory_habit == "Resident", "Resident", "Migratory"),
         ground_eat_IO = ifelse(feeding_behavior == "Ground forager", 1, 0),
         cavity_nest_IO = ifelse(nest_loc == "Cavity", 1, 0))

#Examine pair plots
vars4 <-
  spp_midday_3way_traits[, c("est", "z.body_mass_g", "z.range_size_km2", "habitat.y", 
                             "nest_loc", "resident_IO", "feeding_behavior", "omnivore_IO")]

pairplot4 <- ggpairs(vars4)
ggsave("midday_3way_globalmodel_condense_20250117.png", plot = pairplot4, dpi = 300, width = 10, height = 10)


#Make a global model
traits_global_midday_3way <- glm(est ~ z.body_mass_g +
                                   z.range_size_km2 +
                                   resident_IO +
                                   habitat.y +
                                   #omnivore_IO,
                                   cavity_nest_IO,
                                 family = gaussian,
                                 data = spp_midday_3way_traits,
                                 na.action = na.pass)
plot(traits_global_midday_3way)
vif(traits_global_midday_3way)

model.set.trait_midday3 <- dredge(traits_global_midday_3way, extra = "adjR^2")
importance.trait_midday3 <- sw(model.set.trait_midday3)
top.models.trait_midday3 <- get.models(model.set.trait_midday3, cumsum(weight) <= 0.95)
model.avg.trait_midday3 <- model.avg(top.models.trait_midday3)

summary(model.avg.trait_midday3)
# model.avg(object = top.models.trait_midday3)
# 
# Component model call: 
# glm(formula = est ~ <21 unique rhs>, family = gaussian, data = spp_midday_3way_traits, na.action = 
#     na.pass)
# 
# Component models: 
#       df  logLik   AICc delta weight
# 4      3 -125.68 257.61  0.00   0.19
# 45     4 -125.26 258.96  1.35   0.10
# 34     4 -125.43 259.31  1.69   0.08
# 24     4 -125.52 259.47  1.86   0.08
# 145    5 -124.48 259.63  2.02   0.07
# 14     4 -125.62 259.68  2.07   0.07
# 134    5 -124.67 260.01  2.40   0.06
# 1234   6 -123.87 260.68  3.07   0.04
# 124    5 -125.13 260.92  3.31   0.04
# 5      3 -127.44 261.14  3.53   0.03
# 245    5 -125.26 261.18  3.57   0.03
# 345    5 -125.26 261.18  3.57   0.03
# 1345   6 -124.15 261.23  3.62   0.03
# 234    5 -125.38 261.42  3.81   0.03
# 1245   6 -124.44 261.81  4.20   0.02
# 25     4 -126.69 261.82  4.21   0.02
# 15     4 -126.88 262.20  4.59   0.02
# 3      3 -128.23 262.71  5.10   0.02
# 12345  7 -123.82 262.92  5.31   0.01
# 35     4 -127.41 263.26  5.65   0.01
# 125    5 -126.36 263.38  5.77   0.01
# 
# Term codes: 
# cavity_nest_IO        habitat.y      resident_IO    z.body_mass_g z.range_size_km2 
#              1                2                3                4                5 
# 
# Model-averaged coefficients:  
# (full average) 
#                        Estimate Std. Error Adjusted SE z value Pr(>|z|)  
# (Intercept)             0.16822    0.18478     0.18661   0.901   0.3674  
# z.body_mass_g           0.23181    0.13245     0.13344   1.737   0.0824 .
# z.range_size_km2        0.06626    0.12749     0.12827   0.517   0.6055  
# resident_IOResident    -0.08081    0.21635     0.21789   0.371   0.7107  
# habitat.yOpen woodland  0.02665    0.15652     0.15784   0.169   0.8659  
# cavity_nest_IO          0.12081    0.25329     0.25486   0.474   0.6355  
# 
# (conditional average) 
#                        Estimate Std. Error Adjusted SE z value Pr(>|z|)  
# (Intercept)             0.16822    0.18478     0.18661   0.901   0.3674  
# z.body_mass_g           0.26140    0.10977     0.11111   2.353   0.0186 *
# z.range_size_km2        0.16575    0.15547     0.15706   1.055   0.2913  
# resident_IOResident    -0.25662    0.32176     0.32503   0.790   0.4298  
# habitat.yOpen woodland  0.09292    0.28152     0.28407   0.327   0.7436  
# cavity_nest_IO          0.32293    0.32591     0.32918   0.981   0.3266  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

importance.trait_midday3
#                      z.body_mass_g z.range_size_km2 cavity_nest_IO resident_IO habitat.y
# Sum of weights:      0.84          0.41             0.38           0.34        0.30     
# N containing models:   16            16               16             16          16   

###LMER version-------------------------------------------------------------------
#Make a global model
traits_global_mid_3way_lmer <- 
  lmer(est ~ z.body_mass_g +
         z.range_size_km2 +
         omnivore_IO +
         resident_IO +
         habitat.y + 
         (1|species),
       data = spp_midday_3way_traits,
       na.action = na.pass)
plot(traits_global_mid_3way_lmer)
qqnorm(resid(traits_global_mid_3way_lmer))
vif(traits_global_mid_3way_lmer)
ranef(traits_global_mid_3way_lmer)

# Compare to a model without random effects
traits_global_mid_3way_lm <- 
  lm(est ~ z.body_mass_g +
       z.range_size_km2 +
       omnivore_IO +
       resident_IO +
       habitat.y,
     data = spp_midday_3way_traits,
     na.action = na.pass)

AIC(traits_global_mid_3way_lmer, traits_global_mid_3way_lm)

##mid3 RVI Plot-------------------------------------------------------------------

variable_mid3 <- c("z.body_mass_g", 
                   "z.range_size_km2", 
                   "cavity_nest_IO", 
                   "resident_IO", 
                   "habitat.y")
varimport_mid3 <- as.data.frame(importance.trait_midday3)
varimport_mid3$variable_mid3 <- as.factor(variable_mid3)
levels(varimport_mid3$variable_mid3)
varimport_mid3$variable_mid3 <- factor(varimport_mid3$variable_mid3, 
                                       levels = c("z.body_mass_g", 
                                                  "z.range_size_km2", 
                                                  "cavity_nest_IO", 
                                                  "resident_IO", 
                                                  "habitat.y"))

varimport_mid3$response <- "activity"
colnames(varimport_mid3)[1] <- "importance"

#define labels for x-axis ticks
labs_mid3 <- c("Body Mass",
               "Range Size", 
               "Nest Location",
               "Migratory Habit",
               "Habitat")

png("./Figures_12-20-24/rvi/voc_rvi_mid3_glm_nice_1.21.25.png", width = 6.0, height = 5.0, units = 'in', res = 600)
rvi_plot <- ggplot(varimport_mid3, aes(y=importance, 
                                       x=variable_mid3,
                                       fill = factor(response))) +
  geom_bar(stat="identity", 
           size = 1, width = 0.8, 
           position = position_dodge(width = 0.9)) +
  scale_fill_manual(values=c("#cc4778")) + 
  scale_x_discrete(labels = labs_mid3) +
  theme(axis.text = element_text(size = 18)) + 
  theme(panel.grid.major = element_line(colour = "white")) + 
  theme(panel.border = element_rect(color = "black", size = 1)) +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_text(size = 16)) +
  theme(axis.text.x = element_text(size = 16, 
                                   angle = 45, 
                                   vjust = 1, 
                                   hjust=1)) +
  theme(legend.position = "none") +
  labs(y = expression("Relative variable importance"),
       title = expression("Three-way GLM Midday"))
#scale_y_continuous(limits = c(0, 1), expand = c(0, 0))
rvi_plot
dev.off()
1+1
##mid3 Scatter Plots--------------------------------------------------------------

#RANGE SIZE SCATTER
png("./Figures_12-20-24/scatter/mid3_rangesize_1.21.25.png", width = 5.0, height = 5.0, units = 'in', res = 600)
scatter_rangesize_mid3 <- ggplot(spp_midday_3way_traits, 
                                 aes(y = est, 
                                     x = range_size_km2/1000000)) +
  geom_point() +
  geom_smooth(method = lm, 
              color = "#0d0887", 
              se = TRUE) +
  scale_x_continuous(labels = scales::comma) +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        panel.grid.major = element_line(color = "white"),
        panel.border = element_rect(color = "black", 
                                    size = 1),
        legend.position = "none") +
  labs(y = expression("Estimate"),
       x = expression("Range Size (millions of km²)"),
       title = expression("3-way Midday Range Size vs. Estimate"))
scatter_rangesize_mid3
dev.off()

#BODY MASS SCATTER
png("./Figures_12-20-24/scatter/mid3_bodymass_1.21.25.png", width = 5.0, height = 5.0, units = 'in', res = 600)
scatter_bodymass_mid3 <- ggplot(spp_midday_3way_traits, 
                                aes(y = est, 
                                    x = body_mass_g)) +
  geom_point() +
  geom_smooth(method = lm, 
              color = "#0d0887", 
              se = TRUE) +
  scale_x_continuous(labels = scales::comma) +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        panel.grid.major = element_line(color = "white"),
        panel.border = element_rect(color = "black", 
                                    size = 1),
        legend.position = "none") +
  labs(y = expression("Estimate"),
       x = expression("Body Mass (g)"),
       title = expression("3-way Midday Body Mass vs. Estimate"))
scatter_bodymass_mid3
dev.off()

#---------------------------------------------------------------------------------
#Extract effects from additive models---------------------------------------------
files <- list.files(path = "C:/Users/kirchgrabera/Smithsonian Dropbox/Aidan Kirchgraber/Science and Faith/Aidan/birdsong_heat_project_Aidan/saved_models/asr/additive", 
                    pattern = "\\.rds$",
                    full.names = TRUE)

for (file in files) {
  oldname <- tools::file_path_sans_ext(basename(file))
  newname <- paste0(substr(oldname, 1, 4), "_ext_day_add2")
  assign(newname, readRDS(file)$mod)
}
rm(file, files, newname, oldname)

summary(amgo_ext_day_add2)

# fixef(amgo_ext_day_add2, # return coefficients from additive model
#       pars = c("Intercept", "ext_day_factextreme", "loc_haburbanforest", 
#                "loc_habexurbanopen", "loc_habexurbanforest"))


#---------------------------------------------------------------------------------
#Make other plots----------------------------------------------------------------------

#This plot setup is for examining effects of continuous covariates (e.g., vpd)
ce <- conditional_effects(cawr_ext_day_hab, plot = FALSE)
str(ce)
ce_temp <- ce$'ext_day_fact:loc_hab'
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
  geom_ribbon(data = ce_temp, aes(cawr_asr, estimate__, ymin=lower__, ymax=upper__), fill = "#dd5e66ff", alpha=0.25) +
  geom_line(data = ce_temp, aes(cawr_asr, estimate__,), color = "#dd5e66ff", size = 1.15) +
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
  labs(x = expression("CAWR_asr")) +
  #labs(y = expression("Counts of C. aestivus"))
  #labs(y = expression("Counts of P. stygicus"))
  labs(y = expression("Probability of calling"))
effect_plot
dev.off()


#This extract effects
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

#---------------------------------------------------------------------------------
#TESTING: sum of focal spp detections --------------------------------------------
dat_1 <- dat_1 %>%
  mutate(bird_n = # do these need to be scaled in order to be useful?
           amgo_n + amro_n + blja_n + cach_n + cawr_n + 
           hawo_n + noca_n + rewo_n + tuti_n + woth_n)

# box exploratory
ggplot(dat_1, 
       aes(y = bird_n, 
           x = loc_hab,
           fill = "#ed7953")) +
  geom_boxplot() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        panel.grid.major = element_line(color = "white"),
        panel.border = element_rect(color = "black", 
                                    size = 1),
        legend.position = "none")

# how many of the focal species were present in a given row (ie. on a given site on a given day)
dat_1$species_present <- rowSums(dat_1[, grep("_asr$", names(dat_1))] != 0)

# what is the average # species present at each site type
dat_2 <- dat_1 %>%
  group_by(loc_hab) %>%
  summarise(
    mean_spp = mean(species_present, na.rm = TRUE),
    sd_spp = sd(species_present, na.rm = TRUE),
    n = n()
  )
dat_3 <- dat_1 %>%
  group_by(sites) %>%
  summarise(
    mean_spp = mean(species_present, na.rm = TRUE),
    sd_spp = sd(species_present, na.rm = TRUE),
    n = n()
  )

presence_hab <- brm(
  species_present ~ loc_hab,
  data = dat_1,
  family = gaussian(),
  chains = 4,
  iter = 2000,
  warmup = 500
)

summary(presence_hab)
library(emmeans)
emmeans::emmeans(presence_hab, 
                 pairwise ~ loc_hab)





#Generate nice tables of model coefficients for manuscript ----------------------
library(dplyr)

amgo_asr3_table <- amgo_asr_3way_est$contrasts %>%
  select(c("mean", "LCI", "UCI"))
colnames(amgo_asr3_table)[1] <- "Mean"
colnames(amgo_asr3_table)[2] <- "95% LCI"
colnames(amgo_asr3_table)[3] <- "95% UCI"
amgo_asr3_table$Variable <- c("Urban Open June", "Urban Open July", "Urban Open August",
                              "Urban Forest June", "Urban Forest July", "Urban Forest August",
                              "Exurban Open June", "Exurban Open July", "Exurban Open August",
                              "Exurban Forest June", "Exurban Forest July", "Exurban Forest August")
colnames(amgo_asr3_table)
amgo_asr3_table <- amgo_asr3_table[ , c(4, 1:3)]
install.packages("writexl")
#write xlsx file
library(writexl)
#write_xlsx(amgo_asr3_table, "./Data/model_estimates//amgo_asr3_table_2-26-25.xlsx")
