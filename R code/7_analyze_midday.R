###########################################################################################################################################################
### (7) Analyses of bird detection data during midday hours########################################################################################################
########################################################################################################################################################
###updated 1-7-24

#To do:
#(1) Need to go back to script 3 and subset by confidence level there - done
#(2) Analyze with high VPD/WBT - done
#(2) Roll into functions - done
#(3) Calculate posterior contrasts among groups and add to functions - done
#(4) Aidan will have to remake all 3-way interaction figures bc
#    I found an indexing error in the function for making the plotting df     


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
#dat <- read.csv("Data/analysis_data/temp_soundscape_bird_data_2024_summaries_12.11.24.csv") #bird detections filtered by confidence thresholds
dat <- read.csv("Data/analysis_data/temp_soundscape_bird_data_2024_summaries_1.7.25.csv") #bird detections filtered by confidence thresholds, now with three additional spp

dim(dat) #2176   154

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
dim(dat_1) #1476  159

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
#   path = "./figures_12-4-24/"
# )

# "amgo_mid"       "amro_mid"       "blja_mid"       "cach_mid"      
# "cawr_mid"       "noca_mid"       "rewo_mid"       "tuti_mid" 

#American goldfinch-----------------------------------------------------------

aggregate(dat_1$amgo_mid, by=list(dat_1$month_fact, dat_1$loc_hab, dat_1$ext_day_fact), FUN=sum, na.rm = TRUE)
aggregate(dat_1$amgo_mid, by=list(dat_1$loc_hab, dat_1$ext_day_fact), FUN=sum, na.rm = TRUE)


#additive model
amgo_ext_day_add <-
  bird.model("amgo_mid",
             "ext_day_fact+loc_hab+month_fact",
             dat_1,
             "1-2-25",
             path = "./saved_models/midday/additive/")
#Extreme temp not sig. more common in open habitats

amgo_ext_plt <-
  bird_plot(
    amgo_ext_day_add,
    "amgo_mid",
    "ext_day_fact",
    save = TRUE,
    date = "1-2-25",
    path = "./Figures_12-4-24/midday/additive/"
  )

amgo_hab_plt <-
  bird_plot(
    amgo_ext_day_add,
    "amgo_mid",
    "loc_hab",
    save = TRUE,
    date = "1-2-25",
    path = "./figures_12-4-24/midday/additive/"
  )

amgo_mon_plt <-
  bird_plot(amgo_ext_day_add, "amgo_mid", "month_fact")
amgo_ndsi_plt <- bird_plot(amgo_ext_day_add, "amgo_mid", "z.ndsi")

#2-way interaction between extreme*habitat
amgo_ext_day_hab <-
  bird.model("amgo_mid",
             "ext_day_fact*loc_hab+month_fact",
             dat_1,
             "1-2-25",
             path = "./saved_models/midday/two-way interactions/")
#View contrasts
ext_day.2int.est(amgo_ext_day_hab, "ext_day_fact")
#No significant contrasts between ext and not extreme for each habitat type

amgo_ext_hab_plt <-
  bird_plot(
    amgo_ext_day_hab,
    "amgo_mid",
    "ext_day_fact:loc_hab",
    save = TRUE,
    date = "1-2-25",
    path = "./figures_12-4-24/midday/two-way interactions/ext_day_fact"
  )

#2-way interaction between extreme_vpd*habitat
amgo_ext_vpd_hab <-
  bird.model("amgo_mid",
             "ext_day_vpd*loc_hab+month_fact",
             dat_1,
             "1-2-25",
             path = "./saved_models/midday/two-way interactions/")

#View contrasts
ext_day.2int.est(amgo_ext_vpd_hab, "ext_day_vpd")
#No significant contrasts between ext and not extreme for each habitat type

amgo_extvpd_hab_plt <-
  bird_plot(
    amgo_ext_vbd_hab,
    "amgo_mid",
    "ext_day_vpd:loc_hab",
    save = TRUE,
    date = "1-2-25",
    path = "./figures_12-4-24/midday/two-way interactions/ext_day_vpd"
  )

#2-way interaction between extreme_wbt*habitat
amgo_ext_wbt_hab <-
  bird.model("amgo_mid",
             "ext_day_wbt*loc_hab+month_fact",
             dat_1,
             "1-2-25",
             path = "./saved_models/midday/two-way interactions/")

#View contrasts
ext_day.2int.est(amgo_ext_wbt_hab, "ext_day_wbt")
#More vocalizations during extreme temps in exurban forest

amgo_extwbt_hab_plt <-
  bird_plot(
    amgo_ext_wbt_hab,
    "amgo_mid",
    "ext_day_wbt:loc_hab",
    save = TRUE,
    date = "1-2-25",
    path = "./figures_12-4-24/midday/two-way interactions/ext_day_wbt"
  )


#American robin------------------------------------------------------------------

aggregate(dat_1$amro_mid, by=list(dat_1$month_fact, dat_1$loc_hab, dat_1$ext_day_fact), FUN=sum, na.rm = TRUE)
aggregate(dat_1$amro_mid, by=list(dat_1$loc_hab, dat_1$ext_day_fact), FUN=sum, na.rm = TRUE)

#additive model
amro_ext_day_add <-
  bird.model("amro_mid",
             "ext_day_fact+loc_hab+month_fact",
             dat_1,
             "1-3-25",
             path = "./saved_models/midday/additive/")
#For amro, extreme temp not sig. More common in urban habitat 

amro_ext_plt <-
  bird_plot(
    amro_ext_day_add,
    "amro_mid",
    "ext_day_fact",
    save = TRUE,
    date = "1-3-25",
    path = "./Figures_12-4-24/midday/additive/"
  )

amro_hab_plt <-
  bird_plot(
    amro_ext_day_add,
    "amro_mid",
    "loc_hab",
    save = TRUE,
    date = "1-3-25",
    path = "./figures_12-4-24/midday/additive/"
  )

amro_mon_plt <-
  bird_plot(amro_ext_day_add, "amro_mid", "month_fact")
amro_ndsi_plt <- bird_plot(amro_ext_day_add, "amro_mid", "z.ndsi")

#2-way interaction between extreme*habitat
amro_ext_day_hab <-
  bird.model("amro_mid",
             "ext_day_fact*loc_hab+month_fact",
             dat_1,
             "1-3-25",
             path = "./saved_models/midday/two-way interactions/")
#View contrasts
ext_day.2int.est(amro_ext_day_hab, "ext_day_fact")
#For amro, significantly less vocalization during extreme temps in urban open habitats

amro_ext_hab_plt <-
  bird_plot(
    amro_ext_day_hab,
    "amro_mid",
    "ext_day_fact:loc_hab",
    save = TRUE,
    date = "1-3-25",
    path = "./figures_12-4-24/midday/two-way interactions/ext_day_fact"
  )

#2-way interaction between extreme_vpd*habitat
amro_ext_vpd_hab <-
  bird.model("amro_mid",
             "ext_day_vpd*loc_hab+month_fact",
             dat_1,
             "1-3-25",
             path = "./saved_models/midday/two-way interactions/")

#View contrasts
ext_day.2int.est(amro_ext_vpd_hab, "ext_day_vpd")
#For amro, sig less vocalization in urban open during extremes w/high vpd

amro_extvpd_hab_plt <-
  bird_plot(
    amro_ext_vpd_hab,
    "amro_mid",
    "ext_day_vpd:loc_hab",
    save = TRUE,
    date = "1-3-25",
    path = "./figures_12-4-24/midday/two-way interactions/ext_day_vpd"
  )

#2-way interaction between extreme_wbt*habitat
amro_ext_wbt_hab <-
  bird.model("amro_mid",
             "ext_day_wbt*loc_hab+month_fact",
             dat_1,
             "1-3-25",
             path = "./saved_models/midday/two-way interactions/")

#View contrasts
ext_day.2int.est(amro_ext_wbt_hab, "ext_day_wbt")
#for Amro, More vocalizations during extreme temps in urban open only, not high wbt

amro_extwbt_hab_plt <-
  bird_plot(
    amro_ext_wbt_hab,
    "amro_mid",
    "ext_day_wbt:loc_hab",
    save = TRUE,
    date = "1-3-25",
    path = "./figures_12-4-24/midday/two-way interactions/ext_day_wbt"
  )


#Blue Jay-----------------------------------------------------------------------

aggregate(dat_1$blja_mid, by=list(dat_1$month_fact, dat_1$loc_hab, dat_1$ext_day_fact), FUN=sum, na.rm = TRUE)
aggregate(dat_1$blja_mid, by=list(dat_1$loc_hab, dat_1$ext_day_fact), FUN=sum, na.rm = TRUE)


#additive model
blja_ext_day_add <-
  bird.model("blja_mid",
             "ext_day_fact+loc_hab+month_fact",
             dat_1,
             "1-3-25",
             path = "./saved_models/midday/additive/")
#For blja, extreme temp IS sig. 

blja_ext_plt <-
  bird_plot(
    blja_ext_day_add,
    "blja_mid",
    "ext_day_fact",
    save = TRUE,
    date = "1-3-25",
    path = "./Figures_12-4-24/midday/additive/"
  )

blja_hab_plt <-
  bird_plot(
    blja_ext_day_add,
    "blja_mid",
    "loc_hab",
    save = TRUE,
    date = "1-3-25",
    path = "./figures_12-4-24/midday/additive/"
  )

blja_mon_plt <-
  bird_plot(blja_ext_day_add, "blja_mid", "month_fact")
blja_ndsi_plt <- bird_plot(blja_ext_day_add, "blja_mid", "z.ndsi")

#2-way interaction between extreme*habitat
blja_ext_day_hab <-
  bird.model("blja_mid",
             "ext_day_fact*loc_hab+month_fact",
             dat_1,
             "1-3-25",
             path = "./saved_models/midday/two-way interactions/")
#View contrasts
ext_day.2int.est(blja_ext_day_hab, "ext_day_fact")
#For blja, significantly less vocalization during extreme temps in exurban forest habitats

blja_ext_hab_plt <-
  bird_plot(
    blja_ext_day_hab,
    "blja_mid",
    "ext_day_fact:loc_hab",
    save = TRUE,
    date = "1-3-25",
    path = "./figures_12-4-24/midday/two-way interactions/ext_day_fact"
  )

#2-way interaction between extreme_vpd*habitat
blja_ext_vpd_hab <-
  bird.model("blja_mid",
             "ext_day_vpd*loc_hab+month_fact",
             dat_1,
             "1-3-25",
             path = "./saved_models/midday/two-way interactions/")

#View contrasts
ext_day.2int.est(blja_ext_vpd_hab, "ext_day_vpd")
#For blja, sig less vocalization in exurban forest during extremes but not w/high vpd

blja_extvpd_hab_plt <-
  bird_plot(
    blja_ext_vpd_hab,
    "blja_mid",
    "ext_day_vpd:loc_hab",
    save = TRUE,
    date = "1-3-25",
    path = "./figures_12-4-24/midday/two-way interactions/ext_day_vpd"
  )

#2-way interaction between extreme_wbt*habitat
blja_ext_wbt_hab <-
  bird.model("blja_mid",
             "ext_day_wbt*loc_hab+month_fact",
             dat_1,
             "1-3-25",
             path = "./saved_models/midday/two-way interactions/")

#View contrasts
ext_day.2int.est(blja_ext_wbt_hab, "ext_day_wbt")
#for blja, More vocalizations during extreme temps in exurban forest only, not high wbt

blja_extwbt_hab_plt <-
  bird_plot(
    blja_ext_wbt_hab,
    "blja_mid",
    "ext_day_wbt:loc_hab",
    save = TRUE,
    date = "1-3-25",
    path = "./figures_12-4-24/midday/two-way interactions/ext_day_wbt"
  )



#Carolina Chickadee------------------------------------------------------------------

aggregate(dat_1$cach_mid, by=list(dat_1$month_fact, dat_1$loc_hab, dat_1$ext_day_fact), FUN=sum, na.rm = TRUE)
aggregate(dat_1$cach_mid, by=list(dat_1$loc_hab, dat_1$ext_day_fact), FUN=sum, na.rm = TRUE)


#additive model
cach_ext_day_add <-
  bird.model("cach_mid",
             "ext_day_fact+loc_hab+month_fact",
             dat_1,
             "1-4-25",
             path = "./saved_models/midday/additive/")
#For cach, overall increase in calling during extremes. More common in forest habitat

cach_ext_plt <-
  bird_plot(
    cach_ext_day_add,
    "cach_mid",
    "ext_day_fact",
    save = TRUE,
    date = "1-4-25",
    path = "./Figures_12-4-24/midday/additive/"
  )

cach_hab_plt <-
  bird_plot(
    cach_ext_day_add,
    "cach_mid",
    "loc_hab",
    save = TRUE,
    date = "1-3-25",
    path = "./figures_12-4-24/midday/additive/"
  )

cach_mon_plt <-
  bird_plot(cach_ext_day_add, "cach_mid", "month_fact")
cach_ndsi_plt <- bird_plot(cach_ext_day_add, "cach_mid", "z.ndsi")

#2-way interaction between extreme*habitat
cach_ext_day_hab <-
  bird.model("cach_mid",
             "ext_day_fact*loc_hab+month_fact",
             dat_1,
             "1-4-25",
             path = "./saved_models/midday/two-way interactions/")
#View contrasts
ext_day.2int.est(cach_ext_day_hab, "ext_day_fact")
#For cach, significantly more vocalization during extreme temps in urban forest habitats

cach_ext_hab_plt <-
  bird_plot(
    cach_ext_day_hab,
    "cach_mid",
    "ext_day_fact:loc_hab",
    save = TRUE,
    date = "1-4-25",
    path = "./figures_12-4-24/midday/two-way interactions/ext_day_fact"
  )

#2-way interaction between extreme_vpd*habitat
cach_ext_vpd_hab <-
  bird.model("cach_mid",
             "ext_day_vpd*loc_hab+month_fact",
             dat_1,
             "1-4-25",
             path = "./saved_models/midday/two-way interactions/")

#View contrasts
ext_day.2int.est(cach_ext_vpd_hab, "ext_day_vpd")
#For cach, sig more vocalization in urban forest during extremes w/high vpd
#and more vocalization during extremes in exurban forest, but not during high vpd

cach_extvpd_hab_plt <-
  bird_plot(
    cach_ext_vpd_hab,
    "cach_mid",
    "ext_day_vpd:loc_hab",
    save = TRUE,
    date = "1-4-25",
    path = "./figures_12-4-24/midday/two-way interactions/ext_day_vpd"
  )

#2-way interaction between extreme_wbt*habitat
cach_ext_wbt_hab <-
  bird.model("cach_mid",
             "ext_day_wbt*loc_hab+month_fact",
             dat_1,
             "1-4-25",
             path = "./saved_models/midday/two-way interactions/")

#View contrasts
ext_day.2int.est(cach_ext_wbt_hab, "ext_day_wbt")
#For cach, sig more vocalization in urban forest during extremes w/high wbt
#and more vocalization during extremes in exurban forest, but not during high wbt

cach_extwbt_hab_plt <-
  bird_plot(
    cach_ext_wbt_hab,
    "cach_mid",
    "ext_day_wbt:loc_hab",
    save = TRUE,
    date = "1-4-25",
    path = "./figures_12-4-24/midday/two-way interactions/ext_day_wbt"
  )


#Carolina Wren------------------------------------------------------------------

aggregate(dat_1$cawr_mid, by=list(dat_1$month_fact, dat_1$loc_hab, dat_1$ext_day_fact), FUN=sum, na.rm = TRUE)
aggregate(dat_1$cawr_mid, by=list(dat_1$loc_hab, dat_1$ext_day_fact), FUN=sum, na.rm = TRUE)


#additive model
cawr_ext_day_add <-
  bird.model("cawr_mid",
             "ext_day_fact+loc_hab+month_fact",
             dat_1,
             "1-5-25",
             path = "./saved_models/midday/additive/")
#For cawr, no overall difference in calling during extremes.

cawr_ext_plt <-
  bird_plot(
    cawr_ext_day_add,
    "cawr_mid",
    "ext_day_fact",
    save = TRUE,
    date = "1-5-25",
    path = "./Figures_12-4-24/midday/additive/"
  )

cawr_hab_plt <-
  bird_plot(
    cawr_ext_day_add,
    "cawr_mid",
    "loc_hab",
    save = TRUE,
    date = "1-5-25",
    path = "./figures_12-4-24/midday/additive/"
  )

cawr_mon_plt <-
  bird_plot(cawr_ext_day_add, "cawr_mid", "month_fact")
cawr_ndsi_plt <- bird_plot(cawr_ext_day_add, "cawr_mid", "z.ndsi")

#2-way interaction between extreme*habitat
cawr_ext_day_hab <-
  bird.model("cawr_mid",
             "ext_day_fact*loc_hab+month_fact",
             dat_1,
             "1-5-25",
             path = "./saved_models/midday/two-way interactions/")
#View contrasts
ext_day.2int.est(cawr_ext_day_hab$mod, "ext_day_fact")
#For cawr, no differences during extreme temps in any habitat

cawr_ext_hab_plt <-
  bird_plot(
    cawr_ext_day_hab$mod,
    "cawr_mid",
    "ext_day_fact:loc_hab",
    save = FALSE,
    date = "1-5-25",
    path = "./figures_12-4-24/midday/two-way interactions/ext_day_fact"
  )

#2-way interaction between extreme_vpd*habitat
cawr_ext_vpd_hab <-
  bird.model("cawr_mid",
             "ext_day_vpd*loc_hab+month_fact",
             dat_1,
             "1-5-25",
             path = "./saved_models/midday/two-way interactions/")

#View contrasts
ext_day.2int.est(cawr_ext_vpd_hab, "ext_day_vpd")
#For cawr, sig more vocalization in urban open during extremes, but not during high vpd

cawr_extvpd_hab_plt <-
  bird_plot(
    cawr_ext_vpd_hab,
    "cawr_mid",
    "ext_day_vpd:loc_hab",
    save = TRUE,
    date = "1-5-25",
    path = "./figures_12-4-24/midday/two-way interactions/ext_day_vpd"
  )

#2-way interaction between extreme_wbt*habitat
cawr_ext_wbt_hab <-
  bird.model("cawr_mid",
             "ext_day_wbt*loc_hab+month_fact",
             dat_1,
             "1-5-25",
             path = "./saved_models/midday/two-way interactions/")

#View contrasts
ext_day.2int.est(cawr_ext_wbt_hab, "ext_day_wbt")
#For cawr, no difference for any habitat, high wbt or no

cawr_extwbt_hab_plt <-
  bird_plot(
    cawr_ext_wbt_hab,
    "cawr_mid",
    "ext_day_wbt:loc_hab",
    save = TRUE,
    date = "1-5-25",
    path = "./figures_12-4-24/midday/two-way interactions/ext_day_wbt"
  )



#Eastern Towhee------------------------------------------------------------------

table(dat_1$month_fact, dat_1$loc_hab, dat_1$ext_day_wbt)
aggregate(dat_1$eato_mid, by=list(dat_1$month_fact, dat_1$loc_hab, dat_1$ext_day_fact), FUN=sum, na.rm = TRUE)
aggregate(dat_1$eato_mid, by=list(dat_1$loc_hab, dat_1$ext_day_fact), FUN=sum, na.rm = TRUE)

#!data for EATO only support not extreme-extreme comparisons, not across habitats

#additive model
eato_ext_day_add <-
  bird.model("eato_mid",
             "ext_day_fact+month_fact", #excluded loc_hab - too many 0s
             dat_1,
             "1-7-25",
             path = "./saved_models/midday/additive/")
#For eato, no overall difference in calling during extremes.

eato_ext_plt <-
  bird_plot(
    eato_ext_day_add,
    "eato_mid",
    "ext_day_fact",
    save = TRUE,
    date = "1-7-25",
    path = "./figures_12-4-24/midday/additive/"
  )

# eato_hab_plt <-
#   bird_plot(
#     eato_ext_day_add,
#     "eato_mid",
#     "loc_hab",
#     save = TRUE,
#     date = "1-7-25",
#     path = "./figures_12-4-24/midday/additive/"
#   )

eato_mon_plt <-
  bird_plot(eato_ext_day_add, "eato_mid", "month_fact")
eato_ndsi_plt <- bird_plot(eato_ext_day_add, "eato_mid", "z.ndsi")


#Hairy woodpecker----------------------------------------------------------------

aggregate(dat_1$hawo_mid, by=list(dat_1$month_fact, dat_1$loc_hab, dat_1$ext_day_fact), FUN=sum, na.rm = TRUE)
aggregate(dat_1$hawo_mid, by=list(dat_1$loc_hab, dat_1$ext_day_fact), FUN=sum, na.rm = TRUE)
aggregate(dat_1$hawo_mid, by=list(dat_1$loc_hab, dat_1$ext_day_vpd), FUN=sum, na.rm = TRUE)
aggregate(dat_1$hawo_mid, by=list(dat_1$ext_day_wbt), FUN=sum, na.rm = TRUE)

#Data support analyses across habitats but not across months and not across high vpd/wbt

#additive model
hawo_ext_day_add <-
  bird.model("hawo_mid",
             "ext_day_fact+loc_hab+month_fact",
             dat_1,
             "1-7-25",
             path = "./saved_models/midday/additive/")
#For hawo, no sig overall effect of extremes

hawo_ext_plt <-
  bird_plot(
    hawo_ext_day_add,
    "hawo_mid",
    "ext_day_fact",
    save = TRUE,
    date = "1-7-25",
    path = "./Figures_12-4-24/midday/additive/"
  )

hawo_hab_plt <-
  bird_plot(
    hawo_ext_day_add,
    "hawo_mid",
    "loc_hab",
    save = TRUE,
    date = "1-7-25",
    path = "./figures_12-4-24/midday/additive/"
  )

hawo_mon_plt <-
  bird_plot(hawo_ext_day_add, "hawo_mid", "month_fact")
hawo_ndsi_plt <- bird_plot(hawo_ext_day_add, "hawo_mid", "z.ndsi")


#additive models of vpd and wbt
hawo_ext_wbt_add <-
  bird.model("hawo_mid",
             "ext_day_wbt+loc_hab+month_fact",
             dat_1,
             "1-7-25",
             path = "./saved_models/midday/additive/")
#For hawo, no sig overall effect of extremes

hawo_ext_vpd_add <-
  bird.model("hawo_mid",
             "ext_day_vpd+loc_hab+month_fact",
             dat_1,
             "1-7-25",
             path = "./saved_models/midday/additive/")
#For hawo, no sig overall effect of extremes

hawo_ext_wbt_plt <-
  bird_plot(
    hawo_ext_wbt_add,
    "hawo_mid",
    "ext_day_wbt",
    save = FALSE,
    date = "1-7-25",
    path = "./Figures_12-4-24/midday/additive/"
  )

hawo_ext_vpd_plt <-
  bird_plot(
    hawo_ext_vpd_add,
    "hawo_mid",
    "ext_day_vpd",
    save = FALSE,
    date = "1-7-25",
    path = "./Figures_12-4-24/midday/additive/"
  )

#2-way interaction between extreme*habitat
hawo_ext_day_hab <-
  bird.model("hawo_mid",
             "ext_day_fact*loc_hab+month_fact",
             dat_1,
             "1-7-25",
             path = "./saved_models/midday/two-way interactions/")
#View contrasts
ext_day.2int.est(hawo_ext_day_hab, "ext_day_fact")
#For hawo, no significant differences

hawo_ext_hab_plt <-
  bird_plot(
    hawo_ext_day_hab,
    "hawo_mid",
    "ext_day_fact:loc_hab",
    save = TRUE,
    date = "1-7-25",
    path = "./figures_12-4-24/midday/two-way interactions/ext_day_fact"
  )


#Northern Cardinal------------------------------------------------------------------

#additive model
noca_ext_day_add <-
  bird.model("noca_mid",
             "ext_day_fact+loc_hab+month_fact",
             dat_1,
             "1-5-25",
             path = "./saved_models/midday/additive/")
#For noca, no overall difference in calling during extremes.

noca_ext_plt <-
  bird_plot(
    noca_ext_day_add,
    "noca_mid",
    "ext_day_fact",
    save = TRUE,
    date = "1-5-25",
    path = "./Figures_12-4-24/midday/additive/"
  )

noca_hab_plt <-
  bird_plot(
    noca_ext_day_add,
    "noca_mid",
    "loc_hab",
    save = TRUE,
    date = "1-5-25",
    path = "./figures_12-4-24/midday/additive/"
  )

noca_mon_plt <-
  bird_plot(noca_ext_day_add, "noca_mid", "month_fact")
noca_ndsi_plt <- bird_plot(noca_ext_day_add, "noca_mid", "z.ndsi")

#2-way interaction between extreme*habitat
noca_ext_day_hab <-
  bird.model("noca_mid",
             "ext_day_fact*loc_hab+month_fact",
             dat_1,
             "1-5-25",
             path = "./saved_models/midday/two-way interactions/")
#View contrasts
ext_day.2int.est(noca_ext_day_hab, "ext_day_fact")
#For noca, no differences during extreme temps in any habitat

noca_ext_hab_plt <-
  bird_plot(
    noca_ext_day_hab,
    "noca_mid",
    "ext_day_fact:loc_hab",
    save = TRUE,
    date = "1-5-25",
    path = "./figures_12-4-24/midday/two-way interactions/ext_day_fact"
  )

#2-way interaction between extreme_vpd*habitat
noca_ext_vpd_hab <-
  bird.model("noca_mid",
             "ext_day_vpd*loc_hab+month_fact",
             dat_1,
             "1-5-25",
             path = "./saved_models/midday/two-way interactions/")

#View contrasts
ext_day.2int.est(noca_ext_vpd_hab, "ext_day_vpd")
#For noca, no differences during extreme temps in any habitat, high vpd or no

noca_extvpd_hab_plt <-
  bird_plot(
    noca_ext_vpd_hab,
    "noca_mid",
    "ext_day_vpd:loc_hab",
    save = TRUE,
    date = "1-5-25",
    path = "./figures_12-4-24/midday/two-way interactions/ext_day_vpd"
  )

#2-way interaction between extreme_wbt*habitat
noca_ext_wbt_hab <-
  bird.model("noca_mid",
             "ext_day_wbt*loc_hab+month_fact",
             dat_1,
             "1-5-25",
             path = "./saved_models/midday/two-way interactions/")

#View contrasts
ext_day.2int.est(noca_ext_wbt_hab, "ext_day_wbt")
#For noca, no difference for any habitat, high wbt or no

noca_extwbt_hab_plt <-
  bird_plot(
    noca_ext_wbt_hab,
    "noca_mid",
    "ext_day_wbt:loc_hab",
    save = TRUE,
    date = "1-5-25",
    path = "./figures_12-4-24/midday/two-way interactions/ext_day_wbt"
  )



#Red bellied woodpecker-----------------------------------------------------------

#additive model
rewo_ext_day_add <-
  bird.model("rewo_mid",
             "ext_day_fact+loc_hab+month_fact",
             dat_1,
             "12-23-24",
             path = "./saved_models/midday/additive/")

rewo_ext_plt <-
  bird_plot(
    rewo_ext_day_add,
    "rewo_mid",
    "ext_day_fact",
    save = TRUE,
    date = "12-23-24",
    path = "./Figures_12-4-24/midday/additive/"
  )

rewo_hab_plt <-
  bird_plot(
    rewo_ext_day_add,
    "rewo_mid",
    "loc_hab",
    save = TRUE,
    date = "12-23-24",
    path = "./figures_12-4-24/midday/additive/"
  )

rewo_mon_plt <-
  bird_plot(rewo_ext_day_add, "rewo_mid", "month_fact")
rewo_ndsi_plt <- bird_plot(rewo_ext_day_add, "rewo_mid", "z.ndsi")

#2-way interaction between extreme*habitat
rewo_ext_day_hab <-
  bird.model("rewo_mid",
             "ext_day_fact*loc_hab+month_fact",
             dat_1,
             "1-7-25",
             path = "./saved_models/midday/two-way interactions/")
#View contrasts
ext_day.2int.est(rewo_ext_day_hab, "ext_day_fact")

rewo_ext_hab_plt <-
  bird_plot(
    rewo_ext_day_hab,
    "rewo_mid",
    "ext_day_fact:loc_hab",
    save = TRUE,
    date = "1-7-25",
    path = "./figures_12-4-24/midday/two-way interactions/ext_day_fact"
  )

#2-way interaction between extreme_vpd*habitat
rewo_ext_vpd_hab <-
  bird.model("rewo_mid",
             "ext_day_vpd*loc_hab+month_fact",
             dat_1,
             "12-23-24",
             path = "./saved_models/midday/two-way interactions/")

#View contrasts
ext_day.2int.est(rewo_ext_vpd_hab, "ext_day_vpd")

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
             "12-23-24",
             path = "./saved_models/midday/two-way interactions/")

#View contrasts
ext_day.2int.est(rewo_ext_wbt_hab, "ext_day_wbt")

rewo_extwbt_hab_plt <-
  bird_plot(
    rewo_ext_wbt_hab,
    "rewo_mid",
    "ext_day_wbt:loc_hab",
    save = TRUE,
    date = "12-23-24",
    path = "./figures_12-4-24/midday/two-way interactions/ext_day_wbt"
  )


#Tufted titmouse------------------------------------------------------------------

#additive model
tuti_ext_day_add <-
  bird.model("tuti_mid",
             "ext_day_fact+loc_hab+month_fact",
             dat_1,
             "1-6-25",
             path = "./saved_models/midday/additive/")
#For tuti, no overall difference in calling during extremes.

tuti_ext_plt <-
  bird_plot(
    tuti_ext_day_add,
    "tuti_mid",
    "ext_day_fact",
    save = TRUE,
    date = "1-6-25",
    path = "./Figures_12-4-24/midday/additive/"
  )

tuti_hab_plt <-
  bird_plot(
    tuti_ext_day_add,
    "tuti_mid",
    "loc_hab",
    save = TRUE,
    date = "1-6-25",
    path = "./figures_12-4-24/midday/additive/"
  )

tuti_mon_plt <-
  bird_plot(tuti_ext_day_add, "tuti_mid", "month_fact")
tuti_ndsi_plt <- bird_plot(tuti_ext_day_add, "tuti_mid", "z.ndsi")

#2-way interaction between extreme*habitat
tuti_ext_day_hab <-
  bird.model("tuti_mid",
             "ext_day_fact*loc_hab+month_fact",
             dat_1,
             "1-6-25",
             path = "./saved_models/midday/two-way interactions/")
#View contrasts
ext_day.2int.est(tuti_ext_day_hab, "ext_day_fact")
#For tuti, sig more vocalization during extreme temps in urban forest

tuti_ext_hab_plt <-
  bird_plot(
    tuti_ext_day_hab,
    "tuti_mid",
    "ext_day_fact:loc_hab",
    save = TRUE,
    date = "1-6-25",
    path = "./figures_12-4-24/midday/two-way interactions/ext_day_fact"
  )

#2-way interaction between extreme_vpd*habitat
tuti_ext_vpd_hab <-
  bird.model("tuti_mid",
             "ext_day_vpd*loc_hab+month_fact",
             dat_1,
             "1-6-25",
             path = "./saved_models/midday/two-way interactions/")

#View contrasts
ext_day.2int.est(tuti_ext_vpd_hab, "ext_day_vpd")
#For tuti, sig more vocalization in urban forest during extremes with high vpd

tuti_extvpd_hab_plt <-
  bird_plot(
    tuti_ext_vpd_hab,
    "tuti_mid",
    "ext_day_vpd:loc_hab",
    save = TRUE,
    date = "1-6-25",
    path = "./figures_12-4-24/midday/two-way interactions/ext_day_vpd"
  )

#2-way interaction between extreme_wbt*habitat
tuti_ext_wbt_hab <-
  bird.model("tuti_mid",
             "ext_day_wbt*loc_hab+month_fact",
             dat_1,
             "1-6-25",
             path = "./saved_models/midday/two-way interactions/")

#View contrasts
ext_day.2int.est(tuti_ext_wbt_hab, "ext_day_wbt")
#For tuti, sig more vocalization in urban forest during extremes but not with high wbt

tuti_extwbt_hab_plt <-
  bird_plot(
    tuti_ext_wbt_hab,
    "tuti_mid",
    "ext_day_wbt:loc_hab",
    save = TRUE,
    date = "1-6-25",
    path = "./figures_12-4-24/midday/two-way interactions/ext_day_wbt"
  )


#Wood thrush---------------------------------------------------------------------

aggregate(dat_1$woth_mid, by=list(dat_1$month_fact, dat_1$loc_hab, dat_1$ext_day_fact), FUN=sum, na.rm = TRUE)
aggregate(dat_1$woth_mid, by=list(dat_1$loc_hab, dat_1$ext_day_fact), FUN=sum, na.rm = TRUE)
aggregate(dat_1$woth_mid, by=list(dat_1$loc_hab, dat_1$ext_day_vpd), FUN=sum, na.rm = TRUE)
aggregate(dat_1$woth_mid, by=list(dat_1$ext_day_vpd), FUN=sum, na.rm = TRUE)

#Data support analyses across habitats but not across months and not across high vpd/wbt


#additive model
woth_ext_day_add <-
  bird.model("woth_mid",
             "ext_day_fact+loc_hab+month_fact",
             dat_1,
             "1-7-25",
             path = "./saved_models/midday/additive/")
#For woth, no overall difference in calling during extremes.

woth_ext_plt <-
  bird_plot(
    woth_ext_day_add,
    "woth_mid",
    "ext_day_fact",
    save = TRUE,
    date = "1-7-25",
    path = "./Figures_12-4-24/midday/additive/"
  )

woth_hab_plt <-
  bird_plot(
    woth_ext_day_add,
    "woth_mid",
    "loc_hab",
    save = TRUE,
    date = "1-6-25",
    path = "./figures_12-4-24/midday/additive/"
  )

woth_mon_plt <-
  bird_plot(woth_ext_day_add, "woth_mid", "month_fact")
woth_ndsi_plt <- bird_plot(woth_ext_day_add, "woth_mid", "z.ndsi")


#additive models with wbt and vpd
woth_ext_vpd_add <-
  bird.model("woth_mid",
             "ext_day_vpd+loc_hab+month_fact",
             dat_1,
             "1-7-25",
             path = "./saved_models/midday/additive/")
#For woth, no overall difference in calling during extremes.

woth_ext_wbt_add <-
  bird.model("woth_mid",
             "ext_day_wbt+loc_hab+month_fact",
             dat_1,
             "1-7-25",
             path = "./saved_models/midday/additive/")
#For woth, no overall difference in calling during extremes.


#2-way interaction between extreme*habitat
woth_ext_day_hab <-
  bird.model("woth_mid",
             "ext_day_fact*loc_hab+month_fact",
             dat_1,
             "1-7-25",
             path = "./saved_models/midday/two-way interactions/")

#View contrasts
ext_day.2int.est(woth_ext_day_hab, "ext_day_fact")
#For woth, sig more vocalization during extreme temps in urban forest

woth_ext_hab_plt <-
  bird_plot(
    woth_ext_day_hab,
    "woth_mid",
    "ext_day_fact:loc_hab",
    save = TRUE,
    date = "1-7-25",
    path = "./figures_12-4-24/midday/two-way interactions/ext_day_fact"
  )



#-----------------------------------------------------------------------------------
##SECOND, fit models of vocalization responses to 3-WAY interaction of----------------------
#Main interactive effects = ext_day_fact * loc_hab * month_fact

#For each species:
#(1) Fit model
#(2) Get estimates
#(3) Make and save plots (in organized subfolders)


#American robin------------------------------------------------------------------
#Fit model
amro_ext_day <-
  bird.model("amro_mid",
             "ext_day_fact*loc_hab*month_fact",
             dat_1,
             "1-3-25",
             path = "./saved_models/midday/three-way interactions/")

#get estimates
amro_est_ext <- ext_day.3int.est(amro_ext_day, "ext_day_fact")

#Make plot
amro_ext_plt <-
  bird_plot_3way(
    amro_est_ext[[1]],
    "amro_mid",
    "ext_day_fact",
    save = TRUE,
    "1-3-25",
    path = "./Figures_12-4-24/midday/three-way interactions/ext_day_fact"
  )
#View contrasts
amro_ext_plt
amro_est_ext$contrasts
#For Amro, sig less vocalization during extremes in urban open and urban forest
#during multiple months. More vocalization during extremes in urban forest in July


#American goldfinch----------------------------------------------------------
#Fit model
amgo_ext_day <-
  bird.model("amgo_mid",
             "ext_day_fact*loc_hab*month_fact",
             dat_1,
             "1-2-25",
             path = "./saved_models/midday/three-way interactions/")

#get estimates
amgo_est_ext <- ext_day.3int.est(amgo_ext_day, "ext_day_fact")

#Make plot
amgo_ext_plt <-
  bird_plot_3way(
    amgo_est_ext[[1]],
    "amgo_mid",
    "ext_day_fact",
    save = TRUE,
    "1-2-25",
    path = "./Figures_12-4-24/midday/three-way interactions/ext_day_fact"
  )
#View contrasts
amgo_ext_plt
amgo_est_ext$contrasts
#More vocalization during extremes in exurban forest in Aug
#Less vocalization during extremes in urban and exurban forests in July


#Blue Jay-----------------------------------------------------------------------
#Fit model
blja_ext_day <-
  bird.model("blja_mid",
             "ext_day_fact*loc_hab*month_fact",
             dat_1,
             "1-3-25",
             path = "./saved_models/midday/three-way interactions/")

#get estimates
blja_est_ext <- ext_day.3int.est(blja_ext_day, "ext_day_fact")

#Make plot
blja_ext_plt <-
  bird_plot_3way(
    blja_est_ext[[1]],
    "blja_mid",
    "ext_day_fact",
    save = TRUE,
    "1-3-25",
    path = "./Figures_12-4-24/midday/three-way interactions/ext_day_fact"
  )
#View contrasts
blja_ext_plt
blja_est_ext$contrasts
#For blja, sig less vocalization during extremes in exurban forest in Aug only


#Carolina Chickadee------------------------------------------------------------------
#Fit model
cach_ext_day <-
  bird.model("cach_mid",
             "ext_day_fact*loc_hab*month_fact",
             dat_1,
             "1-4-25",
             path = "./saved_models/midday/three-way interactions/")

#get estimates
cach_est_ext <- ext_day.3int.est(cach_ext_day, "ext_day_fact")

#Make plot
cach_ext_plt <-
  bird_plot_3way(
    cach_est_ext[[1]],
    "cach_mid",
    "ext_day_fact",
    save = TRUE,
    "1-4-25",
    path = "./Figures_12-4-24/midday/three-way interactions/ext_day_fact"
  )
#View contrasts
cach_ext_plt
cach_est_ext$contrasts
#For Cach, sig more vocalization during extremes in urban forest in Aug only
#and sig less vocalization in urban open in Aug only
#Large contrast suggest possible estimation issue/lack of data


#Carolina Wren------------------------------------------------------------------
#Fit model
cawr_ext_day <-
  bird.model("cawr_mid",
             "ext_day_fact*loc_hab*month_fact",
             dat_1,
             "1-5-25",
             path = "./saved_models/midday/three-way interactions/")

#get estimates
cawr_est_ext <- ext_day.3int.est(cawr_ext_day, "ext_day_fact")

#Make plot
cawr_ext_plt <-
  bird_plot_3way(
    cawr_est_ext[[1]],
    "cawr_mid",
    "ext_day_fact",
    save = TRUE,
    "1-5-25",
    path = "./Figures_12-4-24/midday/three-way interactions/ext_day_fact"
  )
#View contrasts
cawr_ext_plt
cawr_est_ext$contrasts
#For Cach, sig more vocalization during extremes in exurban open in June only
#and sig less vocalization in urban open in Aug only


#Northern Cardinal------------------------------------------------------------------
#Fit model
noca_ext_day <-
  bird.model("noca_mid",
             "ext_day_fact*loc_hab*month_fact",
             dat_1,
             "1-5-25",
             path = "./saved_models/midday/three-way interactions/")

#get estimates
noca_est_ext <- ext_day.3int.est(noca_ext_day, "ext_day_fact")

#Make plot
noca_ext_plt <-
  bird_plot_3way(
    noca_est_ext[[1]],
    "noca_mid",
    "ext_day_fact",
    save = TRUE,
    "1-5-25",
    path = "./Figures_12-4-24/midday/three-way interactions/ext_day_fact"
  )
#View contrasts
noca_ext_plt
noca_est_ext$contrasts
#For Noca, sig more vocalization during extremes in exurban forest in July only


#Red-bellied woodpeckers----------------------------------------------------------
#Fit model
rewo_ext_day <-
  bird.model("rewo_mid",
             "ext_day_fact*loc_hab*month_fact",
             dat_1,
             "12-23-24",
             path = "./saved_models/midday/three-way interactions/")

#get estimates
rewo_est_ext <- ext_day.3int.est(rewo_ext_day$mod, "ext_day_fact")
#Make plot
rewo_ext_plt <-
  bird_plot_3way(
    rewo_est_ext[[1]],
    "rewo_mid",
    "ext_day_fact",
    save = TRUE,
    "12-23-24",
    path = "./Figures_12-4-24/midday/three-way interactions/ext_day_fact"
  )
#View contrasts
rewo_ext_plt
rewo_est_ext$contrasts


#Tufted titmouse------------------------------------------------------------------
#Fit model
tuti_ext_day <-
  bird.model("tuti_mid",
             "ext_day_fact*loc_hab*month_fact",
             dat_1,
             "1-6-25",
             path = "./saved_models/midday/three-way interactions/")

#get estimates
tuti_est_ext <- ext_day.3int.est(tuti_ext_day, "ext_day_fact")

#Make plot
tuti_ext_plt <-
  bird_plot_3way(
    tuti_est_ext[[1]],
    "tuti_mid",
    "ext_day_fact",
    save = TRUE,
    "1-6-25",
    path = "./Figures_12-4-24/midday/three-way interactions/ext_day_fact"
  )
#View contrasts
tuti_ext_plt
tuti_est_ext$contrasts
#For tuti, sig more vocalization during extremes in exurban forest in June only



#-----------------------------------------------------------------------------------
#THIRD, fit models of vocalization reponses to 3-WAY interaction of----------------------
#Main interactive effects = ext_day_vpd * loc_hab * month_fact


#American robin------------------------------------------------------------------
#Fit model
amro_ext_vpd <-
  bird.model("amro_mid",
             "ext_day_vpd*loc_hab*month_fact",
             dat_1,
             "1-3-25",
             path = "./saved_models/midday/three-way interactions/")

#get estimates
amro_est_extvpd <- ext_day.3int.est(amro_ext_vpd, "ext_day_vpd")

#Make plot
amro_extvpd_plt <-
  bird_plot_3way(
    amro_est_extvpd[[1]],
    "amro_mid",
    "ext_day_vpd",
    save = TRUE,
    "1-3-25",
    path = "./Figures_12-4-24/midday/three-way interactions/ext_day_vpd"
  )
#View contrasts
amro_extvpd_plt
amro_est_extvpd$contrasts
#For Amro, mixed sig results but more negative effects of extremes than positive
#including some high vpd days, especially in urban open
#NOTE: Weird estimates - not enough data to support estimates for some groups

#rescale y axis and save figure again
png("./Figures_12-4-24/midday/three-way interactions/ext_day_vpd/ext_day_vpd_hab_month_amro_midday_yadj_1-3-25.png", width = 8.8, height = 6.0, units = 'in', res = 600)
amro_extvpd_plt + ylim(-1, 100)
dev.off()




#American goldfinch----------------------------------------------------------
#Fit model
amgo_ext_vpd <-
  bird.model("amgo_mid",
             "ext_day_vpd*loc_hab*month_fact",
             dat_1,
             "1-2-25",
             path = "./saved_models/midday/three-way interactions/")

#get estimates
amgo_est_extvpd <- ext_day.3int.est(amgo_ext_vpd, "ext_day_vpd")

#Make plot
amgo_extvpd_plt <-
  bird_plot_3way(
    amgo_est_extvpd[[1]],
    "amgo_mid",
    "ext_day_vpd",
    save = TRUE,
    "1-2-25",
    path = "./Figures_12-4-24/midday/three-way interactions/ext_day_vpd"
  )
#View contrasts
amgo_extvpd_plt
amgo_est_extvpd$contrasts
#NOTE: Weird estimates - not enough data to support estimates for some groups

#rescale y axis and save figure again
png("./Figures_12-4-24/midday/three-way interactions/ext_day_vpd/ext_day_vpd_hab_month_amgo_midday_yadj_1-3-25.png", width = 8.8, height = 6.0, units = 'in', res = 600)
amgo_extvpd_plt + ylim(-5, 20)
dev.off()


#Blue Jay------------------------------------------------------------------------
#Fit model
blja_ext_vpd <-
  bird.model("blja_mid",
             "ext_day_vpd*loc_hab*month_fact",
             dat_1,
             "1-2-25",
             path = "./saved_models/midday/three-way interactions/")

#get estimates
blja_est_extvpd <- ext_day.3int.est(blja_ext_vpd, "ext_day_vpd")

#Make plot
blja_extvpd_plt <-
  bird_plot_3way(
    blja_est_extvpd[[1]],
    "blja_mid",
    "ext_day_vpd",
    save = TRUE,
    "1-2-25",
    path = "./Figures_12-4-24/midday/three-way interactions/ext_day_vpd"
  )
#View contrasts
blja_extvpd_plt
blja_est_extvpd$contrasts
#NOTE: Weird estimates - not enough data to support estimates for some groups

#rescale y axis and save figure again
png("./Figures_12-4-24/midday/three-way interactions/ext_day_vpd/ext_day_vpd_hab_month_blja_midday_yadj_1-3-25.png", width = 8.8, height = 6.0, units = 'in', res = 600)
blja_extvpd_plt + ylim(-1, 100)
dev.off()


#Carolina Chickadee------------------------------------------------------------------
#Fit model
cach_ext_vpd <-
  bird.model("cach_mid",
             "ext_day_vpd*loc_hab*month_fact",
             dat_1,
             "1-4-25",
             path = "./saved_models/midday/three-way interactions/")

#get estimates
cach_est_extvpd <- ext_day.3int.est(cach_ext_vpd, "ext_day_vpd")

#Make plot
cach_extvpd_plt <-
  bird_plot_3way(
    cach_est_extvpd[[1]],
    "cach_mid",
    "ext_day_vpd",
    save = TRUE,
    "1-4-25",
    path = "./Figures_12-4-24/midday/three-way interactions/ext_day_vpd"
  )
#View contrasts
cach_extvpd_plt
cach_est_extvpd$contrasts
#NOTE: Weird estimates - not enough data to support estimates for some groups

#rescale y axis and save figure again
png("./Figures_12-4-24/midday/three-way interactions/ext_day_vpd/ext_day_vpd_hab_month_cach_midday_yadj_1-4-25.png", width = 8.8, height = 6.0, units = 'in', res = 600)
cach_extvpd_plt + ylim(-1, 10)
dev.off()


#Carolina Wren------------------------------------------------------------------
#Fit model
cawr_ext_vpd <-
  bird.model("cawr_mid",
             "ext_day_vpd*loc_hab*month_fact",
             dat_1,
             "1-5-25",
             path = "./saved_models/midday/three-way interactions/")

#get estimates
cawr_est_extvpd <- ext_day.3int.est(cawr_ext_vpd, "ext_day_vpd")

#Make plot
cawr_extvpd_plt <-
  bird_plot_3way(
    cawr_est_extvpd[[1]],
    "cawr_mid",
    "ext_day_vpd",
    save = TRUE,
    "1-5-25",
    path = "./Figures_12-4-24/midday/three-way interactions/ext_day_vpd"
  )
#View contrasts
cawr_extvpd_plt
cawr_est_extvpd$contrasts
#NOTE: Weird estimates - not enough data to support estimates for some groups

#rescale y axis and save figure again
png("./Figures_12-4-24/midday/three-way interactions/ext_day_vpd/ext_day_vpd_hab_month_cawr_midday_yadj_1-4-25.png", width = 8.8, height = 6.0, units = 'in', res = 600)
cawr_extvpd_plt + ylim(-1, 50)
dev.off()


#Northern Cardinal------------------------------------------------------------------
#Fit model
noca_ext_vpd <-
  bird.model("noca_mid",
             "ext_day_vpd*loc_hab*month_fact",
             dat_1,
             "1-5-25",
             path = "./saved_models/midday/three-way interactions/")

#get estimates
noca_est_extvpd <- ext_day.3int.est(noca_ext_vpd, "ext_day_vpd")

#Make plot
noca_extvpd_plt <-
  bird_plot_3way(
    noca_est_extvpd[[1]],
    "noca_mid",
    "ext_day_vpd",
    save = TRUE,
    "1-5-25",
    path = "./Figures_12-4-24/midday/three-way interactions/ext_day_vpd"
  )
#View contrasts
noca_extvpd_plt
noca_est_extvpd$contrasts
#NOTE: Weird estimates - not enough data to support estimates for some groups

#rescale y axis and save figure again
png("./Figures_12-4-24/midday/three-way interactions/ext_day_vpd/ext_day_vpd_hab_month_noca_midday_yadj_1-4-25.png", width = 8.8, height = 6.0, units = 'in', res = 600)
noca_extvpd_plt + ylim(-1, 50)
dev.off()


#Red-bellied woodpeckers----------------------------------------------------------
#Fit model
rewo_ext_vpd <-
  bird.model("rewo_mid",
             "ext_day_vpd*loc_hab*month_fact",
             dat_1,
             "12-30-24",
             path = "./saved_models/midday/three-way interactions/")

#get estimates
rewo_est_extvpd <- ext_day.3int.est(rewo_ext_vpd$mod, "ext_day_vpd")
#Make plot
rewo_extvpd_plt <-
  bird_plot_3way(
    rewo_est_extvpd[[1]],
    "rewo_mid",
    "ext_day_vpd",
    save = TRUE,
    "12-30-24",
    path = "./Figures_12-4-24/midday/three-way interactions/ext_day_vpd"
  )
#View contrasts
rewo_extvpd_plt
rewo_est_extvpd$contrasts
#NOTE: Weird inflated estimates - not enough data to support estimates for some groups

#rescale y axis and save figure again
png("./Figures_12-4-24/midday/three-way interactions/ext_day_vpd/ext_day_vpd_hab_month_rewo_midday_yadj_1-3-25.png", width = 8.8, height = 6.0, units = 'in', res = 600)
rewo_extvpd_plt + ylim(-1, 20)
dev.off()


#Tufted titmouse------------------------------------------------------------------
#Fit model
tuti_ext_vpd <-
  bird.model("tuti_mid",
             "ext_day_vpd*loc_hab*month_fact",
             dat_1,
             "1-6-25",
             path = "./saved_models/midday/three-way interactions/")

#get estimates
tuti_est_extvpd <- ext_day.3int.est(tuti_ext_vpd, "ext_day_vpd")

#Make plot
tuti_extvpd_plt <-
  bird_plot_3way(
    tuti_est_extvpd[[1]],
    "tuti_mid",
    "ext_day_vpd",
    save = TRUE,
    "1-6-25",
    path = "./Figures_12-4-24/midday/three-way interactions/ext_day_vpd"
  )
#View contrasts
tuti_extvpd_plt
tuti_est_extvpd$contrasts

#For Tuti, mixed results. Sig less vocalization during extremes in urban open 
#during all months. More vocalization in urban forest during extremes with high vpd in June
#NOTE: Weird estimates - not enough data to support estimates for some groups

#rescale y axis and save figure again
png("./Figures_12-4-24/midday/three-way interactions/ext_day_vpd/ext_day_vpd_hab_month_tuti_midday_yadj_1-4-25.png", width = 8.8, height = 6.0, units = 'in', res = 600)
tuti_extvpd_plt + ylim(-1, 50)
dev.off()



#-----------------------------------------------------------------------------------
#FOURTH, Fit models of vocalization reponses to 3-WAY interaction of----------------------
#Main interactive effects = ext_day_wbt * loc_hab * month_fact


#American robin-----------------------------------------------------------------
#Fit model
amro_ext_wbt <-
  bird.model("amro_mid",
             "ext_day_wbt*loc_hab*month_fact",
             dat_1,
             "1-3-25",
             path = "./saved_models/midday/three-way interactions/")

#get estimates
amro_est_extwbt <- ext_day.3int.est(amro_ext_wbt, "amro_ext_wbt")

#Make plot
amro_extwbt_plt <-
  bird_plot_3way(
    amro_est_extwbt[[1]],
    "amro_mid",
    "amro_ext_wbt",
    save = TRUE,
    "1-3-25",
    path = "./Figures_12-4-24/midday/three-way interactions/ext_day_wbt"
  )
#View contrasts
amro_extwbt_plt
amro_est_extwbt$contrasts
#NOTE: Weird estimates - not enough data to support estimates for some groups

#For Amro, mixed sig results but more negative effects of extremes than positive
#including some high vpd days, especially in urban open
#NOTE: Weird estimates - not enough data to support estimates for some groups



#American goldfinch----------------------------------------------------------
#Fit model
amgo_ext_wbt <-
  bird.model("amgo_mid",
             "ext_day_wbt*loc_hab*month_fact",
             dat_1,
             "1-1-24",
             path = "./saved_models/midday/three-way interactions/")

#get estimates
amgo_est_extwbt <- ext_day.3int.est(amgo_ext_wbt, "amgo_ext_wbt")

#Make plot
amgo_extwbt_plt <-
  bird_plot_3way(
    amgo_est_extwbt[[1]],
    "amgo_mid",
    "amgo_ext_wbt",
    save = TRUE,
    "1-1-25",
    path = "./Figures_12-4-24/midday/three-way interactions/ext_day_wbt"
  )
#View contrasts
amgo_extwbt_plt
amgo_est_extwbt$contrasts
#NOTE: Weird estimates - not enough data to support estimates for some groups


#Blue Jay-----------------------------------------------------------------------
#Fit model
blja_ext_wbt <-
  bird.model("blja_mid",
             "ext_day_wbt*loc_hab*month_fact",
             dat_1,
             "1-3-25",
             path = "./saved_models/midday/three-way interactions/")

#get estimates
blja_est_extwbt <- ext_day.3int.est(blja_ext_wbt, "blja_ext_wbt")

#Make plot
blja_extwbt_plt <-
  bird_plot_3way(
    blja_est_extwbt[[1]],
    "blja_mid",
    "blja_ext_wbt",
    save = TRUE,
    "1-3-25",
    path = "./Figures_12-4-24/midday/three-way interactions/ext_day_wbt"
  )
#View contrasts
blja_extwbt_plt
blja_est_extwbt$contrasts
#NOTE: Weird estimates - not enough data to support estimates for some groups

#For Blja, sig negative effects of extremes in exurban open and forests
#but not during high wgt days
#NOTE: Weird estimates - not enough data to support estimates for some groups


#Carolina Chickadee-----------------------------------------------------------------
#Fit model
cach_ext_wbt <-
  bird.model("cach_mid",
             "ext_day_wbt*loc_hab*month_fact",
             dat_1,
             "1-4-25",
             path = "./saved_models/midday/three-way interactions/")

#get estimates
cach_est_extwbt <- ext_day.3int.est(cach_ext_wbt, "cach_ext_wbt")

#Make plot
cach_extwbt_plt <-
  bird_plot_3way(
    cach_est_extwbt[[1]],
    "cach_mid",
    "cach_ext_wbt",
    save = TRUE,
    "1-4-25",
    path = "./Figures_12-4-24/midday/three-way interactions/ext_day_wbt"
  )
#View contrasts
cach_extwbt_plt
cach_est_extwbt$contrasts
#NOTE: Weird estimates - not enough data to support estimates for some groups


#Carolina Wren-----------------------------------------------------------------
#Fit model
cawr_ext_wbt <-
  bird.model("cawr_mid",
             "ext_day_wbt*loc_hab*month_fact",
             dat_1,
             "1-5-25",
             path = "./saved_models/midday/three-way interactions/")

#Load model
cawr_ext_wbt <- readRDS("./saved_models/midday/three-way interactions/cawr_mid_ext_day_wbt_loc_hab_month_fact_1-5-25.rds")

#get estimates
cawr_est_extwbt <- ext_day.3int.est(cawr_ext_wbt$mod, "cawr_ext_wbt")

#Make plot
cawr_extwbt_plt <-
  bird_plot_3way(
    cawr_est_extwbt[[1]],
    "cawr_mid",
    "cawr_ext_wbt",
    save = TRUE,
    "1-5-25",
    path = "./figures_12-4-24/midday/three-way interactions/ext_day_wbt"
  )
#View contrasts
cawr_extwbt_plt
cawr_est_extwbt$contrasts
#NOTE: Weird estimates - not enough data to support estimates for some groups


#Northern Cardinal-----------------------------------------------------------------
#Fit model
noca_ext_wbt <-
  bird.model("noca_mid",
             "ext_day_wbt*loc_hab*month_fact",
             dat_1,
             "1-5-25",
             path = "./saved_models/midday/three-way interactions/")

#Load model
noca_ext_wbt <- readRDS("./saved_models/midday/three-way interactions/noca_mid_ext_day_wbt_loc_hab_month_fact_1-5-25.rds")

#get estimates
noca_est_extwbt <- ext_day.3int.est(noca_ext_wbt$mod, "noca_ext_wbt")

#Make plot
noca_extwbt_plt <-
  bird_plot_3way(
    noca_est_extwbt[[1]],
    "noca_mid",
    "noca_ext_wbt",
    save = TRUE,
    "1-5-25",
    path = "./figures_12-4-24/midday/three-way interactions/ext_day_wbt"
  )
#View contrasts
noca_extwbt_plt
noca_est_extwbt$contrasts
#NOTE: Weird estimates - not enough data to support estimates for some groups



#Red-bellied woodpeckers----------------------------------------------------------
#Fit model
rewo_ext_wbt <-
  bird.model("rewo_mid",
             "ext_day_wbt*loc_hab*month_fact",
             dat_1,
             "1-1-24",
             path = "./saved_models/midday/three-way interactions/")

#get estimates
rewo_est_extwbt <- ext_day.3int.est(rewo_ext_wbt$mod, "rewo_ext_wbt")

#Make plot
rewo_extwbt_plt <-
  bird_plot_3way(
    rewo_est_extwbt[[1]],
    "rewo_mid",
    "rewo_ext_wbt",
    save = TRUE,
    "1-1-25",
    path = "./Figures_12-4-24/midday/three-way interactions/ext_day_wbt"
  )
#View contrasts
rewo_extwbt_plt
rewo_est_extwbt$contrasts
#NOTE: Weird estimates - not enough data to support estimates for some groups

table(dat_1$month_fact, dat_1$loc_hab, dat_1$ext_day_wbt)
aggregate(dat_1$rewo_mid, by=list(dat_1$month_fact, dat_1$loc_hab, dat_1$ext_day_wbt), FUN=sum, na.rm = TRUE)


#Tufted titmouse-----------------------------------------------------------------
#Fit model
tuti_ext_wbt <-
  bird.model("tuti_mid",
             "ext_day_wbt*loc_hab*month_fact",
             dat_1,
             "1-6-25",
             path = "./saved_models/midday/three-way interactions/")


#Load model
tuti_ext_wbt <- readRDS("./saved_models/midday/three-way interactions/tuti_mid_ext_day_wbt_loc_hab_month_fact_1-6-25.rds")

#get estimates
tuti_est_extwbt <- ext_day.3int.est(tuti_ext_wbt$mod, "tuti_ext_wbt")

#Make plot
tuti_extwbt_plt <-
  bird_plot_3way(
    tuti_est_extwbt[[1]],
    "tuti_mid",
    "tuti_ext_wbt",
    save = TRUE,
    "1-6-25",
    path = "./figures_12-4-24/midday/three-way interactions/ext_day_wbt"
  )
#View contrasts
tuti_extwbt_plt
tuti_est_extwbt$contrasts
#Mixed results
#NOTE: Weird estimates - not enough data to support estimates for some groups



## ***************************************************************************************************
## Compare models with different covar structure for a given species
## ***************************************************************************************************


#American robin-----------------------------------------------------------------

amro_ext_day_add <- readRDS("./saved_models/midday/additive/amro_mid_ext_day_fact+loc_hab+month_fact_1-3-25.rds")
amro_ext_day_hab <- readRDS("./saved_models/midday/two-way interactions/amro_mid_ext_day_fact_loc_hab+month_fact_1-3-25.rds")

loo(amro_ext_day_add$mod) #2876.0
loo(amro_ext_day_hab$mod) #2868.3 
loo(amro_ext_vpd_hab) #2874.6
loo(amro_ext_wbt_hab) #2872.1
loo(amro_ext_day) #2830.8  <- for this species, the ext_day_fact:loc_hab:month_fact is best supported
loo(amro_ext_vpd) #2830.3 
loo(amro_ext_wbt) #2843.9
#For Amro, three-way interactive model with ext*hab*month. Considering vpd and wbt
#does not improve model fit



#American goldfinch-------------------------------------------------------------

loo(amgo_ext_day_add) #3752.8
loo(amgo_ext_day_hab) #3758.2 
loo(amgo_ext_vpd_hab) #3761.3
loo(amgo_ext_wbt_hab) #3759.6
loo(amgo_ext_day) #3636.6  <- for this species, the ext_day_fact:loc_hab:month_fact is best supported
loo(amgo_ext_vpd) #3654.8 
loo(amgo_ext_wbt) #3646.0
#Summary: three-way interactive model with ext*hab*month. Considering vpd and wbt
#does not improve model fit


#Blue Jay------------------------------------------------------------------------

loo(blja_ext_day_add) #3269.9
loo(blja_ext_day_hab) #3270.3 
loo(blja_ext_vpd_hab) #3275.8
loo(blja_ext_wbt_hab) #3278.5
loo(blja_ext_day) #3275.6  <- for this species, the ext_day_fact:loc_hab:month_fact is best supported
loo(blja_ext_vpd) #3280.7 
loo(blja_ext_wbt) #3286.7
#Summary: additive model with ext+hab+month best supported. Considering vpd and wbt
#does not improve model fit


#Carolina Chickadee-------------------------------------------------------------

loo(cach_ext_day_add) #2211.5
loo(cach_ext_day_hab) #2211.8 
loo(cach_ext_vpd_hab) #2217.3
loo(cach_ext_wbt_hab) #2222.0 
loo(cach_ext_day) #2200.3  <- for this species, the ext_day_fact:loc_hab:month_fact is best supported
loo(cach_ext_vpd) #2198.1 <- actually best support but lots of weird estimates - likely not enought data to support some categories
loo(cach_ext_wbt) #2207.9
#Summary: three-way interactive model with ext*hab*month. Considering vpd and wbt
#does not substantially improve model fit


#Carolina Wren-------------------------------------------------------------

loo(cawr_ext_day_add) #6253.7 <- for this species, the additive model is best supported
loo(cawr_ext_day_hab) #6259.0  
loo(cawr_ext_vpd_hab) #6257.2 
loo(cawr_ext_wbt_hab) #6266.1  
loo(cawr_ext_day) #6258.3   
loo(cawr_ext_vpd) #6243.8  <- actually best support but lots of weird estimates - likely not enought data to support some categories
loo(cawr_ext_wbt) #6272.3 
#Summary: no consistent evidence of a response to extreme temp days


#Northern Cardinal-------------------------------------------------------------

loo(noca_ext_day_add) #9449.8 
loo(noca_ext_day_hab) #9452.7   
loo(noca_ext_vpd_hab) #9456.0 
loo(noca_ext_wbt_hab) #9458.2  
loo(noca_ext_day) #9429.9 <- for this species, the ext_day_fact:loc_hab:month_fact is best supported
loo(noca_ext_vpd) #9441.2  
loo(noca_ext_wbt) #9440.8  
#Summary: no consistent evidence of a response to extreme temp days


#Red-bellied woodpeckers----------------------------------------------------------

#Load saved models (if needed)
rewo_ext_day_add <- readRDS("./saved_models/midday/additive/rewo_mid_ext_day_fact+loc_hab+month_fact_12-23-24.rds")
rewo_ext_day_hab <- readRDS("./saved_models/midday/two-way interactions/rewo_mid_ext_day_fact_loc_hab+month_fact_12-23-24.rds")
rewo_ext_vpd_hab <- readRDS("./saved_models/midday/two-way interactions/rewo_mid_ext_day_vpd_loc_hab+month_fact_12-23-24.rds")
rewo_ext_wbt_hab <- readRDS("./saved_models/midday/two-way interactions/rewo_mid_ext_day_wbt_loc_hab+month_fact_12-23-24.rds")
rewo_ext_day <- readRDS("./saved_models/midday/three-way interactions/rewo_mid_ext_day_fact_loc_hab_month_fact_12-23-24.rds")
rewo_ext_vpd <- readRDS("./saved_models/midday/three-way interactions/rewo_mid_ext_day_vpd_loc_hab_month_fact_12-30-24.rds")
rewo_ext_wbt <- readRDS("./saved_models/midday/three-way interactions/rewo_mid_ext_day_wbt_loc_hab_month_fact_1-1-24.rds")

#Compare models
loo(rewo_ext_day_add$mod) #2220.3
loo(rewo_ext_day_hab$mod) #2220.0 
loo(rewo_ext_vpd_hab$mod) #2224.6
loo(rewo_ext_wbt_hab$mod) #2224.4
loo(rewo_ext_day$mod) #2187.0  <- for this species, the ext_day_fact:loc_hab:month_fact is best supported
loo(rewo_ext_vpd$mod) #2183.3 <- This is actually best supported model, BUT clearly there are estimateion issues and gaps in the data for categories
loo(rewo_ext_wbt$mod) #2186.2
#Summary: 


#Tufted titmouse-------------------------------------------------------------

loo(tuti_ext_day_add) #3167.4 <- for this species, the additive model is best supported
loo(tuti_ext_day_hab) #3169.4  
loo(tuti_ext_vpd_hab) #3176.2 
loo(tuti_ext_wbt_hab) #3180.1  
loo(tuti_ext_day) #3180.1   
loo(tuti_ext_vpd) #3184.7  
loo(tuti_ext_wbt) #3194.3 


## ***************************************************************************************************
## Make heatmaps summarizing model results
## ***************************************************************************************************

#----------------------------------------------------------------------------------
#Make plot for results of two-way interaction models-------------------------------

#Combine estimates from models-----------------------------------------------------

#Load saved models
amgo_ext_day_hab <- readRDS("./saved_models/midday/two-way interactions/amgo_mid_ext_day_fact_loc_hab+month_fact_1-2-25.rds")
amro_ext_day_hab <- readRDS("./saved_models/midday/two-way interactions/amro_mid_ext_day_fact_loc_hab+month_fact_1-3-25.rds")
blja_ext_day_hab <- readRDS("./saved_models/midday/two-way interactions/blja_mid_ext_day_fact_loc_hab+month_fact_1-3-25.rds")
cach_ext_day_hab <- readRDS("./saved_models/midday/two-way interactions/cach_mid_ext_day_fact_loc_hab+month_fact_1-4-25.rds")
cawr_ext_day_hab <- readRDS("./saved_models/midday/two-way interactions/cawr_mid_ext_day_fact_loc_hab+month_fact_1-5-25.rds")
hawo_ext_day_hab <- readRDS("./saved_models/midday/two-way interactions/hawo_mid_ext_day_fact_loc_hab+month_fact_1-7-25.rds")
noca_ext_day_hab <- readRDS("./saved_models/midday/two-way interactions/noca_mid_ext_day_fact_loc_hab+month_fact_1-5-25.rds")
rewo_ext_day_hab <- readRDS("./saved_models/midday/two-way interactions/rewo_mid_ext_day_fact_loc_hab+month_fact_12-23-24.rds")
tuti_ext_day_hab <- readRDS("./saved_models/midday/two-way interactions/tuti_mid_ext_day_fact_loc_hab+month_fact_1-6-25.rds")
woth_ext_day_hab <- readRDS("./saved_models/midday/two-way interactions/woth_mid_ext_day_fact_loc_hab+month_fact_1-7-25.rds")


#get contrasts again
amgo_2way_est <- ext_day.2int.est(amgo_ext_day_hab$mod, "ext_day_fact")
amro_2way_est <- ext_day.2int.est(amro_ext_day_hab$mod, "ext_day_fact")
blja_2way_est <- ext_day.2int.est(blja_ext_day_hab$mod, "ext_day_fact")
cach_2way_est <- ext_day.2int.est(cach_ext_day_hab$mod, "ext_day_fact")
cawr_2way_est <- ext_day.2int.est(cawr_ext_day_hab$mod, "ext_day_fact")
hawo_2way_est <- ext_day.2int.est(hawo_ext_day_hab$mod, "ext_day_fact")
noca_2way_est <- ext_day.2int.est(noca_ext_day_hab$mod, "ext_day_fact")
rewo_2way_est <- ext_day.2int.est(rewo_ext_day_hab$mod, "ext_day_fact")
tuti_2way_est <- ext_day.2int.est(tuti_ext_day_hab$mod, "ext_day_fact")
woth_2way_est <- ext_day.2int.est(woth_ext_day_hab$mod, "ext_day_fact")

amgo_2way_est$species <- "American Goldfinch"
amro_2way_est$species <- "American Robin"
blja_2way_est$species <- "Blue Jay"
cach_2way_est$species <- "Carolina Chickadee"
cawr_2way_est$species <- "Carolina Wren"
hawo_2way_est$species <-  "Hairy Woodpecker"
noca_2way_est$species <- "Northern Cardinal"
rewo_2way_est$species <- "Red-bellied Woodpecker"
tuti_2way_est$species <- "Tufted Titmouse"
woth_2way_est$species <-  "Wood Thrush"

#combine contrasts across models into df
spp_2way_est <- rbind(amgo_2way_est, amro_2way_est, blja_2way_est,
                      cach_2way_est, cawr_2way_est, hawo_2way_est, noca_2way_est,
                      rewo_2way_est, tuti_2way_est, woth_2way_est)

spp_2way_est$habitat <- as.factor(spp_2way_est$contrasts)
levels(spp_2way_est$habitat) <- c("Exurban forest", "Exurban open", "Urban forest", "Urban open")


#rescale contrasts to limit influece of very large values on visualization
hist(spp_2way_est$mean)
summary(spp_2way_est$mean)
spp_2way_est$est <- ifelse(spp_2way_est$mean >= 2, 2, spp_2way_est$mean)
hist(spp_2way_est$est)


#Make heatmap----------------------------------------------------------------------

library(viridis)

png("./figures_12-4-24/midday/heatmaps/spp10_twoway_heatmap_plasma_1-7-25.png", width = 6.25, height = 5.25, units = 'in', res = 600)
heatplot <- ggplot(data = spp_2way_est, aes(x=habitat, y=species, fill= est)) + 
  scale_alpha_discrete(range = c(0.60, 1)) +
  geom_tile(aes(alpha =  sig)) + 
  scale_fill_viridis(discrete=FALSE, option = "C") +
  #scale_x_discrete(labels= c("Exurban forest", "Exurban open", "Urban forest", "Urban open")) + 
  guides(alpha = FALSE) + #, fill = guide_legend(title = "Response to extremes")
  labs(fill = "Response to \n extremes") +
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


#----------------------------------------------------------------------------------
#Make plot for results of three-way interaction models-------------------------------

#Combine estimates from models-----------------------------------------------------

#Load saved models
amgo_ext_day <- readRDS("./saved_models/midday/three-way interactions/amgo_mid_ext_day_fact_loc_hab_month_fact_1-2-25.rds")
amro_ext_day <- readRDS("./saved_models/midday/three-way interactions/amro_mid_ext_day_fact_loc_hab_month_fact_1-3-25.rds")
blja_ext_day <- readRDS("./saved_models/midday/three-way interactions/blja_mid_ext_day_fact_loc_hab_month_fact_1-3-25.rds")
cach_ext_day <- readRDS("./saved_models/midday/three-way interactions/cach_mid_ext_day_fact_loc_hab_month_fact_1-4-25.rds")
cawr_ext_day <- readRDS("./saved_models/midday/three-way interactions/cawr_mid_ext_day_fact_loc_hab_month_fact_1-5-25.rds")
noca_ext_day <- readRDS("./saved_models/midday/three-way interactions/noca_mid_ext_day_fact_loc_hab_month_fact_1-5-25.rds")
rewo_ext_day <- readRDS("./saved_models/midday/three-way interactions/rewo_mid_ext_day_fact_loc_hab_month_fact_12-23-24.rds")
tuti_ext_day <- readRDS("./saved_models/midday/three-way interactions/tuti_mid_ext_day_fact_loc_hab_month_fact_1-6-25.rds")

#get contrasts again
amgo_3way_est <- ext_day.3int.est(amgo_ext_day$mod, "ext_day_fact")
amro_3way_est <- ext_day.3int.est(amro_ext_day$mod, "ext_day_fact")
blja_3way_est <- ext_day.3int.est(blja_ext_day$mod, "ext_day_fact")
cach_3way_est <- ext_day.3int.est(cach_ext_day$mod, "ext_day_fact")
cawr_3way_est <- ext_day.3int.est(cawr_ext_day$mod, "ext_day_fact")
noca_3way_est <- ext_day.3int.est(noca_ext_day$mod, "ext_day_fact")
rewo_3way_est <- ext_day.3int.est(rewo_ext_day$mod, "ext_day_fact")
tuti_3way_est <- ext_day.3int.est(tuti_ext_day$mod, "ext_day_fact")

amgo_3way_est$contrasts$species <- "American Goldfinch"
amro_3way_est$contrasts$species <- "American Robin"
blja_3way_est$contrasts$species <- "Blue Jay"
cach_3way_est$contrasts$species <- "Carolina Chickadee"
cawr_3way_est$contrasts$species <- "Carolina Wren"
noca_3way_est$contrasts$species <- "Northern Cardinal"
rewo_3way_est$contrasts$species <- "Red-bellied Woodpecker"
tuti_3way_est$contrasts$species <- "Tufted Titmouse"

#combine contrasts across models into df
spp_3way_est <- rbind(amgo_3way_est$contrasts, amro_3way_est$contrasts, blja_3way_est$contrasts,
                      cach_3way_est$contrasts, cawr_3way_est$contrasts, noca_3way_est$contrasts,
                      rewo_3way_est$contrasts, tuti_3way_est$contrasts)

spp_3way_est$habitat <- as.factor(spp_3way_est$contrasts)
spp_3way_est$month <- as.factor(spp_3way_est$contrasts)
levels(spp_3way_est$habitat) <- c("Exurban forest", "Exurban forest", "Exurban forest",
                                  "Exurban open", "Exurban open", "Exurban open",
                                  "Urban forest", "Urban forest", "Urban forest",
                                  "Urban open", "Urban open", "Urban open")

levels(spp_3way_est$month) <- c("June", "July", "August",
                                "June", "July", "August",
                                "June", "July", "August",
                                "June", "July", "August")

#rescale contrasts to limit influece of very large values on visualization
hist(spp_3way_est$mean)
summary(spp_3way_est$mean)
spp_3way_est$est <- ifelse(spp_3way_est$mean >= 2, 2, spp_3way_est$mean)
hist(spp_3way_est$est)


#Make heatmap----------------------------------------------------------------------

library(viridis)

png("./figures_12-4-24/midday/heatmaps/spp_threeway_heatmap_viridis_1-6-25.png", width = 9.4, height = 5.25, units = 'in', res = 600)
heatplot <- ggplot(data = spp_3way_est, aes(x=habitat, y=species, fill= est)) + 
  facet_grid(cols = vars(month)) + #, scales = "free"
  scale_alpha_discrete(range = c(0.60, 1)) +
  geom_tile(aes(alpha =  sig)) + 
  scale_fill_viridis(discrete=FALSE, option = "D") +
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
