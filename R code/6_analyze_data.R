###########################################################################################################################################################
### (6) Analyses of combined soundscape and frog detection data##########################################################################################
########################################################################################################################################################
###updated 7-29-24


## ***************************************************************************************************
## Load packages, data, and saved models
## ***************************************************************************************************


rm(list=ls())

library(brms)
library(ggplot2)
theme_set(theme_bw())


#Load individual level trait data
#dat <- read.csv("Data/analysis_data/temp_soundscape_frog_data_2024_summaries_07.28.24.csv") 
dat <- read.csv("C:/Users/kirchgrabera/Smithsonian Dropbox/Aidan Kirchgraber/Science and Faith/Aidan/audio_analysis/birdnet/temp_soundscape_bird_data_2024_summaries_10.17.24.csv")
dim(dat) #406  41 (522 42)

#get breakdown of extreme heat days by location and habitat
table(dat$ext_fact, dat$location, dat$habitat)

## ***************************************************************************************************
## Analysis frog detections
## ***************************************************************************************************

head(dat)
#levels(as.factor(dat$ext_fact))
levels(as.factor(dat$ext_fact))
summary(dat$count_1)

#make binary variable
#dat$count_1_bin <- ifelse(dat$count_1 > 0, 1, 0)
dat$count_1_bin <- ifelse(dat$count_1 > 0, 1, 0)
dat$count_asr_1_bin <- ifelse(dat$count_asr_1 > 0, 1, 0)

#drop outlier
dats <- subset(dat, count_1 < 200)
dim(dats) #360  41 (403 42)

#Fit model with extreme temp factor--------------------------------------------------

get_prior(count_1 ~ ext_fact,
          data = dat,
          family = bernoulli())

# LEFT OFF HERE - AK

extmod <- brm(
  count_1_bin ~ ext_fact_vpd,
  data = dat,
  family = bernoulli(),
  prior = c(
    prior("normal(0, 100)", class = "b"),
    prior("normal(0, 100)", class = "Intercept")
  ),
  warmup = 500,
  iter = 5000,
  thin = 20,
  cores = 2,
  chains = 2,
  #backend = "cmdstanr",
  seed = 123
) #to run the model


extmod <- brm(
  count_asr_1 ~ wb_temp_95*loc_hab,
  data = dat,
  family = poisson(),
  prior = c(
    prior("normal(0, 100)", class = "b"),
    prior("normal(0, 100)", class = "Intercept")
  ),
  warmup = 500,
  iter = 5000,
  thin = 20,
  cores = 2,
  chains = 2,
  #backend = "cmdstanr",
  seed = 123
) #to run the model


plot(extmod)
summary(extmod)
conditional_effects(extmod)

#CTmax significantly increases with increasing Tpref across individuals
#Notably, much less individual variation in CTmax than Tpref
#On average, CTmax increases by only 0.08 C for every 1 degree increase in Tpref

save(extmod, file='./Saved_models/extreme_vpd_frog_occ_mod_7-29-24.rdata')
save(extmod, file='C:/Users/kirchgrabera/Smithsonian Dropbox/Aidan Kirchgraber/Science and Faith/Aidan/audio_analysis/birdnet/extreme_vpd_frog_occ_mod_10-17-24.rdata')
#load('./Saved_models/Beetle_models/ind_tmax_tpref_mod_7-12-24.rdata')


#Fit model temperature data--------------------------------------------------------

plot(dat$temp_95, dat$count_1)
plot(dat$vpd_kPa, dat$count_1)
plot(dat$wb_temp_95, dat$count_1)
plot(dat$rel_hum_95, dat$count_1)
plot(dat$aci_nt, dat$count)
plot(dat$ndsi_nt, dat$count)
plot(as.factor(dat$ext_fact_vpd), dat$count_1)

summary(dat)

vpdmod <- brm(
  count_asr_1_bin ~ vpd_kPa,
  data = dat[complete.cases(dat[ , c("count_asr_1_bin", "vpd_kPa")]),],
  family = bernoulli(),
  prior = c(
    prior("normal(0, 100)", class = "b"),
    prior("normal(0, 100)", class = "Intercept")
  ),
  warmup = 500,
  iter = 5000,
  thin = 20,
  cores = 2,
  chains = 2,
  #backend = "cmdstanr",
  seed = 123
) #to run the model

plot(vpdmod)
summary(vpdmod)
conditional_effects(vpdmod)

#CTmax significantly increases with increasing Tpref across individuals
#Notably, much less individual variation in CTmax than Tpref
#On average, CTmax increases by only 0.08 C for every 1 degree increase in Tpref

save(tempmod, file='./Saved_models/temp_95_frog_occ_mod_7-29-24.rdata')
save(vpdmod, file='C:/Users/kirchgrabera/Smithsonian Dropbox/Aidan Kirchgraber/Science and Faith/Aidan/audio_analysis/birdnet/extreme_vpd_bird_occ_mod_10-17-24.rdata')
#save(vpdmod, file='./Saved_models/vpd_95_frog_occ_mod_7-29-24.rdata')

#Make plots----------------------------------------------------------------------
ce <- conditional_effects(extmod, plot = FALSE)
str(ce)
ce_temp <- ce$'wb_temp_95'
str(ce_temp)

#Plasma
col2rgb("#44039eff")
col2rgb("#8305a7ff")
col2rgb("#dd5e66ff")
col2rgb("#fba238ff")

#png("./Figures_7-26-24/frog_calling_temp_07.29.24.png", width = 4.5, height = 3.5, units = 'in', res = 600)
png("./Figures_7-26-24/frog_calling_vpd_07.29.24.png", width = 4.5, height = 3.5, units = 'in', res = 600)
png("C:/Users/kirchgrabera/Downloads/noca_calling_wbt_11.01.24.png", width = 4.5, height = 3.5, units = 'in', res = 1200)

effect_plot <- ggplot() + 
  #geom_vline(xintercept = min(abun_jne[which(abun_jne$D_elongatus_count > 0), "D_elongatus_wt"]), color =  "grey55", size = 1, lty = "dashed") +
  #geom_point(data = abun_jne, aes(D_elongatus_wt, D_elongatus_count), fill = "grey55", color = "white", shape = 21, size = 4, stroke = 1.5, alpha = 0.25) + 
  geom_ribbon(data = ce_temp, aes(wb_temp_95, estimate__, ymin=lower__, ymax=upper__), fill = "#dd5e66ff", alpha=0.25) +
  geom_line(data = ce_temp, aes(wb_temp_95, estimate__,), color = "#dd5e66ff", size = 1.15) +
  theme(legend.title=element_blank()) +
  theme(legend.text = element_text(size = 12)) +
  theme(panel.grid.major = element_line(colour = "white")) + 
  theme(panel.grid.minor = element_line(colour = "white")) + 
  theme(panel.grid.major.y = element_line(colour = "white")) +
  theme(panel.border = element_rect(color = "black", linewidth = 1)) +
  theme(axis.text = element_text(size = 14)) + 
  theme(axis.title.y=element_text(size = 14)) +
  theme(axis.title.x=element_text(size = 14)) +
  scale_x_continuous(expand = c(0, 0)) +
  theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, unit = "cm")) +
  #labs(x = expression("Prior day temperature (°C)")) +
  labs(x = expression("Wet Bulb Temperature (°C)")) +
  #labs(y = expression("Counts of C. aestivus"))
  #labs(y = expression("Counts of P. stygicus"))
  labs(y = expression("Bird Vocalizations Detected"))
effect_plot
dev.off()



#extract effects
ce <- conditional_effects(extmod, plot = FALSE)
str(ce)
ce_ext <- ce$ext_fact_vpd
levels(ce_ext$ext_fact_vpd)

ce_ext$ext_fact_vpd <-
  factor(ce_ext$ext_fact_vpd,
         levels = c("not extreme", "extreme", "extreme high vpd"))
#iplc_est$pa_cat <- c("outside_pa", "pa_edge", "in_pa", "outside_pa", "pa_edge", "in_pa")

#Plot main effects of iplc lands and PAs on forest change
#Plot once with logged effects and once without loging effects
#Just switch between commented lines for points, error bars, and y axis labels
#Update filenames to reflect models and whether effect is log transformed or not

png("./Figures_7-26-24/frog_calling_extreme_temp_vpd_07.29.24.png", width = 4.75, height = 3.75, units = 'in', res = 600)
effect_plot_plus <- ggplot(ce_ext, aes(factor(ext_fact_vpd), estimate__)) + 
  #geom_errorbar(aes(y = log(estimate__), ymin = log(lower__), ymax = log(upper__), color = factor(iplc_pa)), size = 2, width = 0) +
  #geom_point(aes(y = log(estimate__), color = factor(iplc_pa)), fill = "grey85", shape = 21, size = 3.5, stroke = 3.0) + 
  geom_errorbar(aes(y = estimate__, ymin = lower__, ymax = upper__, color = factor(ext_fact_vpd)), size = 2, width = 0) +
  geom_point(aes(y = estimate__, color = factor(ext_fact_vpd)), fill = "grey85", shape = 21, size = 3.5, stroke = 3.0) + 
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
  labs(y = expression("Probability of calling")) +
  scale_x_discrete(labels=c("Not extreme", "Extreme",
                            "Extreme \n+ high VPD")) +
  theme(axis.text = element_text(size = 14)) + 
  theme(axis.title.y=element_text(size = 14)) +
  theme(legend.text=element_text(size = 14)) +#+ scale_y_continuous(limits = c(22, 36)) +
  #theme(axis.text.x=element_text(size = 14, angle=45, hjust=1)) +
  theme(plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"))
effect_plot_plus
dev.off()


