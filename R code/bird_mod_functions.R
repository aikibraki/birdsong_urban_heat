

#-------------------------------------------------------------------------------
#Function to fit and save models------------------------------------------------


#Define function for fitting and saving models------------------------------------

bird.model <-
  function(response,
           main,
           data,
           date,
           month_main = TRUE,
           iter = 4000,
           family = negbinomial(),
           path = "./saved_models/") {
    
    require(beepr)
    require(stringr)
    
    # Clear up some space
    gc()
    
    # Parameter readout
    cat("MODELING:\n   ")
    cat(paste0("Response: ", response, ", Main effect: ", main), "\n\n")
    
    # Specify formula
    if (month_main == TRUE) {
      form <- paste(response, "~", main,  "+ z.ndsi + (1|sites)")
      
    } else{
      form <-
        paste(response, "~", main,  "+ z.ndsi + (1|sites) + (1|month)")
    }
    
    # Print formula
    cat("Running model:\n   ", form, "\n\n")
    
    form <- as.formula(form)
    
    # Run model
    stime <- Sys.time() # Start time
    cat("Start time: ", as.character(stime), "\n")
    
    bird_mod <- brm(
      form,
      data = data,
      family = family,
      prior = c(
        prior("normal(0, 100)", class = "b"),
        prior("normal(0, 100)", class = "Intercept")
      ),
      warmup = 500,
      iter = iter,
      thin = 20,
      cores = 3,
      chains = 3,
      #backend = "cmdstanr",
      seed = 123
    ) #to run the model
    
    
    etime <- Sys.time() # End time
    cat("End time: ", as.character(etime), "\n")
    dtime <- difftime(etime, stime) # Run time
    print(dtime)
    
    # Save model
    main_txt <- str_replace_all(main, "\\*", "_")
    main_txt <- str_replace_all(main_txt, "\\+", "_")
    
    s.path <- paste0(path, response, "_", main_txt, "_", date, ".rds")
    saveRDS(list(
      mod = bird_mod,
      dat = data,
      runtime = dtime
    ), s.path) #
    
    cat("\n\nModel saved to:\n   ", s.path)
    
    beep(8)
    
    return(bird_mod)
    
  }



#-------------------------------------------------------------------------------
#Function to extract three-way interaction terms--------------------------------

#define function for ext_day_fact * loc_hab * month_fact' interaction
ext_day.3int.est <- function(brmsmod, extreme_var) {
  
  #Get posterior estimates of there-way interactions--------------------------------
  params <- as.data.frame(fixef(brmsmod, summary = FALSE)) #
  dim(params) #350 25
  
  if(extreme_var == "ext_day_fact"){ 
  
  #Calculate mean biomass and slopes for each category
  params$urbanopen_notext_6 <-  params$`Intercept`
  params$urbanopen_ext_6 <-
    params$`Intercept` + params$`ext_day_factextreme`
  params$urbanopen_notext_7 <-
    params$`Intercept` + params$`month_fact7`
  params$urbanopen_ext_7 <-
    params$`Intercept` + params$`ext_day_factextreme` + params$`month_fact7`
  params$urbanopen_notext_8 <-
    params$`Intercept` + params$`month_fact8`
  params$urbanopen_ext_8 <-
    params$`Intercept` + params$`ext_day_factextreme` + params$`month_fact8`
  
  params$urbanforest_notext_6 <-
    params$`Intercept` + params$`loc_haburbanforest`
  params$urbanforest_ext_6 <-
    params$`Intercept` + params$`loc_haburbanforest` + params$`ext_day_factextreme` +
    params$`ext_day_factextreme:loc_haburbanforest`
  params$urbanforest_notext_7 <-
    params$`Intercept` + params$`loc_haburbanforest` + params$`month_fact7` +
    params$`loc_haburbanforest:month_fact7`
  params$urbanforest_ext_7 <-
    params$`Intercept` + params$`loc_haburbanforest` + params$`ext_day_factextreme` +
    params$`month_fact7` + params$`ext_day_factextreme:loc_haburbanforest` +
    params$`ext_day_factextreme:month_fact7` +  params$`loc_haburbanforest:month_fact7` +
    params$`ext_day_factextreme:loc_haburbanforest:month_fact7`
  params$urbanforest_notext_8 <-
    params$`Intercept` + params$`loc_haburbanforest` + params$`month_fact8` +
    params$`loc_haburbanforest:month_fact8`
  params$urbanforest_ext_8 <-
    params$`Intercept` + params$`loc_haburbanforest` + params$`ext_day_factextreme` +
    params$`month_fact8` + params$`ext_day_factextreme:loc_haburbanforest` +
    params$`ext_day_factextreme:month_fact8` +  params$`loc_haburbanforest:month_fact8` +
    params$`ext_day_factextreme:loc_haburbanforest:month_fact8`
  
  params$exurbanopen_notext_6 <-
    params$`Intercept` + params$`loc_habexurbanopen`
  params$exurbanopen_ext_6 <-
    params$`Intercept` + params$`loc_habexurbanopen` + params$`ext_day_factextreme` +
    params$`ext_day_factextreme:loc_habexurbanopen`
  params$exurbanopen_notext_7 <-
    params$`Intercept` + params$`loc_habexurbanopen` + params$`month_fact7` +
    params$`loc_habexurbanopen:month_fact7`
  params$exurbanopen_ext_7 <-
    params$`Intercept` + params$`loc_habexurbanopen` + params$`ext_day_factextreme` +
    params$`month_fact7` + params$`ext_day_factextreme:loc_habexurbanopen` +
    params$`ext_day_factextreme:month_fact7` +  params$`loc_habexurbanopen:month_fact7` +
    params$`ext_day_factextreme:loc_habexurbanopen:month_fact7`
  params$exurbanopen_notext_8 <-
    params$`Intercept` + params$`loc_habexurbanopen` + params$`month_fact8` +
    params$`loc_habexurbanopen:month_fact8`
  params$exurbanopen_ext_8 <-
    params$`Intercept` + params$`loc_habexurbanopen` + params$`ext_day_factextreme` +
    params$`month_fact8` + params$`ext_day_factextreme:loc_habexurbanopen` +
    params$`ext_day_factextreme:month_fact8` +  params$`loc_habexurbanopen:month_fact8` +
    params$`ext_day_factextreme:loc_habexurbanopen:month_fact8`
  
  params$exurbanforest_notext_6 <-
    params$`Intercept` + params$`loc_habexurbanforest`
  params$exurbanforest_ext_6 <-
    params$`Intercept` + params$`loc_habexurbanforest` + params$`ext_day_factextreme` +
    params$`ext_day_factextreme:loc_habexurbanforest`
  params$exurbanforest_notext_7 <-
    params$`Intercept` + params$`loc_habexurbanforest` + params$`month_fact7` +
    params$`loc_habexurbanforest:month_fact7`
  params$exurbanforest_ext_7 <-
    params$`Intercept` + params$`loc_habexurbanforest` + params$`ext_day_factextreme` +
    params$`month_fact7` + params$`ext_day_factextreme:loc_habexurbanforest` +
    params$`ext_day_factextreme:month_fact7` +  params$`loc_habexurbanforest:month_fact7` +
    params$`ext_day_factextreme:loc_habexurbanforest:month_fact7`
  params$exurbanforest_notext_8 <-
    params$`Intercept` + params$`loc_habexurbanforest` + params$`month_fact8` +
    params$`loc_habexurbanforest:month_fact8`
  params$exurbanforest_ext_8 <-
    params$`Intercept` + params$`loc_habexurbanforest` + params$`ext_day_factextreme` +
    params$`month_fact8` + params$`ext_day_factextreme:loc_habexurbanforest` +
    params$`ext_day_factextreme:month_fact8` +  params$`loc_habexurbanforest:month_fact8` +
    params$`ext_day_factextreme:loc_habexurbanforest:month_fact8`
  
  colnames(params)
  
  library(dplyr)
  params_cnt <- params %>%
    mutate(across(
      .cols = 26:49,
      .fns = exp,
      .names = "{.col}_cnt"
    ))
  detach("package:dplyr", unload = TRUE)
  
  #get means into data frame for plotting
  colnames(params_cnt)
  est <-
    data.frame(mean = apply(params_cnt[, c(50:73)], 2, FUN = mean, na.rm = TRUE))
  est <-
    cbind(est, t(apply(
      params_cnt[, c(50:73)],
      2,
      quantile,
      probs = c(0.025, 0.975),
      na.rm = TRUE
    )))
  
  est$loc_hab <-
    c(
      rep("urban open", 6),
      rep("urban forest", 6),
      rep("exurban open", 6),
      rep("exurban forest", 6)
    )
  est$extreme <- rep(c("not extreme", "extreme"), 12)
  est$month <-
    rep(c("June", "June", "July", "July", "August", "August"), 4)
  colnames(est)[2:3] <- c("LCI", "UCI")
  
  
  } else{



#define function for 'ext_day_vpd * loc_hab * month_fact' 
#or 'ext_day_wbt * loc_hab * month_fact' interactions

# ext_day_vpd.3int.est <- function(brmsmod, extreme_var) {
#   
#   #Get posterior estimates of there-way interactions--------------------------------
#   params <- as.data.frame(fixef(brmsmod, summary = FALSE)) #
#   dim(params) #350 37
  
  #add if else statement for vpd or wbt vars
  if(extreme_var == "ext_day_vpd"){ 
  
  #Calculate mean and CIs of counts for each category
  params$urbanopen_notext_6 <-  params$`Intercept`
  params$urbanopen_ext_6 <-
    params$`Intercept` + params$`ext_day_vpdextreme`
  params$urbanopen_extvpd_6 <-
    params$`Intercept` + params$`ext_day_vpdextremehighvpd`
  params$urbanopen_notext_7 <-
    params$`Intercept` + params$`month_fact7`
  params$urbanopen_ext_7 <-
    params$`Intercept` + params$`ext_day_vpdextreme` + params$`month_fact7`
  params$urbanopen_extvpd_7 <-
    params$`Intercept` + params$`ext_day_vpdextremehighvpd` + params$`month_fact7`
  params$urbanopen_notext_8 <-
    params$`Intercept` + params$`month_fact8`
  params$urbanopen_ext_8 <-
    params$`Intercept` + params$`ext_day_vpdextreme` + params$`month_fact8`
  params$urbanopen_extvpd_8 <-
    params$`Intercept` + params$`ext_day_vpdextremehighvpd` + params$`month_fact8`
  
  params$urbanforest_notext_6 <-
    params$`Intercept` + params$`loc_haburbanforest`
  params$urbanforest_ext_6 <-
    params$`Intercept` + params$`loc_haburbanforest` + params$`ext_day_vpdextreme` +
    params$`ext_day_vpdextreme:loc_haburbanforest`
  params$urbanforest_extvpd_6 <-
    params$`Intercept` + params$`loc_haburbanforest` + params$`ext_day_vpdextremehighvpd` +
    params$`ext_day_vpdextremehighvpd:loc_haburbanforest`
  params$urbanforest_notext_7 <-
    params$`Intercept` + params$`loc_haburbanforest` + params$`month_fact7` +
    params$`loc_haburbanforest:month_fact7`
  params$urbanforest_ext_7 <-
    params$`Intercept` + params$`loc_haburbanforest` + params$`ext_day_vpdextreme` +
    params$`month_fact7` + params$`ext_day_vpdextreme:loc_haburbanforest` +
    params$`ext_day_vpdextreme:month_fact7` +  params$`loc_haburbanforest:month_fact7` +
    params$`ext_day_vpdextreme:loc_haburbanforest:month_fact7`
  params$urbanforest_extvpd_7 <-
    params$`Intercept` + params$`loc_haburbanforest` + params$`ext_day_vpdextremehighvpd` +
    params$`month_fact7` + params$`ext_day_vpdextremehighvpd:loc_haburbanforest` +
    params$`ext_day_vpdextremehighvpd:month_fact7` +  params$`loc_haburbanforest:month_fact7` +
    params$`ext_day_vpdextremehighvpd:loc_haburbanforest:month_fact7`
  params$urbanforest_notext_8 <-
    params$`Intercept` + params$`loc_haburbanforest` + params$`month_fact8` +
    params$`loc_haburbanforest:month_fact8`
  params$urbanforest_ext_8 <-
    params$`Intercept` + params$`loc_haburbanforest` + params$`ext_day_vpdextreme` +
    params$`month_fact8` + params$`ext_day_vpdextreme:loc_haburbanforest` +
    params$`ext_day_vpdextreme:month_fact8` +  params$`loc_haburbanforest:month_fact8` +
    params$`ext_day_vpdextreme:loc_haburbanforest:month_fact8`
  params$urbanforest_extvpd_8 <-
    params$`Intercept` + params$`loc_haburbanforest` + params$`ext_day_vpdextremehighvpd` +
    params$`month_fact8` + params$`ext_day_vpdextremehighvpd:loc_haburbanforest` +
    params$`ext_day_vpdextremehighvpd:month_fact8` +  params$`loc_haburbanforest:month_fact8` +
    params$`ext_day_vpdextremehighvpd:loc_haburbanforest:month_fact8`
  
  params$exurbanopen_notext_6 <-
    params$`Intercept` + params$`loc_habexurbanopen`
  params$exurbanopen_ext_6 <-
    params$`Intercept` + params$`loc_habexurbanopen` + params$`ext_day_vpdextreme` +
    params$`ext_day_vpdextreme:loc_habexurbanopen`
  params$exurbanopen_extvpd_6 <-
    params$`Intercept` + params$`loc_habexurbanopen` + params$`ext_day_vpdextremehighvpd` +
    params$`ext_day_vpdextremehighvpd:loc_habexurbanopen`
  params$exurbanopen_notext_7 <-
    params$`Intercept` + params$`loc_habexurbanopen` + params$`month_fact7` +
    params$`loc_habexurbanopen:month_fact7`
  params$exurbanopen_ext_7 <-
    params$`Intercept` + params$`loc_habexurbanopen` + params$`ext_day_vpdextreme` +
    params$`month_fact7` + params$`ext_day_vpdextreme:loc_habexurbanopen` +
    params$`ext_day_vpdextreme:month_fact7` +  params$`loc_habexurbanopen:month_fact7` +
    params$`ext_day_vpdextreme:loc_habexurbanopen:month_fact7`
  params$exurbanopen_extvpd_7 <-
    params$`Intercept` + params$`loc_habexurbanopen` + params$`ext_day_vpdextremehighvpd` +
    params$`month_fact7` + params$`ext_day_vpdextremehighvpd:loc_habexurbanopen` +
    params$`ext_day_vpdextremehighvpd:month_fact7` +  params$`loc_habexurbanopen:month_fact7` +
    params$`ext_day_vpdextremehighvpd:loc_habexurbanopen:month_fact7`
  params$exurbanopen_notext_8 <-
    params$`Intercept` + params$`loc_habexurbanopen` + params$`month_fact8` +
    params$`loc_habexurbanopen:month_fact8`
  params$exurbanopen_ext_8 <-
    params$`Intercept` + params$`loc_habexurbanopen` + params$`ext_day_vpdextreme` +
    params$`month_fact8` + params$`ext_day_vpdextreme:loc_habexurbanopen` +
    params$`ext_day_vpdextreme:month_fact8` +  params$`loc_habexurbanopen:month_fact8` +
    params$`ext_day_vpdextreme:loc_habexurbanopen:month_fact8`
  params$exurbanopen_extvpd_8 <-
    params$`Intercept` + params$`loc_habexurbanopen` + params$`ext_day_vpdextremehighvpd` +
    params$`month_fact8` + params$`ext_day_vpdextremehighvpd:loc_habexurbanopen` +
    params$`ext_day_vpdextremehighvpd:month_fact8` +  params$`loc_habexurbanopen:month_fact8` +
    params$`ext_day_vpdextremehighvpd:loc_habexurbanopen:month_fact8`
  
  params$exurbanforest_notext_6 <-
    params$`Intercept` + params$`loc_habexurbanforest`
  params$exurbanforest_ext_6 <-
    params$`Intercept` + params$`loc_habexurbanforest` + params$`ext_day_vpdextreme` +
    params$`ext_day_vpdextreme:loc_habexurbanforest`
  params$exurbanforest_extvpd_6 <-
    params$`Intercept` + params$`loc_habexurbanforest` + params$`ext_day_vpdextremehighvpd` +
    params$`ext_day_vpdextremehighvpd:loc_habexurbanforest`
  params$exurbanforest_notext_7 <-
    params$`Intercept` + params$`loc_habexurbanforest` + params$`month_fact7` +
    params$`loc_habexurbanforest:month_fact7`
  params$exurbanforest_ext_7 <-
    params$`Intercept` + params$`loc_habexurbanforest` + params$`ext_day_vpdextreme` +
    params$`month_fact7` + params$`ext_day_vpdextreme:loc_habexurbanforest` +
    params$`ext_day_vpdextreme:month_fact7` +  params$`loc_habexurbanforest:month_fact7` +
    params$`ext_day_vpdextreme:loc_habexurbanforest:month_fact7`
  params$exurbanforest_extvpd_7 <-
    params$`Intercept` + params$`loc_habexurbanforest` + params$`ext_day_vpdextremehighvpd` +
    params$`month_fact7` + params$`ext_day_vpdextremehighvpd:loc_habexurbanforest` +
    params$`ext_day_vpdextremehighvpd:month_fact7` +  params$`loc_habexurbanforest:month_fact7` +
    params$`ext_day_vpdextremehighvpd:loc_habexurbanforest:month_fact7`
  params$exurbanforest_notext_8 <-
    params$`Intercept` + params$`loc_habexurbanforest` + params$`month_fact8` +
    params$`loc_habexurbanforest:month_fact8`
  params$exurbanforest_ext_8 <-
    params$`Intercept` + params$`loc_habexurbanforest` + params$`ext_day_vpdextreme` +
    params$`month_fact8` + params$`ext_day_vpdextreme:loc_habexurbanforest` +
    params$`ext_day_vpdextreme:month_fact8` +  params$`loc_habexurbanforest:month_fact8` +
    params$`ext_day_vpdextreme:loc_habexurbanforest:month_fact8`
  params$exurbanforest_extvpd_8 <-
    params$`Intercept` + params$`loc_habexurbanforest` + params$`ext_day_vpdextremehighvpd` +
    params$`month_fact8` + params$`ext_day_vpdextremehighvpd:loc_habexurbanforest` +
    params$`ext_day_vpdextremehighvpd:month_fact8` +  params$`loc_habexurbanforest:month_fact8` +
    params$`ext_day_vpdextremehighvpd:loc_habexurbanforest:month_fact8`
  
  
  colnames(params)
  
  library(dplyr)
  params_cnt <- params %>%
    mutate(across(
      .cols = 38:73,
      .fns = exp,
      .names = "{.col}_cnt"
    ))
  detach("package:dplyr", unload = TRUE)
  
  #get means into data frame for plotting
  colnames(params_cnt)
  est <-
    data.frame(mean = apply(params_cnt[, c(74:109)], 2, FUN = mean, na.rm = TRUE))
  est <-
    cbind(est, t(apply(
      params_cnt[, c(74:109)],
      2,
      quantile,
      probs = c(0.025, 0.975),
      na.rm = TRUE
    )))
  
  est$loc_hab <-
    c(
      rep("urban open", 9),
      rep("urban forest", 9),
      rep("exurban open", 9),
      rep("exurban forest", 9)
    )
  est$extreme <- rep(c("not extreme", "extreme", "extreme high vpd"), 12)
  est$month <-
    rep(c("June", "June", "June", "July", "July", "July", "August", "August", "August"), 4)
  colnames(est)[2:3] <- c("LCI", "UCI")
  
  } else{
  
  #Calculate mean and CIs of counts for each category
  params$urbanopen_notext_6 <-  params$`Intercept`
  params$urbanopen_ext_6 <-
    params$`Intercept` + params$`ext_day_wbtextreme`
  params$urbanopen_extwbt_6 <-
    params$`Intercept` + params$`ext_day_wbtextremehighwbt`
  params$urbanopen_notext_7 <-
    params$`Intercept` + params$`month_fact7`
  params$urbanopen_ext_7 <-
    params$`Intercept` + params$`ext_day_wbtextreme` + params$`month_fact7`
  params$urbanopen_extwbt_7 <-
    params$`Intercept` + params$`ext_day_wbtextremehighwbt` + params$`month_fact7`
  params$urbanopen_notext_8 <-
    params$`Intercept` + params$`month_fact8`
  params$urbanopen_ext_8 <-
    params$`Intercept` + params$`ext_day_wbtextreme` + params$`month_fact8`
  params$urbanopen_extwbt_8 <-
    params$`Intercept` + params$`ext_day_wbtextremehighwbt` + params$`month_fact8`
  
  params$urbanforest_notext_6 <-
    params$`Intercept` + params$`loc_haburbanforest`
  params$urbanforest_ext_6 <-
    params$`Intercept` + params$`loc_haburbanforest` + params$`ext_day_wbtextreme` +
    params$`ext_day_wbtextreme:loc_haburbanforest`
  params$urbanforest_extwbt_6 <-
    params$`Intercept` + params$`loc_haburbanforest` + params$`ext_day_wbtextremehighwbt` +
    params$`ext_day_wbtextremehighwbt:loc_haburbanforest`
  params$urbanforest_notext_7 <-
    params$`Intercept` + params$`loc_haburbanforest` + params$`month_fact7` +
    params$`loc_haburbanforest:month_fact7`
  params$urbanforest_ext_7 <-
    params$`Intercept` + params$`loc_haburbanforest` + params$`ext_day_wbtextreme` +
    params$`month_fact7` + params$`ext_day_wbtextreme:loc_haburbanforest` +
    params$`ext_day_wbtextreme:month_fact7` +  params$`loc_haburbanforest:month_fact7` +
    params$`ext_day_wbtextreme:loc_haburbanforest:month_fact7`
  params$urbanforest_extwbt_7 <-
    params$`Intercept` + params$`loc_haburbanforest` + params$`ext_day_wbtextremehighwbt` +
    params$`month_fact7` + params$`ext_day_wbtextremehighwbt:loc_haburbanforest` +
    params$`ext_day_wbtextremehighwbt:month_fact7` +  params$`loc_haburbanforest:month_fact7` +
    params$`ext_day_wbtextremehighwbt:loc_haburbanforest:month_fact7`
  params$urbanforest_notext_8 <-
    params$`Intercept` + params$`loc_haburbanforest` + params$`month_fact8` +
    params$`loc_haburbanforest:month_fact8`
  params$urbanforest_ext_8 <-
    params$`Intercept` + params$`loc_haburbanforest` + params$`ext_day_wbtextreme` +
    params$`month_fact8` + params$`ext_day_wbtextreme:loc_haburbanforest` +
    params$`ext_day_wbtextreme:month_fact8` +  params$`loc_haburbanforest:month_fact8` +
    params$`ext_day_wbtextreme:loc_haburbanforest:month_fact8`
  params$urbanforest_extwbt_8 <-
    params$`Intercept` + params$`loc_haburbanforest` + params$`ext_day_wbtextremehighwbt` +
    params$`month_fact8` + params$`ext_day_wbtextremehighwbt:loc_haburbanforest` +
    params$`ext_day_wbtextremehighwbt:month_fact8` +  params$`loc_haburbanforest:month_fact8` +
    params$`ext_day_wbtextremehighwbt:loc_haburbanforest:month_fact8`
  
  params$exurbanopen_notext_6 <-
    params$`Intercept` + params$`loc_habexurbanopen`
  params$exurbanopen_ext_6 <-
    params$`Intercept` + params$`loc_habexurbanopen` + params$`ext_day_wbtextreme` +
    params$`ext_day_wbtextreme:loc_habexurbanopen`
  params$exurbanopen_extwbt_6 <-
    params$`Intercept` + params$`loc_habexurbanopen` + params$`ext_day_wbtextremehighwbt` +
    params$`ext_day_wbtextremehighwbt:loc_habexurbanopen`
  params$exurbanopen_notext_7 <-
    params$`Intercept` + params$`loc_habexurbanopen` + params$`month_fact7` +
    params$`loc_habexurbanopen:month_fact7`
  params$exurbanopen_ext_7 <-
    params$`Intercept` + params$`loc_habexurbanopen` + params$`ext_day_wbtextreme` +
    params$`month_fact7` + params$`ext_day_wbtextreme:loc_habexurbanopen` +
    params$`ext_day_wbtextreme:month_fact7` +  params$`loc_habexurbanopen:month_fact7` +
    params$`ext_day_wbtextreme:loc_habexurbanopen:month_fact7`
  params$exurbanopen_extwbt_7 <-
    params$`Intercept` + params$`loc_habexurbanopen` + params$`ext_day_wbtextremehighwbt` +
    params$`month_fact7` + params$`ext_day_wbtextremehighwbt:loc_habexurbanopen` +
    params$`ext_day_wbtextremehighwbt:month_fact7` +  params$`loc_habexurbanopen:month_fact7` +
    params$`ext_day_wbtextremehighwbt:loc_habexurbanopen:month_fact7`
  params$exurbanopen_notext_8 <-
    params$`Intercept` + params$`loc_habexurbanopen` + params$`month_fact8` +
    params$`loc_habexurbanopen:month_fact8`
  params$exurbanopen_ext_8 <-
    params$`Intercept` + params$`loc_habexurbanopen` + params$`ext_day_wbtextreme` +
    params$`month_fact8` + params$`ext_day_wbtextreme:loc_habexurbanopen` +
    params$`ext_day_wbtextreme:month_fact8` +  params$`loc_habexurbanopen:month_fact8` +
    params$`ext_day_wbtextreme:loc_habexurbanopen:month_fact8`
  params$exurbanopen_extwbt_8 <-
    params$`Intercept` + params$`loc_habexurbanopen` + params$`ext_day_wbtextremehighwbt` +
    params$`month_fact8` + params$`ext_day_wbtextremehighwbt:loc_habexurbanopen` +
    params$`ext_day_wbtextremehighwbt:month_fact8` +  params$`loc_habexurbanopen:month_fact8` +
    params$`ext_day_wbtextremehighwbt:loc_habexurbanopen:month_fact8`
  
  params$exurbanforest_notext_6 <-
    params$`Intercept` + params$`loc_habexurbanforest`
  params$exurbanforest_ext_6 <-
    params$`Intercept` + params$`loc_habexurbanforest` + params$`ext_day_wbtextreme` +
    params$`ext_day_wbtextreme:loc_habexurbanforest`
  params$exurbanforest_extwbt_6 <-
    params$`Intercept` + params$`loc_habexurbanforest` + params$`ext_day_wbtextremehighwbt` +
    params$`ext_day_wbtextremehighwbt:loc_habexurbanforest`
  params$exurbanforest_notext_7 <-
    params$`Intercept` + params$`loc_habexurbanforest` + params$`month_fact7` +
    params$`loc_habexurbanforest:month_fact7`
  params$exurbanforest_ext_7 <-
    params$`Intercept` + params$`loc_habexurbanforest` + params$`ext_day_wbtextreme` +
    params$`month_fact7` + params$`ext_day_wbtextreme:loc_habexurbanforest` +
    params$`ext_day_wbtextreme:month_fact7` +  params$`loc_habexurbanforest:month_fact7` +
    params$`ext_day_wbtextreme:loc_habexurbanforest:month_fact7`
  params$exurbanforest_extwbt_7 <-
    params$`Intercept` + params$`loc_habexurbanforest` + params$`ext_day_wbtextremehighwbt` +
    params$`month_fact7` + params$`ext_day_wbtextremehighwbt:loc_habexurbanforest` +
    params$`ext_day_wbtextremehighwbt:month_fact7` +  params$`loc_habexurbanforest:month_fact7` +
    params$`ext_day_wbtextremehighwbt:loc_habexurbanforest:month_fact7`
  params$exurbanforest_notext_8 <-
    params$`Intercept` + params$`loc_habexurbanforest` + params$`month_fact8` +
    params$`loc_habexurbanforest:month_fact8`
  params$exurbanforest_ext_8 <-
    params$`Intercept` + params$`loc_habexurbanforest` + params$`ext_day_wbtextreme` +
    params$`month_fact8` + params$`ext_day_wbtextreme:loc_habexurbanforest` +
    params$`ext_day_wbtextreme:month_fact8` +  params$`loc_habexurbanforest:month_fact8` +
    params$`ext_day_wbtextreme:loc_habexurbanforest:month_fact8`
  params$exurbanforest_extwbt_8 <-
    params$`Intercept` + params$`loc_habexurbanforest` + params$`ext_day_wbtextremehighwbt` +
    params$`month_fact8` + params$`ext_day_wbtextremehighwbt:loc_habexurbanforest` +
    params$`ext_day_wbtextremehighwbt:month_fact8` +  params$`loc_habexurbanforest:month_fact8` +
    params$`ext_day_wbtextremehighwbt:loc_habexurbanforest:month_fact8`
  
  
  colnames(params)
  
  library(dplyr)
  params_cnt <- params %>%
    mutate(across(
      .cols = 38:73,
      .fns = exp,
      .names = "{.col}_cnt"
    ))
  detach("package:dplyr", unload = TRUE)
  
  #get means into data frame for plotting
  colnames(params_cnt)
  est <-
    data.frame(mean = apply(params_cnt[, c(74:109)], 2, FUN = mean, na.rm = TRUE))
  est <-
    cbind(est, t(apply(
      params_cnt[, c(74:109)],
      2,
      quantile,
      probs = c(0.025, 0.975),
      na.rm = TRUE
    )))
  
  est$loc_hab <-
    c(
      rep("urban open", 9),
      rep("urban forest", 9),
      rep("exurban open", 9),
      rep("exurban forest", 9)
    )
  est$extreme <- rep(c("not extreme", "extreme", "extreme high wbt"), 12)
  est$month <-
    rep(c("June", "June", "June", "July", "July", "July", "August", "August", "August"), 4)
  colnames(est)[2:3] <- c("LCI", "UCI")
  
  }
  
 
  }
  
  cat(
    "Caution (read): this is a precarious step! \n
    Function is specific to 'ext_day_fact * loc_hab * month_fact' main effect! \r
    Also works with 'ext_day_vpd * loc_hab * month_fact' main effect. \r
    Also works with 'ext_day_wbt * loc_hab * month_fact' main effect. \r
    Only use with models that include these variables entered in this order! \r
    Must correctly specify the extreme_var as an argument in the function! \r
    These variables must be specified in the model above or estimates will be wrong! \r
    Factor levels must be ordered as in script above or estimates will be wrong!\n
    Have a nice day🙂!"
  )
  
  return(est)
  
}


#-------------------------------------------------------------------------------
#Make the plot------------------------------------------------------------------

#Define function to make plots for models with three way interactions
#with 'ext_day_fact * loc_hab * month_fact' in the model

ext_day.plot <- function(est,
                         response,
                         variable,
                         date = "12-11-24",
                         path = "./Figures_12-4-24/") {
  
  
  est$extreme <- as.factor(est$extreme)
  
  if (variable == "ext_day_fact") {
    
  
    est$loc_hab <- as.factor(est$loc_hab)
    est$loc_hab <-
      factor(est$loc_hab,
             levels = c("exurban forest", "exurban open", "urban forest", "urban open"))
    
    est$extreme <-
      factor(est$extreme,
             levels = c("not extreme", "extreme"))
    
    est$month <- as.factor(est$month)
    est$month <-
      factor(est$month,
             levels = c("June", "July", "August"))
    
    
    effect_plot <-
      ggplot(est, aes(
        loc_hab,
        mean,
        color = factor(extreme),
        fill = factor(extreme)
      )) +
      facet_grid(rows = vars(month), scales = "free") +
      geom_errorbar(
        aes(
          y = mean,
          ymin = LCI,
          ymax = UCI,
          color = factor(extreme)
        ),
        position = position_dodge(width = 0.4),
        size = 2,
        width = 0
      ) +
      geom_point(
        aes(y = mean, color = factor(extreme)),
        fill = "grey85",
        position = position_dodge(width = 0.4),
        shape = 21,
        size = 2.5,
        stroke = 3.0
      ) +
      scale_color_manual(values = c("#44039eff", "#dd5e66ff"),
                         na.translate = F) +
      scale_fill_manual(values = c("#44039eff", "#dd5e66ff"),
                        na.translate = F) +
      theme(panel.grid.minor = element_line(colour = "white")) +
      theme(panel.grid.major = element_line(colour = "white")) +
      theme(panel.border = element_rect(color = "black", size = 1)) +
      theme(axis.title.x = element_blank()) +
      theme(legend.title = element_blank()) +
      #theme(legend.position="none") +
      labs(y = expression("Vocalization activity")) +
      # scale_x_discrete(labels=c("Not extreme", "Extreme",
      #                           "Extreme \n+ high VPD")) +
      theme(strip.text = element_text(size = 12)) +
      theme(axis.text = element_text(size = 14)) +
      theme(axis.title.y = element_text(size = 14)) +
      theme(legend.text = element_text(size = 14)) + #+
      #scale_y_continuous(limits = c(0, 5)) +
      #theme(axis.text.x=element_text(size = 14, angle=45, hjust=1)) +
      theme(plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"))
    effect_plot
    
    fname <-
      paste0("ext_day_fact_hab_month_", response, "_", date, ".png")
    
    ggsave(
      fname,
      plot = effect_plot,
      path = path,
      scale = 1,
      width = 8.5,
      height = 6.0,
      units = c("in"),
      dpi = 600
    )
    
    effect_plot
    
#Define function to make plots for models with three way interactions
#with 'ext_day_vpd * loc_hab * month_fact'  or
#with 'ext_day_wbt * loc_hab * month_fact' in the model


# ext_vpd.plot <-
#   function(est,
#            response,
#            variable,
#            date = "12-8.24",
#            path = "./Figures_12-4-24/") {
#     
#     est$extreme <- as.factor(est$extreme)
#     
    
  } else{
    
    if (variable == "ext_day_vpd") {
      est$extreme <-
        factor(est$extreme,
               levels = c("not extreme", "extreme", "extreme high vpd"))
      
    } else{
      est$extreme <-
        factor(est$extreme,
               levels = c("not extreme", "extreme", "extreme high wbt"))
      
    }
    
    est$loc_hab <- as.factor(est$loc_hab)
    est$loc_hab <-
      factor(est$loc_hab,
             levels = c("exurban forest", "exurban open", "urban forest", "urban open"))
    
    est$month <- as.factor(est$month)
    est$month <-
      factor(est$month,
             levels = c("June", "July", "August"))
    
    effect_plot <-
      ggplot(est, aes(
        loc_hab,
        mean,
        color = factor(extreme),
        fill = factor(extreme)
      )) +
      facet_grid(rows = vars(month), scales = "free") +
      geom_errorbar(
        aes(
          y = mean,
          ymin = LCI,
          ymax = UCI,
          color = factor(extreme)
        ),
        position = position_dodge(width = 0.55),
        size = 2,
        width = 0
      ) +
      geom_point(
        aes(y = mean, color = factor(extreme)),
        fill = "grey85",
        position = position_dodge(width = 0.55),
        shape = 21,
        size = 2.5,
        stroke = 3.0
      ) +
      scale_color_manual(
        values = c("#44039eff", "#dd5e66ff", "#fba238ff"),
        na.translate = F
      ) +
      scale_fill_manual(
        values = c("#44039eff", "#dd5e66ff", "#fba238ff"),
        na.translate = F
      ) +
      theme(panel.grid.minor = element_line(colour = "white")) +
      theme(panel.grid.major = element_line(colour = "white")) +
      theme(panel.border = element_rect(color = "black", size = 1)) +
      theme(axis.title.x = element_blank()) +
      theme(legend.title = element_blank()) +
      #theme(legend.position="none") +
      labs(y = expression("Vocalization activity")) +
      # scale_x_discrete(labels=c("Not extreme", "Extreme",
      #                           "Extreme \n+ high VPD")) +
      theme(strip.text = element_text(size = 12)) +
      theme(axis.text = element_text(size = 14)) +
      theme(axis.title.y = element_text(size = 14)) +
      theme(legend.text = element_text(size = 14)) + #+
      #scale_y_continuous(limits = c(0, 5)) +
      #theme(axis.text.x=element_text(size = 14, angle=45, hjust=1)) +
      theme(plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"))
    effect_plot
    
    fname <- paste0(variable, "_hab_month_", response, "_", date, ".png")
    
    ggsave(
      fname,
      plot = effect_plot,
      path = path,
      scale = 1,
      width = 8.8,
      height = 6.0,
      units = c("in"),
      dpi = 600
    )
    
    effect_plot
    
    
  }
    
    return(effect_plot)
    
  }
