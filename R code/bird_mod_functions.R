

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
    #main_txt <- str_replace_all(main_txt, "\\+", "_")
    
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
#Function to extract two-way interaction terms--------------------------------


#define function for ext_day_fact * loc_hab * month_fact' interaction
ext_day.2int.est <- function(brmsmod, extreme_var) {
  
  #Get posterior estimates of there-way interactions--------------------------------
  params <- as.data.frame(fixef(brmsmod, summary = FALSE)) #
  dim(params) #525  25
  
  if(extreme_var == "ext_day_fact"){ 
    
    #Calculate mean biomass and slopes for each category
    params$urbanopen_notext <-  params$`Intercept`
    params$urbanopen_ext <-
      params$`Intercept` + params$`ext_day_factextreme`
    params$urbanforest_notext <-
      params$`Intercept` + params$`loc_haburbanforest`
    params$urbanforest_ext <-
      params$`Intercept` + params$`loc_haburbanforest` + 
      params$`ext_day_factextreme` + params$`ext_day_factextreme:loc_haburbanforest`
    params$exurbanopen_notext <-
      params$`Intercept` + params$`loc_habexurbanopen`
    params$exurbanopen_ext <-
      params$`Intercept` + params$`loc_habexurbanopen` + 
      params$`ext_day_factextreme` + params$`ext_day_factextreme:loc_habexurbanopen`
    params$exurbanforest_notext <-
      params$`Intercept` + params$`loc_habexurbanforest`
    params$exurbanforest_ext <-
      params$`Intercept` + params$`loc_habexurbanforest` + 
      params$`ext_day_factextreme` + params$`ext_day_factextreme:loc_habexurbanforest`
    
    #calculate contrasts
    
    params$urbanopen_cntrst <- params$urbanopen_notext - params$urbanopen_ext
    params$urbanforest_cntrst <- params$urbanforest_notext - params$urbanforest_ext
    params$exurbanopen_cntrst <- params$exurbanopen_notext - params$exurbanopen_ext
    params$exurbanforest_cntrst <- params$exurbanforest_notext - params$exurbanforest_ext
    
    
    cntrst_cols <- c("urbanopen_cntrst", "urbanforest_cntrst", "exurbanopen_cntrst",
                     "exurbanforest_cntrst")
    
    contrasts <-
      data.frame(mean = apply(params[, cntrst_cols], 2, FUN = mean, na.rm = TRUE))
    contrasts <-
      cbind(contrasts, t(apply(
        params[, cntrst_cols],
        2,
        quantile,
        probs = c(0.025, 0.975),
        na.rm = TRUE
      )))
    
    colnames(contrasts)[2:3] <- c("LCI", "UCI")
    contrasts$contrasts <- rownames(contrasts)
    contrasts$sig <-ifelse(contrasts$LCI < 0 & contrasts$UCI > 0, "No", "Yes")
    
    contrasts$outcome <- ifelse(
      contrasts$sig == "Yes" &
        contrasts$mean > 0,
      "less vocalization during extremes",
      
      ifelse(
        contrasts$sig == "Yes" &
          contrasts$mean < 0,
        "more vocalization during extremes",
        " - "
      )
    )
    
    
  } else{
    
    
    if(extreme_var == "ext_day_vpd"){ 
      
      #Calculate estimates for each combination of vactor levels
      params$urbanopen_notext <-  params$`Intercept`
      params$urbanopen_ext <-
        params$`Intercept` + params$`ext_day_vpdextreme`
      params$urbanopen_extvpd <-
        params$`Intercept` + params$`ext_day_vpdextremehighvpd`
      
      params$urbanforest_notext <-
        params$`Intercept` + params$`loc_haburbanforest`
      params$urbanforest_ext <-
        params$`Intercept` + params$`loc_haburbanforest` + 
        params$`ext_day_vpdextreme` + params$`ext_day_vpdextreme:loc_haburbanforest`
      params$urbanforest_extvpd <-
        params$`Intercept` + params$`loc_haburbanforest` + 
        params$`ext_day_vpdextremehighvpd` + params$`ext_day_vpdextremehighvpd:loc_haburbanforest`
      
      params$exurbanopen_notext <-
        params$`Intercept` + params$`loc_habexurbanopen`
      params$exurbanopen_ext <-
        params$`Intercept` + params$`loc_habexurbanopen` + 
        params$`ext_day_vpdextreme` + params$`ext_day_vpdextreme:loc_habexurbanopen`
      params$exurbanopen_extvpd <-
        params$`Intercept` + params$`loc_habexurbanopen` + 
        params$`ext_day_vpdextremehighvpd` + params$`ext_day_vpdextremehighvpd:loc_habexurbanopen`
      
      params$exurbanforest_notext <-
        params$`Intercept` + params$`loc_habexurbanforest`
      params$exurbanforest_ext <-
        params$`Intercept` + params$`loc_habexurbanforest` + 
        params$`ext_day_vpdextreme` + params$`ext_day_vpdextreme:loc_habexurbanforest`
      params$exurbanforest_extvpd <-
        params$`Intercept` + params$`loc_habexurbanforest` + 
        params$`ext_day_vpdextremehighvpd` + params$`ext_day_vpdextremehighvpd:loc_habexurbanforest`
      
      
      
      #calculate contrasts
      params$urbanopen_cntrst <- params$urbanopen_notext - params$urbanopen_ext
      params$urbanopen_vpd_cntrst <- params$urbanopen_notext - params$urbanopen_extvpd
      
      params$urbanforest_cntrst <- params$urbanforest_notext - params$urbanforest_ext
      params$urbanforest_vpd_cntrst <- params$urbanforest_notext - params$urbanforest_extvpd
      
      params$exurbanopen_cntrst <- params$exurbanopen_notext - params$exurbanopen_ext
      params$exurbanopen_vpd_cntrst <- params$exurbanopen_notext - params$exurbanopen_extvpd
      
      params$exurbanforest_cntrst <- params$exurbanforest_notext - params$exurbanforest_ext
      params$exurbanforest_vpd_cntrst <- params$exurbanforest_notext - params$exurbanforest_extvpd
      
      
      cntrst_cols <- c("urbanopen_cntrst",  "urbanopen_vpd_cntrst", "urbanforest_cntrst", 
                       "urbanforest_vpd_cntrst", "exurbanopen_cntrst", "exurbanopen_vpd_cntrst",
                       "exurbanforest_cntrst", "exurbanforest_vpd_cntrst")
      
      contrasts <-
        data.frame(mean = apply(params[, cntrst_cols], 2, FUN = mean, na.rm = TRUE))
      contrasts <-
        cbind(contrasts, t(apply(
          params[, cntrst_cols],
          2,
          quantile,
          probs = c(0.025, 0.975),
          na.rm = TRUE
        )))
      
      colnames(contrasts)[2:3] <- c("LCI", "UCI")
      contrasts$contrasts <- rownames(contrasts)
      contrasts$sig <-ifelse(contrasts$LCI < 0 & contrasts$UCI > 0, "No", "Yes")
      
      contrasts$outcome <- ifelse(
        contrasts$sig == "Yes" &
          contrasts$mean > 0,
        "less vocalization during extremes",
        
        ifelse(
          contrasts$sig == "Yes" &
            contrasts$mean < 0,
          "more vocalization during extremes",
          " - "
        )
      )
      
    } else{
      
      #Calculate estimates for each combination of vactor levels
      params$urbanopen_notext <-  params$`Intercept`
      params$urbanopen_ext <-
        params$`Intercept` + params$`ext_day_wbtextreme`
      params$urbanopen_extwbt <-
        params$`Intercept` + params$`ext_day_wbtextremehighwbt`
      
      params$urbanforest_notext <-
        params$`Intercept` + params$`loc_haburbanforest`
      params$urbanforest_ext <-
        params$`Intercept` + params$`loc_haburbanforest` + 
        params$`ext_day_wbtextreme` + params$`ext_day_wbtextreme:loc_haburbanforest`
      params$urbanforest_extwbt <-
        params$`Intercept` + params$`loc_haburbanforest` + 
        params$`ext_day_wbtextremehighwbt` + params$`ext_day_wbtextremehighwbt:loc_haburbanforest`
      
      params$exurbanopen_notext <-
        params$`Intercept` + params$`loc_habexurbanopen`
      params$exurbanopen_ext <-
        params$`Intercept` + params$`loc_habexurbanopen` + 
        params$`ext_day_wbtextreme` + params$`ext_day_wbtextreme:loc_habexurbanopen`
      params$exurbanopen_extwbt <-
        params$`Intercept` + params$`loc_habexurbanopen` + 
        params$`ext_day_wbtextremehighwbt` + params$`ext_day_wbtextremehighwbt:loc_habexurbanopen`
      
      params$exurbanforest_notext <-
        params$`Intercept` + params$`loc_habexurbanforest`
      params$exurbanforest_ext <-
        params$`Intercept` + params$`loc_habexurbanforest` + 
        params$`ext_day_wbtextreme` + params$`ext_day_wbtextreme:loc_habexurbanforest`
      params$exurbanforest_extwbt <-
        params$`Intercept` + params$`loc_habexurbanforest` + 
        params$`ext_day_wbtextremehighwbt` + params$`ext_day_wbtextremehighwbt:loc_habexurbanforest`
      
      
      
      #calculate contrasts
      params$urbanopen_cntrst <- params$urbanopen_notext - params$urbanopen_ext
      params$urbanopen_wbt_cntrst <- params$urbanopen_notext - params$urbanopen_extwbt
      
      params$urbanforest_cntrst <- params$urbanforest_notext - params$urbanforest_ext
      params$urbanforest_wbt_cntrst <- params$urbanforest_notext - params$urbanforest_extwbt
      
      params$exurbanopen_cntrst <- params$exurbanopen_notext - params$exurbanopen_ext
      params$exurbanopen_wbt_cntrst <- params$exurbanopen_notext - params$exurbanopen_extwbt
      
      params$exurbanforest_cntrst <- params$exurbanforest_notext - params$exurbanforest_ext
      params$exurbanforest_wbt_cntrst <- params$exurbanforest_notext - params$exurbanforest_extwbt
      
      
      cntrst_cols <- c("urbanopen_cntrst",  "urbanopen_wbt_cntrst", "urbanforest_cntrst", 
                       "urbanforest_wbt_cntrst", "exurbanopen_cntrst", "exurbanopen_wbt_cntrst",
                       "exurbanforest_cntrst", "exurbanforest_wbt_cntrst")
      
      contrasts <-
        data.frame(mean = apply(params[, cntrst_cols], 2, FUN = mean, na.rm = TRUE))
      contrasts <-
        cbind(contrasts, t(apply(
          params[, cntrst_cols],
          2,
          quantile,
          probs = c(0.025, 0.975),
          na.rm = TRUE
        )))
      
      colnames(contrasts)[2:3] <- c("LCI", "UCI")
      contrasts$contrasts <- rownames(contrasts)
      contrasts$sig <-ifelse(contrasts$LCI < 0 & contrasts$UCI > 0, "No", "Yes")
      
      contrasts$outcome <- ifelse(
        contrasts$sig == "Yes" &
          contrasts$mean > 0,
        "less vocalization during extremes",
        
        ifelse(
          contrasts$sig == "Yes" &
            contrasts$mean < 0,
          "more vocalization during extremes",
          " - "
        )
      )
      
    }
  }
  
  return(contrasts)
  
}


#-------------------------------------------------------------------------------
#Function to extract three-way interaction terms--------------------------------

#define function for ext_day_fact * loc_hab * month_fact' interaction
ext_day.3int.est <- function(brmsmod, extreme_var) {
  
  #Get posterior estimates of there-way interactions--------------------------------
  params <- as.data.frame(fixef(brmsmod, summary = FALSE)) #
  dim(params) #525  25
  
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
    
    #get contrasts
    params$urbanopen_cntrst_6 <- params$urbanopen_notext_6 - params$urbanopen_ext_6
    params$urbanopen_cntrst_7 <- params$urbanopen_notext_7 - params$urbanopen_ext_7
    params$urbanopen_cntrst_8 <- params$urbanopen_notext_8 - params$urbanopen_ext_8
    
    params$urbanforest_cntrst_6 <- params$urbanforest_notext_6 - params$urbanforest_ext_6
    params$urbanforest_cntrst_7 <- params$urbanforest_notext_7 - params$urbanforest_ext_7
    params$urbanforest_cntrst_8 <- params$urbanforest_notext_8 - params$urbanforest_ext_8
    
    params$exurbanopen_cntrst_6 <- params$exurbanopen_notext_6 - params$exurbanopen_ext_6
    params$exurbanopen_cntrst_7 <- params$exurbanopen_notext_7 - params$exurbanopen_ext_7
    params$exurbanopen_cntrst_8 <- params$exurbanopen_notext_8 - params$exurbanopen_ext_8
    
    params$exurbanforest_cntrst_6 <- params$exurbanforest_notext_6 - params$exurbanforest_ext_6
    params$exurbanforest_cntrst_7 <- params$exurbanforest_notext_7 - params$exurbanforest_ext_7
    params$exurbanforest_cntrst_8 <- params$exurbanforest_notext_8 - params$exurbanforest_ext_8
    
    
    cntrst_cols <- c("urbanopen_cntrst_6", "urbanopen_cntrst_7", "urbanopen_cntrst_8",
                     "urbanforest_cntrst_6", "urbanforest_cntrst_7", "urbanforest_cntrst_8",
                     "exurbanopen_cntrst_6", "exurbanopen_cntrst_7", "exurbanopen_cntrst_8",
                     "exurbanforest_cntrst_6", "exurbanforest_cntrst_7", "exurbanforest_cntrst_8")
    
    contrasts <-
      data.frame(mean = apply(params[, cntrst_cols], 2, FUN = mean, na.rm = TRUE))
    contrasts <-
      cbind(contrasts, t(apply(
        params[, cntrst_cols],
        2,
        quantile,
        probs = c(0.025, 0.975),
        na.rm = TRUE
      )))
    
    colnames(contrasts)[2:3] <- c("LCI", "UCI")
    contrasts$contrasts <- rownames(contrasts)
    contrasts$sig <-ifelse(contrasts$LCI < 0 & contrasts$UCI > 0, "No", "Yes")
    
    contrasts$outcome <- ifelse(
      contrasts$sig == "Yes" &
        contrasts$mean > 0,
      "less vocalization during extremes",
      
      ifelse(
        contrasts$sig == "Yes" &
          contrasts$mean < 0,
        "more vocalization during extremes",
        " - "
      )
    )
    
    contrasts$caution <- ifelse(
      contrasts$sig == "Yes" &
        contrasts$mean > 20 | contrasts$mean < -20,
      "large contrast - may be estimation issue",
      " - "
    )
    
    #get estimates as predicted counts for plotting
    
    library(dplyr)
    params_cnt <- params %>%
      mutate(across(
        .cols = 26:49,
        .fns = exp,
        .names = "{.col}_cnt"
      ))
    detach("package:dplyr", unload = TRUE)
    
    #get means into data frame for plotting
    
    cols <-
      c(
        "urbanopen_notext_6_cnt",
        "urbanopen_ext_6_cnt",
        "urbanopen_notext_7_cnt",
        "urbanopen_ext_7_cnt",
        "urbanopen_notext_8_cnt",
        "urbanopen_ext_8_cnt",
        "urbanforest_notext_6_cnt",
        "urbanforest_ext_6_cnt",
        "urbanforest_notext_7_cnt",
        "urbanforest_ext_7_cnt",
        "urbanforest_notext_8_cnt",
        "urbanforest_ext_8_cnt",
        "exurbanopen_notext_6_cnt",
        "exurbanopen_ext_6_cnt",
        "exurbanopen_notext_7_cnt",
        "exurbanopen_ext_7_cnt",
        "exurbanopen_notext_8_cnt",
        "exurbanopen_ext_8_cnt",
        "exurbanforest_notext_6_cnt",
        "exurbanforest_ext_6_cnt",
        "exurbanforest_notext_7_cnt",
        "exurbanforest_ext_7_cnt",
        "exurbanforest_notext_8_cnt",
        "exurbanforest_ext_8_cnt"
      )
    
    est <-
      data.frame(mean = apply(params_cnt[, cols], 2, FUN = mean, na.rm = TRUE))
    est <-
      cbind(est, t(apply(
        params_cnt[, cols],
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
      
      
      #get contrasts
      params$urbanopen_cntrst_6 <- params$urbanopen_notext_6 - params$urbanopen_ext_6
      params$urbanopen_cntrst_7 <- params$urbanopen_notext_7 - params$urbanopen_ext_7
      params$urbanopen_cntrst_8 <- params$urbanopen_notext_8 - params$urbanopen_ext_8
      params$urbanopen_vpd_cntrst_6 <- params$urbanopen_notext_6 - params$urbanopen_extvpd_6
      params$urbanopen_vpd_cntrst_7 <- params$urbanopen_notext_7 - params$urbanopen_extvpd_7
      params$urbanopen_vpd_cntrst_8 <- params$urbanopen_notext_8 - params$urbanopen_extvpd_8
      
      params$urbanforest_cntrst_6 <- params$urbanforest_notext_6 - params$urbanforest_ext_6
      params$urbanforest_cntrst_7 <- params$urbanforest_notext_7 - params$urbanforest_ext_7
      params$urbanforest_cntrst_8 <- params$urbanforest_notext_8 - params$urbanforest_ext_8
      params$urbanforest_vpd_cntrst_6 <- params$urbanforest_notext_6 - params$urbanforest_extvpd_6
      params$urbanforest_vpd_cntrst_7 <- params$urbanforest_notext_7 - params$urbanforest_extvpd_7
      params$urbanforest_vpd_cntrst_8 <- params$urbanforest_notext_8 - params$urbanforest_extvpd_8
      
      params$exurbanopen_cntrst_6 <- params$exurbanopen_notext_6 - params$exurbanopen_ext_6
      params$exurbanopen_cntrst_7 <- params$exurbanopen_notext_7 - params$exurbanopen_ext_7
      params$exurbanopen_cntrst_8 <- params$exurbanopen_notext_8 - params$exurbanopen_ext_8
      params$exurbanopen_vpd_cntrst_6 <- params$exurbanopen_notext_6 - params$exurbanopen_extvpd_6
      params$exurbanopen_vpd_cntrst_7 <- params$exurbanopen_notext_7 - params$exurbanopen_extvpd_7
      params$exurbanopen_vpd_cntrst_8 <- params$exurbanopen_notext_8 - params$exurbanopen_extvpd_8
      
      params$exurbanforest_cntrst_6 <- params$exurbanforest_notext_6 - params$exurbanforest_ext_6
      params$exurbanforest_cntrst_7 <- params$exurbanforest_notext_7 - params$exurbanforest_ext_7
      params$exurbanforest_cntrst_8 <- params$exurbanforest_notext_8 - params$exurbanforest_ext_8
      params$exurbanforest_vpd_cntrst_6 <- params$exurbanforest_notext_6 - params$exurbanforest_extvpd_6
      params$exurbanforest_vpd_cntrst_7 <- params$exurbanforest_notext_7 - params$exurbanforest_extvpd_7
      params$exurbanforest_vpd_cntrst_8 <- params$exurbanforest_notext_8 - params$exurbanforest_extvpd_8
      
      
      cntrst_cols <- c("urbanopen_cntrst_6", "urbanopen_cntrst_7", "urbanopen_cntrst_8",
                       "urbanopen_vpd_cntrst_6", "urbanopen_vpd_cntrst_7", "urbanopen_vpd_cntrst_8",
                       "urbanforest_cntrst_6", "urbanforest_cntrst_7", "urbanforest_cntrst_8",
                       "urbanforest_vpd_cntrst_6", "urbanforest_vpd_cntrst_7", "urbanforest_vpd_cntrst_8",
                       "exurbanopen_cntrst_6", "exurbanopen_cntrst_7", "exurbanopen_cntrst_8",
                       "exurbanopen_vpd_cntrst_6", "exurbanopen_vpd_cntrst_7", "exurbanopen_vpd_cntrst_8",
                       "exurbanforest_cntrst_6", "exurbanforest_cntrst_7", "exurbanforest_cntrst_8",
                       "exurbanforest_vpd_cntrst_6", "exurbanforest_vpd_cntrst_7", "exurbanforest_vpd_cntrst_8")
      
      contrasts <-
        data.frame(mean = apply(params[, cntrst_cols], 2, FUN = mean, na.rm = TRUE))
      contrasts <-
        cbind(contrasts, t(apply(
          params[, cntrst_cols],
          2,
          quantile,
          probs = c(0.025, 0.975),
          na.rm = TRUE
        )))
      
      colnames(contrasts)[2:3] <- c("LCI", "UCI")
      contrasts$contrasts <- rownames(contrasts)
      contrasts$sig <-ifelse(contrasts$LCI < 0 & contrasts$UCI > 0, "No", "Yes")
      
      contrasts$outcome <- ifelse(
        contrasts$sig == "Yes" &
          contrasts$mean > 0,
        "less vocalization during extremes",
        
        ifelse(
          contrasts$sig == "Yes" &
            contrasts$mean < 0,
          "more vocalization during extremes",
          " - "
        )
      )
      
      contrasts$caution <- ifelse(
        contrasts$sig == "Yes" &
          contrasts$mean > 20 | contrasts$mean < -20,
        "large contrast - may be estimation issue",
        " - "
      )
      
      
      colnames(params)
      
      library(dplyr)
      params_cnt <- params %>%
        mutate(across(
          .cols = 38:73,
          .fns = exp,
          .names = "{.col}_cnt"
        ))
      detach("package:dplyr", unload = TRUE)
      
      
      cols <-
        c(
          "urbanopen_notext_6_cnt",
          "urbanopen_ext_6_cnt",
          "urbanopen_extvpd_6_cnt",
          "urbanopen_notext_7_cnt",
          "urbanopen_ext_7_cnt",
          "urbanopen_extvpd_7_cnt",
          "urbanopen_notext_8_cnt",
          "urbanopen_ext_8_cnt",
          "urbanopen_extvpd_8_cnt",
          "urbanforest_notext_6_cnt",
          "urbanforest_ext_6_cnt",
          "urbanforest_extvpd_6_cnt",
          "urbanforest_notext_7_cnt",
          "urbanforest_ext_7_cnt",
          "urbanforest_extvpd_7_cnt",
          "urbanforest_notext_8_cnt",
          "urbanforest_ext_8_cnt",
          "urbanforest_extvpd_8_cnt",
          "exurbanopen_notext_6_cnt",
          "exurbanopen_ext_6_cnt",
          "exurbanopen_extvpd_6_cnt",
          "exurbanopen_notext_7_cnt",
          "exurbanopen_ext_7_cnt",
          "exurbanopen_extvpd_7_cnt",
          "exurbanopen_notext_8_cnt",
          "exurbanopen_ext_8_cnt",
          "exurbanopen_extvpd_8_cnt",
          "exurbanforest_notext_6_cnt",
          "exurbanforest_ext_6_cnt",
          "exurbanforest_extvpd_6_cnt",
          "exurbanforest_notext_7_cnt",
          "exurbanforest_ext_7_cnt",
          "exurbanforest_extvpd_7_cnt",
          "exurbanforest_notext_8_cnt",
          "exurbanforest_ext_8_cnt",
          "exurbanforest_extvpd_8_cnt"
        )
      
      #get means into data frame for plotting
      colnames(params_cnt)
      est <-
        data.frame(mean = apply(params_cnt[, cols], 2, FUN = mean, na.rm = TRUE))
      est <-
        cbind(est, t(apply(
          params_cnt[, cols],
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
      
      
      #get contrasts
      params$urbanopen_cntrst_6 <- params$urbanopen_notext_6 - params$urbanopen_ext_6
      params$urbanopen_cntrst_7 <- params$urbanopen_notext_7 - params$urbanopen_ext_7
      params$urbanopen_cntrst_8 <- params$urbanopen_notext_8 - params$urbanopen_ext_8
      params$urbanopen_wbt_cntrst_6 <- params$urbanopen_notext_6 - params$urbanopen_extwbt_6
      params$urbanopen_wbt_cntrst_7 <- params$urbanopen_notext_7 - params$urbanopen_extwbt_7
      params$urbanopen_wbt_cntrst_8 <- params$urbanopen_notext_8 - params$urbanopen_extwbt_8
      
      params$urbanforest_cntrst_6 <- params$urbanforest_notext_6 - params$urbanforest_ext_6
      params$urbanforest_cntrst_7 <- params$urbanforest_notext_7 - params$urbanforest_ext_7
      params$urbanforest_cntrst_8 <- params$urbanforest_notext_8 - params$urbanforest_ext_8
      params$urbanforest_wbt_cntrst_6 <- params$urbanforest_notext_6 - params$urbanforest_extwbt_6
      params$urbanforest_wbt_cntrst_7 <- params$urbanforest_notext_7 - params$urbanforest_extwbt_7
      params$urbanforest_wbt_cntrst_8 <- params$urbanforest_notext_8 - params$urbanforest_extwbt_8
      
      params$exurbanopen_cntrst_6 <- params$exurbanopen_notext_6 - params$exurbanopen_ext_6
      params$exurbanopen_cntrst_7 <- params$exurbanopen_notext_7 - params$exurbanopen_ext_7
      params$exurbanopen_cntrst_8 <- params$exurbanopen_notext_8 - params$exurbanopen_ext_8
      params$exurbanopen_wbt_cntrst_6 <- params$exurbanopen_notext_6 - params$exurbanopen_extwbt_6
      params$exurbanopen_wbt_cntrst_7 <- params$exurbanopen_notext_7 - params$exurbanopen_extwbt_7
      params$exurbanopen_wbt_cntrst_8 <- params$exurbanopen_notext_8 - params$exurbanopen_extwbt_8
      
      params$exurbanforest_cntrst_6 <- params$exurbanforest_notext_6 - params$exurbanforest_ext_6
      params$exurbanforest_cntrst_7 <- params$exurbanforest_notext_7 - params$exurbanforest_ext_7
      params$exurbanforest_cntrst_8 <- params$exurbanforest_notext_8 - params$exurbanforest_ext_8
      params$exurbanforest_wbt_cntrst_6 <- params$exurbanforest_notext_6 - params$exurbanforest_extwbt_6
      params$exurbanforest_wbt_cntrst_7 <- params$exurbanforest_notext_7 - params$exurbanforest_extwbt_7
      params$exurbanforest_wbt_cntrst_8 <- params$exurbanforest_notext_8 - params$exurbanforest_extwbt_8
      
      
      cntrst_cols <- c("urbanopen_cntrst_6", "urbanopen_cntrst_7", "urbanopen_cntrst_8",
                       "urbanopen_wbt_cntrst_6", "urbanopen_wbt_cntrst_7", "urbanopen_wbt_cntrst_8",
                       "urbanforest_cntrst_6", "urbanforest_cntrst_7", "urbanforest_cntrst_8",
                       "urbanforest_wbt_cntrst_6", "urbanforest_wbt_cntrst_7", "urbanforest_wbt_cntrst_8",
                       "exurbanopen_cntrst_6", "exurbanopen_cntrst_7", "exurbanopen_cntrst_8",
                       "exurbanopen_wbt_cntrst_6", "exurbanopen_wbt_cntrst_7", "exurbanopen_wbt_cntrst_8",
                       "exurbanforest_cntrst_6", "exurbanforest_cntrst_7", "exurbanforest_cntrst_8",
                       "exurbanforest_wbt_cntrst_6", "exurbanforest_wbt_cntrst_7", "exurbanforest_wbt_cntrst_8")
      
      contrasts <-
        data.frame(mean = apply(params[, cntrst_cols], 2, FUN = mean, na.rm = TRUE))
      contrasts <-
        cbind(contrasts, t(apply(
          params[, cntrst_cols],
          2,
          quantile,
          probs = c(0.025, 0.975),
          na.rm = TRUE
        )))
      
      colnames(contrasts)[2:3] <- c("LCI", "UCI")
      contrasts$contrasts <- rownames(contrasts)
      contrasts$sig <-ifelse(contrasts$LCI < 0 & contrasts$UCI > 0, "No", "Yes")
      
      contrasts$outcome <- ifelse(
        contrasts$sig == "Yes" &
          contrasts$mean > 0,
        "less vocalization during extremes",
        
        ifelse(
          contrasts$sig == "Yes" &
            contrasts$mean < 0,
          "more vocalization during extremes",
          " - "
        )
      )
      
      contrasts$caution <- ifelse(
        contrasts$sig == "Yes" &
          contrasts$mean > 20 | contrasts$mean < -20,
        "large contrast - may be estimation issue",
        " - "
      )
      
      library(dplyr)
      params_cnt <- params %>%
        mutate(across(
          .cols = 38:73,
          .fns = exp,
          .names = "{.col}_cnt"
        ))
      detach("package:dplyr", unload = TRUE)
      
      cols <-
        c(
          "urbanopen_notext_6_cnt",
          "urbanopen_ext_6_cnt",
          "urbanopen_extwbt_6_cnt",
          "urbanopen_notext_7_cnt",
          "urbanopen_ext_7_cnt",
          "urbanopen_extwbt_7_cnt",
          "urbanopen_notext_8_cnt",
          "urbanopen_ext_8_cnt",
          "urbanopen_extwbt_8_cnt",
          "urbanforest_notext_6_cnt",
          "urbanforest_ext_6_cnt",
          "urbanforest_extwbt_6_cnt",
          "urbanforest_notext_7_cnt",
          "urbanforest_ext_7_cnt",
          "urbanforest_extwbt_7_cnt",
          "urbanforest_notext_8_cnt",
          "urbanforest_ext_8_cnt",
          "urbanforest_extwbt_8_cnt",
          "exurbanopen_notext_6_cnt",
          "exurbanopen_ext_6_cnt",
          "exurbanopen_extwbt_6_cnt",
          "exurbanopen_notext_7_cnt",
          "exurbanopen_ext_7_cnt",
          "exurbanopen_extwbt_7_cnt",
          "exurbanopen_notext_8_cnt",
          "exurbanopen_ext_8_cnt",
          "exurbanopen_extwbt_8_cnt",
          "exurbanforest_notext_6_cnt",
          "exurbanforest_ext_6_cnt",
          "exurbanforest_extwbt_6_cnt",
          "exurbanforest_notext_7_cnt",
          "exurbanforest_ext_7_cnt",
          "exurbanforest_extwbt_7_cnt",
          "exurbanforest_notext_8_cnt",
          "exurbanforest_ext_8_cnt",
          "exurbanforest_extwbt_8_cnt"
        )
      
      
      #get means into data frame for plotting
      colnames(params_cnt)
      est <-
        data.frame(mean = apply(params_cnt[, cols], 2, FUN = mean, na.rm = TRUE))
      est <-
        cbind(est, t(apply(
          params_cnt[, cols],
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
  
  
  est_cntrst <- list(est, contrasts)
  names(est_cntrst) <- c("count estimates", "contrasts")
  
  return(est_cntrst)
  
}


#-------------------------------------------------------------------------------
#Function to extract interaction terms for plotting diel analysis-------------------------

#define function for ext_day_fact * loc_hab * month_fact' interaction
diel_est <- function(brmsmod, interaction = "two-way") {
  
  
  #Get posterior estimates of there-way interactions--------------------------------
  params <- as.data.frame(fixef(brmsmod, summary = FALSE)) #
  dim(params) #525  25
  
  if(interaction == "two-way"){ 
    
    #Calculate means for each category
    params$morning_notext <-  params$`Intercept`
    params$morning_ext <-
      params$`Intercept` + params$`ext_day_factextreme`
    params$midday_notext <-
      params$`Intercept` + params$`periodmidday`
    params$midday_ext <-
      params$`Intercept` + params$`ext_day_factextreme` + 
      params$`periodmidday` + params$`ext_day_factextreme:periodmidday`
    params$evening_notext <-
      params$`Intercept` + params$`periodevening`
    params$evening_ext <-
      params$`Intercept` + params$`ext_day_factextreme` + 
      params$`periodevening` + params$`ext_day_factextreme:periodevening`
    
    #calculate contrasts
    
    params$morning_cntrst <- params$morning_notext - params$morning_ext
    params$midday_cntrst <- params$midday_notext - params$midday_ext
    params$evening_cntrst <- params$evening_notext - params$evening_ext
    
    
    cntrst_cols <- c("morning_cntrst", "midday_cntrst", "evening_cntrst")
    
    contrasts <-
      data.frame(mean = apply(params[, cntrst_cols], 2, FUN = mean, na.rm = TRUE))
    contrasts <-
      cbind(contrasts, t(apply(
        params[, cntrst_cols],
        2,
        quantile,
        probs = c(0.025, 0.975),
        na.rm = TRUE
      )))
    
    colnames(contrasts)[2:3] <- c("LCI", "UCI")
    contrasts$contrasts <- rownames(contrasts)
    contrasts$sig <-ifelse(contrasts$LCI < 0 & contrasts$UCI > 0, "No", "Yes")
    
    contrasts$outcome <- ifelse(
      contrasts$sig == "Yes" &
        contrasts$mean > 0,
      "less vocalization during extremes",
      
      ifelse(
        contrasts$sig == "Yes" &
          contrasts$mean < 0,
        "more vocalization during extremes",
        " - "
      )
    )
    
    return(contrasts)
    
  } else{
    
    
    #Calculate mean biomass and slopes for each category
    params$urbanopen_notext_morn <-  params$`Intercept`
    params$urbanopen_ext_morn <-
      params$`Intercept` + params$`ext_day_factextreme`
    params$urbanopen_notext_mid <-
      params$`Intercept` + params$`periodmidday`
    params$urbanopen_ext_mid <-
      params$`Intercept` + params$`ext_day_factextreme` + params$`periodmidday`
    params$urbanopen_notext_eve <-
      params$`Intercept` + params$`periodevening`
    params$urbanopen_ext_eve <-
      params$`Intercept` + params$`ext_day_factextreme` + params$`periodevening`
    
    params$urbanforest_notext_morn <-
      params$`Intercept` + params$`loc_haburbanforest`
    params$urbanforest_ext_morn <-
      params$`Intercept` + params$`loc_haburbanforest` + params$`ext_day_factextreme` +
      params$`ext_day_factextreme:loc_haburbanforest`
    params$urbanforest_notext_mid <-
      params$`Intercept` + params$`loc_haburbanforest` + params$`periodmidday` +
      params$`loc_haburbanforest:periodmidday`
    params$urbanforest_ext_mid <-
      params$`Intercept` + params$`loc_haburbanforest` + params$`ext_day_factextreme` +
      params$`periodmidday` + params$`ext_day_factextreme:loc_haburbanforest` +
      params$`ext_day_factextreme:periodmidday` +  params$`loc_haburbanforest:periodmidday` +
      params$`ext_day_factextreme:loc_haburbanforest:periodmidday`
    params$urbanforest_notext_eve <-
      params$`Intercept` + params$`loc_haburbanforest` + params$`periodevening` +
      params$`loc_haburbanforest:periodevening`
    params$urbanforest_ext_eve <-
      params$`Intercept` + params$`loc_haburbanforest` + params$`ext_day_factextreme` +
      params$`periodevening` + params$`ext_day_factextreme:loc_haburbanforest` +
      params$`ext_day_factextreme:periodevening` +  params$`loc_haburbanforest:periodevening` +
      params$`ext_day_factextreme:loc_haburbanforest:periodevening`
    
    params$exurbanopen_notext_morn <-
      params$`Intercept` + params$`loc_habexurbanopen`
    params$exurbanopen_ext_morn <-
      params$`Intercept` + params$`loc_habexurbanopen` + params$`ext_day_factextreme` +
      params$`ext_day_factextreme:loc_habexurbanopen`
    params$exurbanopen_notext_mid <-
      params$`Intercept` + params$`loc_habexurbanopen` + params$`periodmidday` +
      params$`loc_habexurbanopen:periodmidday`
    params$exurbanopen_ext_mid <-
      params$`Intercept` + params$`loc_habexurbanopen` + params$`ext_day_factextreme` +
      params$`periodmidday` + params$`ext_day_factextreme:loc_habexurbanopen` +
      params$`ext_day_factextreme:periodmidday` +  params$`loc_habexurbanopen:periodmidday` +
      params$`ext_day_factextreme:loc_habexurbanopen:periodmidday`
    params$exurbanopen_notext_eve <-
      params$`Intercept` + params$`loc_habexurbanopen` + params$`periodevening` +
      params$`loc_habexurbanopen:periodevening`
    params$exurbanopen_ext_eve <-
      params$`Intercept` + params$`loc_habexurbanopen` + params$`ext_day_factextreme` +
      params$`periodevening` + params$`ext_day_factextreme:loc_habexurbanopen` +
      params$`ext_day_factextreme:periodevening` +  params$`loc_habexurbanopen:periodevening` +
      params$`ext_day_factextreme:loc_habexurbanopen:periodevening`
    
    params$exurbanforest_notext_morn <-
      params$`Intercept` + params$`loc_habexurbanforest`
    params$exurbanforest_ext_morn <-
      params$`Intercept` + params$`loc_habexurbanforest` + params$`ext_day_factextreme` +
      params$`ext_day_factextreme:loc_habexurbanforest`
    params$exurbanforest_notext_mid <-
      params$`Intercept` + params$`loc_habexurbanforest` + params$`periodmidday` +
      params$`loc_habexurbanforest:periodmidday`
    params$exurbanforest_ext_mid <-
      params$`Intercept` + params$`loc_habexurbanforest` + params$`ext_day_factextreme` +
      params$`periodmidday` + params$`ext_day_factextreme:loc_habexurbanforest` +
      params$`ext_day_factextreme:periodmidday` +  params$`loc_habexurbanforest:periodmidday` +
      params$`ext_day_factextreme:loc_habexurbanforest:periodmidday`
    params$exurbanforest_notext_eve <-
      params$`Intercept` + params$`loc_habexurbanforest` + params$`periodevening` +
      params$`loc_habexurbanforest:periodevening`
    params$exurbanforest_ext_eve <-
      params$`Intercept` + params$`loc_habexurbanforest` + params$`ext_day_factextreme` +
      params$`periodevening` + params$`ext_day_factextreme:loc_habexurbanforest` +
      params$`ext_day_factextreme:periodevening` +  params$`loc_habexurbanforest:periodevening` +
      params$`ext_day_factextreme:loc_habexurbanforest:periodevening`
    
    colnames(params)
    
    #get contrasts
    params$urbanopen_cntrst_morn <- params$urbanopen_notext_morn - params$urbanopen_ext_morn
    params$urbanopen_cntrst_mid <- params$urbanopen_notext_mid - params$urbanopen_ext_mid
    params$urbanopen_cntrst_eve <- params$urbanopen_notext_eve - params$urbanopen_ext_eve
    
    params$urbanforest_cntrst_morn <- params$urbanforest_notext_morn - params$urbanforest_ext_morn
    params$urbanforest_cntrst_mid <- params$urbanforest_notext_mid - params$urbanforest_ext_mid
    params$urbanforest_cntrst_eve <- params$urbanforest_notext_eve - params$urbanforest_ext_eve
    
    params$exurbanopen_cntrst_morn <- params$exurbanopen_notext_morn - params$exurbanopen_ext_morn
    params$exurbanopen_cntrst_mid <- params$exurbanopen_notext_mid - params$exurbanopen_ext_mid
    params$exurbanopen_cntrst_eve <- params$exurbanopen_notext_eve - params$exurbanopen_ext_eve
    
    params$exurbanforest_cntrst_morn <- params$exurbanforest_notext_morn - params$exurbanforest_ext_morn
    params$exurbanforest_cntrst_mid <- params$exurbanforest_notext_mid - params$exurbanforest_ext_mid
    params$exurbanforest_cntrst_eve <- params$exurbanforest_notext_eve - params$exurbanforest_ext_eve
    
    
    cntrst_cols <- c("urbanopen_cntrst_morn", "urbanopen_cntrst_mid", "urbanopen_cntrst_eve",
                     "urbanforest_cntrst_morn", "urbanforest_cntrst_mid", "urbanforest_cntrst_eve",
                     "exurbanopen_cntrst_morn", "exurbanopen_cntrst_mid", "exurbanopen_cntrst_eve",
                     "exurbanforest_cntrst_morn", "exurbanforest_cntrst_mid", "exurbanforest_cntrst_eve")
    
    contrasts <-
      data.frame(mean = apply(params[, cntrst_cols], 2, FUN = mean, na.rm = TRUE))
    contrasts <-
      cbind(contrasts, t(apply(
        params[, cntrst_cols],
        2,
        quantile,
        probs = c(0.025, 0.975),
        na.rm = TRUE
      )))
    
    colnames(contrasts)[2:3] <- c("LCI", "UCI")
    contrasts$contrasts <- rownames(contrasts)
    contrasts$sig <-ifelse(contrasts$LCI < 0 & contrasts$UCI > 0, "No", "Yes")
    
    contrasts$outcome <- ifelse(
      contrasts$sig == "Yes" &
        contrasts$mean > 0,
      "less vocalization during extremes",
      
      ifelse(
        contrasts$sig == "Yes" &
          contrasts$mean < 0,
        "more vocalization during extremes",
        " - "
      )
    )
    
    contrasts$caution <- ifelse(
      contrasts$sig == "Yes" &
        contrasts$mean > 20 | contrasts$mean < -20,
      "large contrast - may be estimation issue",
      " - "
    )
    
    #get estimates as predicted counts for plotting
    
    library(dplyr)
    params_cnt <- params %>%
      mutate(across(
        .cols = 28:51,
        .fns = exp,
        .names = "{.col}_cnt"
      ))
    detach("package:dplyr", unload = TRUE)
    
    #get means into data frame for plotting
    
    cols <-
      c(
        "urbanopen_notext_morn_cnt",
        "urbanopen_ext_morn_cnt",
        "urbanopen_notext_mid_cnt",
        "urbanopen_ext_mid_cnt",
        "urbanopen_notext_eve_cnt",
        "urbanopen_ext_eve_cnt",
        "urbanforest_notext_morn_cnt",
        "urbanforest_ext_morn_cnt",
        "urbanforest_notext_mid_cnt",
        "urbanforest_ext_mid_cnt",
        "urbanforest_notext_eve_cnt",
        "urbanforest_ext_eve_cnt",
        "exurbanopen_notext_morn_cnt",
        "exurbanopen_ext_morn_cnt",
        "exurbanopen_notext_mid_cnt",
        "exurbanopen_ext_mid_cnt",
        "exurbanopen_notext_eve_cnt",
        "exurbanopen_ext_eve_cnt",
        "exurbanforest_notext_morn_cnt",
        "exurbanforest_ext_morn_cnt",
        "exurbanforest_notext_mid_cnt",
        "exurbanforest_ext_mid_cnt",
        "exurbanforest_notext_eve_cnt",
        "exurbanforest_ext_eve_cnt"
      )
    
    est <-
      data.frame(mean = apply(params_cnt[, cols], 2, FUN = mean, na.rm = TRUE))
    est <-
      cbind(est, t(apply(
        params_cnt[, cols],
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
    est$period <-
      rep(c("Morning", "Morning", "Midday", "Midday", "Evening", "Evening"), 4)
    colnames(est)[2:3] <- c("LCI", "UCI")
    
  }
  
  
  est_cntrst <- list(est, contrasts)
  names(est_cntrst) <- c("count estimates", "contrasts")
  
  return(est_cntrst)
  
}


#-------------------------------------------------------------------------------
#Make the plot------------------------------------------------------------------

#Define function to make plots for models with three way interactions
#with 'ext_day_fact * loc_hab * month_fact' in the model

bird_plot_3way <- function(est,
                           response,
                           variable,
                           save = FALSE,
                           date = "12-11-24",
                           path = "./Figures_12-4-24/",
                           facetvar = "month") {
  
  
  est$extreme <- as.factor(est$extreme)
  
  if (variable == "ext_day_fact") {
    
    
    est$loc_hab <- as.factor(est$loc_hab)
    est$loc_hab <-
      factor(est$loc_hab,
             levels = c("exurban forest", "exurban open", "urban forest", "urban open"))
    
    est$extreme <-
      factor(est$extreme,
             levels = c("not extreme", "extreme"))
    
    
    if (facetvar == "month") {
      
      est$month <- as.factor(est$month)
      est$month <-
        factor(est$month,
               levels = c("June", "July", "August"))
      
      est$facetby <- est$month
      est$xvar <- est$loc_hab
      
      fname <-
        paste0("ext_day_fact_hab_month_", response, "_", date, ".png")
      
      
      
    } else{
      
      est$period <- as.factor(est$period)
      est$period <-
        factor(est$period,
               levels = c("Morning", "Midday", "Evening"))
      
      est$facetby <- est$loc_hab
      est$xvar <- est$period
      
      fname <-
        paste0("ext_day_fact_hab_period_", response, "_", date, ".png")
      
      
    }
    
    
    effect_plot <-
      ggplot(est, aes(
        xvar,
        mean,
        color = factor(extreme),
        fill = factor(extreme)
      )) +
      facet_grid(rows = vars(facetby), scales = "free") +
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
    
    
    if (save == TRUE) {
      
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
      
      cat("plot was saved")
      
    } else{
      
      cat("plot was not saved")
      
    }
    
    
    
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
    
    if (save == TRUE) {
      
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
      
      cat("plot was saved")
      
    } else{
      
      cat("plot was not saved")
      
    }
    
    
  }
  
  return(effect_plot)
  
}




# #This plot setup is for examining additive effects
#, two-way interactions, and continuous covariates (e.g., vpd)

# mod <-rewo_ext_day_hab
# var1 <- "ext_day_fact:loc_hab"

bird_plot <- function(mod,
                      response,
                      var1,
                      col = "#8305A7FF",
                      save = FALSE,
                      date = "12-11-24",
                      path = "./figures_12-17-24/") {
  
  ce <- conditional_effects(mod, plot = FALSE)
  ce_temp <- ce[[var1]]
  
  
  if(grepl(":", var1, fixed = TRUE) == TRUE) {
    
    tmp <- strsplit(var1, ":")[[1]]
    
    var1 <- tmp[1]
    var2 <- tmp[2]
    
  } else{
    
    var2 <- var1
    
  }
  
  if(is.numeric(ce_temp[, var1]) == FALSE) {
    
    effect_plot <-
      ggplot(ce_temp,
             aes(
               ce_temp[, var2],
               estimate__,
               color = factor(ce_temp[, var1]),
               fill = factor(ce_temp[, var1])
             )) +
      #facet_grid(rows = vars(month), scales = "free") +
      geom_errorbar(
        aes(
          y = estimate__,
          ymin = lower__,
          ymax = upper__,
          color = factor(ce_temp[, var1])
        ),
        position = position_dodge(width = 0.55),
        size = 2,
        width = 0
      ) +
      geom_point(
        aes(y = estimate__, color = factor(ce_temp[, var1])),
        fill = "grey85",
        position = position_dodge(width = 0.55),
        shape = 21,
        size = 2.5,
        stroke = 3.0
      ) +
      scale_color_manual(
        values = c("#44039eff", "#dd5e66ff", "#fba238ff", "grey45"),
        na.translate = F
      ) +
      scale_fill_manual(
        values = c("#44039eff", "#dd5e66ff", "#fba238ff", "grey45"),
        na.translate = F
      ) +
      theme(panel.grid.minor = element_line(colour = "white")) +
      theme(panel.grid.major = element_line(colour = "white")) +
      theme(panel.border = element_rect(color = "black", size = 1)) +
      theme(axis.title.x = element_blank()) +
      theme(legend.title = element_blank()) +
      #theme(legend.position="none") +
      labs(y = expression("Vocalization activity")) +
      theme(strip.text = element_text(size = 12)) +
      theme(axis.text = element_text(size = 14)) +
      theme(axis.title.y = element_text(size = 14)) +
      theme(legend.text = element_text(size = 14)) + #+
      #scale_y_continuous(limits = c(0, 5)) +
      #theme(axis.text.x=element_text(size = 14, angle=45, hjust=1)) +
      theme(plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"))
    effect_plot
    
    fname <-
      paste0(var1, "_", response, "_add_", date, ".png")
    
    
    if (save == TRUE) {
      
      ggsave(
        fname,
        plot = effect_plot,
        path = path,
        scale = 1,
        width = 8.85,
        height = 5.0,
        units = c("in"),
        dpi = 600
      )
      
      effect_plot
      
      cat("plot was saved")
      
    } else{
      cat("plot was not saved")
      
    }
    
    
  } else{
    
    ifelse(
      var1 == "z.ndsi",
      
      xlabel <- "NDSI",
      
      ifelse(
        var1 == "z.temp_95",
        
        xlabel <- "Air temperature (°C)",
        
        ifelse(
          var1 == "z.vpd_kPa",
          
          xlabel <- "Vapor pressure deficit (kPa)",
          
          ifelse(
            var1 == "z.wb_temp_95",
            
            xlabel <- "Wet bulb temperature (°C)",
            
            xlabel <- "Some other variable (add to plot function)"
          )
          
        )
        
      )
      
    )
    
    
    effect_plot <- ggplot() +
      geom_ribbon(
        data = ce_temp,
        aes(ce_temp[, var1], estimate__, ymin = lower__, ymax = upper__),
        fill = col,
        alpha = 0.25
      ) +
      geom_line(
        data = ce_temp,
        aes(ce_temp[, var1], estimate__, ),
        color = col,
        size = 1.15
      ) +
      theme(legend.title = element_blank()) +
      theme(legend.text = element_text(size = 12)) +
      theme(panel.grid.major = element_line(colour = "white")) +
      theme(panel.grid.minor = element_line(colour = "white")) +
      theme(panel.grid.major.y = element_line(colour = "white")) +
      theme(panel.border = element_rect(color = "black", size = 1)) +
      theme(axis.text = element_text(size = 14)) +
      theme(axis.title.y = element_text(size = 14)) +
      theme(axis.title.x = element_text(size = 14)) +
      scale_x_continuous(expand = c(0, 0)) +
      theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, unit = "cm")) +
      #labs(x = expression("Prior day temperature (°C)")) +
      xlab(xlabel) +
      labs(y = expression("Vacalization activity"))
    effect_plot
    
    fname <-
      paste0(var1, "_", response, "_add_", date, ".png")
    
    if (save == TRUE) {
      ggsave(
        fname,
        plot = effect_plot,
        path = path,
        scale = 1,
        width = 5.5,
        height = 5.0,
        units = c("in"),
        dpi = 600
      )
      
      effect_plot
      
      cat("plot was saved")
      
    } else{
      cat("plot was not saved")
      
    }
    
  }
  
  return(effect_plot)
  
}
