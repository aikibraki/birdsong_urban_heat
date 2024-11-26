###########################################################################################################################################################
###Setting up and testing BridNET##########################################################################################
########################################################################################################################################################
###updated 12-5-23

#To do:
#(1)pre-process audio files (need to add site to filenames)
#Format must be SITEID_YYYYMMDD_HHMMSS for functions to work
#(2)Decide how to handle files after batch processing in BridNET (delete samples with
#human vocal? Using what threshold for confidence? Test further with voice recordings
#at different distances?)
#(3)Develop workflow for calculating soundscape metrics for each file and 
#summarizing

#Following steps here: https://github.com/nationalparkservice/NSNSDAcoustics/blob/main/README.md#running-birdnet-from-rstudio-with-birdnet_analyzer

# install.packages('devtools')
# library(devtools)
# devtools::install_github('nationalparkservice/NSNSDAcoustics')

rm(list=ls())
library(NSNSDAcoustics)

#Installed BirdNET analyzer from here: https://github.com/kahst/BirdNET-Analyzer#setup-windows
#environment location: C:\Users\Nowak\anaconda3\envs\pybirdanalyze

?birdnet_analyzer()

# # Must set environment BEFORE calling in the reticulate package
# Sys.setenv(RETICULATE_PYTHON = "C:/Users/Nowak/anaconda3/envs/pybirdanalyze/python.exe")
# library(reticulate)
# 
# # Set your conda environment
# use_condaenv(condaenv = "pybirdanalyze", required = TRUE)
# #Sometimes this I receive this message: "Error: Unable to find conda binary. Is Anaconda installed?"
# #Remiving and reinstalling "reticulate" package has resolved this (for some reason)
# 
# #Example data--------------------------------------------------------------------
# # Create an audio directory for this example
# dir.create('example-audio-directory')
# 
# # Create a results directory for this example
# dir.create('example-results-directory')

# Read in example wave files
# data(exampleAudio1)
# data(exampleAudio2)
# 
# # Write example waves to example audio directory
# tuneR::writeWave(object = exampleAudio1,
#                  filename = 'example-audio-directory/Rivendell_20210623_113602.wav')
# tuneR::writeWave(object = exampleAudio2,
#                  filename = 'example-audio-directory/Rivendell_20210623_114602.wav')


# # Use optional "audio.files" argument to process specific files
# birdnet_analyzer(audio.directory = 'C:/Users/Nowak/Documents/JUSTIN - Active_Documents/SERC/Research projects/Science and Faith/Acoustic_monitoring/example-audio-directory',
#                  audio.files = 'Rivendell_20210623_113602.wav',
#                  results.directory = 'C:/Users/Nowak/Documents/JUSTIN - Active_Documents/SERC/Research projects/Science and Faith/Acoustic_monitoring/example-results-directory',
#                  birdnet.directory = 'C:/Users/Nowak/Desktop/BirdNET-Analyzer',
#                  use.week = TRUE,
#                  lat = 46.09924,
#                  lon = -123.8765)
#THis did not work

# Write examples of raw BirdNET outputs to example results directory
# data(exampleBirdNET1)
# write.table(x = exampleBirdNET1,
#             file = 'example-results-directory/Rivendell_20210623_113602.BirdNET.results.csv',
#             row.names = FALSE, quote = FALSE, sep = ',')
# data(exampleBirdNET2)
# write.table(x = exampleBirdNET2,
#             file = 'example-results-directory/Rivendell_20210623_114602.BirdNET.results.csv',
#             row.names = FALSE, quote = FALSE, sep = ',')


## ***************************************************************************************************
## Format and verify raw BridNET data data
## ***************************************************************************************************

#Format raw BirdNET files
#Important to know timezone of timestamps for the recorder used.
#From NSNSDAcoustics documentation:
#"If recordings were taken in local time at your study site, specify an 
#Olson-names-formatted character timezone for the location 
#(e.g., "America/Los_Angeles"). https://en.wikipedia.org/wiki/List_of_tz_database_time_zones#List
#If recordings were taken in UTC, #you can put either 'GMT' or 'UTC' (both are acceptable in R for downstream 
#date-time formatting)."
?birdnet_format
birdnet_format(results.directory = 'SERC_test_results',
               timezone = 'US/Eastern')


# Gather formatted BirdNET results
bird.dat <- birdnet_gather(
  results.directory = 'SERC_test_results',
  formatted = TRUE
)

dim(bird.dat) #3671   17

#Here we want to test the effect of different confidence levels on
#detections - rates of false positives/negatives
#Lets look at rates of detection across different conf thresholds for
#a handful of common species (e.g., Carolina wrne and less commons species)
#Lets look at differences among 0.1, 0.2, 0.3
#This gives the species list at different conf levels
unique(bird.dat[which(bird.dat$confidence > 0.3), "common_name"])

#Here subset data based on confidence level
bird.dat_0.3 <- bird.dat[which(bird.dat$confidence > 0.3), ]
bird.dat_0.2 <- bird.dat[which(bird.dat$confidence > 0.2), ]

dim(bird.dat_0.3) #377  17

#Manually verify BirdNET detections----------------------------------------------
?birdnet_verify
# Create a random sample of three detections to verify
set.seed(4)
to.verify <- bird.dat[common_name == "Red-bellied Woodpecker"][sample(.N, 5)]

#Subset to all detections for a given species to verify 
to.verify <- bird.dat[common_name == "Carolina Wren"]


#troubleshoot file error---------------------------------------------------------

to.verify$filepath

new_path <- c(
"/Pilot data/SERC1_deployed_20231004/20231009_130000.WAV",
"/Pilot data/SERC1_deployed_20231004/20231009_170000.WAV",
"/Pilot data/SERC1_deployed_20231004/20231005_174000.WAV",
"/Pilot data/SERC1_deployed_20231004/20231005_131000.WAV",
"/Pilot data/SERC1_deployed_20231004/20231012_121000.WAV"
)

to.verify$filepath <- new_path

this.wav <- list.files(
  path = 'SERC_test_results',
  full.names = TRUE,
  recursive = TRUE
)

finame <- this.wav[grep(pattern = "_formatted_", x = this.wav)]


#--------------------------------------------------------------------------------

# Create a verification library for this species
ver.lib <- c('y', 'n', 'unsure')
#This will just manually distinguish whether the vocalization is actually from the 
#AI-identified species - i.e., does not indicate the type of vocalization

birdnet_verify(data = to.verify,
               verification.library = ver.lib,
               audio.directory = 'SERC_test_audio_files',
               results.directory = 'SERC_test_results',
               overwrite = FALSE, 
               play = TRUE,
               frq.lim = c(0, 12),
               buffer = 1,
               box.col = 'blue',
               spec.col = viridis::viridis(30)) #monitoR::gray.3()

# Check that underlying files have been updated with user verifications
dat <- birdnet_gather(results.directory = 'example-results-directory',
                      formatted = TRUE)
dat[!is.na(verify)] #shows the randomly selected records above, now with entries
#in the verify column


#try hacked function---------------------------------------------------------------


birdnet_verify_h <- function (data, verification.library, audio.directory, results.directory, 
          overwrite = FALSE, play = TRUE, frq.lim = c(0, 12), buffer = 1, 
          box.col = "blue", spec.col = monitoR::gray.3()) 
{
  if (missing(verification.library)) {
    stop("Please input verification.library argument. See ?birdnet_verify.")
  }
  owd <- setwd(getwd())
  on.exit(setwd(owd))
  if (grepl("\\/$", audio.directory) == FALSE) {
    audio.directory <- paste0(audio.directory, "/")
  }
  if (grepl("\\/$", results.directory) == FALSE) {
    results.directory <- paste0(results.directory, "/")
  }
  data <- as.data.table(data)
  setkey(data, recordingID)
  message("Gathering all results in results.directory...")
  ext.type <- unique(file_ext(list.files(results.directory, 
                                         recursive = TRUE)))
  if (length(ext.type) != 1) 
    stop("Multiple file extension types found in folder. Please make sure results are all txt or all csv. Do not mix file types.")
  if (length(unique(data$common_name)) > 1) {
    stop("Please input data for one species at a time. You have input a dataset with ", 
         length(unique(data$common_name)), " species.")
  }
  results <- birdnet_gather(results.directory = results.directory)
  results[, `:=`(composite.key, paste(recordingID, start, end, 
                                      common_name, sep = "-"))]
  data[, `:=`(composite.key, paste(recordingID, start, end, 
                                   common_name, sep = "-"))]
  all.focal <- results[composite.key %in% data$composite.key]
  if (overwrite == FALSE) {
    message("Since overwrite == FALSE, only detections from unverified results will be verified.\n")
    all.focal.verify <- all.focal[is.na(verify)]
    if (nrow(all.focal.verify) == 0) 
      stop("overwrite == FALSE and there are no more unverified detections in this data. Quitting function.")
  }
  else {
    message("Since overwrite == TRUE, all detections will be verified, even if verification data already exists. This will overwrite any existing verifications.\n")
    all.focal.verify <- all.focal
  }
  all.wav <- list.files(audio.directory, full.names = TRUE, 
                        recursive = TRUE)
  rec.ids <- unique(all.focal.verify$recordingID)
  og.rec.ids <- rec.ids
  correct.ids <- grep(pattern = "temp-", x = rec.ids)
  rec.ids[correct.ids] <- gsub(pattern = ".wav|.WAV", replacement = ".mp3", 
                               x = rec.ids[correct.ids])
  rec.ids[correct.ids] <- gsub(pattern = "temp-", replacement = "", 
                               x = rec.ids[correct.ids])
  wav.paths <- unique(grep(paste(rec.ids, collapse = "|"), 
                           all.wav, value = TRUE))
  if (length(wav.paths) == 0) {
    correct.audio.moth <- gsub(x = rec.ids, pattern = ".wav", 
                               replacement = ".WAV", ignore.case = FALSE)
    wav.paths <- unique(grep(paste(correct.audio.moth, collapse = "|"), 
                             all.wav, value = TRUE))
  }
  check.file <- wav.paths[1]
  if (file_ext(check.file) == "mp3") {
    message("It looks like there may be mp3 files in this audio folder, so we're checking on a few parameters. Thank you for your patience. NOTE: R and Windows, together, are not the best at handling mp3 files. If you need to use mp3 files instead of wave, then for a much speedier BirdNET validation workflow, we suggest running BirdNET directly from the command line and then using segments.py for validation as described here: https://github.com/kahst/BirdNET-Analyzer/")
    r <- readMP3(check.file)
    temp.file <- paste0(audio.directory, "temp-", gsub(".mp3", 
                                                       ".wav", basename(check.file), ignore.case = TRUE))
    writeWave(r, temp.file, extensible = FALSE)
    check.file <- temp.file
    message("Done converting temporary wave file.")
  }
  checker <- readWave(filename = check.file, from = 0, to = 3, 
                      units = "seconds")
  check.sp <- monitoR:::spectro(wave = checker)
  which.frq.bins <- which(check.sp$freq >= frq.lim[1] & check.sp$freq <= 
                            frq.lim[2])
  if (exists("temp.file")) 
    unlink(temp.file)
  counter <- 0
  verify.list <- list()
  for (w in 1:length(wav.paths)) {
    this.wav <- list.files(path = results.directory,  full.names = TRUE, recursive = TRUE)
    finame <- this.wav[grep(pattern = "_formatted_", x = this.wav)]
    verify <- all.focal.verify[recordingID == og.rec.ids[w]]
    ask <- FALSE
    oldask <- par(ask = par("ask"))
    on.exit(par(oldask))
    vers <- NULL
    is.mp3 <- file_ext(wav.paths[w]) == "mp3"
    if (is.mp3) {
      message("This is an mp3. Converting to wave...")
      r <- readMP3(wav.paths[w])
      temp.file <- paste0(audio.directory, "temp-", gsub(".mp3", 
                                                         ".wav", basename(wav.paths[w]), ignore.case = TRUE))
      writeWave(r, temp.file, extensible = FALSE)
      wav.paths[w] <- temp.file
      message("Done converting temporary wave file.")
    }
    for (i in 1:verify[, .N]) {
      counter <- counter + 1
      x <- "x"
      t.start <- max(verify[i, start] - buffer, 0)
      t.end <- verify[i, end] + buffer
      wav <- tuneR::readWave(wav.paths[w], from = t.start, 
                             to = t.end, units = "seconds")
      reclen <- length(wav@left)/wav@samp.rate
      fft.data <- monitoR:::spectro(wave = wav)
      trec <- fft.data$time
      frec <- fft.data$freq
      arec <- fft.data$amp
      frec <- frec[which.frq.bins]
      arec <- arec[which.frq.bins, ]
      trec.times <- as.ITime(trec)
      time.lab <- "Time in recording (hh:mm:ss)"
      t.step <- trec[2] - trec[1]
      true.times.in.rec <- seq(from = t.start, to = t.end, 
                               by = t.step)[1 - length(trec)]
      par(mfrow = c(1, 1), mar = c(3, 3, 2, 1), mgp = c(2, 
                                                        1, 0))
      image(x = trec, y = frec, z = t(arec), col = spec.col, 
            xlab = time.lab, ylab = "Frequency (kHz)", xaxt = "n", 
            bty = "n", axes = FALSE, main = paste0(verify[i, 
                                                          composite.key], " [Conf. = ", round(verify$confidence[i], 
                                                                                              2), "]"))
      xleft <- trec[which.min(abs(true.times.in.rec - verify[i, 
                                                             start]))]
      xright <- trec[which.min(abs(true.times.in.rec - 
                                     verify[i, end]))]
      ylwr <- min(frec)
      yupr <- max(frec)
      polygon(x = c(xleft, xleft, xright, xright), y = c(ylwr, 
                                                         yupr, yupr, ylwr), border = box.col)
      axis(2, at = pretty(frec), labels = pretty(frec))
      axis(1, at = pretty(trec), labels = as.ITime(pretty(true.times.in.rec))[1:length(pretty(trec))])
      if (play) {
        fn <- paste0(getwd(), "/", "temp-", verify$recordingID[i], 
                     "-", verify$start[i])
        fn <- paste0(getwd(), "/temp-", gsub(pattern = ".wav", 
                                             replacement = "", x = paste0(verify[i, c("recordingID", 
                                                                                      "start", "common_name")], collapse = "-")), 
                     ".wav")
        tuneR::writeWave(object = wav, filename = fn)
        message("\nWriting temporary file clip to ", 
                fn, " so that you can manually check it. Temporary file will be deleted after you are finished with this verification and have closed the player window.")
      }
      while (length(x) == 0 || !x %in% c(verification.library, 
                                         NA)) {
        cat(paste0("\n This is recording ", rec.ids[w], 
                   ".", " This is verification ", counter, " out of ", 
                   nrow(all.focal.verify), ".\n"))
        cat(paste0("\n", i, ". Showing user input verification library options for ", 
                   verify[i]$common_name, ": ", paste0(verification.library, 
                                                       collapse = ", "), "\n Enter an option in ", 
                   paste0(verification.library, collapse = ", "), 
                   ", s to skip, or q to exit this recording (q will exit and save any verifications you have already completed for this recording). If you have many recordings and wish to escape out of the function completely, press 'Esc': "))
        x <- tolower(readLines(n = 1)[1])
        if (length(x) == 0) {
          cat("\nYou didn't enter a response.\n")
          next
        }
        if (!is.na(x) && x == "na") 
          x <- NA
        if (is.na(x)) {
          cat("NA\n")
          break
        }
        if (x %in% verification.library) {
          vers[i] <- x
        }
        if (x == "s") {
          message("Skipping to next verification.\n")
          break
        }
        if (x == "q") {
          message("Quitting out of this recording and saving what you have already verified for this recording.\n")
          break
        }
        if (!x %in% c(verification.library, "r", "s", 
                      "q")) {
          message("Value not recognized. Enter an option from your verification library, or enter s, r, or q.\n")
          next
        }
      }
      if (!is.na(x) & x == "q") 
        break
      if (is.na(x) || x != "r") 
        vers[i] <- x
      par(ask = ask)
      if (!is.na(x) && x == "r") 
        i <- i - 1
      else i <- i + 1
      if (i < 1) 
        i <- 1
      if (play) {
        file.remove(fn)
      }
    }
    cat("\n")
    update.composite <- verify$composite.key
    if (x == "q") {
      update.composite <- verify$composite.key[seq_along(vers)]
    }
    if (length(update.composite) > 0) {
      verify[composite.key %in% update.composite, `:=`(verify, 
                                                       vers)]
      new.results <- results[recordingID == og.rec.ids[w]]
      new.results[composite.key %in% update.composite, 
                  `:=`(verify, vers)]
      new.results[, `:=`(composite.key, NULL)]
      message("Updating ", basename(finame), " with new verifications.")
      write.csv(x = new.results, file = finame, row.names = FALSE)
    }
    cat("Finished verifying for this recording.\n")
    verify.list[[w]] <- verify
    if (exists("temp.file")) 
      unlink(temp.file)
  }
  verify.bind <- rbindlist(verify.list)
  verify.bind[, `:=`(composite.key, NULL)]
  return(verify.bind)
}


birdnet_verify_h(data = to.verify,
               verification.library = ver.lib,
               audio.directory = 'SERC_test_audio_files',
               results.directory = 'SERC_test_results',
               overwrite = FALSE, 
               play = TRUE,
               frq.lim = c(0, 12),
               buffer = 1,
               box.col = 'blue',
               spec.col = viridis::viridis(30)) #monitoR::gray.3()

# Check that underlying files have been updated with user verifications
dat <- birdnet_gather(results.directory = 'example-results-directory',
                      formatted = TRUE)
dat[!is.na(verify)] #shows the randomly selected records above, now with entries
#in the verify column




## ***************************************************************************************************
## Make some plots
## ***************************************************************************************************


#--------------------------------------------------------------------------------
#plot spectrograms of BirdNET detections-------------------------------------------

# Read in example data.table/data.frame for plotting
data(examplePlotData)

# Check the structure of this example data
str(examplePlotData)
str(formatted.results) #Look the same

library(viridis) 

plot.songs <- examplePlotData[common_name == "Swainson's Thrush" & verify == "call"]
dim(plot.songs)
birdnet_plot(data = plot.songs,
             audio.directory = 'example-audio-directory',
             title = "Swainson's Thrush Songs",
             frq.lim = c(0.5, 12),
             new.window = TRUE,
             spec.col = viridis::viridis(30), #gray.3()
             box = TRUE,
             box.lwd = 1,
             box.col = 'gray')


#Make barplots and heatmaps of detections through time--------------------------
#Documentation from NSNSDAcoustics package"
#"Generally, this data object may be preceded by a call to add_time_cols. 
#Regardless, the data object input to birdnet_barchart should contain BirdNET 
#detection data that comes from a single site, and the object must contain 
#columns named "locationID" (character), "recordingID" (character), and 
#"dateTimeLocal" (POSIXct)."


# Read in exampleBarchartData
data(exampleBarchartData)
str(exampleBarchartData)

#Add location ID (this should happen automatically after file names
#are appended to include SITEID)
bird.dat$locationID <- "SERC1" #fplot function needs locationID


#Drop row with no observations
dim(bird.dat) #3687   18
summary(as.factor(is.na(bird.dat$scientific_name))) #1067 NAs
bird.dat.c <- bird.dat[complete.cases(bird.dat[ , "scientific_name"]),]
dim(bird.dat.c) #2620   18

#Add site to recordingID
bird.dat.c$recordingID <-
  paste(bird.dat.c$locationID, bird.dat.c$recordingID, sep = "_")

# Generally, add_time_cols() may be called as part of preprocessing
# (if not, please ensure data object has columns that include locationID (character),
# recordingID (character), and dateTimeLocal (POSIXct))
bird.dat.dt <- add_time_cols(dt = bird.dat.c,
                     tz.recorder = 'UTC',
                     tz.local = 'US/Eastern')

# dat2 <- add_time_cols(dt = formatted.results,
#                      tz.recorder = 'GMT',
#                      tz.local = 'America/Los_angeles')

range(bird.dat.dt$dateTimeLocal)

# Produce an interactive plotly barchart with interactive = TRUE
birdnet_barchart(
  data = bird.dat.dt,
  julian.breaks = c(276, 288),
  interactive = TRUE
)

# Produce a static ggplot barchat with interactive = FALSE,
# add focal.species with custom colors (any species in the data object
# that are not in focal.species will be plotted in black as "Other".)
birdnet_barchart(
  data = bird.dat.dt,
  julian.breaks = c(276, 288),
  interactive = FALSE,
  focal.species = c(
    "Blue Jay",
    "Carolina Wren",
    "Northern Cardinal",
    "Red-bellied Woodpecker",
    "Tufted Titmouse"
  ),
  focal.colors = c('blue', 'tan', 'darkred', "red", "slategray")
)


# Generate a heatmap with user-input julian.breaks------------------------------
#From package documentation here: https://github.com/nationalparkservice/NSNSDAcoustics/blob/main/README.md
#"the dates.sampled argument requires either a Date vector or character vector
#of dates that were sampled and should be visualized on the heat map. This 
#information is required because your data input may only contain detection 
#data, and not non-detection data (i.e., zeroes). For example, you might have
#recorded audio on 2021-03-14, but have no BirdNET detections in your "data" 
#object. This will result in an inaccurate visualization. Since BirdNET results 
#do not automatically contain non-detection data, it is incumbent on the user 
#to input which dates were sampled."
#See Julian date chart here: https://landweb.modaps.eosdis.nasa.gov/ltdr/browse/calendar.html

birdnet_heatmap(
  data = bird.dat.dt,
  locationID = 'SERC1',
  common.name = 'Carolina Wren',
  conf.threshold = 0.2,
  dates.sampled = seq.Date(from = as.Date('2023-10-03'),
                           to = as.Date('2023-10-15'),
                           by = 1),
  julian.breaks = seq(from = 276, to = 288, by = 0.5))

## ***************************************************************************************************
## Calculate acoustic indices
## ***************************************************************************************************

#Calculate acoustic indices using NSNSDAcoustics package-------------------------------
#NOTE: doesn't seem to work well with example data

#From package documentation here: https://github.com/nationalparkservice/NSNSDAcoustics/blob/main/README.md
#"wave_to_nvspl() uses PAMGuide code to convert wave files into an NVSPL 
#formatted table. NVSPL stands for NPS-Volpe Sound Pressure Level, and is the
#standard format used in NSNSD analyses. These are hourly files comprised of 
#1/3 octave data in 1-sec LEQ increments. PAMGuide was developed by 
#Nathan D. Merchant et al. 2015."

#"The suggested workflow for this function is to first set test.file = TRUE to 
#verify that your workflow has been accurately parameterized. When 
#test.file = TRUE, wave_to_nvspl() will assess one file and encourage the user
#to check all outputs. Users should ensure there isn't an NA in the "Time stamp 
#start time" output (if so, something is wrong). Lastly, the test.file = TRUE 
#argument will create a plot allowing the user to verify that time is continuous. 
#If there are breaks in the plotted line, there is an issue with your 
#parameterization."

# Create an input directory for this example
dir.create('example-input-directory')

# Read in example wave files
data(exampleAudio1)
data(exampleAudio2)

# Write example waves to example input directory
tuneR::writeWave(object = exampleAudio1,
                 filename = 'example-input-directory/Rivendell_20210623_113602.wav')
tuneR::writeWave(object = exampleAudio2,
                 filename = 'example-input-directory/Rivendell_20210623_114602.wav')

# Perform wave_to_nvspl in test mode (test.file = TRUE)
wave_to_nvspl(
  input.directory = 'example-input-directory',
  data.directory = FALSE,
  test.file = TRUE,
  project = 'testproject',
  timezone = 'GMT')

#After checking parameters and if all looks good, run in batch mode 
#i.e., set test.file = FALSE
# Perform wave_to_nvspl in batch mode (test.file = FALSE)
wave_to_nvspl(
  input.directory = 'example-input-directory',
  data.directory = FALSE,
  test.file = FALSE,
  project = 'testproject',
  timezone = 'GMT')
#This doesn't work :(

# # Verify that NVSPL outputs have been created
# nvspls <- list.files('example-input-directory/NVSPL', full.names = TRUE)
# 
# # View one of the NVSPL outputs
# one.nvspl <- read.delim(file = nvspls[1], sep = ',')

# Create an input and output directory for this example
dir.create('example-input-directory')
dir.create('example-output-directory')

# Read in example NVSPL data
data(exampleNVSPL)

# Write example NVSPL data to example input directory
for (i in 1:length(exampleNVSPL)) {
  write.table(x = exampleNVSPL[[i]],
              file = paste0('example-input-directory/', names(exampleNVSPL)[i]),
              sep = ',',
              quote = FALSE)
}

# Run nvspl_to_ai to generate acoustic indices csv for example NVSPL files,
nvspl_to_ai(input.directory = 'example-input-directory',
            output.directory = 'example-output-directory',
            project = 'example-project')

# View Results
(ai.results <- read.csv(list.files(path = 'example-output-directory',
                                   pattern = '.csv', full.names = TRUE)))

#--------------------------------------------------------------------------------
#Calculate acoustic indices seewave package--------------------------------------

#install.packages("seewave", dependencies = TRUE)
library(seewave)

#test functions using seewave examples
data(tico)
tico
ACI(tico, nbwindows = 4)
ACI(tico)

#Load example wave file/s
riv1 <- readWave("example-audio-directory/Rivendell_20210623_113602.wav")
riv2 <- readWave("example-audio-directory/Rivendell_20210623_114602.wav")

#Calculate ACI and other indices using example data from NSNSDAcoustics
#package for comparisons
ACI(riv1) #178.3143
ACI(riv2) #178.2607

duration <- length(riv1@left)/riv1@samp.rate # 600 seconds

ACI(riv1, nbwindows = (duration/5)) #21336.29
ACI(riv2, nbwindows = (duration/5)) #21349.78

#Process many wave files-------------------------------------------------------
#Load one example file
exserc <- readWave("Pilot data/SERC1_deployed_20231004/20231004_165000.WAV")
length(exserc@left)/exserc@samp.rate # 60 seconds

#Load wave files as list
files_serc <-
  list.files("Pilot data/SERC1_deployed_20231004", pattern =
               "*.WAV$")
length(unique(files_serc)) #1543 unique wav files

#Load all wav files from folder
wav_serc <- lapply(paste0("Pilot data/SERC1_deployed_20231004/",
                         files_serc), function(x) {
                            readWave(x)
                          }) #big list of data files
#names(wav_serc) <- sens_num #Assign sensor names to each df in list
length(wav_serc) #1543

#calculate ACI for each wav file
serc1_aci <- lapply(wav_raw, function(x) ACI(x))

#--------------------------------------------------------------------------------
#Calculate acoustic indices using soundecology package--------------------------------------

#install.packages("soundecology")
library(soundecology)
data(tropicalsound)
acoustic_complexity(riv1) #Right channel: 21359.47
acoustic_complexity(riv2) #Right channel: 21367.77



