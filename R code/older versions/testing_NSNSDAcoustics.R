###########################################################################################################################################################
###Setting up and testing BridNET##########################################################################################
########################################################################################################################################################
###updated 12-4-23


#Following steps here: https://github.com/nationalparkservice/NSNSDAcoustics/blob/main/README.md#running-birdnet-from-rstudio-with-birdnet_analyzer

# install.packages('devtools')
# library(devtools)
# devtools::install_github('nationalparkservice/NSNSDAcoustics')

rm(list=ls())
library(NSNSDAcoustics)

#Installed BirdNET analyzer from here: https://github.com/kahst/BirdNET-Analyzer#setup-windows
#environment location: C:\Users\Nowak\anaconda3\envs\pybirdanalyze

?birdnet_analyzer()

# Must set environment BEFORE calling in the reticulate package
Sys.setenv(RETICULATE_PYTHON = "C:/Users/Nowak/anaconda3/envs/pybirdanalyze/python.exe")
library(reticulate)

# Set your conda environment
use_condaenv(condaenv = "pybirdanalyze", required = TRUE)
#Sometimes this I receive this message: "Error: Unable to find conda binary. Is Anaconda installed?"
#Remiving and reinstalling "reticulate" package has resolved this (for some reason)

#Example data--------------------------------------------------------------------
# Create an audio directory for this example
dir.create('example-audio-directory')

# Create a results directory for this example
dir.create('example-results-directory')

# Read in example wave files
data(exampleAudio1)
data(exampleAudio2)

# Write example waves to example audio directory
tuneR::writeWave(object = exampleAudio1,
                 filename = 'example-audio-directory/Rivendell_20210623_113602.wav')
tuneR::writeWave(object = exampleAudio2,
                 filename = 'example-audio-directory/Rivendell_20210623_114602.wav')


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
data(exampleBirdNET1)
write.table(x = exampleBirdNET1,
            file = 'example-results-directory/Rivendell_20210623_113602.BirdNET.results.csv',
            row.names = FALSE, quote = FALSE, sep = ',')
data(exampleBirdNET2)
write.table(x = exampleBirdNET2,
            file = 'example-results-directory/Rivendell_20210623_114602.BirdNET.results.csv',
            row.names = FALSE, quote = FALSE, sep = ',')


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
birdnet_format(results.directory = 'example-results-directory',
               timezone = 'GMT')

#bnet_frmt <- read.csv("example-results-directory/Rivendell_20210623_113602.BirdNET_formatted_results.csv")

# Gather formatted BirdNET results
formatted.results <- birdnet_gather(
  results.directory = 'example-results-directory',
  formatted = TRUE
)

dim(formatted.results) #284  17

#Manually verify BirdNET detections----------------------------------------------
?birdnet_verify
# Create a random sample of three detections to verify
set.seed(4)
to.verify <- formatted.results[common_name == "Swainson's Thrush"][sample(.N, 5)]

# Create a verification library for this species
ver.lib <- c('y', 'n', 'unsure')
#This will just manually distinguish whether the vocalization is actually from the 
#AI-identified species - i.e., does not indicate the type of vocalization

birdnet_verify(data = to.verify,
               verification.library = ver.lib,
               audio.directory = 'example-audio-directory',
               results.directory = 'example-results-directory',
               overwrite = FALSE, 
               play = TRUE,
               frq.lim = c(0, 12),
               buffer = 1,
               box.col = 'blue',
               spec.col = monitoR::gray.3())

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

# Generally, add_time_cols() may be called as part of preprocessing
# (if not, please ensure data object has columns that include locationID (character),
# recordingID (character), and dateTimeLocal (POSIXct))
dat <- add_time_cols(dt = exampleBarchartData,
                     tz.recorder = 'America/Los_angeles',
                     tz.local = 'America/Los_angeles')

# dat2 <- add_time_cols(dt = formatted.results,
#                      tz.recorder = 'GMT',
#                      tz.local = 'America/Los_angeles')

# Produce an interactive plotly barchart with interactive = TRUE
birdnet_barchart(data = dat, interactive = TRUE)

# Produce a static ggplot barchat with interactive = FALSE,
# add focal.species with custom colors (any species in the data object
# that are not in focal.species will be plotted in black as "Other".)
birdnet_barchart(data = dat,
                 interactive = FALSE,
                 focal.species = c("Pacific Wren", "Swainson's Thrush", "Varied Thrush"),
                 focal.colors = c('#00BE67', '#C77CFF', '#c51b8a'))


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
  data = dat,
  locationID = 'Rivendell',
  common.name = 'Pacific Wren',
  conf.threshold = 0.2,
  dates.sampled = seq.Date(from = as.Date('2021-03-14'),
                           to = as.Date('2021-08-15'),
                           by = 1),
  julian.breaks = seq(from = 70, to = 250, by = 30))

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

#Load wave file/s
riv1 <- readWave("example-audio-directory/Rivendell_20210623_113602.wav")
riv2 <- readWave("example-audio-directory/Rivendell_20210623_114602.wav")

#Calculate ACI and other indices using example data from NSNSDAcoustics
#package for comparisons
ACI(riv1) #178.3143
ACI(riv2) #178.2607

duration <- length(riv1@left)/riv1@samp.rate # 600 seconds

ACI(riv1, nbwindows = (duration/5)) #21336.29
ACI(riv2, nbwindows = (duration/5)) #21349.78

#--------------------------------------------------------------------------------
#Calculate acoustic indices using soundecology package--------------------------------------

#install.packages("soundecology")
library(soundecology)
acoustic_complexity(riv1) #Right channel: 21359.47
acoustic_complexity(riv2) #Right channel: 21367.77



