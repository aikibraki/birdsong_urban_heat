# Packages ----------------------------------------------------------------

library(bslib)
library(usethis)
library(devtools)
library(ggExtra)
library(ggplot2)
library(tidyverse)
library(shiny)
library(NSNSDAcoustics)

# use this to install NSNSDAcoustics package
# devtools::install_github('nationalparkservice/NSNSDAcoustics')

# usethis::create_github_token()
# my temporary github token: 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

# Formatting the .wav file titles ------------------------------------------
# wav files follow a SITEID_YYYYMMDD_HHMMSS naming convention

# only need to do this once per set of files, doing it more than once will
# add "SERC_" one too many times to the start of the file name

setwd("~/R/Sci_Faith/NSNSDACoustics/pilot_data")

file.rename(list.files(pattern = '2023'),
            str_replace(list.files(pattern = '2023'), pattern = '2023', 'SERC_2023'))

# BirdNET formatting and verifying ---------------------------------------
## Formatting and Gathering the formatted results --------------------------

# use the path for the results directory
birdnet_format('~/R/Sci_Faith/NSNSDACoustics/pilot_data/pilot_results_testing', 
               timezone = 'EST')

# makes an object containing all the detections
formatted.results <- birdnet_gather(
  results.directory = '~/R/Sci_Faith/NSNSDACoustics/pilot_data/pilot_results_testing',
  formatted = TRUE
)

# remove unformatted files from results directory
setwd("~/R/Sci_Faith/NSNSDACoustics/pilot_data/pilot_results_testing")
#to_be_deleted <- list.files("~/R/Sci_Faith/NSNSDACoustics/pilot_data/pilot_results_testing", pattern = "NET.results")
#file.remove(to_be_deleted)

## Species counts -----------------------------------------------------------

# filtering by confidence >= 0.1, 0.2, and 0.3
filt.1 <- filter(formatted.results, formatted.results$confidence >= 0.1)
filt.2 <- filter(formatted.results, formatted.results$confidence >= 0.2)
filt.3 <- filter(formatted.results, formatted.results$confidence >= 0.3)

# count of # observation of each species at confidence >= 0.1, 0.2, and 0.3
species.1 <- count(filt.1, filt.1$common_name, sort = TRUE)
species.2 <- count(filt.2, filt.2$common_name, sort = TRUE)
species.3 <- count(filt.3, filt.3$common_name, sort = TRUE)

## Verifying the results ---------------------------------------------------

# Create a random sample of N detections to verify
set.seed(4)
to.verify <- filt.1[common_name == "Blue Jay"][sample(.N, 35)]

# Create a verification library for this species
ver.lib <- c('yes', 'no', 'unsure')

# Verify detections
birdnet_verify(data = to.verify,
               verification.library = ver.lib,
               audio.directory = '~/R/Sci_Faith/NSNSDACoustics/pilot_data',
               results.directory = '~/R/Sci_Faith/NSNSDACoustics/pilot_data/pilot_results_testing',
               overwrite = FALSE,
               play = TRUE,
               frq.lim = c(0, 12),
               buffer = 1,
               box.col = 'blue',
               spec.col = monitoR::gray.3())


# Check that underlying files have been updated with user verifications
formatted.results <- birdnet_gather(results.directory = '~/R/Sci_Faith/NSNSDACoustics/pilot_data/pilot_results_testing',
                      formatted = TRUE)

# Make tibbles of the verifications for >= 0.1, 0.2, and 0.3 confidence
verified_0.1 <- as_tibble(formatted.results[!is.na(verify)]) %>% 
  select(scientific_name, common_name, confidence, verify)
verified_0.2 <- filter(verified_0.1, confidence >= 0.2)
verified_0.3 <- filter(verified_0.1, confidence >= 0.3)

# Make tibbles of the number of yes/no/unsure for each species
verify_subset_0.1 <- verified_0.1 %>% count(common_name, verify)
verify_subset_0.2 <- verified_0.2 %>% count(common_name, verify)
verify_subset_0.3 <- verified_0.3 %>% count(common_name, verify)

## Plot the verifications --------------------------------------------------

# Plot of yes/no/unsure by species and confidence at >=0.1 confidence
ggplot(verified_0.1, aes(fill=verify, y=confidence, x=common_name)) + 
  geom_bar(position="dodge", stat="identity") + 
  scale_fill_manual(values=c("firebrick", "steelblue", "chartreuse4")) + 
  xlab("Common Name") + ylab("Confidence")


# >= 0.1 confidence
ggplot(verify_subset_0.1, aes(fill=verify, y=n, x=common_name)) + 
  geom_bar(position='stack', stat='identity') +
  xlab("Common Name") + ylab("# Observations") +
  scale_fill_manual(values=c("firebrick", "steelblue", "chartreuse4")) +
  scale_x_discrete(labels=c("AMCR", "AMRO", "BLJA", "CACH", "CAWR", "MALL",
                            "MODO", "NOCA", "RSHA", "RTHA", "TUTI", "RBWO")) + 
  ggtitle(">=0.1 confidence") +
  theme_light()

# >= 0.2 confidence
ggplot(verify_subset_0.2, aes(fill=verify, y=n, x=common_name)) + 
  geom_bar(position='stack', stat='identity') +
  xlab("Common Name") + ylab("# Observations") +
  scale_fill_manual(values=c("firebrick", "steelblue", "chartreuse4")) +
  scale_x_discrete(labels=c("AMCR", "BLJA", "CACH", "CAWR", "MALL", "MODO",
                            "NOCA", "RBWO", "RSHA", "RTHA", "TUTI")) +
  ggtitle(">=0.2 confidence") +
  theme_light()

# >= 0.3 confidence
ggplot(verify_subset_0.3, aes(fill=verify, y=n, x=common_name)) + 
  geom_bar(position='stack', stat='identity') +
  xlab("Common Name") + ylab("# Observations") +
  scale_fill_manual(values=c("firebrick", "steelblue", "chartreuse4")) +
  scale_x_discrete(labels=c("AMCR", "BLJA", "CACH", "CAWR", "MALL", "MODO",
                            "NOCA", "RBWO", "RSHA", "RTHA", "TUTI")) +
  ggtitle(">=0.3 confidence") +
  theme_light()

# BirdNET Example code ----------------------------------------------------
## birdnet_format example code ---------------------------------------------

# Create a BirdNET results directory for this example
dir.create('example-results-directory')

# Write examples of raw BirdNET outputs to example results directory
data(exampleBirdNET1)
write.table(x = exampleBirdNET1,
            file = 'example-results-directory/Rivendell_20210623_113602.BirdNET.results.csv',
            row.names = FALSE, quote = FALSE, sep = ',')
data(exampleBirdNET2)
write.table(x = exampleBirdNET2,
            file = 'example-results-directory/Rivendell_20210623_114602.BirdNET.results.csv',
            row.names = FALSE, quote = FALSE, sep = ',')

# Run birdnet_format:
birdnet_format(results.directory = 'example-results-directory',
               timezone = 'GMT')
# Delete all temporary example files when finished
unlink(x = 'example-results-directory', recursive = TRUE)

## birdnet_gather example code ---------------------------------------------

# Create a BirdNET results directory for this example
dir.create('example-results-directory')

# Write examples of formatted BirdNET outputs to example results directory
data(exampleFormatted1)
write.table(x = exampleFormatted1,
            file = 'example-results-directory/Rivendell_20210623_113602.BirdNET_formatted_results.csv',
            row.names = FALSE, quote = FALSE, sep = ',')
data(exampleFormatted2)
write.table(x = exampleFormatted2,
            file = 'example-results-directory/Rivendell_20210623_114602.BirdNET_formatted_results.csv',
            row.names = FALSE, quote = FALSE, sep = ',')

# Write examples of raw BirdNET outputs to example results directory
data(exampleBirdNET1)
write.table(x = exampleBirdNET1,
            file = 'example-results-directory/Rivendell_20210623_113602.BirdNET.results.csv',
            row.names = FALSE, quote = FALSE, sep = ',')
data(exampleBirdNET2)
write.table(x = exampleBirdNET2,
            file = 'example-results-directory/Rivendell_20210623_114602.BirdNET.results.csv',
            row.names = FALSE, quote = FALSE, sep = ',')

# Gather formatted BirdNET results
formatted.results <- birdnet_gather(
  results.directory = 'example-results-directory',
  formatted = TRUE
)

raw.results <- birdnet_gather(
  results.directory = 'example-results-directory',
  formatted = FALSE
)

# Delete all temporary example files when finished
unlink(x = 'example-results-directory', recursive = TRUE)

## birdnet_verify example code ---------------------------------------------

# Create an audio directory for this example
dir.create('example-audio-directory')

# Read in example wave files
data(exampleAudio1)
data(exampleAudio2)

# Write example waves to example audio directory
tuneR::writeWave(object = exampleAudio1,
                 filename = 'example-audio-directory/Rivendell_20210623_113602.wav')
tuneR::writeWave(object = exampleAudio2,
                 filename = 'example-audio-directory/Rivendell_20210623_114602.wav')

# Create a BirdNET results directory for this example
dir.create('example-results-directory')

# Write examples of formatted BirdNET outputs to example results directory
data(exampleFormatted1)
write.table(x = exampleFormatted1,
            file = 'example-results-directory/Rivendell_20210623_113602.BirdNET_formatted_results.csv',
            row.names = FALSE, quote = FALSE, sep = ',')
data(exampleFormatted2)
write.table(x = exampleFormatted2,
            file = 'example-results-directory/Rivendell_20210623_114602.BirdNET_formatted_results.csv',
            row.names = FALSE, quote = FALSE, sep = ',')

# Gather formatted BirdNET results
dat <- birdnet_gather(results.directory = 'example-results-directory',
                      formatted = TRUE)

# Create a random sample of three detections to verify
set.seed(4)
to.verify <- dat[common_name == "Swainson's Thrush"][sample(.N, 3)]

# Create a verification library for this species
ver.lib <- c('y', 'n', 'unsure')

# Verify detections
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
dat[!is.na(verify)]

# Delete all temporary example files when finished
unlink(x = 'example-audio-directory', recursive = TRUE)
unlink(x = 'example-results-directory', recursive = TRUE)