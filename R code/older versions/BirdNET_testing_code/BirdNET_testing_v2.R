# Packages -----------------------------------------------------------------

library(tidyverse)
library(bslib)
library(usethis)
library(devtools)
library(ggExtra)
library(ggplot2)
library(shiny)

# usethis::create_github_token()
# my temporary github token: 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
# gitcreds::gitcreds_set()
# Sys.setenv(GITHUB_PAT = "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")
# https://sites.northwestern.edu/researchcomputing/resources/using-git-and-github-with-r-rstudio/
# https://stackoverflow.com/questions/70908295/failed-to-install-unknown-package-from-github

# use this to install NSNSDAcoustics package
devtools::install_github('nationalparkservice/NSNSDAcoustics')
library(NSNSDAcoustics)

# Formatting the .wav file titles ------------------------------------------
# wav files follow a SITEID_YYYYMMDD_HHMMSS naming convention

# change the pattern for each site (e.g. SERC, SM, SL, SH, LG)
# only need to do this once per set of files, doing it more than once will
# add "SERC_"/"SL_"/etc. one too many times to the start of the file name.
# (this is done before running BirdNET-Analyzer)

setwd("D:/Science and Faith Audio Files/SERC/NEON019_N19/SD_B/20240605")

file.rename(list.files(pattern = '2024'),
            str_replace(list.files(pattern = '2024'), 
                        pattern = '2024', 'SERC_2024')
            )

# BirdNET formatting and verifying -----------------------------------------
## Formatting and Gathering the formatted results --------------------------

setwd("D:/Science and Faith Audio Files")

# set the file paths for the audio and results folders
audio.path <- "D:/Science and Faith Audio Files/SERC/MuddyCreek_MC1/SD_A/20240508"
results.path <- "D:/Science and Faith Audio Files/SERC/MuddyCreek_MC1/SD_A/20240508/results"

# use the path for the results directory
birdnet_format(results.path, 
               timezone = 'EST')

# makes an object containing all the detections
formatted.results <- birdnet_gather(
  results.directory = results.path,
  formatted = TRUE
)

# move unformatted files to their own folder
setwd(audio.path)
dir.create(file.path(getwd(), 'unformatted'))
to_be_moved <- list.files(results.path, pattern = "NET.results", full.names = TRUE)
file.rename(from = to_be_moved,
            to = file.path(getwd(), 'unformatted', basename(to_be_moved)))
rm(to_be_moved)

# # remove unformatted files from results directory
# setwd('D:/Science and Faith Audio Files/Stillmeadow/Classroom_SMC7/SD_A/20240507')
# to_be_deleted <- list.files("D:/Science and Faith Audio Files/Stillmeadow/Classroom_SMC7/SD_A/20240507", pattern = "NET.results")
# file.remove(to_be_deleted)
# rm(to_be_deleted)

## Species counts ----------------------------------------------------------

# filtering by confidence >= 0.1, 0.2, and 0.3
filt.1 <- filter(formatted.results, formatted.results$confidence >= 0.1)
filt.2 <- filter(formatted.results, formatted.results$confidence >= 0.2)
filt.3 <- filter(formatted.results, formatted.results$confidence >= 0.3)

# count of # observation of each species at confidence >= 0.1, 0.2, and 0.3
species.1 <- count(filt.1, filt.1$common_name, sort = TRUE)
species.2 <- count(filt.2, filt.2$common_name, sort = TRUE)
species.3 <- count(filt.3, filt.3$common_name, sort = TRUE)

## Human vocal info --------------------------------------------------------

# df with only the Human vocal detections below 0.2 confidence
fileslost.1 <- formatted.results %>% # 2317 detections
  select(c(1,5,7)) %>%
  filter(confidence < 0.2) %>%
  filter(scientific_name == 'Human vocal')
# 2/3 of these are below 0.15

# df with only the Human vocal detections below 0.3 confidence
fileslost.2 <- formatted.results %>% # 3013 detections
  select(c(1,5,7)) %>%
  filter(confidence < 0.3) %>%
  filter(scientific_name == 'Human vocal')
# 1/2 of these are below 0.15

# Histogram of Human vocal detections below 0.3 conf, binned by confidence
hist(fileslost.2$confidence, 
     breaks = 20, 
     main = "'Human Vocal' detections by Confidence", 
     xlab = "Confidence")

# Number of unique files containing human vocal
length(unique(fileslost.2$recordingID))
# at ci <0.2: 1199/2897
# at ci <0.3: 1314/2897

######### Northern Cardinal
# df with only the NOCA detections below 0.3 conf
noca.2 <- formatted.results %>%
  select(c(1,6,7)) %>%
  filter(confidence < 0.3) %>%
  filter(common_name == 'Northern Cardinal')
# Histogram of NOCA detections below 0.3 conf, binned by confidence
hist(noca.2$confidence, 
     breaks = 20, 
     main = "NOCA detections by Confidence", 
     xlab = "Confidence")
######### Northern Cardinal


# Human vocal scrubbing ---------------------------------------------------

# Create a folder in the audio folder to move human *detection* files to
setwd('D:/Science and Faith Audio Files/StLukes/Forest2_SLF2/SD_A/20240502')
dir.create(file.path(getwd(), 'humanvoice'))

# df of files containing 'Human vocal'
humans <- formatted.results %>%
  select(c(1,2,5,7)) %>%
  filter(scientific_name == 'Human vocal')

# Check that # file paths = # of recording IDs
if(length(unique(humans$filepath)) == length(unique(humans$recordingID))) {
  print(length(unique(humans$filepath)))
}else{
  print('number of unique file paths does not match number of unique recording IDs')
}

# df of unique recording IDs of files containing 'Human vocal'
unique.humans <- humans %>%
  distinct(humans$recordingID, .keep_all = TRUE) %>%
  select('filepath') %>%
  mutate(resultspath = str_replace(filepath, # add column for results paths
                                   pattern = '.WAV', 
                                   replacement = '.BirdNET_formatted_results.csv'))

# Replace backslashes with forward slashes so file.rename sees the strings as files
unique.humans$filepath <- gsub("\\\\", "/", unique.humans$filepath)
unique.humans$resultspath <- gsub("\\\\", "/", unique.humans$resultspath)

# Add columns with the new path for the human voice files
unique.humans$filepath2 <- str_replace(unique.humans$filepath, 
                                       pattern = "(\\d{8})/", # selecting the 8-digit YYYYMMDD date as a pattern
                                       replace = "\\1/humanvoice/") # adding /humanvoice/ after the pattern
unique.humans$resultspath2 <- str_replace(unique.humans$resultspath, 
                                          pattern = "(\\d{8})/", 
                                          replace = "\\1/humanvoice/")
# Move the human voice files
file.rename(from = unique.humans$filepath,
            to = unique.humans$filepath2)
file.rename(from = unique.humans$resultspath,
            to = unique.humans$resultspath2)

# currently this is removing ALL entries with human voice detections at ANY confidence level
# can re-work this for other thresholds as we see fit

## Verifying the results ---------------------------------------------------

# Create a random sample of N detections to verify
set.seed(4)
to.verify <- filt.1[common_name == "Northern Cardinal"][sample(.N, 50)]

# Create a verification library for this species
ver.lib <- c('y', 'n', 'unsure')

# Verify detections
birdnet_verify(data = to.verify,
               verification.library = ver.lib,
               audio.directory = 'D:/Science and Faith Audio Files/StLukes/Forest2_SLF2/SD_A/20240502',
               results.directory = 'D:/Science and Faith Audio Files/StLukes/Forest2_SLF2/20240502/results',
               overwrite = FALSE,
               play = TRUE,
               frq.lim = c(0, 12),
               buffer = 1,
               box.col = 'blue',
               spec.col = monitoR::gray.3()
               )


# Check that underlying files have been updated with user verifications
formatted.results <- birdnet_gather(results.directory = 'D:/Science and Faith Audio Files/StLukes/Forest2_SLF2/20240502/results',
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