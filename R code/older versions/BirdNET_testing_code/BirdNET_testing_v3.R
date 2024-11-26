# Packages -----------------------------------------------------------------

library(tidyverse)
library(bslib)
library(usethis)
library(devtools)
library(ggExtra)
library(ggplot2)
library(shiny)

# usethis::create_github_token()
# Token Permissions:
#   packages, gist, repo, user, workflow, write:packages, delete:packages
# my temporary github token: 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

# gitcreds::gitcreds_set()
# Sys.setenv(GITHUB_PAT = "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")

# Info on token troubleshooting:
#   https://sites.northwestern.edu/researchcomputing/resources/using-git-and-github-with-r-rstudio/
#   https://stackoverflow.com/questions/70908295/failed-to-install-unknown-package-from-github

# Install NSNSDAcoustics package
devtools::install_github('nationalparkservice/NSNSDAcoustics')
library(NSNSDAcoustics)

# Format the .wav file titles ------------------------------------------
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
## Format and Gather the formatted results ---------------------------------

setwd("E:/Science and Faith Audio Files")

# set the file paths for the audio and results folders
audio.path <- "E:/Science and Faith Audio Files/LibertyGrace/Back_LGB1/SD_A/20240718"
results.path <- paste0(audio.path, "/results_pass2")

# use the path for the results directory
birdnet_format(results.path, 
               timezone = 'EST')

# move formatted files to their own folder
setwd(audio.path)
dir.create(file.path(getwd(), 
                     paste0('results_pass2_', 'formatted')))
to_be_moved <- list.files(results.path, 
                          pattern = "NET_formatted", 
                          full.names = TRUE)
file.rename(from = to_be_moved,
            to = file.path(getwd(), 
                           paste0('results_pass2_', 'formatted'), 
                           basename(to_be_moved)))
rm(to_be_moved)

# set path for formatted results
results.path.fm <- paste0(results.path, "_formatted")

# makes an object containing all the detections
formatted.results <- birdnet_gather(results.directory = results.path.fm,
                                    formatted = TRUE)


## Species counts ----------------------------------------------------------

# filter by confidence >= 0.1, 0.2, and 0.3
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


# Scrub human voices -------------------------------------------------------
setwd(audio.path)

# make a vector filt_g.1 with all the filepaths w/ a human vocal detection
filt_g.1 <- filt.1 %>%
  filter(scientific_name == 'Human vocal') %>%
  distinct(filepath) %>%
  pull(filepath)
# filter filt.1 to keep only the filepaths without human vocal detection
filt_h.1 <- filt.1 %>%
  filter(!filepath %in% filt_g.1)

# Note:
# currently this is removing ALL entries with human voice detections at ANY confidence level

## Verify the results ------------------------------------------------------

# Create a random sample of N detections to verify
set.seed(4)
to.verify <- filt.1[common_name == "Northern Cardinal"][sample(.N, 50)]

# Create a verification library for this species
ver.lib <- c('y', 'n', 'unsure')

# left off on first Lib Grace set

# Verify detections
birdnet_verify(data = to.verify,
               verification.library = ver.lib,
               audio.directory = audio.path,
               results.directory = results.path.fm,
               overwrite = FALSE,
               play = TRUE,
               frq.lim = c(0, 12),
               buffer = 1,
               box.col = 'blue',
               spec.col = monitoR::gray.3()
               )


# Check that underlying files have been updated with user verifications
formatted.results <- birdnet_gather(results.directory = results.path.fm,
                                    formatted = TRUE)

# Make tibbles of the verifications for >= 0.1, 0.2, and 0.3 confidence
verified_0.1 <- as_tibble(formatted.results[!is.na(verify)]) %>% 
  select(scientific_name, common_name, confidence, verify)
verified_0.2 <- filter(verified_0.1, confidence >= 0.2)
verified_0.3 <- filter(verified_0.1, confidence >= 0.3)

# Get info about true positives by confidence level
results_test <- verified_0.1 %>%
  mutate(confidence_bin = cut(confidence, breaks = seq(0, 1, by = 0.01), 
                              labels = paste0(seq(0, 0.99, by = 0.01), "-", seq(0.01, 1, by = 0.01)),
                              include.lowest = TRUE)) %>%
  group_by(common_name, confidence_bin) %>%
  summarize(total_detections = n(),
            positive_ids = sum(verify == "y"),
            positive_rate = positive_ids / total_detections,
            .groups = "keep") %>%
  arrange(common_name, confidence_bin) %>%
  group_by(common_name) %>%
  mutate(cumulative_detections = cumsum(total_detections),
         cumulative_positives = cumsum(positive_ids),
         cumulative_rate = cumulative_positives / cumulative_detections)%>%
  ungroup()

# threshold_results <- results_test %>%
#   group_by(common_name) %>%
#   summarize(threshold_bin = confidence_bin[which(cumulative_rate < 0.95)[1]],
#             threshold_rate = cumulative_rate[which(cumulative_rate < 0.95)[1]],
#             .groups = "drop")

# Get the thresholds at which "true pos/total" = 95%
threshold_results <- results_test %>%
  group_by(common_name) %>%
  summarize(threshold_bin = case_when(
      all(positive_rate >= 0.95) ~ "All bins",
      all(positive_rate < 0.95) ~ "No bins",
      TRUE ~ as.character(confidence_bin[which(positive_rate >= 0.95)[1]])),
    total_detections = sum(total_detections),
    .groups = "drop")

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