# Packages -----------------------------------------------------------------

# Code to install NSNSDAcoustics for the first time
# usethis::create_github_token()
# Token Permissions:
#   packages, gist, repo, user, workflow, write:packages, delete:packages
# my temporary github token: my token goes here

# gitcreds::gitcreds_set()
# Sys.setenv(GITHUB_PAT = "my token here")

# Info on token troubleshooting:
#   https://sites.northwestern.edu/researchcomputing/resources/using-git-and-github-with-r-rstudio/
#   https://stackoverflow.com/questions/70908295/failed-to-install-unknown-package-from-github

# Install NSNSDAcoustics package
#devtools::install_github('nationalparkservice/NSNSDAcoustics')
library(NSNSDAcoustics)
library(tidyverse)
# library(bslib)
# library(usethis)
# library(devtools)
# library(ggExtra)
# library(ggplot2)


# Format the .wav file titles ------------------------------------------
# Do this before running through BirdNET Analyzer
# wav files follow a SITEID_YYYYMMDD_HHMMSS naming convention

# change the pattern for each site (e.g. SERC, SM, SL, SH, LG)
# only need to do this once per set of files, doing it more than once will
# add "SERC_"/"SL_"/etc. one too many times to the start of the file name.

setwd("F:/Science and Faith Audio Files/SERC/NEON019_N19/SD_B/20240605")

file.rename(list.files(pattern = '2024'),
            str_replace(list.files(pattern = '2024'), 
                        pattern = '2024', 'SERC_2024')
            )

# Format and Gather the formatted results ---------------------------------

setwd("F:/Science and Faith Audio Files")

# set the file paths for the audio and results folders
audio_path <- "F:/Science and Faith Audio Files/StLukes/Forest2_SLF2/SD_B/20240923"
results_path <- paste0(audio_path, "/results")
#results_path <- paste0(audio_path, "/results_pass2")

# format the results
birdnet_format(results_path, 
               timezone = 'EST')
rm(to_be_moved)

# move formatted files to their own folder
setwd(audio_path)
dir.create(file.path(getwd(), 
                     #paste0('results_pass2_', 'formatted')))
                     paste0('results_', 'formatted')))
to_be_moved <- list.files(results_path, 
                          pattern = "NET_formatted", 
                          full.names = TRUE)
file.rename(from = to_be_moved,
            to = file.path(getwd(), 
                           #paste0('results_pass2_', 'formatted'), 
                           paste0('results_', 'formatted'),
                           basename(to_be_moved)))
# TODO is there a dir.copy() that would let me automatically back up the formatted folder to the other drive?
rm(to_be_moved)


# set path for formatted results
results_path_fm <- paste0(results_path, "_formatted")

# makes an object containing all the detections
formatted.results <- birdnet_gather(results.directory = results_path_fm,
                                    formatted = TRUE)


# VALIDATION PIPELINE MK.2 -------------------------------------------------

# Function to get deployment folders
get_deployment_folders <- function(base_path) {
  deployments <- list.dirs(base_path, recursive = FALSE)
  deployment_dates <- list()
  
  for (deployment in deployments) {
    dates <- list.dirs(deployment, recursive = FALSE)
    deployment_dates <- c(deployment_dates, dates)
  }
  
  return(deployment_dates)
}

# Update this for each site
base_path <- "D:/Science and Faith Audio Files/StLukes/Forest3_SLR3"

# Get deployment folders
all_deployments <- get_deployment_folders(base_path)


## First pass results ------------------------------------------------------
# first pass is for scrubbing human voices

all_results_p1 <- data.frame()

# Process each deployment
for (deployment in all_deployments) {
  results_path <- file.path(deployment, "results_formatted")
  # Check if the results folder exists
  if (dir.exists(results_path)) {
    # Gather results for this deployment
    formatted_results <- birdnet_gather(results.directory = results_path, formatted = TRUE)
    # Add deployment info
    formatted_results$deployment <- basename(deployment)
    # Append to all_results_p2
    all_results_p1 <- rbind(all_results_p1, formatted_results)
  }
}

# Fix resultspath names
all_results_p1$resultspath <- sub(
  "(.*\\\\SD_[AB]\\\\)(\\d{8}\\\\)(.*)",
  "\\1\\2results_formatted\\\\\\3",
  sub("\\.WAV$", ".BirdNET_formatted_results.csv", all_results_p1$filepath)
)

# Specify drive letters if changed by the code above
all_results_p1$filepath <- sub("^.:", "D:", all_results_p1$filepath)
all_results_p1$resultspath <- sub("^.:", "D:", all_results_p1$resultspath)
rm(formatted_results)

# Filter by confidence >= 0.1 (also removes NAs)
filt_p1_1 <- all_results_p1[all_results_p1$confidence >= 0.1, ]


## Second pass results -----------------------------------------------------

all_results_p2 <- data.frame()

# Process each deployment
for (deployment in all_deployments) {
  results_path2 <- file.path(deployment, "results_pass2_formatted")
  # Check if the results folder exists
  if (dir.exists(results_path2)) {
    # Gather results for this deployment
    formatted_results <- birdnet_gather(results.directory = results_path2, formatted = TRUE)
    # Add deployment info
    formatted_results$deployment <- basename(deployment)
    # Append to all_results_p2
    all_results_p2 <- rbind(all_results_p2, formatted_results)
  }
}

# Fix resultspath names
all_results_p2$resultspath <- sub(
  "(.*\\\\SD_[AB]\\\\)(\\d{8}\\\\)(.*)",
  "\\1\\2results_pass2_formatted\\\\\\3",
  sub("\\.WAV$", ".BirdNET_formatted_results.csv", all_results_p2$filepath)
)

# Specify drive letters if changed by the code above
all_results_p2$filepath <- sub("^.:", "D:", all_results_p2$filepath)
all_results_p2$resultspath <- sub("^.:", "D:", all_results_p2$resultspath)
rm(formatted_results)

# Filter by confidence >= 0.1 (also removes NAs)
filt_p2_1 <- all_results_p2[all_results_p2$confidence >= 0.1, ]

## Human Voice Scrubbing ---------------------------------------------------
# TODO : have filt_h.1 pull filepath-start/end combos to limit loss of detections for validation

# make a vector filt_g.1 with all the filepaths w/ a human vocal detection
filt_g.1 <- filt_p1_1 %>%
  filter(scientific_name == 'Human vocal') %>%
  distinct(filepath) %>%
  pull(filepath)

# filter filt.1 to keep only the filepaths without human vocal detection
filt_h.1 <- filt_p2_1 %>%
  filter(!filepath %in% filt_g.1)
rm(filt_g.1, filt_p1_1) # cleanup


# Count # of observations of each species at confidence >= 0.1
species_p2 <- count(filt_p2_1, filt_p2_1$common_name, sort = TRUE)

# Count # of observations of each species after scrubbing human voices
species_filt <- count(filt_h.1, filt_h.1$common_name, sort = TRUE)

#write_csv(species_filt, "Data/filtered_counts/SLR3filt.csv")

## Validate Results --------------------------------------------------------
# Create a random sample of N detections to verify

# NOCA - DONE
# AMRO - DONE
# CARW - DONE
# TUTI - DONE
# CACH - DONE + redone
# AGOL - DONE
# RBWO - DONE
# BLJA - DONE

set.seed(4)
to_verify <- filt_h.1[common_name == "Carolina Chickadee"][sample(.N, 41)]

# Create a verification library
ver_lib <- c('y', 'n', 'unsure')

# Set directories for audio and results
audio_directories <- dirname(to_verify$filepath)
results_directories <- dirname(to_verify$resultspath)

# Create list to hold validation results
verification_results <- vector("list", nrow(to_verify))

# Ensure you don't have to hunt down each temp audio file
setwd("Data/validation_results_files")

## birdnet_verify ##
for (i in 1:nrow(to_verify)) {
  detection <- to_verify[i, ]
  
  verification_results[[i]] <- birdnet_verify(data = detection,
                                              verification.library = ver_lib,
                                              audio.directory = audio_directories[i],
                                              results.directory = results_directories[i],
                                              overwrite = TRUE,
                                              play = TRUE,
                                              frq.lim = c(0, 12),
                                              buffer = 1,
                                              box.col = 'blue',
                                              spec.col = monitoR::gray.3())
}

# Update the verify column with results
to_verify$verify <- sapply(verification_results, function(res) {
  if (!is.null(res)) {
    return(res$verify)
  } else {
    return(NA)
  }
})

# Update the all_results_p2 verify column with results
for (i in 1:nrow(to_verify)) {
  chk_filepath    <- to_verify$filepath[i]
  chk_confidence  <- to_verify$confidence[i]
  chk_common_name <- to_verify$common_name[i]
  all_results_p2$verify[
    all_results_p2$filepath    == chk_filepath & 
      all_results_p2$confidence  == chk_confidence & 
      all_results_p2$common_name == chk_common_name
    ] <- to_verify$verify[i]
}

### FIXME so it updates one spreadsheet instead of a new one for each species
# maybe it already kinda does
setwd("C:/Users/kirchgrabera/Smithsonian Dropbox/Aidan Kirchgraber/Science and Faith/Aidan/birdsong_heat_project_Aidan")
write_csv(all_results_p2, "Data/validation_results_files/CACH/SMP11results_CACH.csv")
1+1
## Set Confidence Thresholds -----------------------------------------------
setwd("Data/validation_results_files/CACH")

temp = list.files(pattern = "*.csv")

col_types = cols( # this will ensure 'verify' is not imported as bool, which loses data
  .default = col_guess(),
  verify = col_character())

# Import dataframes
big_data = lapply(temp, function(file) read_delim(file, col_types = col_types))
rm(col_types)

# Subset to verified results
lil_data <- map(big_data, ~ filter(.x, !is.na(verify)))

# Find threshold confidence value
# Get info about true positives by confidence level
results_list <- lapply(lil_data, function(df) {
  df %>%
    mutate(confidence_bin = cut(confidence, breaks = seq(0, 1, by = 0.01), 
                                labels = paste0(seq(0, 0.99, by = 0.01), "-", 
                                                seq(0.01, 1, by = 0.01)),
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
})
names(results_list) <- gsub("\\.csv$", "", temp)

# Get the thresholds at which "true pos/total" = 95%
threshold_list <- lapply(results_list, function(df) {
  df %>%
    group_by(common_name) %>%
    summarize(threshold_bin = case_when(
      all(positive_rate >= 0.95) ~ "All bins",
      all(positive_rate < 0.95) ~ "No bins",
      TRUE ~ as.character(confidence_bin[which(positive_rate >= 0.95)[1]])),
      total_detections = sum(total_detections),
      .groups = "drop")
})


# Pooled Confidence Thresholds --------------------------------------------

# Combine each site's results into one dataframe 
pool_data <- bind_rows(lil_data, .id = "column_label")

# Find threshold confidence value
# Get info about true positives by confidence level
pool_results_list <- pool_data %>%
  mutate(confidence_bin = cut(confidence, breaks = seq(0, 1, by = 0.01), 
                              labels = paste0(seq(0, 0.99, by = 0.01), "-", 
                                              seq(0.01, 1, by = 0.01)),
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


# Get the thresholds at which "true pos/total" = 95%
pool_threshold_list <- pool_results_list %>%
  group_by(common_name) %>%
  summarize(threshold_bin = case_when(
    all(positive_rate >= 0.95) ~ "All bins",
    all(positive_rate < 0.95) ~ "No bins",
    TRUE ~ as.character(confidence_bin[which(positive_rate >= 0.95)[1]])),
    total_detections = sum(total_detections),
    .groups = "drop")

# Checking out how many CACH were validated at each site
cach1 <- filter(pool_data, common_name == "Carolina Chickadee") %>%
cach2 <- table(cach1$column_label)

#   common_name            threshold_bin              total_detections
# 1 American Goldfinch     0.24-0.25                  850
# 2 American Robin         0.26-0.27                  850
# 3 Black-capped Chickadee 0.44-0.45                  531  will probably remove BCCH from consideration + do more CACH, where possible
# 4 Blue Jay               0.31-0.32                  854
# 5 Carolina Chickadee     0.12-0.13                  321
# 6 Carolina Wren          0.2-0.21                   850
# 7 Northern Cardinal      0.09-0.1                   876
# 8 Red-bellied Woodpecker 0.27-0.28                  848
# 9 Tufted Titmouse        0.22-0.23                  850


# Counts by site ----------------------------------------------------------

counts_dir <- 'Data/filtered_counts'

# List the csv files in the directory
count_files <- list.files(path = counts_dir, pattern = "\\.csv$", full.names = TRUE)

# Read each csv and add a site name column
read_site_csv <- function(file_path) {
  # Extract site name from filename
  site_name <- tools::file_path_sans_ext(basename(file_path))
  # Categorize sites as exurban vs. urban
  site_type <- ifelse(
    grepl("^N", site_name) | grepl("^MC", site_name),
    "exurban", "urban")
  # Read the csv
  df <- read_csv(file_path, col_names = c("species", "detections"), skip = 1)
  # Add site name column
  df$site <- site_name
  df$site_type <- site_type
  return(df)
}

# Read CSV files
all_site_data <- lapply(count_files, read_site_csv)

# Combine dfs
all_site_data <- do.call(rbind, all_site_data)

################################################################

habitat_analysis <- all_site_data %>%
  group_by(species, site_type) %>%
  # Sum detections for each species in each habitat type
  summarise(total_detections = sum(detections), .groups = "drop") %>%
  # Spread to wide format
  pivot_wider(
    names_from = site_type,
    values_from = total_detections,
    values_fill = 0) %>%
  filter(exurban > 10) %>% 
  mutate(
    log_urban = log(urban),
    log_exurban = log(exurban),
    exurban_urban_ratio = exurban/urban,
  )

################################################################

# Pivot the data to wide format
all_site_wide <- all_site_data %>%
  pivot_wider(
    names_from = site, 
    values_from = detections, 
    values_fill = 0  # Fill missing values with 0 if a species is not detected in a site
  )

# Print the resulting dataframe
print(all_site_wide)

#write_csv(all_site_wide, "Data/serc_bird_detections.csv")

# Boneyard -----------------------------------------------------------------
## Species counts ----------------------------------------------------------

# filter by confidence >= 0.1, 0.2, and 0.3
filt.1 <- filter(formatted.results, confidence >= 0.1)
filt.2 <- filter(formatted.results, confidence >= 0.2)
filt.3 <- filter(formatted.results, confidence >= 0.3)

# count of # observation of each species at confidence >= 0.1, 0.2, and 0.3
species.1 <- count(filt.1, filt.1$common_name, sort = TRUE)
species.2 <- count(filt.2, filt.2$common_name, sort = TRUE)
species.3 <- count(filt.3, filt.3$common_name, sort = TRUE)

## Scrub human voices -------------------------------------------------------
setwd(audio_path)

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

## VALIDATION PIPELINE MK.1 ------------------------------------------------

# Create a random sample of N detections to verify
set.seed(4)
to.verify <- filt.1[common_name == "Northern Cardinal"][sample(.N, 50)]

# Create a verification library for this species
ver.lib <- c('y', 'n', 'unsure')

# Verify detections
birdnet_verify(data = to.verify,
               verification.library = ver.lib,
               audio.directory = audio_path,
               results.directory = results_path_fm,
               overwrite = FALSE,
               play = TRUE,
               frq.lim = c(0, 12),
               buffer = 1,
               box.col = 'blue',
               spec.col = monitoR::gray.3()
)


# Check that underlying files have been updated with user verifications
formatted.results <- birdnet_gather(results.directory = results_path_fm,
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



