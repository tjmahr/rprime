## ---- echo = FALSE, message = FALSE--------------------------------------
library("rprime")
library("knitr")
opts_chunk$set(
  comment = "#>",
  error = FALSE,
  tidy = FALSE,
  collapse = TRUE)

## ------------------------------------------------------------------------
# Read in an Eprime text file
experiment_lines <- read_eprime("data/SAILS/SAILS_001X00XS1.txt")

# Extract and parse the log-frames from the file
experiment_data <- FrameList(experiment_lines)

## ------------------------------------------------------------------------
# Run-down of different log-frames in the FrameList
preview_levels(experiment_data)

# Filter (out) by depth of nesting
not_level_1 <- drop_levels(experiment_data, 1)
preview_levels(not_level_1)

# Filter (in) by depth of nesting
just_level_3 <- keep_levels(experiment_data, 3)

# Preview each kind of log-frame in the FrameList
preview_eprime(just_level_3)

# Filter (out) by attribute values
no_header <- filter_out(experiment_data, "Running", values = "Header")
preview_levels(no_header)

# Filter (in) by attribute values
not_practice <- filter_in(experiment_data, "Running", "TrialLists")
preview_eprime(not_practice)

# Drill down into a FrameList
sue_trials <- filter_in(not_practice, "Module", "SUE")
preview_eprime(sue_trials)

## ------------------------------------------------------------------------
# Export to dataframe
columns_to_keep <- c("Eprime.Basename", "Module", "Sample", 
                     "Correct", "Response")
sue_trials_df <- to_data_frame(sue_trials)[columns_to_keep]
head(sue_trials_df)

