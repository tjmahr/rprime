## ----, echo = FALSE, message = FALSE-------------------------------------
library(rprime)

knitr::opts_chunk$set(
  comment = "#>",
  error = FALSE,
  tidy = FALSE)


## ------------------------------------------------------------------------
library(plyr)
reduce_sails <- function(sails_path) {
  sails_lines <- read_eprime(sails_path)
  sails_frames <- make_eprime_frames(extract_chunks(sails_lines))
  
  # Trials occur at level 3
  sails_frames <- keep_levels(sails_frames, 3)
  sails <- to_data_frame(sails_frames)
  
  # Tidy up
  to_pick <- c("Eprime.Basename", "Running", "Module", "Sound", "Sample",
               "Correct", "Response")
  sails <- sails[to_pick]
  running_map <- c(TrialLists = "Trial", PracticeBlock = "Practice")
  sails$Running <- revalue(sails$Running, running_map)
  
  # Number trials in the practice and experimental blocks separately
  sails <- ddply(sails, .(Running), mutate, 
                 TrialNumber = seq(from = 1, to = length(Running)),
                 CorrectResponse = ifelse(Correct == Response, 1, 0))
  sails$Sample <- NULL
  
  # Optionally, you might save the processed file via: 
  # csv <- paste0(file_path_sans_ext(sails_path), ".csv")
  # write.csv(sails, csv, row.names = FALSE)
  sails
}

## ------------------------------------------------------------------------
head(reduce_sails("data/SAILS/SAILS_001X00XS1.txt"))

## ------------------------------------------------------------------------
sails_paths <- list.files("data/SAILS/", pattern = ".txt", full.names = TRUE)
sails_paths
ensemble <- ldply(sails_paths, reduce_sails)

## ------------------------------------------------------------------------
# Score modules within subjects
modules <- ddply(ensemble, .(Eprime.Basename, Running, Module), summarize, 
                 Score = sum(CorrectResponse),
                 PropCorrect = Score / length(CorrectResponse))
modules

# Drop practice values, then print data in wide format
library(reshape2)
dcast(modules, Eprime.Basename ~ Module, 
      subset = .(Running == "Trial"), 
      fun.aggregate = mean, value.var = "PropCorrect")

# Score trials within subjects
overall <- ddply(ensemble, .(Eprime.Basename, Running), summarize, 
                 Score = sum(CorrectResponse),
                 PropCorrect = Score / length(CorrectResponse))
overall

