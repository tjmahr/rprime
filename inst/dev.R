require("stringr")
require("stringi")
require("tools")
require("reshape2")

filename <- "inst/tests/data/Blending_001L00XS4.txt"
eprime_log <- read_eprime(filename)
chunked <- extract_chunks(eprime_log)

make_eprime_frames(chunked[3])
make_eprime_frames(chunked[3:5], tidy = FALSE)
make_eprime_frames(chunked[[3]])


frame_list <- make_eprime_frames(chunked)
preview_levels(frame_list)
preview_eprime(frame_list)


level_2 <- drop_levels(frame_list, 1)
level_2 <- filter_in(frame_list, "Running", "TrialList")
responses <- pick_apply(c("Eprime.Basename", "Sample", "Stimulus1", "Stimulus2", "Stimulus3", "CorrectResponse", "Response"), level_2)

to_data_frame(responses)
to_data_frame(level_2[1:3])
to_data_frame(level_2[[1]])



preview_levels(keep_levels(frame_list, 2))
preview_levels(drop_levels(frame_list, 1))
preview_levels(drop_levels(frame_list, c(1,2)))
preview_levels(filter_out(frame_list, "Running", NA))
preview_levels(filter_in(frame_list, "Running", "TrialList"))
preview_levels(filter_out(frame_list, "Running", c(NA, "Header", "PracticeList")))
preview_levels(drop_levels(frame_list, 0))



# Crashed experiment
filename <- "inst/tests/data/MP_Block1_001P00XA1.txt"
eprime_log <- read_eprime(filename)
chunked <- extract_chunks(eprime_log)
eprime_list <- make_eprime_frames(chunked)
preview_levels(eprime_list)

eprime_list <- keep_levels(eprime_list, 3)
preview_frames(eprime_list)
to_data_frame(eprime_list)






# Crashed experiment
filename <- "inst/tests/data/MP_Block1_001P00XA1.txt"
eprime_log <- read_eprime(filename)
chunked <- extract_chunks(eprime_log)




# No footer
filename <- "inst/tests/data/Coartic_Block1_001P00XS1.txt"
eprime_log <- read_eprime(filename)
chunked <- extract_chunks(eprime_log)

# Not an Eprime file
filename <- "inst/tests/data/not_an_eprime_file.txt"
eprime_log <- read_eprime(filename)
chunked <- extract_chunks(eprime_log)
eprime_list <- make_eprime_frames(chunked)
preview_levels(eprime_list)


















pluck_apply("Eprime.Level", frame_list[1:6])
pluck_apply("Running", frame_list)
pluck_apply( "Procedure", frame_list)

