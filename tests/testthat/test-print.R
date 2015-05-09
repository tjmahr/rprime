# Test files
good_file <- "data/Blending_001L00XS4.txt"

context("preview eprime files")

test_that("preview levels", {
  eprime_log <- read_eprime(good_file)
  chunked <- FrameList(eprime_log)
  output <- capture.output(preview_levels(chunked))

  # This file has 5 levels
  expect_equal(length(output), 5 + 2)
  expect_equal(output[1], "Level Counts: ")

})

test_that("preview levels", {
  eprime_log <- read_eprime(good_file)
  chunked <- FrameList(eprime_log)
  output <- capture.output(preview_frames(chunked))

  # First chunk is the Header
  expect_true(output[2] == " Eprime.Level Running Procedure")
  expect_true(output[3] == "            1  Header    Header")

})
