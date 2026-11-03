# Test files
good_file <- testthat::test_path("data", "Blending_001L00XS4.txt")


test_that("preview_levels() prints a heading and one row per unique level", {
  eprime_log <- read_eprime(good_file)
  chunked <- FrameList(eprime_log)
  output <- capture.output(preview_levels(chunked))

  # This file has 5 levels
  expect_equal(length(output), 5 + 2)
  expect_equal(output[1], "Level Counts: ")
})

test_that("preview_frames() prints the header frame metadata first", {
  eprime_log <- read_eprime(good_file)
  chunked <- FrameList(eprime_log)
  output <- capture.output(preview_frames(chunked))

  # First chunk is the Header
  expect_true(output[2] == " Eprime.Level Running Procedure")
  expect_true(output[3] == "            1  Header    Header")
})

test_that("preview_eprime() prints the level preview followed by the frame preview", {
  eprime_log <- read_eprime(good_file)
  chunked <- FrameList(eprime_log)
  output <- capture.output(preview_eprime(chunked))
  output_levels <- capture.output(preview_levels(chunked))
  output_frames <- capture.output(preview_frames(chunked))

  expect_true(all(output == c(output_levels, output_frames)))
})

test_that("preview_levels() returns its input invisibly", {
  frames <- FrameList(read_eprime(testthat::test_path("data", "Blending_001L00XS4.txt")))
  capture.output(result <- withVisible(preview_levels(frames)))

  expect_identical(result$value, frames)
  expect_identical(result$visible, FALSE)
})

test_that("preview_frames() returns its input invisibly", {
  frames <- FrameList(read_eprime(testthat::test_path("data", "Blending_001L00XS4.txt")))
  capture.output(result <- withVisible(preview_frames(frames)))

  expect_identical(result$value, frames)
  expect_identical(result$visible, FALSE)
})

test_that("preview_eprime() returns its input invisibly", {
  frames <- FrameList(read_eprime(testthat::test_path("data", "Blending_001L00XS4.txt")))
  capture.output(result <- withVisible(preview_eprime(frames)))

  expect_identical(result$value, frames)
  expect_identical(result$visible, FALSE)
})
