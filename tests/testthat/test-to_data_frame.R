test_that("to_data_frame() converts an EprimeFrame to one row with metadata", {
  frame <- as.EprimeFrame(list(
    Eprime.Level = 2,
    Eprime.LevelName = "Trial_1",
    Eprime.Basename = "experiment",
    Eprime.FrameNumber = "3",
    Procedure = "TrialProc",
    Running = "Trial",
    Response = "",
    Duration = "001"
  ))

  expected <- data.frame(
    Eprime.Level = 2,
    Eprime.LevelName = "Trial_1",
    Eprime.Basename = "experiment",
    Eprime.FrameNumber = "3",
    Procedure = "TrialProc",
    Running = "Trial",
    Response = "",
    Duration = "001",
    stringsAsFactors = FALSE
  )

  expect_identical(to_data_frame(frame), expected)
})

test_that("to_data_frame() combines plain lists and fills missing columns", {
  frames <- list(
    list(Item = "bear", Correct = TRUE),
    list(Duration = 885L, Item = "chair"),
    list(Item = "keys", Correct = FALSE, Duration = 900L)
  )

  expected <- data.frame(
    Item = c("bear", "chair", "keys"),
    Correct = c(TRUE, NA, FALSE),
    Duration = c(NA_integer_, 885L, 900L),
    stringsAsFactors = FALSE
  )

  expect_identical(to_data_frame(frames), expected)
})

test_that("to_data_frame() combines FrameList rows with metadata and missing values", {
  frames <- as.FrameList(list(
    as.EprimeFrame(list(Item = "bear", Response = "")),
    as.EprimeFrame(list(Duration = "001", Item = "chair"))
  ))

  expected <- data.frame(
    Eprime.Level = c(NA, NA),
    Eprime.LevelName = c(NA, NA),
    Eprime.Basename = c(NA, NA),
    Eprime.FrameNumber = c(NA, NA),
    Procedure = c(NA, NA),
    Running = c(NA, NA),
    Item = c("bear", "chair"),
    Response = c("", NA_character_),
    Duration = c(NA_character_, "001"),
    stringsAsFactors = FALSE
  )

  expect_identical(to_data_frame(frames), expected)
})

test_that("to_data_frame() returns NULL for empty lists", {
  expect_null(to_data_frame(list()))
  expect_null(to_data_frame(as.FrameList(list())))
})
