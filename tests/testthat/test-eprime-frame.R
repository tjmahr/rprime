# Empty EprimeFrame
default_list <- structure(list(
  Eprime.LevelName = NA,
  Eprime.Level = NA,
  Eprime.Basename = NA,
  Eprime.FrameNumber = NA,
  Procedure = NA,
  Running = NA), class = c("list", "EprimeFrame"))

key_value_list <- list(
  Procedure = "FamTask",
  item1 = "bear",
  item2 = "chair",
  CorrectResponse = "bear",
  Familiarization.Cycle = "1",
  Familiarization.Sample = "1",
  Running = "Familiarization",
  Correct = "True")

# Empty EprimeFrame + some values in a list
updated_list <- default_list
updated_list[names(key_value_list)] <- key_value_list

# Empty EprimeFrame + some values in a list + overwritten values from ...
final_list <- updated_list
final_list$Procedure <- "Demo"
final_list$Running <- "Practice"


context("EprimeFrame")

test_that("Uses default values", {
  expect_identical(EprimeFrame(list()), default_list)
  expect_identical(as.EprimeFrame(list()), default_list)
})

test_that("Works with a key-value list", {
  expect_identical(EprimeFrame(key_value_list), updated_list)
  expect_identical(as.EprimeFrame(key_value_list), updated_list)
})

test_that("Dots overwrite key-value pairs in list", {
  t3 <- EprimeFrame(key_value_list, Procedure = "Demo", Running = "Practice")
  expect_identical(t3, final_list)
})


test_that("listify works", {
  garbage_lines <- c("\t*** LogFrame Start ***", "*** LogFrame End ***", "", NA)
  usable_lines <- c(
    "Item: 1",
    "\tWhiteSpaceItem: Stuff  ",
    "AnotherItem: Two Colons: Yes",
    "NoValue: ",
    "Last Item: Whatever")
  expected <- list(
    Item = "1",
    WhiteSpaceItem = "Stuff",
    AnotherItem = "Two Colons: Yes",
    NoValue = "",
    `Last Item` = "Whatever")
  expect_identical(listify(c(garbage_lines, usable_lines)), expected)
})
