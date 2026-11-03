## Test functions that create lists from character vectors of colon-separated
## values.

# expect_identical lists to have the same order
expect_nearly_identical <- function(x, y, ...) {
  expect_identical(sort_names(x), sort_names(y), ...)
}
sort_names <- function(x) x[sort(names(x))]



test_that("EprimeFrame() and as.EprimeFrame() fill missing metadata with defaults", {
  # Empty EprimeFrame
  default_list <- structure(list(
    Eprime.LevelName = NA,
    Eprime.Level = NA,
    Eprime.Basename = NA,
    Eprime.FrameNumber = NA,
    Procedure = NA,
    Running = NA), class = c("EprimeFrame", "list"))

  expect_nearly_identical(as.EprimeFrame(list()), default_list)

  # It's not possible to have an NA Eprime.Level when constructing from a
  # character vector
  default_character <- default_list
  default_character$Eprime.Level <- 1
  expect_nearly_identical(EprimeFrame(character()), default_character)
})

test_that("EprimeFrame() parses key-value lines and ignores unrelated text", {
  test_values <- c(
    "key: value",
    "question: answer",
    "garbage text")

  expected_list <- structure(list(
    Eprime.LevelName = NA,
    Eprime.Level = 1,
    Eprime.Basename = NA,
    Eprime.FrameNumber = NA,
    Procedure = NA,
    Running = NA,
    key = "value",
    question = "answer"), class = c("EprimeFrame", "list"))

  expect_nearly_identical(EprimeFrame(test_values), expected_list)
})

test_that("EprimeFrame() normalizes Running-related field names and level labels", {
  keys_values <- c(
    "Running: Demo",
    "Demo: ExampleCode",
    "Demo.Cycle: 1",
    "Demo.Sample: 1",
    "Key: Value")

  normalized_running <- structure(list(
    Eprime.Level = 1,
    Eprime.LevelName = "Demo_ExampleCode",
    Eprime.Basename = NA,
    Eprime.FrameNumber = NA,
    Procedure = NA,
    Running = "Demo",
    Cycle = "1",
    Sample = "1",
    Key = "Value"), class = c("EprimeFrame", "list"))

  expect_nearly_identical(EprimeFrame(keys_values), normalized_running)
})


test_that("listify() parses indented key-value lines and ignores frame markers", {
  example_lines <- c(
    "\t*** LogFrame Start ***",
    "\tProcedure: PracticeProc",
    "\tStimulus1: toothbrush",
    "\t*** LogFrame End ***")

  expected_example <- list(
    Procedure = "PracticeProc",
    Stimulus1 = "toothbrush")

  expect_identical(listify(example_lines), expected_example)
})

test_that("listify() trims whitespace and preserves empty values and extra colons", {
  garbage_lines <- c("", NA,
    "\t*** LogFrame Start ***",
    "*** LogFrame End ***")

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


test_that("print.EprimeFrame() includes field values and class names", {
  eprime_log <- read_eprime(testthat::test_path("data", "Blending_001L00XS4.txt"))
  chunked <- FrameList(eprime_log)

  # Print the frame
  frame <- chunked[[1]]
  printed_frame <- capture.output(frame)

  # Check specific lines in the print function
  proc_line <- ' $ Procedure          : chr "Header"'
  classes <- ' - attr(*, "class")= chr [1:2] "EprimeFrame" "list"'
  expect_equal(printed_frame[5 + 1], proc_line)
  expect_equal(printed_frame[22 + 2], classes)
})

test_that("print.FrameList() includes list sizes and nested class names", {
  eprime_log <- read_eprime(testthat::test_path("data", "Blending_001L00XS4.txt"))
  chunked <- FrameList(eprime_log)

  # Print the frame
  printed_list <- capture.output(chunked)
  nlines <- length(printed_list)

  # Check specific lines in the print function
  l1 <- "List of 25"
  l2 <- " $ :List of 22"
  last_1 <- '  ..- attr(*, "class")= chr [1:2] "EprimeFrame" "list"'
  last_2 <- ' - attr(*, "class")= chr [1:2] "list" "FrameList"'

  expect_equal(printed_list[1], l1)
  expect_equal(printed_list[2], l2)
  expect_equal(printed_list[nlines - 1], last_1)
  expect_equal(printed_list[nlines], last_2)
})
