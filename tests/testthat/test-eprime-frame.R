expect_nearly_identical <- function(x, y, ...) {
  expect_identical(sort_names(x), sort_names(y), ...)
}

sort_names <- function(x) x[sort(names(x))]

# Empty EprimeFrame
default_list <- structure(list(
  Eprime.LevelName = NA,
  Eprime.Level = NA,
  Eprime.Basename = NA,
  Eprime.FrameNumber = NA,
  Procedure = NA,
  Running = NA), class = c("EprimeFrame", "list"))

# It's not possible to have an NA Eprime.Level when constructing from a
# character vector
default_character <- default_list
default_character$Eprime.Level <- 1

updated <- default_character
updated$Test <- "Passed"
test_character <- "Test: Passed"


context("EprimeFrame")

test_that("Uses default values", {
  expect_nearly_identical(as.EprimeFrame(list()), default_list)
  expect_nearly_identical(EprimeFrame(character()), default_character)
})

test_that("Simple from character construction", {
  expect_nearly_identical(EprimeFrame(test_character), updated)
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
