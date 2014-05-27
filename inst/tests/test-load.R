require("stringr")

# Test files
bad_encoding <- "data/Blending_001L00XS4.txt"
crashed_experiment <- "data/MP_Block1_001P00XA1.txt"
no_footer <- "data/Coartic_Block1_001P00XS1.txt"

is_header_line <- function(x) x == "*** Header Start ***"
first_line <- function(xs) xs[1]
count_blanks <- function(xs) sum(str_detect(xs, "^$"))


context("load file with unexpected encoding")

test_that("naive file-loading fails", {
  expect_warning(readLines(bad_encoding, encoding = "UCS-2LE"),
                 regexp = "embedded nul")
  expect_more_than(count_blanks(readLines(bad_encoding, skipNul = TRUE)), 1)
})

test_that("read_eprime works", {
  bad_encoding_lines <- read_eprime(bad_encoding)
  expect_true(is_header_line(first_line(bad_encoding_lines)))
  expect_less_than(count_blanks(bad_encoding_lines), 1)
})


context("load file from a crashed experiment")

test_that("naive file-loading fails", {
  expect_warning(readLines(crashed_experiment, skipNul = TRUE),
                 regexp = "incomplete final line")
})

test_that("read_eprime works", {
  crashed_experiment_lines <- read_eprime(bad_encoding)
  expect_true(is_header_line(first_line(crashed_experiment_lines)))
  expect_less_than(count_blanks(crashed_experiment_lines), 1)
})







