test_that("example_eprime_file() locates files by short name or full filename", {
  filenames <- c(
    Blending = "Blending_001L00XS4.txt",
    MINP = "MINP_001L00XS1.txt",
    MP = "MP_Block1_001P00XA1.txt",
    SAILS1 = "SAILS_001X00XS1.txt",
    SAILS2 = "SAILS_002X00XS1.txt",
    not_an_eprime_file = "not_an_eprime_file.txt"
  )

  for (short_name in names(filenames)) {
    filename <- unname(filenames[[short_name]])
    path <- example_eprime_file(short_name)

    expect_identical(basename(path), filename)
    expect_identical(file.exists(path), TRUE)
    expect_identical(example_eprime_file(filename), path)
  }
})

test_that("example_eprime_file() lists available names when which is missing", {
  output <- capture.output(result <- withVisible(example_eprime_file()))

  expect_identical(names(result$value), c(
    "Blending", "MINP", "MP", "SAILS1", "SAILS2", "not_an_eprime_file"
  ))
  expect_identical(result$visible, FALSE)
  for (name in names(result$value)) {
    expect_match(paste(output, collapse = "\n"), name, fixed = TRUE)
  }
})

test_that("example_eprime_file() rejects unknown or multiple names", {
  expect_error(
    example_eprime_file("missing"), "Unknown example file",
    fixed = TRUE, class = "simpleError"
  )
  expect_error(
    example_eprime_file(c("MINP", "MP")), "single filename",
    fixed = TRUE, class = "simpleError"
  )
  expect_error(
    example_eprime_file(NA_character_), "single filename",
    fixed = TRUE, class = "simpleError"
  )
  expect_error(
    example_eprime_file(1), "single filename",
    fixed = TRUE, class = "simpleError"
  )
})
