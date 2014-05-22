stooge1 <- list(name = "moe", age = 40)
stooge2 <- list(name = "larry", age = 50)
stooge3 <- list(name = "curly", age = 60, age = NA)
stooges <- list(stooge1, stooge2, stooge3)



context("pluck")

test_that("pluck only the first matching name", {
  expect_identical(pluck("name")(stooge1), "moe")
  expect_identical(pluck("age")(stooge3), 60)
  expect_equal(length(pluck("age")(stooge3)), 1)
})

test_that("pluck NULL if there is no matching name", {
  expect_null(pluck("nmae_misspelled")(stooge1))
})

test_that("pluck_apply plucks unnamed values from each list", {
  expect_null(names(pluck_apply("age", stooges)))
  expect_equal(length(pluck_apply("nmae_misspelled", stooges)), 3)
})


context("pick")

test_that("pick keeps names", {
  expect_named(pick("age")(stooge1))
  expect_named(pick_apply("name", stooges)[[1]])
})

test_that("pick extracts the names that it can", {
  expect_equal(length(pick(c("age", "nmae_misspelled"))(stooge1)), 1)
})

test_that("pick extracts all matching name", {
  expect_equal(length(pick("age")(stooge3)), 2)
})


context("omit")

test_that("omit keeps names", {
  expect_named(omit("age")(stooge1))
  expect_named(omit_apply("name", stooges)[[1]])
})

test_that("omit drops all matching names", {
  expect_equal(length(omit("age")(stooge3)), 1)
})

test_that("omit drops all matching names", {
  expect_equal(omit("nmae_misspelled")(stooge3), stooge3)
  expect_equal(omit("nmae_misspelled")(stooge3), stooge3)
})




