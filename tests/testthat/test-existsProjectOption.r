##------------------------------------------------------------------------------
context("existsProjectOption/basics")
##------------------------------------------------------------------------------

test_that("existsProjectOption/basics", {
  
  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }

  container <- initializeOptionContainer(overwrite = TRUE)
  expect_true(setProjectOption(id = "test", value = TRUE))
  expect_true(existsProjectOption(id = "test"))
  expect_false(existsProjectOption(id = "test_1"))
  
  expect_true(setProjectOption(id = "a/b/c", value = 10))
  expect_true(existsProjectOption(id = "a"))
  expect_true(existsProjectOption(id = "a/b"))
  expect_true(existsProjectOption(id = "a/b/c"))
  expect_false(existsProjectOption(id = "a/b/c/d"))
  
  on.exit(setwd(wd_0))
  
})

##------------------------------------------------------------------------------
context("existsProjectOption/where")
##------------------------------------------------------------------------------

test_that("existsProjectOption/where", {

  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  where <- "test"
  container <- initializeOptionContainer(id = where, overwrite = TRUE)
  expect_true(setProjectOption(id = "a/b/c", value = 10, where = where))
  expect_true(existsProjectOption(id = "a/b/c", where = where))
  expect_false(existsProjectOption(id = "a/b/c/d", where = where))
  
  where <- structure(list(id = "test"), class = "OptionContext.Test")
  container <- initializeOptionContainer(id = where, overwrite = TRUE)
  expect_true(setProjectOption(id = "a/b/c", value = 10, where = where))
  expect_true(existsProjectOption(id = "a/b/c", where = where))
  expect_false(existsProjectOption(id = "a/b/c/d", where = where))
  
  on.exit(setwd(wd_0))
})