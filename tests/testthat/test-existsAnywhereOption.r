##------------------------------------------------------------------------------
context("existsAnywhereOption/basics")
##------------------------------------------------------------------------------

test_that("existsAnywhereOption/basics", {
  
  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }

  container <- initializeOptionContainer(overwrite = TRUE)
  expect_true(setAnywhereOption(id = "test", value = TRUE))
  expect_true(existsAnywhereOption(id = "test"))
  expect_false(existsAnywhereOption(id = "test_1"))
  
  expect_true(setAnywhereOption(id = "a/b/c", value = 10))
  expect_true(existsAnywhereOption(id = "a"))
  expect_true(existsAnywhereOption(id = "a/b"))
  expect_true(existsAnywhereOption(id = "a/b/c"))
  expect_false(existsAnywhereOption(id = "a/b/c/d"))
  
  on.exit(setwd(wd_0))
  
})

##------------------------------------------------------------------------------
context("existsAnywhereOption/where")
##------------------------------------------------------------------------------

test_that("existsAnywhereOption/where", {

  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  where <- "test"
  container <- initializeOptionContainer(id = where, overwrite = TRUE)
  expect_true(setAnywhereOption(id = "a/b/c", value = 10, where = where))
  expect_true(existsAnywhereOption(id = "a/b/c", where = where))
  expect_false(existsAnywhereOption(id = "a/b/c/d", where = where))
  
  where <- structure(list(id = "test"), class = "OptionContext.Test")
  container <- initializeOptionContainer(id = where, overwrite = TRUE)
  expect_true(setAnywhereOption(id = "a/b/c", value = 10, where = where))
  expect_true(existsAnywhereOption(id = "a/b/c", where = where))
  expect_false(existsAnywhereOption(id = "a/b/c/d", where = where))
  
  on.exit(setwd(wd_0))
})

