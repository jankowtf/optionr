# rapp::createPackageProject(id = "test.package", path = "tests/testthat")

##------------------------------------------------------------------------------
context("getAnywhereOption/basics")
##------------------------------------------------------------------------------

test_that("getAnywhereOption/basics", {
  
  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }

  container <- initializeOptionContainer(overwrite = TRUE)
  expect_true(res <- setAnywhereOption(id = "test", value = TRUE))
  expect_equal(res <- getAnywhereOption(id = "test"), TRUE)
  
  expect_true(res <- setAnywhereOption(id = "a/b/c", value = 10))
  expect_is(res <- getAnywhereOption(id = "a"), "environment")
  expect_is(res <- getAnywhereOption(id = "a/b"), "environment")
  expect_equal(res <- getAnywhereOption(id = "a/b/c"), 10)
  
  on.exit(setwd(wd_0))
  
})

##------------------------------------------------------------------------------
context("getAnywhereOption/default")
##------------------------------------------------------------------------------

test_that("getAnywhereOption/default", {
  
  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }

  container <- initializeOptionContainer(overwrite = TRUE)
  expect_equal(getAnywhereOption(id = "test", default = NA), NA)
  expect_equal(res <- getAnywhereOption(id = "a/b/c", default = NA), NA)
  
  on.exit(setwd(wd_0))
  
})

##------------------------------------------------------------------------------
context("getAnywhereOption/where")
##------------------------------------------------------------------------------

test_that("getAnywhereOption/where", {

  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  where <- "test"
  container <- initializeOptionContainer(id = where, overwrite = TRUE)
  expect_true(res <- setAnywhereOption(id = "a/b/c", value = 10, where = where))
  expect_equal(res <- getAnywhereOption(id = "a/b/c", where = where), 10)
  
  where <- structure(list(id = "test"), class = "OptionContext.Test")
  container <- initializeOptionContainer(id = where, overwrite = TRUE)
  expect_true(res <- setAnywhereOption(id = "a/b/c", value = 10, where = where))
  expect_equal(res <- getAnywhereOption(id = "a/b/c", where = where), 10)
  
  on.exit(setwd(wd_0))
})

