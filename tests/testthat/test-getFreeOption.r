# rapp::createPackageProject(id = "test.package", path = "tests/testthat")

##------------------------------------------------------------------------------
context("getFreeOption/basics")
##------------------------------------------------------------------------------

test_that("setFreeOption/basics", {
  
  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }

  container <- initializeOptionContainer(overwrite = TRUE)
  expect_true(res <- setFreeOption(id = "test", value = TRUE))
  expect_equal(res <- getFreeOption(id = "test"), TRUE)
  
  expect_true(res <- setFreeOption(id = "a/b/c", value = 10, gap = TRUE))
  expect_is(res <- getFreeOption(id = "a"), "environment")
  expect_is(res <- getFreeOption(id = "a/b"), "environment")
  expect_equal(res <- getFreeOption(id = "a/b/c"), 10)
  
  on.exit(setwd(wd_0))
  
})

##------------------------------------------------------------------------------
context("getFreeOption/default")
##------------------------------------------------------------------------------

test_that("setFreeOption/default", {
  
  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }

  container <- initializeOptionContainer(overwrite = TRUE)
  expect_equal(getFreeOption(id = "test", default = NA), NA)
  expect_equal(res <- getFreeOption(id = "a/b/c", default = NA), NA)
  
  on.exit(setwd(wd_0))
  
})

##------------------------------------------------------------------------------
context("setFreeOption/where")
##------------------------------------------------------------------------------

test_that("setFreeOption/where", {

  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  where <- "test"
  container <- initializeOptionContainer(id = where, overwrite = TRUE)
  expect_true(res <- setFreeOption(id = "a/b/c", value = 10, 
    where = where, gap = TRUE))
  expect_equal(res <- getFreeOption(id = "a/b/c", where = where), 10)
  
  where <- structure(list(id = "test"), class = "OptionContext.Test")
  container <- initializeOptionContainer(id = where, overwrite = TRUE)
  expect_true(res <- setFreeOption(id = "a/b/c", value = 10, 
    where = where, gap = TRUE))
  expect_equal(res <- getFreeOption(id = "a/b/c", where = where), 10)
  
  on.exit(setwd(wd_0))
})

