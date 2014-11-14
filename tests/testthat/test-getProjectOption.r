##------------------------------------------------------------------------------
context("getProjectOption/basics")
##------------------------------------------------------------------------------

test_that("getProjectOption/basics", {
  
  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }

  container <- initializeOptionContainer(overwrite = TRUE)
  expect_true(res <- setProjectOption(id = "test", value = TRUE))
  expect_equal(res <- getProjectOption(id = "test"), TRUE)
  
  expect_true(res <- setProjectOption(id = "a/b/c", value = 10))
  expect_is(res <- getProjectOption(id = "a"), "environment")
  expect_is(res <- getProjectOption(id = "a/b"), "environment")
  expect_equal(res <- getProjectOption(id = "a/b/c"), 10)
  
  on.exit(setwd(wd_0))
  
})

##------------------------------------------------------------------------------
context("getProjectOption/default")
##------------------------------------------------------------------------------

test_that("getProjectOption/default", {
  
  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }

  container <- initializeOptionContainer(overwrite = TRUE)
  expect_equal(getProjectOption(id = "test", default = NA), NA)
  expect_equal(res <- getProjectOption(id = "a/b/c", default = NA), NA)
  
  on.exit(setwd(wd_0))
  
})

##------------------------------------------------------------------------------
context("getProjectOption/where")
##------------------------------------------------------------------------------

test_that("getProjectOption/where", {

  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  where <- "test"
  container <- initializeOptionContainer(id = where, overwrite = TRUE)
  expect_true(res <- setProjectOption(id = "a/b/c", value = 10, where = where))
  expect_equal(res <- getProjectOption(id = "a/b/c", where = where), 10)
  
  where <- structure(list(id = "test"), class = "OptionContext.Test")
  container <- initializeOptionContainer(id = where, overwrite = TRUE)
  expect_true(res <- setProjectOption(id = "a/b/c", value = 10, where = where))
  expect_equal(res <- getProjectOption(id = "a/b/c", where = where), 10)
  
  on.exit(setwd(wd_0))
  
})

