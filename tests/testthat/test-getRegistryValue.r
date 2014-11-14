##------------------------------------------------------------------------------
context("getRegistryValue/basics")
##------------------------------------------------------------------------------

test_that("getRegistryValue/basics", {
  
  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }

  container <- initializeOptionContainer(overwrite = TRUE)
  expect_true(res <- setRegistryValue(id = "test", value = TRUE))
  expect_equal(res <- getRegistryValue(id = "test"), TRUE)
  
  expect_true(res <- setRegistryValue(id = "a/b/c", value = 10))
  expect_is(res <- getRegistryValue(id = "a"), "environment")
  expect_is(res <- getRegistryValue(id = "a/b"), "environment")
  expect_equal(res <- getRegistryValue(id = "a/b/c"), 10)
  
  on.exit(setwd(wd_0))
  
})

##------------------------------------------------------------------------------
context("getRegistryValue/default")
##------------------------------------------------------------------------------

test_that("getRegistryValue/default", {
  
  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }

  container <- initializeOptionContainer(overwrite = TRUE)
  expect_equal(getRegistryValue(id = "test", default = NA), NA)
  expect_equal(res <- getRegistryValue(id = "a/b/c", default = NA), NA)
  
  on.exit(setwd(wd_0))
  
})

##------------------------------------------------------------------------------
context("getRegistryValue/where")
##------------------------------------------------------------------------------

test_that("getRegistryValue/where", {

  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  where <- "test"
  container <- initializeOptionContainer(id = where, overwrite = TRUE)
  expect_true(res <- setRegistryValue(id = "a/b/c", value = 10, where = where))
  expect_equal(res <- getRegistryValue(id = "a/b/c", where = where), 10)
  
  where <- structure(list(id = "test"), class = "OptionContext.Test")
  container <- initializeOptionContainer(id = where, overwrite = TRUE)
  expect_true(res <- setRegistryValue(id = "a/b/c", value = 10, where = where))
  expect_equal(res <- getRegistryValue(id = "a/b/c", where = where), 10)
  
  on.exit(setwd(wd_0))
  
})

