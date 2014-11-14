##------------------------------------------------------------------------------
context("getMetaValue/basics")
##------------------------------------------------------------------------------

test_that("getMetaValue/basics", {
  
  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }

  container <- initializeOptionContainer(overwrite = TRUE)
  expect_true(res <- setMetaValue(id = "test", value = TRUE))
  expect_equal(res <- getMetaValue(id = "test"), TRUE)
  
  expect_true(res <- setMetaValue(id = "a/b/c", value = 10))
  expect_is(res <- getMetaValue(id = "a"), "environment")
  expect_is(res <- getMetaValue(id = "a/b"), "environment")
  expect_equal(res <- getMetaValue(id = "a/b/c"), 10)
  
  on.exit(setwd(wd_0))
  
})

##------------------------------------------------------------------------------
context("getMetaValue/default")
##------------------------------------------------------------------------------

test_that("getMetaValue/default", {
  
  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }

  container <- initializeOptionContainer(overwrite = TRUE)
  expect_equal(getMetaValue(id = "test", default = NA), NA)
  expect_equal(res <- getMetaValue(id = "a/b/c", default = NA), NA)
  
  on.exit(setwd(wd_0))
  
})

##------------------------------------------------------------------------------
context("getMetaValue/where")
##------------------------------------------------------------------------------

test_that("getMetaValue/where", {

  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  where <- "test"
  container <- initializeOptionContainer(id = where, overwrite = TRUE)
  expect_true(res <- setMetaValue(id = "a/b/c", value = 10, where = where))
  expect_equal(res <- getMetaValue(id = "a/b/c", where = where), 10)
  
  where <- structure(list(id = "test"), class = "OptionContext.Test")
  container <- initializeOptionContainer(id = where, overwrite = TRUE)
  expect_true(res <- setMetaValue(id = "a/b/c", value = 10, where = where))
  expect_equal(res <- getMetaValue(id = "a/b/c", where = where), 10)
  
  on.exit(setwd(wd_0))
  
})

