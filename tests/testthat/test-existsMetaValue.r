##------------------------------------------------------------------------------
context("existsMetaValue/basics")
##------------------------------------------------------------------------------

test_that("existsMetaValue/basics", {
  
  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }

  container <- initializeOptionContainer(overwrite = TRUE)
  expect_true(setMetaValue(id = "test", value = TRUE))
  expect_true(existsMetaValue(id = "test"))
  expect_false(existsMetaValue(id = "test_1"))
  
  expect_true(setMetaValue(id = "a/b/c", value = 10))
  expect_true(existsMetaValue(id = "a"))
  expect_true(existsMetaValue(id = "a/b"))
  expect_true(existsMetaValue(id = "a/b/c"))
  expect_false(existsMetaValue(id = "a/b/c/d"))
  
  on.exit(setwd(wd_0))
  
})

##------------------------------------------------------------------------------
context("existsMetaValue/where")
##------------------------------------------------------------------------------

test_that("existsMetaValue/where", {

  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  where <- "test"
  container <- initializeOptionContainer(id = where, overwrite = TRUE)
  expect_true(setMetaValue(id = "a/b/c", value = 10, where = where))
  expect_true(existsMetaValue(id = "a/b/c", where = where))
  expect_false(existsMetaValue(id = "a/b/c/d", where = where))
  
  where <- structure(list(id = "test"), class = "OptionContext.Test")
  container <- initializeOptionContainer(id = where, overwrite = TRUE)
  expect_true(setMetaValue(id = "a/b/c", value = 10, where = where))
  expect_true(existsMetaValue(id = "a/b/c", where = where))
  expect_false(existsMetaValue(id = "a/b/c/d", where = where))
  
  on.exit(setwd(wd_0))
})