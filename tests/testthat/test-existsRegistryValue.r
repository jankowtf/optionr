##------------------------------------------------------------------------------
context("existsRegistryValue/basics")
##------------------------------------------------------------------------------

test_that("existsRegistryValue/basics", {
  
  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }

  container <- initializeOptionContainer(overwrite = TRUE)
  expect_true(setRegistryValue(id = "test", value = TRUE))
  expect_true(existsRegistryValue(id = "test"))
  expect_false(existsRegistryValue(id = "test_1"))
  
  expect_true(setRegistryValue(id = "a/b/c", value = 10))
  expect_true(existsRegistryValue(id = "a"))
  expect_true(existsRegistryValue(id = "a/b"))
  expect_true(existsRegistryValue(id = "a/b/c"))
  expect_false(existsRegistryValue(id = "a/b/c/d"))
  
  on.exit(setwd(wd_0))
  
})

##------------------------------------------------------------------------------
context("existsRegistryValue/where")
##------------------------------------------------------------------------------

test_that("existsRegistryValue/where", {

  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  where <- "test"
  container <- initializeOptionContainer(id = where, overwrite = TRUE)
  expect_true(setRegistryValue(id = "a/b/c", value = 10, where = where))
  expect_true(existsRegistryValue(id = "a/b/c", where = where))
  expect_false(existsRegistryValue(id = "a/b/c/d", where = where))
  
  where <- structure(list(id = "test"), class = "OptionContext.Test")
  container <- initializeOptionContainer(id = where, overwrite = TRUE)
  expect_true(setRegistryValue(id = "a/b/c", value = 10, where = where))
  expect_true(existsRegistryValue(id = "a/b/c", where = where))
  expect_false(existsRegistryValue(id = "a/b/c/d", where = where))
  
  on.exit(setwd(wd_0))
})