##------------------------------------------------------------------------------
context("rmRegistryValue")
##------------------------------------------------------------------------------

test_that("rmRegistryValue", {
  
  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }

  container <- initializeOptionContainer(overwrite = TRUE)
  setRegistryValue(id = "a", value = TRUE)
  expect_true(rmRegistryValue(id = "a"))
  expect_false(exists("a", container$.registry, inherits = FALSE))
  
  setRegistryValue(id = "a/b/c", value = 10, gap = TRUE)
  expect_true(rmRegistryValue(id = "a/b/c"))
  expect_false(exists("c", container$.registry$a$b, inherits = FALSE))
  
  on.exit(setwd(wd_0))
  
})

##------------------------------------------------------------------------------
context("rmRegistryValue/where")
##------------------------------------------------------------------------------

test_that("rmRegistryValue/where", {

  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  where <- "test"
  container <- initializeOptionContainer(id = where, overwrite = TRUE)
  expect_true(".test" %in% names(options()))
  setRegistryValue(id = "a/b/c", value = 10, where = where, gap = TRUE)
  expect_true(rmRegistryValue(id = "a/b/c", where = where))
  expect_false(exists("c", container$.registry$a$b, inherits = FALSE))
  options(".test" = NULL)
  
  where <- structure(list(id = "test"), class = "OptionContext.Test")
  container <- initializeOptionContainer(id = where, overwrite = TRUE)
  expect_true(".test" %in% names(options()))
  setRegistryValue(id = "a/b/c", value = 10, where = where, gap = TRUE)
  expect_true(rmRegistryValue(id = "a/b/c", where = where))
  expect_false(exists("c", container$.registry$a$b, inherits = FALSE))
  
  on.exit(setwd(wd_0))
  
})

##------------------------------------------------------------------------------
context("rmRegistryValue/strict")
##------------------------------------------------------------------------------

test_that("rmRegistryValue/strict", {

  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  container <- initializeOptionContainer(overwrite = TRUE)
  expect_false(rmRegistryValue(id = "a"))
  expect_error(rmRegistryValue(id = "a", strict = TRUE))
  
  expect_false(rmRegistryValue(id = "a/b/c"))
  expect_error(rmRegistryValue(id = "a/b/c", strict = TRUE))
  
  expect_false(rmRegistryValue(id = character()))
  expect_error(rmRegistryValue(id = character(), strict = TRUE))
    
  on.exit(setwd(wd_0))
  
})
