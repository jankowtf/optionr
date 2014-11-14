##------------------------------------------------------------------------------
context("rmAnywhereOption")
##------------------------------------------------------------------------------

test_that("rmAnywhereOption", {
  
  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }

  container <- initializeOptionContainer(overwrite = TRUE)
  setAnywhereOption(id = "a", value = TRUE)
  expect_true(res <- rmAnywhereOption(id = "a"))
  expect_false(exists("a", container, inherits = FALSE))
  
  setAnywhereOption(id = "a/b/c", value = 10)
  expect_true(rmAnywhereOption(id = "a/b/c"))
  expect_false(exists("c", container$a$b, inherits = FALSE))
  
  on.exit(setwd(wd_0))
  
})

##------------------------------------------------------------------------------
context("rmAnywhereOption/where")
##------------------------------------------------------------------------------

test_that("rmAnywhereOption/where", {

  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  where <- "test"
  container <- initializeOptionContainer(id = where, overwrite = TRUE)
  expect_true(".test" %in% names(options()))
  setAnywhereOption(id = "a/b/c", value = 10, where = where)
  expect_true(rmAnywhereOption(id = "a/b/c", where = where))
  expect_false(exists("c", container$a$b, inherits = FALSE))
  options(".test" = NULL)
  
  where <- structure(list(id = "test"), class = "OptionContext.Test")
  container <- initializeOptionContainer(id = where, overwrite = TRUE)
  expect_true(".test" %in% names(options()))
  setAnywhereOption(id = "a/b/c", value = 10, where = where)
  expect_true(rmAnywhereOption(id = "a/b/c", where = where))
  expect_false(exists("c", container$a$b, inherits = FALSE))
  
  on.exit(setwd(wd_0))
  
})

##------------------------------------------------------------------------------
context("rmAnywhereOption/strict")
##------------------------------------------------------------------------------

test_that("rmAnywhereOption/strict", {

  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  container <- initializeOptionContainer(overwrite = TRUE)
  expect_false(rmAnywhereOption(id = "a"))
  expect_error(rmAnywhereOption(id = "a", strict = 2))
  
  expect_false(rmAnywhereOption(id = "a/b/c"))
  expect_error(rmAnywhereOption(id = "a/b/c", strict = 2))
  
  expect_false(rmAnywhereOption(id = character()))
  expect_error(rmAnywhereOption(id = character(), strict = 2))
    
  on.exit(setwd(wd_0))
  
})
