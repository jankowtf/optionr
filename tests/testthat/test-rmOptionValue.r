##------------------------------------------------------------------------------
context("rmProjectOption")
##------------------------------------------------------------------------------

test_that("rmProjectOption", {
  
  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }

  container <- initializeOptionContainer(overwrite = TRUE)
  setProjectOption(id = "a", value = TRUE)
  expect_true(rmProjectOption(id = "a"))
  expect_false(exists("a", container$options, inherits = FALSE))
  
  setProjectOption(id = "a/b/c", value = 10)
  expect_true(rmProjectOption(id = "a/b/c"))
  expect_false(exists("c", container$options$a$b, inherits = FALSE))
  
  on.exit(setwd(wd_0))
  
})

##------------------------------------------------------------------------------
context("rmProjectOption/where")
##------------------------------------------------------------------------------

test_that("rmProjectOption/where", {

  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  where <- "test"
  container <- initializeOptionContainer(id = where, overwrite = TRUE)
  expect_true(".test" %in% names(options()))
  setProjectOption(id = "a/b/c", value = 10, where = where)
  expect_true(rmProjectOption(id = "a/b/c", where = where))
  expect_false(exists("c", container$options$a$b, inherits = FALSE))
  options(".test" = NULL)
  
  where <- structure(list(id = "test"), class = "OptionContext.Test")
  container <- initializeOptionContainer(id = where, overwrite = TRUE)
  expect_true(".test" %in% names(options()))
  setProjectOption(id = "a/b/c", value = 10, where = where)
  expect_true(rmProjectOption(id = "a/b/c", where = where))
  expect_false(exists("c", container$options$a$b, inherits = FALSE))
  
  on.exit(setwd(wd_0))
  
})

##------------------------------------------------------------------------------
context("rmProjectOption/strict")
##------------------------------------------------------------------------------

test_that("rmProjectOption/strict", {

  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  container <- initializeOptionContainer(overwrite = TRUE)
  expect_false(rmProjectOption(id = "a"))
  expect_error(rmProjectOption(id = "a", strict = 2))
  
  expect_false(rmProjectOption(id = "a/b/c"))
  expect_error(rmProjectOption(id = "a/b/c", strict = 2))
  
  expect_false(rmProjectOption(id = character()))
  expect_error(rmProjectOption(id = character(), strict = 2))
    
  on.exit(setwd(wd_0))
  
})
