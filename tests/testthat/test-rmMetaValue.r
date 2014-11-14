##------------------------------------------------------------------------------
context("rmMetaValue")
##------------------------------------------------------------------------------

test_that("rmMetaValue", {
  
  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }

  container <- initializeOptionContainer(overwrite = TRUE)
  setMetaValue(id = "a", value = TRUE)
  expect_true(rmMetaValue(id = "a"))
  expect_false(exists("a", container$.meta, inherits = FALSE))
  
  setMetaValue(id = "a/b/c", value = 10)
  expect_true(rmMetaValue(id = "a/b/c"))
  expect_false(exists("c", container$.meta$a$b, inherits = FALSE))
  
  on.exit(setwd(wd_0))
  
})

##------------------------------------------------------------------------------
context("rmMetaValue/where")
##------------------------------------------------------------------------------

test_that("rmMetaValue/where", {

  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  where <- "test"
  container <- initializeOptionContainer(id = where, overwrite = TRUE)
  expect_true(".test" %in% names(options()))
  setMetaValue(id = "a/b/c", value = 10, where = where)
  expect_true(rmMetaValue(id = "a/b/c", where = where))
  expect_false(exists("c", container$.meta$a$b, inherits = FALSE))
  options(".test" = NULL)
  
  where <- structure(list(id = "test"), class = "OptionContext.Test")
  container <- initializeOptionContainer(id = where, overwrite = TRUE)
  expect_true(".test" %in% names(options()))
  setMetaValue(id = "a/b/c", value = 10, where = where)
  expect_true(rmMetaValue(id = "a/b/c", where = where))
  expect_false(exists("c", container$.meta$a$b, inherits = FALSE))
  
  on.exit(setwd(wd_0))
  
})

##------------------------------------------------------------------------------
context("rmMetaValue/strict")
##------------------------------------------------------------------------------

test_that("rmMetaValue/strict", {

  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  container <- initializeOptionContainer(overwrite = TRUE)
  expect_false(rmMetaValue(id = "a"))
  expect_error(rmMetaValue(id = "a", strict = 2))
  
  expect_false(rmMetaValue(id = "a/b/c"))
  expect_error(rmMetaValue(id = "a/b/c", strict = 2))
  
  expect_false(rmMetaValue(id = character()))
  expect_error(rmMetaValue(id = character(), strict = 2))
    
  on.exit(setwd(wd_0))
  
})
