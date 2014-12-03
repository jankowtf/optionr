##------------------------------------------------------------------------------
context("ensureOptionContainer")
##------------------------------------------------------------------------------

test_that("ensureOptionContainer", {

  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  options(".test.package" = NULL)
  expect_is(res <- ensureOptionContainer(), "environment")
  expect_true(".test.package" %in% names(options()))
  expect_is(res <- ensureOptionContainer(overwrite = TRUE), "environment")
  expect_identical(getOption(".test.package"), res)
  
  on.exit({
    options(".test.package" = NULL)
    setwd(wd_0)
  })
  
})

##------------------------------------------------------------------------------
context("ensureOptionContainer/check")
##------------------------------------------------------------------------------

test_that("ensureOptionContainer/check", {

  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  options(".test.package" = NULL)
  expect_is(res <- ensureOptionContainer(), "environment")
  expect_error(ensureOptionContainer(check = TRUE))
  
  on.exit({
    options(".test.package" = NULL)
    setwd(wd_0)
  })
  
})

##------------------------------------------------------------------------------
context("ensureOptionContainer/hidden")
##------------------------------------------------------------------------------

test_that("ensureOptionContainer/hidden", {

  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  options("test.package" = NULL)
  expect_is(res <- ensureOptionContainer(hidden = FALSE), "environment")
  expect_true("test.package" %in% names(options()))
  expect_identical(getOption("test.package"), res)
  
  on.exit({
    options("test.package" = NULL)
    setwd(wd_0)
  })
  
})

##------------------------------------------------------------------------------
context("ensureOptionContainer/interface test")
##------------------------------------------------------------------------------

test_that("ensureOptionContainer/interface test", {
  
  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  id <- structure(list(id = "test"), class = "OptionContext.Test")
  options("test" = NULL)
  expect_is(res <- ensureOptionContainer(id, hidden = FALSE), "environment")
  expect_true("test" %in% names(options()))
  expect_identical(getOption(id$id), res)  
  
  on.exit({
    options("test" = NULL)
    setwd(wd_0)
  })
  
})

##------------------------------------------------------------------------------
context("ensureOptionContainer/sub ID")
##------------------------------------------------------------------------------

test_that("ensureOptionContainer/sub ID", {

  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  options(".test.package" = NULL)
  expect_is(res <- ensureOptionContainer(sub_id = "a"), "environment")
  expect_true("a" %in% ls(res))
  expect_error(ensureOptionContainer(check = TRUE))
  
  on.exit({
    options(".test.package" = NULL)
    setwd(wd_0)
  })
  
})