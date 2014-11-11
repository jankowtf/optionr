##------------------------------------------------------------------------------
context("getOptionContainer")
##------------------------------------------------------------------------------

test_that("getOptionContainer", {

  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  res <- ensureOptionContainer(overwrite = TRUE)
  expect_identical(getOptionContainer(), res)
  
  on.exit({
    options(".test.package" = NULL)
    setwd(wd_0)
  })
  
})

##------------------------------------------------------------------------------
context("getOptionContainer/hide")
##------------------------------------------------------------------------------

test_that("getOptionContainer/hide", {

  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  res <- ensureOptionContainer(hidden = FALSE, overwrite = TRUE)
  expect_identical(getOptionContainer(hidden = FALSE), res)
  
  on.exit({
    options("test.package" = NULL)
    setwd(wd_0)
  })
  
})

##------------------------------------------------------------------------------
context("getOptionContainer/interface test")
##------------------------------------------------------------------------------

test_that("getOptionContainer/interface test", {
  
  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  id <- structure(list(id = "test"), class = "OptionContext.Test")
  res <- ensureOptionContainer(id, hidden = FALSE, overwrite = TRUE)
  expect_identical(getOptionContainer(id, hidden = FALSE), res)
  
  on.exit({
    options("test" = NULL)
    setwd(wd_0)
  })
  
})
