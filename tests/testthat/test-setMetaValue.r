##------------------------------------------------------------------------------
context("setMetaValue/basics")
##------------------------------------------------------------------------------

test_that("setMetaValue/basics", {

  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  container <- initializeOptionContainer(overwrite = TRUE)
  expect_true(res <- setMetaValue(id = "test", value = TRUE))
  expect_identical(container$.meta$test, TRUE)
  expect_equal(res <- getMetaValue(id = "test"), TRUE)
  expect_identical(container$.meta$test, res)
  
  expect_true(res <- setMetaValue(id = "a/b/c", value = 10, gap = TRUE))
  expect_equal(res <- getMetaValue(id = "a/b/c"), 10)
  expect_false(res <- setMetaValue(
    id = "a/b/c/d", 
    value = TRUE,
    must_exist = TRUE
  ))
  expect_error(res <- setMetaValue(
    id = "a/b/c/d", 
    value = TRUE,
    must_exist = TRUE,
    strict = TRUE
  ))
  
  on.exit(setwd(wd_0))
  
})

##------------------------------------------------------------------------------
context("setMetaValue/typed")
##------------------------------------------------------------------------------

test_that("setMetaValue/typed", {
  
  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  ## Strict = 0 //
  expect_true(res <- setMetaValue(
    id = "a/b/c", 
    value = "hello world!",
    typed = TRUE
  ))
  expect_true(res <- setMetaValue(id = "a/b/c", value = 10))
  expect_identical(getMetaValue("a/b/c"), "hello world!")
  
  ## Strict = 1 //
  expect_true(res <- setMetaValue(
    id = "a/b/c", 
    value = "hello world!",
    typed = TRUE,
    strict_set = 1
  ))
  expect_warning(res <- setMetaValue(id = "a/b/c", value = 10))
  expect_identical(getMetaValue("a/b/c"), "hello world!")
  
  ## Strict = 2 //
  expect_true(res <- setMetaValue(
    id = "a/b/c", 
    value = "hello world!",
    typed = TRUE,
    strict_set = 2
  ))
  expect_error(setMetaValue(id = "a/b/c", value = 10))
  expect_identical(getMetaValue("a/b/c"), "hello world!")
  
  expect_true(res <- setMetaValue(
    id = "a/b/c", 
    value = "something else"
  ))
  expect_equal(res <- getMetaValue(id = "a/b/c"), "something else")
  
  on.exit(setwd(wd_0))
  
})

##------------------------------------------------------------------------------
context("setMetaValue/numerical names/IDs")
##------------------------------------------------------------------------------

test_that("setMetaValue/numerical names", {
  
  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  container <- initializeOptionContainer(overwrite = TRUE)
  expect_true(setMetaValue(id = "20140101", value = TRUE))
  expect_equal(res <- getMetaValue(id = "20140101"), TRUE)
  
  on.exit(setwd(wd_0))
  
})

##------------------------------------------------------------------------------
context("setMetaValue/gap")
##------------------------------------------------------------------------------

test_that("setMetaValue/gap", {
  
  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  container <- initializeOptionContainer(overwrite = TRUE)
  expect_false(setMetaValue(id = "a/b/c/d", value = TRUE))
  expect_error(setMetaValue(id = "a/b/c/d", value = TRUE, strict = TRUE))
  expect_true(setMetaValue(id = "a/b/c/d", value = TRUE, gap = TRUE))
  expect_equal(res <- getMetaValue(id = "a/b/c/d"), TRUE)
  
  on.exit(setwd(wd_0))
  
})

##------------------------------------------------------------------------------
context("setMetaValue/force")
##------------------------------------------------------------------------------

test_that("setMetaValue/force 1", {
  
  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  container <- initializeOptionContainer(overwrite = TRUE)
  expect_true(setMetaValue(id = "a", value = "hello world!"))
  expect_false(setMetaValue(id = "a/b/c/d", value = TRUE, gap = TRUE))
  expect_error(setMetaValue(id = "a/b/c/d", value = TRUE, 
     gap = TRUE, strict = TRUE))
  expect_true(setMetaValue(id = "a/b/c/d", value = TRUE, 
     gap = TRUE, force = TRUE))
  expect_equal(res <- getMetaValue(id = "a/b/c/d"), TRUE)
  
  on.exit(setwd(wd_0))
  
})

test_that("setMetaValue/force 2", {
  
  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  container <- initializeOptionContainer(overwrite = TRUE)
  expect_true(setMetaValue(id = "a", value = "hello world!"))
  expect_false(setMetaValue(id = "a/b", value = TRUE, gap = TRUE))
  expect_error(setMetaValue(id = "a/b", value = TRUE, 
     gap = TRUE, strict = TRUE))
  expect_true(setMetaValue(id = "a/b", value = TRUE, force = TRUE))
  expect_equal(res <- getMetaValue(id = "a/b"), TRUE)
  
  on.exit(setwd(wd_0))
   
})

##------------------------------------------------------------------------------
context("setMetaValue/where")
##------------------------------------------------------------------------------

test_that("setMetaValue/where", {

  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  where <- "test"
  container <- initializeOptionContainer(id = where, overwrite = TRUE)
  expect_true(res <- setMetaValue(id = "a/b/c", value = 10, 
    where = where, gap = TRUE))
  expect_equal(res <- getMetaValue(id = "a/b/c", where = where), 10)
  expect_identical(getOptionContainer(where), container)
  expect_true(exists("a", container$.meta))
  
  where <- structure(list(id = "test"), class = "OptionContext.Test")
  container <- initializeOptionContainer(id = where, overwrite = TRUE)
  expect_true(res <- setMetaValue(id = "a/b/c", value = 10, 
    where = where, gap = TRUE))
  expect_equal(res <- getMetaValue(id = "a/b/c", where = where), 10)
  expect_identical(getOptionContainer(where), container)
  expect_true(exists("a", container$.meta))
  
  on.exit(setwd(wd_0))
  
})

##------------------------------------------------------------------------------
context("setMetaValue/reactive/atomic")
##------------------------------------------------------------------------------

test_that("setMetaValue/reactive/atomic", {

  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  container <- initializeOptionContainer(overwrite = TRUE)
  expect_true(res <- setMetaValue(id = "x_1", value = TRUE, reactive = TRUE))
  expect_equal(res <- getMetaValue(id = "x_1"), TRUE)
  expect_true(res <- setMetaValue(id = "x_2", 
    value = reactiveOption(
      expr = !getMetaValue(id = "x_1")
    ), 
    reactive = TRUE)
  )
  expect_equal(res <- getMetaValue(id = "x_1"), TRUE)
  expect_equal(res <- getMetaValue(id = "x_2"), FALSE)
  
  expect_true(setMetaValue(id = "x_1", value = FALSE))
  expect_equal(res <- getMetaValue(id = "x_2"), TRUE)
  
  on.exit(setwd(wd_0))
  
})

##------------------------------------------------------------------------------
context("setMetaValue/reactive/path")
##------------------------------------------------------------------------------

test_that("setMetaValue/reactive/path", {

  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  container <- initializeOptionContainer(overwrite = TRUE)
  expect_true(res <- setMetaValue(id = "a/test", value = TRUE, 
    reactive = TRUE, gap = TRUE))
  expect_equal(res <- getMetaValue(id = "a/test"), TRUE)
  expect_true(setMetaValue(id = "b/test", 
    value = reactiveOption(!getMetaValue(id = "a/test")), 
    reactive = TRUE, 
    gap = TRUE
  ))
  
  expect_equal(getMetaValue(id = "b/test"), FALSE)
  expect_true(setMetaValue(id = "a/test", value = FALSE))
  expect_equal(getMetaValue(id = "a/test"), FALSE)
  expect_equal(getMetaValue(id = "b/test"), TRUE)
  
  on.exit(setwd(wd_0))
  
})
