##------------------------------------------------------------------------------
context("setProjectOption/basics")
##------------------------------------------------------------------------------

test_that("setProjectOption/basics", {

  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  container <- initializeOptionContainer(overwrite = TRUE)
  expect_true(res <- setProjectOption(id = "test", value = TRUE))
  expect_identical(container$options$test, TRUE)
  expect_equal(res <- getProjectOption(id = "test"), TRUE)
  expect_identical(container$options$test, res)
  
  expect_true(res <- setProjectOption(id = "a/b/c", value = 10))
  expect_equal(res <- getProjectOption(id = "a/b/c"), 10)
  expect_false(res <- setProjectOption(
    id = "a/b/c/d", 
    value = TRUE,
    must_exist = TRUE
  ))
  expect_error(res <- setProjectOption(
    id = "a/b/c/d", 
    value = TRUE,
    must_exist = TRUE,
    strict = 2
  ))
  
  on.exit(setwd(wd_0))
  
})

##------------------------------------------------------------------------------
context("setProjectOption/typed")
##------------------------------------------------------------------------------

test_that("setProjectOption/typed", {
  
  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  ## Strict = 0 //
  expect_true(res <- setProjectOption(
    id = "a/b/c", 
    value = "hello world!",
    typed = TRUE
  ))
  expect_true(res <- setProjectOption(id = "a/b/c", value = 10))
  expect_identical(getProjectOption("a/b/c"), "hello world!")
  
  ## Strict = 1 //
  expect_true(res <- setProjectOption(
    id = "a/b/c", 
    value = "hello world!",
    typed = TRUE,
    strict = 1
  ))
  expect_warning(res <- setProjectOption(id = "a/b/c", value = 10))
  expect_identical(getProjectOption("a/b/c"), "hello world!")
  
  ## Strict = 2 //
  expect_true(res <- setProjectOption(
    id = "a/b/c", 
    value = "hello world!",
    typed = TRUE,
    strict = 2
  ))
  expect_error(setProjectOption(id = "a/b/c", value = 10))
  expect_identical(getProjectOption("a/b/c"), "hello world!")
  
  expect_true(res <- setProjectOption(
    id = "a/b/c", 
    value = "something else"
  ))
  expect_equal(res <- getProjectOption(id = "a/b/c"), "something else")
  
  on.exit(setwd(wd_0))
  
})

##------------------------------------------------------------------------------
context("setProjectOption/numerical names/IDs")
##------------------------------------------------------------------------------

test_that("setProjectOption/numerical names", {
  
  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  container <- initializeOptionContainer(overwrite = TRUE)
  expect_true(setProjectOption(id = "20140101", value = TRUE))
  expect_equal(res <- getProjectOption(id = "20140101"), TRUE)
  
  on.exit(setwd(wd_0))
  
})

##------------------------------------------------------------------------------
context("setProjectOption/gap")
##------------------------------------------------------------------------------

test_that("setProjectOption/gap", {
  
  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  container <- initializeOptionContainer(overwrite = TRUE)
  expect_false(setProjectOption(id = "a/b/c/d", value = TRUE, gap = FALSE))
  expect_error(setProjectOption(id = "a/b/c/d", value = TRUE, 
    gap = FALSE, strict = 2))
  expect_true(setProjectOption(id = "a/b/c/d", value = TRUE))
  expect_equal(res <- getProjectOption(id = "a/b/c/d"), TRUE)
  
  on.exit(setwd(wd_0))
  
})

##------------------------------------------------------------------------------
context("setProjectOption/force")
##------------------------------------------------------------------------------

test_that("setProjectOption/force 1", {
  
  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  container <- initializeOptionContainer(overwrite = TRUE)
  expect_true(setProjectOption(id = "a", value = "hello world!"))
  expect_false(setProjectOption(id = "a/b/c/d", value = TRUE))
  expect_error(setProjectOption(id = "a/b/c/d", value = TRUE, strict = 2))
  expect_true(setProjectOption(id = "a/b/c/d", value = TRUE, force = TRUE))
  expect_equal(res <- getProjectOption(id = "a/b/c/d"), TRUE)
  
  on.exit(setwd(wd_0))
  
})

test_that("setProjectOption/force 2", {
  
  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  container <- initializeOptionContainer(overwrite = TRUE)
  expect_true(setProjectOption(id = "a", value = "hello world!"))
  expect_false(setProjectOption(id = "a/b", value = TRUE))
  expect_error(setProjectOption(id = "a/b", value = TRUE, strict = 2))
  expect_true(setProjectOption(id = "a/b", value = TRUE, force = TRUE))
  expect_equal(res <- getProjectOption(id = "a/b"), TRUE)
  
  on.exit(setwd(wd_0))
   
})

##------------------------------------------------------------------------------
context("setProjectOption/where")
##------------------------------------------------------------------------------

test_that("setProjectOption/where", {

  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  where <- "test"
  container <- initializeOptionContainer(id = where, overwrite = TRUE)
  expect_true(res <- setProjectOption(id = "a/b/c", value = 10, 
    where = where))
  expect_equal(res <- getProjectOption(id = "a/b/c", where = where), 10)
  expect_identical(getOptionContainer(where), container)
  expect_true(exists("a", container$options))
  
  where <- structure(list(id = "test"), class = "OptionContext.Test")
  container <- initializeOptionContainer(id = where, overwrite = TRUE)
  expect_true(res <- setProjectOption(id = "a/b/c", value = 10, 
    where = where))
  expect_equal(res <- getProjectOption(id = "a/b/c", where = where), 10)
  expect_identical(getOptionContainer(where), container)
  expect_true(exists("a", container$options))
  
  on.exit(setwd(wd_0))
  
})

##------------------------------------------------------------------------------
context("setProjectOption/reactive/atomic")
##------------------------------------------------------------------------------

test_that("setProjectOption/reactive/atomic", {

  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  container <- initializeOptionContainer(overwrite = TRUE)
  expect_true(res <- setProjectOption(id = "x_1", value = TRUE, reactive = TRUE))
  expect_equal(res <- getProjectOption(id = "x_1"), TRUE)
  expect_true(res <- setProjectOption(id = "x_2", 
    value = reactiveOption(
      expr = !getProjectOption(id = "x_1")
    ), 
    reactive = TRUE)
  )
  expect_equal(res <- getProjectOption(id = "x_1"), TRUE)
  expect_equal(res <- getProjectOption(id = "x_2"), FALSE)
  
  expect_true(setProjectOption(id = "x_1", value = FALSE))
  expect_equal(res <- getProjectOption(id = "x_2"), TRUE)
  
  on.exit(setwd(wd_0))
  
})

##------------------------------------------------------------------------------
context("setProjectOption/reactive/path")
##------------------------------------------------------------------------------

test_that("setProjectOption/reactive/path", {

  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  container <- initializeOptionContainer(overwrite = TRUE)
  expect_true(res <- setProjectOption(id = "a/test", value = TRUE, reactive = TRUE))
  expect_equal(res <- getProjectOption(id = "a/test"), TRUE)
  expect_true(setProjectOption(id = "b/test", 
    value = reactiveOption(!getProjectOption(id = "a/test")), 
    reactive = TRUE
  ))
  
  expect_equal(getProjectOption(id = "b/test"), FALSE)
  expect_true(setProjectOption(id = "a/test", value = FALSE))
  expect_equal(getProjectOption(id = "a/test"), FALSE)
  expect_equal(getProjectOption(id = "b/test"), TRUE)
  
  on.exit(setwd(wd_0))
  
})
