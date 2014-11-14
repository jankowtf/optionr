##------------------------------------------------------------------------------
context("setRegistryValue/basics")
##------------------------------------------------------------------------------

test_that("setRegistryValue/basics", {

  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  container <- initializeOptionContainer(overwrite = TRUE)
  expect_true(res <- setRegistryValue(id = "test", value = TRUE))
  expect_identical(container$.registry$test, TRUE)
  expect_equal(res <- getRegistryValue(id = "test"), TRUE)
  expect_identical(container$.registry$test, res)
  
  expect_true(res <- setRegistryValue(id = "a/b/c", value = 10))
  expect_equal(res <- getRegistryValue(id = "a/b/c"), 10)
  expect_false(res <- setRegistryValue(
    id = "a/b/c/d", 
    value = TRUE,
    must_exist = TRUE
  ))
  expect_error(res <- setRegistryValue(
    id = "a/b/c/d", 
    value = TRUE,
    must_exist = TRUE,
    strict = 2
  ))
  
  on.exit(setwd(wd_0))
  
})

##------------------------------------------------------------------------------
context("setRegistryValue/typed")
##------------------------------------------------------------------------------

test_that("setRegistryValue/typed", {
  
  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  ## Strict = 0 //
  expect_true(res <- setRegistryValue(
    id = "a/b/c", 
    value = "hello world!",
    typed = TRUE
  ))
  expect_true(res <- setRegistryValue(id = "a/b/c", value = 10))
  expect_identical(getRegistryValue("a/b/c"), "hello world!")
  
  ## Strict = 1 //
  expect_true(res <- setRegistryValue(
    id = "a/b/c", 
    value = "hello world!",
    typed = TRUE,
    strict = 1
  ))
  expect_warning(res <- setRegistryValue(id = "a/b/c", value = 10))
  expect_identical(getRegistryValue("a/b/c"), "hello world!")
  
  ## Strict = 2 //
  expect_true(res <- setRegistryValue(
    id = "a/b/c", 
    value = "hello world!",
    typed = TRUE,
    strict = 2
  ))
  expect_error(setRegistryValue(id = "a/b/c", value = 10))
  expect_identical(getRegistryValue("a/b/c"), "hello world!")
  
  expect_true(res <- setRegistryValue(
    id = "a/b/c", 
    value = "something else"
  ))
  expect_equal(res <- getRegistryValue(id = "a/b/c"), "something else")
  
  on.exit(setwd(wd_0))
  
})

##------------------------------------------------------------------------------
context("setRegistryValue/numerical names/IDs")
##------------------------------------------------------------------------------

test_that("setRegistryValue/numerical names", {
  
  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  container <- initializeOptionContainer(overwrite = TRUE)
  expect_true(setRegistryValue(id = "20140101", value = TRUE))
  expect_equal(res <- getRegistryValue(id = "20140101"), TRUE)
  
  on.exit(setwd(wd_0))
  
})

##------------------------------------------------------------------------------
context("setRegistryValue/gap")
##------------------------------------------------------------------------------

test_that("setRegistryValue/gap", {
  
  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  container <- initializeOptionContainer(overwrite = TRUE)
  expect_false(setRegistryValue(id = "a/b/c/d", value = TRUE, gap = FALSE))
  expect_error(setRegistryValue(id = "a/b/c/d", value = TRUE, 
    gap = FALSE, strict = 2))
  expect_true(setRegistryValue(id = "a/b/c/d", value = TRUE))
  expect_equal(res <- getRegistryValue(id = "a/b/c/d"), TRUE)
  
  on.exit(setwd(wd_0))
  
})

##------------------------------------------------------------------------------
context("setRegistryValue/force")
##------------------------------------------------------------------------------

test_that("setRegistryValue/force 1", {
  
  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  container <- initializeOptionContainer(overwrite = TRUE)
  expect_true(setRegistryValue(id = "a", value = "hello world!"))
  expect_false(setRegistryValue(id = "a/b/c/d", value = TRUE))
  expect_error(setRegistryValue(id = "a/b/c/d", value = TRUE, strict = 2))
  expect_true(setRegistryValue(id = "a/b/c/d", value = TRUE, force = TRUE))
  expect_equal(res <- getRegistryValue(id = "a/b/c/d"), TRUE)
  
  on.exit(setwd(wd_0))
  
})

test_that("setRegistryValue/force 2", {
  
  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  container <- initializeOptionContainer(overwrite = TRUE)
  expect_true(setRegistryValue(id = "a", value = "hello world!"))
  expect_false(setRegistryValue(id = "a/b", value = TRUE))
  expect_error(setRegistryValue(id = "a/b", value = TRUE, strict = 2))
  expect_true(setRegistryValue(id = "a/b", value = TRUE, force = TRUE))
  expect_equal(res <- getRegistryValue(id = "a/b"), TRUE)
  
  on.exit(setwd(wd_0))
   
})

##------------------------------------------------------------------------------
context("setRegistryValue/where")
##------------------------------------------------------------------------------

test_that("setRegistryValue/where", {

  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  where <- "test"
  container <- initializeOptionContainer(id = where, overwrite = TRUE)
  expect_true(res <- setRegistryValue(id = "a/b/c", value = 10, 
    where = where))
  expect_equal(res <- getRegistryValue(id = "a/b/c", where = where), 10)
  expect_identical(getOptionContainer(where), container)
  expect_true(exists("a", container$.registry))
  
  where <- structure(list(id = "test"), class = "OptionContext.Test")
  container <- initializeOptionContainer(id = where, overwrite = TRUE)
  expect_true(res <- setRegistryValue(id = "a/b/c", value = 10, 
    where = where))
  expect_equal(res <- getRegistryValue(id = "a/b/c", where = where), 10)
  expect_identical(getOptionContainer(where), container)
  expect_true(exists("a", container$.registry))
  
  on.exit(setwd(wd_0))
  
})

##------------------------------------------------------------------------------
context("setRegistryValue/reactive/atomic")
##------------------------------------------------------------------------------

test_that("setRegistryValue/reactive/atomic", {

  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  container <- initializeOptionContainer(overwrite = TRUE)
  expect_true(res <- setRegistryValue(id = "x_1", value = TRUE, reactive = TRUE))
  expect_equal(res <- getRegistryValue(id = "x_1"), TRUE)
  expect_true(res <- setRegistryValue(id = "x_2", 
    value = reactiveOption(
      expr = !getRegistryValue(id = "x_1")
    ), 
    reactive = TRUE)
  )
  expect_equal(res <- getRegistryValue(id = "x_1"), TRUE)
  expect_equal(res <- getRegistryValue(id = "x_2"), FALSE)
  
  expect_true(setRegistryValue(id = "x_1", value = FALSE))
  expect_equal(res <- getRegistryValue(id = "x_2"), TRUE)
  
  on.exit(setwd(wd_0))
  
})

##------------------------------------------------------------------------------
context("setRegistryValue/reactive/path")
##------------------------------------------------------------------------------

test_that("setRegistryValue/reactive/path", {

  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  container <- initializeOptionContainer(overwrite = TRUE)
  expect_true(res <- setRegistryValue(id = "a/test", value = TRUE, 
    reactive = TRUE))
  expect_equal(res <- getRegistryValue(id = "a/test"), TRUE)
  expect_true(setRegistryValue(id = "b/test", 
    value = reactiveOption(!getRegistryValue(id = "a/test")), 
    reactive = TRUE
  ))
  
  expect_equal(getRegistryValue(id = "b/test"), FALSE)
  expect_true(setRegistryValue(id = "a/test", value = FALSE))
  expect_equal(getRegistryValue(id = "a/test"), FALSE)
  expect_equal(getRegistryValue(id = "b/test"), TRUE)
  
  on.exit(setwd(wd_0))
  
})
