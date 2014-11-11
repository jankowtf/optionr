##------------------------------------------------------------------------------
context("setFreeOption/basics")
##------------------------------------------------------------------------------

test_that("setFreeOption/basics", {

  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  container <- initializeOptionContainer(overwrite = TRUE)
  expect_true(res <- setFreeOption(id = "test", value = TRUE))
  expect_equal(res <- getFreeOption(id = "test"), TRUE)
  expect_true(res <- setFreeOption(id = "test", value = new.env()))
  expect_true(res <- setFreeOption(id = "test/a", value = TRUE))
  expect_equal(res <- getFreeOption(id = "test/a"), TRUE)
  expect_false(res <- setFreeOption(
    id = "test/b", 
    value = TRUE,
    must_exist = TRUE
  ))
  expect_error(res <- setFreeOption(
    id = "test/b", 
    value = TRUE,
    must_exist = TRUE,
    strict = TRUE
  ))
  
  on.exit(setwd(wd_0))
  
})

##------------------------------------------------------------------------------
context("setFreeOption/typed")
##------------------------------------------------------------------------------

test_that("setFreeOption/typed", {
  
  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  ## Strict = 0 //
  expect_true(res <- setFreeOption(
    id = "test/a", 
    value = "hello world!",
    typed = TRUE
  ))
  expect_true(res <- setFreeOption(id = "test/a", value = 10))
  expect_identical(getFreeOption("test/a"), "hello world!")
  
  ## Strict = 1 //
  expect_true(res <- setFreeOption(
    id = "test/a", 
    value = "hello world!",
    typed = TRUE,
    strict_set = 1
  ))
  expect_warning(res <- setFreeOption(id = "test/a", value = 10))
  expect_identical(getFreeOption("test/a"), "hello world!")
  
  ## Strict = 2 //
  expect_true(res <- setFreeOption(
    id = "test/a", 
    value = "hello world!",
    typed = TRUE,
    strict_set = 2
  ))
  expect_error(setFreeOption(id = "test/a", value = 10))
  expect_identical(getFreeOption("test/a"), "hello world!")
  
  expect_true(res <- setFreeOption(
    id = "test/a", 
    value = "something else"
  ))
  expect_equal(res <- getFreeOption(id = "test/a"), "something else")
  
  on.exit(setwd(wd_0))
  
})

##------------------------------------------------------------------------------
context("setFreeOption/numerical names/IDs")
##------------------------------------------------------------------------------

test_that("setFreeOption/numerical names", {
  
  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  container <- initializeOptionContainer()
  expect_true(setFreeOption(id = "20140101", value = TRUE))
  expect_equal(res <- getFreeOption(id = "20140101"), TRUE)
  
  on.exit(setwd(wd_0))
  
})

##------------------------------------------------------------------------------
context("setFreeOption/gap")
##------------------------------------------------------------------------------

test_that("setFreeOption/gap", {
  
  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  container <- initializeOptionContainer()
  expect_false(setFreeOption(id = "a/b/c/d", value = TRUE))
  expect_error(setFreeOption(id = "a/b/c/d", value = TRUE, strict = TRUE))
  expect_true(setFreeOption(id = "a/b/c/d", value = TRUE, gap = TRUE))
  expect_equal(res <- getFreeOption(id = "a/b/c/d"), TRUE)
  
  on.exit(setwd(wd_0))
  
})

##------------------------------------------------------------------------------
context("setFreeOption/force")
##------------------------------------------------------------------------------

test_that("setFreeOption/force 1", {
  
  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  container <- initializeOptionContainer()
  expect_true(setFreeOption(id = "a", value = "hello world!"))
  expect_false(setFreeOption(id = "a/b/c/d", value = TRUE, gap = TRUE))
  expect_error(setFreeOption(id = "a/b/c/d", value = TRUE, 
     gap = TRUE, strict = TRUE))
  expect_true(setFreeOption(id = "a/b/c/d", value = TRUE, 
     gap = TRUE, force = TRUE))
  expect_equal(res <- getFreeOption(id = "a/b/c/d"), TRUE)
  
  on.exit(setwd(wd_0))
  
})

test_that("setFreeOption/force 2", {
  
  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  container <- initializeOptionContainer()
  expect_true(setFreeOption(id = "a", value = "hello world!"))
  expect_false(setFreeOption(id = "a/b", value = TRUE, gap = TRUE))
  expect_error(setFreeOption(id = "a/b", value = TRUE, 
     gap = TRUE, strict = TRUE))
  expect_true(setFreeOption(id = "a/b", value = TRUE, force = TRUE))
  expect_equal(res <- getFreeOption(id = "a/b"), TRUE)
  
  on.exit(setwd(wd_0))
   
})

##------------------------------------------------------------------------------
context("setFreeOption/where")
##------------------------------------------------------------------------------

test_that("setFreeOption/where", {

  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  where <- "test"
  container <- initializeOptionContainer(id = where, overwrite = TRUE)
  expect_true(res <- setFreeOption(id = "a/b/c", value = 10, 
    where = where, gap = TRUE))
  expect_equal(res <- getFreeOption(id = "a/b/c", where = where), 10)
  expect_identical(getOptionContainer(where), container)
  expect_true(exists("a", container))
  
  where <- structure(list(id = "test"), class = "OptionContext.Test")
  container <- initializeOptionContainer(id = where, overwrite = TRUE)
  expect_true(res <- setFreeOption(id = "a/b/c", value = 10, 
    where = where, gap = TRUE))
  expect_equal(res <- getFreeOption(id = "a/b/c", where = where), 10)
  expect_identical(getOptionContainer(where), container)
  expect_true(exists("a", container))
  
  on.exit(setwd(wd_0))
  
})

##------------------------------------------------------------------------------
context("setFreeOption/reactive/atomic")
##------------------------------------------------------------------------------

test_that("setFreeOption/reactive/atomic", {

  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  container <- initializeOptionContainer()  
  expect_true(res <- setFreeOption(id = "x_1", value = TRUE, reactive = TRUE))
  expect_equal(res <- getFreeOption(id = "x_1"), TRUE)
  expect_true(res <- setFreeOption(id = "x_2", 
    value = reactiveOption(
      expr = !getFreeOption(id = "x_1")
    ), 
    reactive = TRUE)
  )
  expect_equal(res <- getFreeOption(id = "x_1"), TRUE)
  expect_equal(res <- getFreeOption(id = "x_2"), FALSE)
  
  expect_true(setFreeOption(id = "x_1", value = FALSE))
  expect_equal(res <- getFreeOption(id = "x_2"), TRUE)
  
  on.exit(setwd(wd_0))
  
})

##------------------------------------------------------------------------------
context("setFreeOption/reactive/path")
##------------------------------------------------------------------------------

test_that("setFreeOption/reactive/path", {

  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  container <- initializeOptionContainer()
  expect_true(res <- setFreeOption(id = "a/test", value = TRUE, 
    reactive = TRUE, gap = TRUE))
  expect_equal(res <- getFreeOption(id = "a/test"), TRUE)
  expect_true(setFreeOption(id = "b/test", 
    value = reactiveOption(!getFreeOption(id = "a/test")), 
    reactive = TRUE, 
    gap = TRUE
  ))
  
  expect_equal(getFreeOption(id = "b/test"), FALSE)
  expect_true(setFreeOption(id = "a/test", value = FALSE))
  expect_equal(getFreeOption(id = "a/test"), FALSE)
  expect_equal(getFreeOption(id = "b/test"), TRUE)
  
  on.exit(setwd(wd_0))
  
})
