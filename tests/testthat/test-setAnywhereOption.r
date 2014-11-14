##------------------------------------------------------------------------------
context("setAnywhereOption/basics")
##------------------------------------------------------------------------------

test_that("setAnywhereOption/basics", {

  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  container <- initializeOptionContainer(overwrite = TRUE)
  expect_true(res <- setAnywhereOption(id = "test", value = TRUE))
  expect_equal(res <- getAnywhereOption(id = "test"), TRUE)
  expect_true(res <- setAnywhereOption(id = "test", value = new.env()))
  expect_true(res <- setAnywhereOption(id = "test/a", value = TRUE))
  expect_equal(res <- getAnywhereOption(id = "test/a"), TRUE)
  expect_false(res <- setAnywhereOption(
    id = "test/b", 
    value = TRUE,
    must_exist = TRUE,
    gap = FALSE
  ))
  expect_error(res <- setAnywhereOption(
    id = "test/b", 
    value = TRUE,
    must_exist = TRUE,
    strict = 2
  ))
  
  on.exit(setwd(wd_0))
  
})

##------------------------------------------------------------------------------
context("setAnywhereOption/typed")
##------------------------------------------------------------------------------

test_that("setAnywhereOption/typed", {
  
  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  container <- initializeOptionContainer(overwrite = TRUE)
  
  ## Strict = 0 //
  expect_true(res <- setAnywhereOption(
    id = "test/a", 
    value = "hello world!",
    typed = TRUE
  ))
  expect_true(setAnywhereOption(id = "test/a", value = 10))
  expect_identical(getAnywhereOption("test/a"), "hello world!")
  
  ## Strict = 1 //
  expect_true(res <- setAnywhereOption(
    id = "test/a", 
    value = "hello world!",
    typed = TRUE,
    strict = 1
  ))
  expect_warning(res <- setAnywhereOption(id = "test/a", value = 10))
  expect_identical(getAnywhereOption("test/a"), "hello world!")
  
  ## Strict = 2 //
  expect_true(res <- setAnywhereOption(
    id = "test/a", 
    value = "hello world!",
    typed = TRUE,
    strict = 2
  ))
  expect_error(setAnywhereOption(id = "test/a", value = 10))
  expect_identical(getAnywhereOption("test/a"), "hello world!")
  
  expect_true(res <- setAnywhereOption(
    id = "test/a", 
    value = "something else"
  ))
  expect_equal(res <- getAnywhereOption(id = "test/a"), "something else")
  
  on.exit(setwd(wd_0))
  
})

##------------------------------------------------------------------------------
context("setAnywhereOption/numerical names/IDs")
##------------------------------------------------------------------------------

test_that("setAnywhereOption/numerical names", {
  
  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  container <- initializeOptionContainer(overwrite = TRUE)
  expect_true(setAnywhereOption(id = "20140101", value = TRUE))
  expect_equal(res <- getAnywhereOption(id = "20140101"), TRUE)
  
  on.exit(setwd(wd_0))
  
})

##------------------------------------------------------------------------------
context("setAnywhereOption/gap")
##------------------------------------------------------------------------------

test_that("setAnywhereOption/gap", {
  
  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  container <- initializeOptionContainer(overwrite = TRUE)
  expect_false(setAnywhereOption(id = "a/b/c/d", value = TRUE, 
                                 gap = FALSE))
  expect_error(setAnywhereOption(id = "a/b/c/d", value = TRUE, 
    gap = FALSE, strict = 2))
  expect_true(setAnywhereOption(id = "a/b/c/d", value = TRUE))
  expect_equal(res <- getAnywhereOption(id = "a/b/c/d"), TRUE)
  
  on.exit(setwd(wd_0))
  
})

##------------------------------------------------------------------------------
context("setAnywhereOption/force")
##------------------------------------------------------------------------------

test_that("setAnywhereOption/force leaf to branch 1", {
  
  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  container <- initializeOptionContainer(overwrite = TRUE)
  expect_true(setAnywhereOption(id = "a", value = "hello world!"))
  expect_false(setAnywhereOption(id = "a/b/c/d", value = TRUE))
  expect_error(setAnywhereOption(id = "a/b/c/d", value = TRUE, strict = 2))
  expect_true(setAnywhereOption(id = "a/b/c/d", value = TRUE, force = TRUE))
  expect_equal(getAnywhereOption(id = "a/b/c/d"), TRUE)
  
  on.exit(setwd(wd_0))
  
})

test_that("setAnywhereOption/force leaf to branch 2", {
  
  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  container <- initializeOptionContainer(overwrite = TRUE)
  expect_true(setAnywhereOption(id = "a", value = "hello world!"))
  expect_false(setAnywhereOption(id = "a/b", value = TRUE))
  expect_error(setAnywhereOption(id = "a/b", value = TRUE, strict = 2))
  expect_true(setAnywhereOption(id = "a/b", value = TRUE, force = TRUE))
  expect_equal(getAnywhereOption(id = "a/b"), TRUE)
  
  on.exit(setwd(wd_0))
   
})

test_that("setAnywhereOption/force branch to leaf ", {
  
  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  container <- initializeOptionContainer(overwrite = TRUE)
  expect_true(setAnywhereOption(id = "a/b", value = "hello world!"))
  expect_false(setAnywhereOption(id = "a", value = TRUE))
  expect_error(setAnywhereOption(id = "a", value = TRUE, strict = 2))
  expect_true(setAnywhereOption(id = "a", value = TRUE, force = TRUE))
  expect_equal(getAnywhereOption(id = "a"), TRUE)
  expect_equal(getAnywhereOption(id = "a/b"), NULL)
  
  on.exit(setwd(wd_0))
   
})

##------------------------------------------------------------------------------
context("setAnywhereOption/where")
##------------------------------------------------------------------------------

test_that("setAnywhereOption/where", {

  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  where <- "test"
  container <- initializeOptionContainer(id = where, overwrite = TRUE)
  expect_true(res <- setAnywhereOption(id = "a/b/c", value = 10, 
    where = where))
  expect_equal(res <- getAnywhereOption(id = "a/b/c", where = where), 10)
  expect_identical(getOptionContainer(where), container)
  expect_true(exists("a", container))
  
  where <- structure(list(id = "test"), class = "OptionContext.Test")
  container <- initializeOptionContainer(id = where, overwrite = TRUE)
  expect_true(res <- setAnywhereOption(id = "a/b/c", value = 10, 
    where = where))
  expect_equal(res <- getAnywhereOption(id = "a/b/c", where = where), 10)
  expect_identical(getOptionContainer(where), container)
  expect_true(exists("a", container))
  
  on.exit(setwd(wd_0))
  
})

##------------------------------------------------------------------------------
context("setAnywhereOption/reactive/atomic")
##------------------------------------------------------------------------------

test_that("setAnywhereOption/reactive/atomic", {

  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  container <- initializeOptionContainer(overwrite = TRUE)  
  expect_true(setAnywhereOption(id = "x_1", value = TRUE, reactive = TRUE))
  expect_equal(getAnywhereOption(id = "x_1"), TRUE)
  expect_true(res <- setAnywhereOption(id = "x_2", 
    value = reactr::reactiveExpression(
      !getAnywhereOption(id = "x_1")
    ), 
    reactive = TRUE)
  )
#   expect_true(res <- setAnywhereOption(id = "x_2", 
#     value = reactiveOption(
#       !getAnywhereOption(id = "x_1")
#     ), 
#     reactive = TRUE)
#   )
  expect_equal(getAnywhereOption(id = "x_1"), TRUE)
  expect_equal(getAnywhereOption(id = "x_2"), FALSE)
  
  expect_true(setAnywhereOption(id = "x_1", value = FALSE))
  expect_equal(res <- getAnywhereOption(id = "x_2"), TRUE)
  
  on.exit(setwd(wd_0))
  
})

##------------------------------------------------------------------------------
context("setAnywhereOption/reactive/path")
##------------------------------------------------------------------------------

test_that("setAnywhereOption/reactive/path", {

  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  container <- initializeOptionContainer(overwrite = TRUE)
  expect_true(res <- setAnywhereOption(id = "a/test", value = TRUE, 
    reactive = TRUE))
  expect_equal(res <- getAnywhereOption(id = "a/test"), TRUE)
  expect_true(setAnywhereOption(id = "b/test", 
    value = reactiveOption(!getAnywhereOption(id = "a/test")), 
    reactive = TRUE
  ))
  
  expect_equal(getAnywhereOption(id = "b/test"), FALSE)
  expect_true(setAnywhereOption(id = "a/test", value = FALSE))
  expect_equal(getAnywhereOption(id = "a/test"), FALSE)
  expect_equal(getAnywhereOption(id = "b/test"), TRUE)
  
  on.exit(setwd(wd_0))
  
})
