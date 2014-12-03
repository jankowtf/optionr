##------------------------------------------------------------------------------
context("initializeOptionContainer/missing id")
##------------------------------------------------------------------------------

test_that("initializeOptionContainer/missing id", {
  
  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  options(".test.package" = NULL)
  expect_is(initializeOptionContainer(), "environment")
  expect_is(getOption(".test.package")$options, "environment")  
  expect_is(getOption(".test.package")$.meta, "environment")  
  expect_is(getOption(".test.package")$.registry, "environment")  
  options(".test.package" = NULL)
  
  options("test.package" = NULL)
  expect_is(initializeOptionContainer(hidden = FALSE), "environment")
  expect_is(getOption("test.package")$options, "environment")  
  expect_is(getOption("test.package")$.meta, "environment")  
  expect_is(getOption("test.package")$.registry, "environment")  
  options("test.package" = NULL)
  
  on.exit(setwd(wd_0))
  
})

##------------------------------------------------------------------------------
context("initializeOptionContainer/id")
##------------------------------------------------------------------------------

test_that("initializeOptionContainer/id", {
  
  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  id <- "test"
  options(".test" = NULL)
  expect_is(initializeOptionContainer(id), "environment")
  expect_is(getOption(paste0(".", id))$options, "environment")  
  expect_is(getOption(paste0(".", id))$.meta, "environment")  
  expect_is(getOption(paste0(".", id))$.registry, "environment")  
  options(".test" = NULL)
  
  id <- "test"
  options("test" = NULL)
  expect_is(initializeOptionContainer(id, hidden = FALSE), "environment")
  expect_is(getOption(id)$options, "environment")  
  expect_is(getOption(id)$.meta, "environment")  
  expect_is(getOption(id)$.registry, "environment")  
  options("test" = NULL)
  
  on.exit(setwd(wd_0))
  
})

##------------------------------------------------------------------------------
context("initializeOptionContainer/reuse")
##------------------------------------------------------------------------------

test_that("initializeOptionContainer/reuse", {
  
  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  id <- "test"
  options("test" = NULL)
  expect_true(is.null(getOption(id)))
  expect_is(res <- initializeOptionContainer(id, hidden = FALSE), "environment")
  expect_is(initializeOptionContainer(id, overwrite = TRUE), "environment")
  expect_identical(getOption(id), res)  
  options("test" = NULL)
  
  on.exit(setwd(wd_0))
  
})

##------------------------------------------------------------------------------
context("initializeOptionContainer/partial")
##------------------------------------------------------------------------------

test_that("initializeOptionContainer/partial", {
  
  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  id <- "test"
  options("test" = NULL)
  expect_is(initializeOptionContainer(id, hidden = FALSE, 
    components = "options"), "environment")
  expect_is(getOption(id)$options, "environment")  
  expect_is(getOption(id)$.meta, "NULL")  
  expect_is(getOption(id)$.registry, "NULL")  
  
  expect_is(initializeOptionContainer(id, hidden = FALSE,
    components = c("options", ".meta"), overwrite = TRUE), "environment")
  expect_is(getOption(id)$options, "environment")  
  expect_is(getOption(id)$.meta, "environment")
  expect_is(getOption(id)$.registry, "NULL")  
  
  options("test" = NULL)
  
  on.exit(setwd(wd_0))
  
})

##------------------------------------------------------------------------------
context("initializeOptionContainer/interface test")
##------------------------------------------------------------------------------

test_that("initializeOptionContainer/interface test", {
  
  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  id <- structure(list(id = "test"), class = "OptionContext.Test")
  options(".test" = NULL)
  expect_is(initializeOptionContainer(id), "environment")
  expect_is(getOption(paste0(".", id$id))$options, "environment")  
  expect_is(getOption(paste0(".", id$id))$.meta, "environment")  
  expect_is(getOption(paste0(".", id$id))$.registry, "environment")  
  options(".test" = NULL)
  
  id <- structure(list(id = "test"), class = "OptionContext.Test")
  options("test" = NULL)
  expect_is(initializeOptionContainer(id, hidden = FALSE), "environment")
  expect_is(getOption(id$id)$options, "environment")  
  expect_is(getOption(id$id)$.meta, "environment")  
  expect_is(getOption(id$id)$.registry, "environment")  
  options("test" = NULL)
  
  on.exit(setwd(wd_0))
  
})

##------------------------------------------------------------------------------
context("initializeOptionContainer/sub ID")
##------------------------------------------------------------------------------

test_that("initializeOptionContainer/sub ID", {
  
  if (basename(getwd()) == "testthat") {
    wd_0 <- setwd("data/test.package")
  } else {
    wd_0 <- setwd("tests/testthat/data/test.package")
  }
  
  id <- "test"
  sub_id <- "a"
  options(".test" = NULL)
  expect_is(initializeOptionContainer(id = id, sub_id = sub_id), "environment")
  expect_identical(getOption(paste0(".", id))$options, NULL)  
  expect_is(getOption(paste0(".", id))[[sub_id]]$options, "environment")  
  expect_identical(getOption(paste0(".", id))$.meta, NULL)  
  expect_is(getOption(paste0(".", id))[[sub_id]]$.meta, "environment")  
  expect_identical(getOption(paste0(".", id))$.registry, NULL)  
  expect_is(getOption(paste0(".", id))[[sub_id]]$.registry, "environment")  
  options(".test" = NULL)
  
  id <- "test"
  sub_id <- "a"
  options("test" = NULL)
  expect_is(initializeOptionContainer(id = id, sub_id = sub_id, 
    hidden = FALSE), "environment")
  expect_identical(getOption(paste0(".", id))$options, NULL)  
  expect_is(getOption(id)[[sub_id]]$options, "environment")  
  expect_identical(getOption(id)$.meta, NULL)  
  expect_is(getOption(id)[[sub_id]]$.meta, "environment")  
  expect_identical(getOption(id)$.registry, NULL)  
  expect_is(getOption(id)[[sub_id]]$.registry, "environment")  
  options("test" = NULL)
  
  on.exit(setwd(wd_0))
  
})
