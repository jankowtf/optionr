##------------------------------------------------------------------------------
## Define //
##------------------------------------------------------------------------------

setGeneric(
  name = "foo",
  signature = c("x", "..."),
  def = function(x, ...) standardGeneric("foo")      
)
setMethod(
  f = "foo", 
  signature = signature(x = "character", ... = "MyThreeDotsForBar"), 
 definition = function(x, ...) bar(x = x)
)
## --> does not work

setGeneric(
  name = "foo",
  signature = c("x"),
  def = function(x, ...) standardGeneric("foo")      
)
setMethod(
  f = "foo", 
  signature = signature(x = "character"), 
 definition = function(x, x_foo, ...) {
   message("foo ----------")
   message("foo/threedots")
   try(print(list(...)))
   message("foo/x_foo")
   try(print(x_foo))
   
   bar(x = x, ...)
})

setGeneric(
  name = "bar",
  signature = c("x"),
  def = function(x, x_foo, x_bar, ...) standardGeneric("bar")      
)
setMethod(
  f = "bar", 
  signature = signature(x = "character"), 
 definition = function(x, x_foo, x_bar, ...) {
   message("bar ----------")
   message("bar/threedots")
   try(print(list(...)))
   message("bar/x_foo")
   try(print(x_foo))
   message("bar/x_bar")
   try(print(x_bar))
   
   fooS3(x = x, ...)
})

setGeneric(
  name = "foobar",
  signature = c("x"),
  def = function(x, ...) standardGeneric("foobar")      
)
setMethod(
  f = "foobar", 
  signature = signature(x = "character"), 
 definition = function(x, ...) {
   message("foobar ----------")
   message("foobar/threedots")
   try(print(list(...)))
   
   foo(x = x, ...)
})

fooS3 <- function(x, x_foo, x_bar, x_foos3 = TRUE, ...) {
  message("fooS3 ----------")
  message("fooS3/threedots")
  try(print(list(...)))
  message("fooS3/x_foo")
  try(print(x_foo))
  message("fooS3/x_bar")
  try(print(x_bar))
  message("fooS3/x_foos3")
  try(print(x_foos3))
  return(paste0("hello: ", x))
}

fooS3(x = "John Doe")
bar(x = "John Doe")
bar(x = "John Doe", x_bar = TRUE)
foo(x = "John Doe")
foo(x = "John Doe", x_foo = TRUE)
foo(x = "John Doe", x_bar = TRUE)

################################################################################

foo <- function(x, y = "some character", ...) {
  message("foo ----------")
  message("foo/threedots")
  try(print(list(...)))
  message("foo/y")
  try(print(y))
  bar(x = x, ...)
}
bar <- function(x, y = TRUE, ...) {
  message("bar ----------")
  message("bar/threedots")
  try(print(list(...)))
  message("bar/y")
  try(print(y))
  return(paste0("hello: ", x))
}
foobar <- function(x, ...) {
  message("foobar ----------")
  message("foobar/threedots")
  try(print(list(...)))
  foo(x = x, ...)
}

foobar(x = "John Doe", y = "hi there")
# foobar ----------
# foobar/threedots
# $y
# [1] "hi there"
# 
# foo ----------
# foo/threedots
# list()
# foo/y
# [1] "hi there"
# bar ----------
# bar/threedots
# list()
# bar/y
# [1] TRUE
# [1] "hello: John Doe"

foobar(x = "John Doe", y_foo = "hello world!", y_bar = FALSE)

## Prototype //
foo <- function(x, y = "some character", ...) {
  message("foo ----------")
  message("foo/threedots")
  try(print(list(...)))
  message("foo/y")
  arg <- paste0("y_", sys.call()[[1]])
  if (arg %in% names(list(...))) {
    y <- list(...)[[arg]]
  }
  try(print(y))
  bar(x = x, ...)
}
bar <- function(x, y = TRUE, ...) {
  message("bar ----------")
  message("bar/threedots")
  try(print(list(...)))
  message("bar/y")
  arg <- paste0("y_", sys.call()[[1]])
  if (arg %in% names(list(...))) {
    y <- list(...)[[arg]]
  }
  try(print(y))
  return(paste0("hello: ", x))
}

foobar(x = "John Doe", y_foo = "hello world!", y_bar = FALSE)
# foobar ----------
# foobar/threedots
# $y_foo
# [1] "hello world!"
# 
# $y_bar
# [1] FALSE
# 
# foo ----------
# foo/threedots
# $y_foo
# [1] "hello world!"
# 
# $y_bar
# [1] FALSE
# 
# foo/y
# [1] "hello world!"
# bar ----------
# bar/threedots
# $y_foo
# [1] "hello world!"
# 
# $y_bar
# [1] FALSE
# 
# bar/y
# [1] FALSE
# [1] "hello: John Doe"

################################################################################

foobar <- function(x, ...) {
  message("foobar ----------")
  message("foobar/threedots")
  threedots <- list(...)
  try(print(threedots))
  message("foobar/combined args")
  try(print(c(x, threedots)))
  ## --> list gets flattened (i.e. `args_foo.y` instead of nested structure)
  ## --> that's why subsequent functions will not recognize "their" arguments
  ## from it
  if (any(idx <- names(threedots) %in% "args_foo")) {
    do.call("foo", c(x = x, threedots$args_foo, threedots[-idx]))  
  } else {
    foo(x = x, ...)
  }
}
foo <- function(x, y = "some character", ...) {
  message("foo ----------")
  message("foo/threedots")
  threedots <- list(...)
  try(print(threedots))
  message("foo/y")
  try(print(y))
  if (any(idx <- names(threedots) %in% "args_bar")) {
    do.call("bar", c(x = x, threedots$args_bar, threedots[-idx]))
  } else {
    bar(x = x, ...)
  }
}
bar <- function(x, y = TRUE, ...) {
  message("bar ----------")
  message("bar/threedots")
  try(print(list(...)))
  message("bar/y")
  try(print(y))
  return(paste0("hello: ", x))
}

foobar(x = "John Doe", args_foo = list(y = "hello world!"))
foobar(x = "John Doe", args_bar = list(y = FALSE))
foobar(x = "John Doe", 
       args_foo = list(y = "hello world!"), 
       args_bar = list(y = FALSE)
)

################################################################################

foobar <- function(x, args_foo = list(), args_bar = list(), ...) {
  message("foobar ----------")
  message("foobar/threedots")
  try(print(list(...)))
#   do.call("foo", c(x = x, ...))
  do.call("foo", c(x = x, args_foo, args_bar, ...))
}
foo <- function(x, y = "some character", ...) {
  message("foo ----------")
  message("foo/threedots")
  try(print(list(...)))
  message("foo/y")
  try(print(y))
  do.call("bar", c(x = x, ...))
}
bar <- function(x, y = TRUE, ...) {
  message("bar ----------")
  message("bar/threedots")
  try(print(list(...)))
  message("bar/y")
  try(print(y))
  return(paste0("hello: ", x))
}

foobar(x = "John Doe", 
       args_foo = list(y = "hello world!"), 
       args_bar = list(y = FALSE)
)

################################################################################

.A = setClass("A", contains="numeric")
.B = setClass("B", contains="A")

setGeneric("foo", function(...) standardGeneric("foo"))
setMethod("foo", "A", function(...) "foo,A-method")

setGeneric("bar", signature="...", 
           function(..., verbose=TRUE) standardGeneric("bar")
)
setMethod("bar", c("A", "ANY"),
          function(..., verbose=TRUE) {
            if (verbose) "bar,A-method" else "hello world!"
          })

foo(.A(), .B())
bar(.A(), .B())
bar(.B())
bar("a")
bar(.A(), "a")
bar(.A(), .B(), verbose=FALSE)
bar(.A(), .B(), verbose=FALSE)
