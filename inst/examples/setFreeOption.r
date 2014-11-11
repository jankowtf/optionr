\dontrun{

##------------------------------------------------------------------------------
## Basics //
##------------------------------------------------------------------------------

container <- initializeOptionContainer(overwrite = TRUE)

## Simple name/ID //
setFreeOption(id = "test", value = TRUE)
getFreeOption(id = "test")

## Path-like name/ID //
setFreeOption(id = "test", value = new.env())
## --> note that `test` is overwritten and thus transformed from a "leaf"
## to a "branch" component (i.e. an environment)
getFreeOption(id = "test")
ls(getFreeOption(id = "test"))

setFreeOption(id = "test/a", value = TRUE)
ls(getFreeOption(id = "test"))
getFreeOption(id = "test/a")

## Must exist //
setFreeOption(id = "test/b", value = TRUE, must_exist = TRUE)
try(setFreeOption(id = "test/b", value = TRUE, must_exist = TRUE, strict = TRUE))

## Typed //
setFreeOption(id = "test/c", value = "hello world!", typed = TRUE, gap = TRUE)
setFreeOption(id = "test/c", value = 1:3)
## --> wrong class, but `strict_set = 0` --> disregarded without warning or error
getFreeOption(id = "test/c")
## --> still `hello world!` because `value = 1:3` had wrong class

setFreeOption(id = "test/c", value = "hello world!", typed = TRUE, strict_set = 1)
try(setFreeOption(id = "test/c", value = 1:3))
## --> warning and no assignment
getFreeOption(id = "test/c")
## --> still `hello world!`

setFreeOption(id = "test/c", value = "hello world!", typed = TRUE, strict_set = 2)
try(setFreeOption(id = "test/c", value = 1:3))
## --> error
getFreeOption(id = "test/c")
## --> still `hello world!`

setFreeOption(id = "test/a", value = "something else")
## --> correct class --> value changed 
getFreeOption(id = "test/a")
  
##------------------------------------------------------------------------------
## Numerical names/IDs //
##------------------------------------------------------------------------------

container <- initializeOptionContainer(overwrite = TRUE)
setFreeOption(id = "20140101", value = TRUE)
ls(container, all.names = TRUE)
getFreeOption(id = "20140101")

##------------------------------------------------------------------------------
## Branch gaps //
##------------------------------------------------------------------------------
  
container <- initializeOptionContainer(overwrite = TRUE)
setFreeOption(id = "a/b/c/d", value = TRUE)
try(setFreeOption(id = "a/b/c/d", value = TRUE, strict = TRUE))
## --> branch gap: branches a, b and c do not exist yet

## Closing the gap //
setFreeOption(id = "a/b/c/d", value = TRUE, gap = TRUE)

## Inspect //
ls(container)
getFreeOption(id = "a")
getFreeOption(id = "a/b")
getFreeOption(id = "a/b/c")
getFreeOption(id = "a/b/c/d")

##------------------------------------------------------------------------------
## Forcing leafs to branches //
##------------------------------------------------------------------------------
  
container <- initializeOptionContainer(overwrite = TRUE)
setFreeOption(id = "a", value = "hello world!")
setFreeOption(id = "a/b", value = 10, gap = TRUE)
try(setFreeOption(id = "a/b", value = 10, gap = TRUE, strict = TRUE))
## --> starting branch `a` is not an environment 
getFreeOption(id = "a")

## Forcing leaf into a branch //
setFreeOption(id = "a/b", value = 10, force = TRUE)
getFreeOption(id = "a")
getFreeOption(id = "a/b")

##------------------------------------------------------------------------------
## Different `where` //
##------------------------------------------------------------------------------

where <- "test"
container <- initializeOptionContainer(id = where, overwrite = TRUE)
setFreeOption(id = "a/b/c", value = 10, where = where, gap = TRUE)
getFreeOption(id = "a/b/c", where = where)
identical(getOptionContainer(where), container)
exists("a", container)

where <- structure(list(id = "test"), class = "OptionContext.Test")
container <- initializeOptionContainer(id = where, overwrite = TRUE)
setFreeOption(id = "a/b/c", value = 10, where = where, gap = TRUE)
getFreeOption(id = "a/b/c", where = where)
identical(getOptionContainer(where), container)
exists("a", container)

##------------------------------------------------------------------------------
## Reactive options: simple name/ID //
##------------------------------------------------------------------------------

container <- initializeOptionContainer(overwrite = TRUE)  
setFreeOption(id = "x_1", value = TRUE, reactive = TRUE)
setFreeOption(
  id = "x_2", 
  value = reactiveOption(!getFreeOption(id = "x_1"))
)
## `x_2` should always be the opposite of `x_1`
## Note that you can ommit `reactive = TRUE` when `value = reactiveOption(...)`

getFreeOption(id = "x_1")
getFreeOption(id = "x_2")

## Changing via options //
setFreeOption(id = "x_1", value = FALSE)
getFreeOption(id = "x_1")
getFreeOption(id = "x_2")

## When changed manually //
container <- getOptionContainer()
container$x_1 <- TRUE
container$x_1
container$x_2

## Trying to change bound variable //
setFreeOption(id = "x_2", value = TRUE)
getFreeOption(id = "x_2")
## --> has no effect; warning and error behavior can be specified via `strict_set`

##------------------------------------------------------------------------------
## Reactive options: path-like name/ID //
##------------------------------------------------------------------------------

container <- initializeOptionContainer(overwrite = TRUE)
setFreeOption(id = "a/test", value = TRUE, reactive = TRUE, gap = TRUE)
setFreeOption(id = "b/test", 
  value = reactiveOption(!getFreeOption(id = "a/test")), 
  gap = TRUE
)

getFreeOption(id = "a/test")  
getFreeOption(id = "b/test")
setFreeOption(id = "a/test", value = FALSE)
getFreeOption(id = "a/test")
getFreeOption(id = "b/test")
  
}
