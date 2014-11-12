\dontrun{

##------------------------------------------------------------------------------
## Basics //
##------------------------------------------------------------------------------

container <- initializeOptionContainer(overwrite = TRUE)

## Simple name/ID //
setAnywhereOption(id = "test", value = TRUE)
getAnywhereOption(id = "test")

## Path-like name/ID //
setAnywhereOption(id = "test", value = new.env())
## --> note that `test` is overwritten and thus transformed from a "leaf"
## to a "branch" component (i.e. an environment)
getAnywhereOption(id = "test")
ls(getAnywhereOption(id = "test"))

setAnywhereOption(id = "test/a", value = TRUE)
ls(getAnywhereOption(id = "test"))
getAnywhereOption(id = "test/a")

## Must exist //
setAnywhereOption(id = "test/b", value = TRUE, must_exist = TRUE)
try(setAnywhereOption(id = "test/b", value = TRUE, must_exist = TRUE, strict = TRUE))

## Typed //
setAnywhereOption(id = "test/c", value = "hello world!", typed = TRUE, gap = TRUE)
setAnywhereOption(id = "test/c", value = 1:3)
## --> wrong class, but `strict_set = 0` --> disregarded without warning or error
getAnywhereOption(id = "test/c")
## --> still `hello world!` because `value = 1:3` had wrong class

setAnywhereOption(id = "test/c", value = "hello world!", typed = TRUE, strict_set = 1)
try(setAnywhereOption(id = "test/c", value = 1:3))
## --> warning and no assignment
getAnywhereOption(id = "test/c")
## --> still `hello world!`

setAnywhereOption(id = "test/c", value = "hello world!", typed = TRUE, strict_set = 2)
try(setAnywhereOption(id = "test/c", value = 1:3))
## --> error
getAnywhereOption(id = "test/c")
## --> still `hello world!`

setAnywhereOption(id = "test/a", value = "something else")
## --> correct class --> value changed 
getAnywhereOption(id = "test/a")
  
##------------------------------------------------------------------------------
## Numerical names/IDs //
##------------------------------------------------------------------------------

container <- initializeOptionContainer(overwrite = TRUE)
setAnywhereOption(id = "20140101", value = TRUE)
ls(container, all.names = TRUE)
getAnywhereOption(id = "20140101")

##------------------------------------------------------------------------------
## Branch gaps //
##------------------------------------------------------------------------------
  
container <- initializeOptionContainer(overwrite = TRUE)
setAnywhereOption(id = "a/b/c/d", value = TRUE)
try(setAnywhereOption(id = "a/b/c/d", value = TRUE, strict = TRUE))
## --> branch gap: branches a, b and c do not exist yet

## Closing the gap //
setAnywhereOption(id = "a/b/c/d", value = TRUE, gap = TRUE)

## Inspect //
ls(container)
getAnywhereOption(id = "a")
getAnywhereOption(id = "a/b")
getAnywhereOption(id = "a/b/c")
getAnywhereOption(id = "a/b/c/d")

##------------------------------------------------------------------------------
## Forcing leafs to branches //
##------------------------------------------------------------------------------
  
container <- initializeOptionContainer(overwrite = TRUE)
setAnywhereOption(id = "a", value = "hello world!")
setAnywhereOption(id = "a/b", value = 10, gap = TRUE)
try(setAnywhereOption(id = "a/b", value = 10, gap = TRUE, strict = TRUE))
## --> starting branch `a` is not an environment 
getAnywhereOption(id = "a")

## Forcing leaf into a branch //
setAnywhereOption(id = "a/b", value = 10, force = TRUE)
getAnywhereOption(id = "a")
getAnywhereOption(id = "a/b")

##------------------------------------------------------------------------------
## Different `where` //
##------------------------------------------------------------------------------

where <- "test"
container <- initializeOptionContainer(id = where, overwrite = TRUE)
setAnywhereOption(id = "a/b/c", value = 10, where = where, gap = TRUE)
getAnywhereOption(id = "a/b/c", where = where)
identical(getOptionContainer(where), container)
exists("a", container)

where <- structure(list(id = "test"), class = "OptionContext.Test")
container <- initializeOptionContainer(id = where, overwrite = TRUE)
setAnywhereOption(id = "a/b/c", value = 10, where = where, gap = TRUE)
getAnywhereOption(id = "a/b/c", where = where)
identical(getOptionContainer(where), container)
exists("a", container)

##------------------------------------------------------------------------------
## Reactive options: simple name/ID //
##------------------------------------------------------------------------------

container <- initializeOptionContainer(overwrite = TRUE)  
setAnywhereOption(id = "x_1", value = TRUE, reactive = TRUE)
setAnywhereOption(
  id = "x_2", 
  value = reactiveOption(!getAnywhereOption(id = "x_1"))
)
## `x_2` should always be the opposite of `x_1`
## Note that you can ommit `reactive = TRUE` when `value = reactiveOption(...)`

getAnywhereOption(id = "x_1")
getAnywhereOption(id = "x_2")

## Changing via options //
setAnywhereOption(id = "x_1", value = FALSE)
getAnywhereOption(id = "x_1")
getAnywhereOption(id = "x_2")

## When changed manually //
container <- getOptionContainer()
container$x_1 <- TRUE
container$x_1
container$x_2

## Trying to change bound variable //
setAnywhereOption(id = "x_2", value = TRUE)
getAnywhereOption(id = "x_2")
## --> has no effect; warning and error behavior can be specified via `strict_set`

##------------------------------------------------------------------------------
## Reactive options: path-like name/ID //
##------------------------------------------------------------------------------

container <- initializeOptionContainer(overwrite = TRUE)
setAnywhereOption(id = "a/test", value = TRUE, reactive = TRUE, gap = TRUE)
setAnywhereOption(id = "b/test", 
  value = reactiveOption(!getAnywhereOption(id = "a/test")), 
  gap = TRUE
)

getAnywhereOption(id = "a/test")  
getAnywhereOption(id = "b/test")
setAnywhereOption(id = "a/test", value = FALSE)
getAnywhereOption(id = "a/test")
getAnywhereOption(id = "b/test")
  
}
