\dontrun{

## Also see examples at `?setFreeOption`
  
##------------------------------------------------------------------------------
## Basics //
##------------------------------------------------------------------------------

container <- initializeOptionContainer(overwrite = TRUE)
setFreeOption(id = "test", value = TRUE)
getFreeOption(id = "test")

setFreeOption(id = "a/b/c", value = 10, gap = TRUE)
getFreeOption(id = "a")
getFreeOption(id = "a/b")
getFreeOption(id = "a/b/c")
  
##------------------------------------------------------------------------------
## Different `where` //
##------------------------------------------------------------------------------

where <- "test"
container <- initializeOptionContainer(id = where, overwrite = TRUE)
setFreeOption(id = "a/b/c", value = 10, where = where, gap = TRUE)
getFreeOption(id = "a/b/c", where = where)

where <- structure(list(id = "test"), class = "OptionContext.Test")
container <- initializeOptionContainer(id = where, overwrite = TRUE)
setFreeOption(id = "a/b/c", value = 10, where = where, gap = TRUE)
getFreeOption(id = "a/b/c", where = where)
  
}
