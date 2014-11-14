\dontrun{

## Also see examples at `?setAnywhereOption`
  
##------------------------------------------------------------------------------
## Basics //
##------------------------------------------------------------------------------

container <- initializeOptionContainer(overwrite = TRUE)
setAnywhereOption(id = "test", value = TRUE)
getAnywhereOption(id = "test")

setAnywhereOption(id = "a/b/c", value = 10)
getAnywhereOption(id = "a")
getAnywhereOption(id = "a/b")
getAnywhereOption(id = "a/b/c")
  
##------------------------------------------------------------------------------
## Different `where` //
##------------------------------------------------------------------------------

where <- "test"
container <- initializeOptionContainer(id = where, overwrite = TRUE)
setAnywhereOption(id = "a/b/c", value = 10, where = where)
getAnywhereOption(id = "a/b/c", where = where)

where <- structure(list(id = "test"), class = "OptionContext.Test")
container <- initializeOptionContainer(id = where, overwrite = TRUE)
setAnywhereOption(id = "a/b/c", value = 10, where = where)
getAnywhereOption(id = "a/b/c", where = where)
  
}
