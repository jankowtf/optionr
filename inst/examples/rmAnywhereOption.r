\dontrun{

##------------------------------------------------------------------------------
## Default `where` //
##------------------------------------------------------------------------------

container <- initializeOptionContainer(overwrite = TRUE)
setAnywhereOption(id = "a", value = TRUE)
rmAnywhereOption(id = "a")
exists("a", container, inherits = FALSE)

setAnywhereOption(id = "a/b/c", value = 10)
rmAnywhereOption(id = "a/b/c")
exists("c", container$a$b, inherits = FALSE)

##------------------------------------------------------------------------------
## Different `where` // 
##------------------------------------------------------------------------------

where <- "test"
container <- initializeOptionContainer(id = where, overwrite = TRUE)
".test" %in% names(options())
## --> option container assigned to R option `.test`

setAnywhereOption(id = "a/b/c", value = 10, where = where)
rmAnywhereOption(id = "a/b/c", where = where)
exists("c", container$a$b, inherits = FALSE)

where <- structure(list(id = "test"), class = "OptionContext.Test")
container <- initializeOptionContainer(id = where, overwrite = TRUE)
setAnywhereOption(id = "a/b/c", value = 10, where = where)
rmAnywhereOption(id = "a/b/c", where = where)
exists("c", container$a$b, inherits = FALSE)
  
##------------------------------------------------------------------------------
## Strictness //
##------------------------------------------------------------------------------

container <- initializeOptionContainer(overwrite = TRUE)
rmAnywhereOption(id = "a")
try(rmAnywhereOption(id = "a", strict = 2))

rmAnywhereOption(id = "a/b/c")
try(rmAnywhereOption(id = "a/b/c", strict = 2))

rmAnywhereOption(id = character())
try(rmAnywhereOption(id = character(), strict = 2))
    
}
