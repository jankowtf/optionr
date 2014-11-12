\dontrun{

##------------------------------------------------------------------------------
## Default `where` //
##------------------------------------------------------------------------------

container <- initializeOptionContainer(overwrite = TRUE)
setRegistryValue(id = "a", value = TRUE)
rmRegistryValue(id = "a")
exists("a", container$.registry, inherits = FALSE)

setRegistryValue(id = "a/b/c", value = 10, gap = TRUE)
rmRegistryValue(id = "a/b/c")
exists("c", container$.registry$a$b, inherits = FALSE)

##------------------------------------------------------------------------------
## Different `where` //
##------------------------------------------------------------------------------
  
where <- "test"
container <- initializeOptionContainer(id = where, overwrite = TRUE)
".test" %in% names(options())
setRegistryValue(id = "a/b/c", value = 10, where = where, gap = TRUE)
rmRegistryValue(id = "a/b/c", where = where)
exists("c", container$.registry$a$b, inherits = FALSE)
  
where <- structure(list(id = "test"), class = "OptionContext.Test")
container <- initializeOptionContainer(id = where, overwrite = TRUE)
".test" %in% names(options())
setRegistryValue(id = "a/b/c", value = 10, where = where, gap = TRUE)
rmRegistryValue(id = "a/b/c", where = where)
exists("c", container$.registry$a$b, inherits = FALSE)

##------------------------------------------------------------------------------
## Strictness //
##------------------------------------------------------------------------------
  
container <- initializeOptionContainer(overwrite = TRUE)
rmRegistryValue(id = "a")
try(rmRegistryValue(id = "a", strict = TRUE))

rmRegistryValue(id = "a/b/c")
try(rmRegistryValue(id = "a/b/c", strict = TRUE))

rmRegistryValue(id = character())
try(rmRegistryValue(id = character(), strict = TRUE))
    
}
