\dontrun{

##------------------------------------------------------------------------------
## Basics //
##------------------------------------------------------------------------------

setRegistryValue(id = "test", value = TRUE)
existsRegistryValue(id = "test")

setRegistryValue(id = "a/b/c", value = 10)
existsRegistryValue(id = "a")
existsRegistryValue(id = "a/b")
existsRegistryValue(id = "a/b/c")
existsRegistryValue(id = "a/b/c/d")

existsRegistryValue(id = "c")
existsRegistryValue(id = "c/d/e")
  
##------------------------------------------------------------------------------
## Strictness levels //
##------------------------------------------------------------------------------

## Empty ID //
existsRegistryValue(id = character())
try(existsRegistryValue(id = character(), strict = 1))
try(existsRegistryValue(id = character(), strict = 2))

## Not-existing //  
existsRegistryValue(id = "c/d/e")
try(existsRegistryValue(id = "c/d/e", strict = 1))
try(existsRegistryValue(id = "c/d/e", strict = 2))

##------------------------------------------------------------------------------
## Explicit `where` //
##------------------------------------------------------------------------------

where <- new.env()
setRegistryValue(id = "a/b/c", value = 10, where = where)
existsRegistryValue(id = "a/b/c", where = where)
existsRegistryValue(id = "a/b/c/d", where = where)
existsRegistryValue(id = "c/d/e", where = where)

}
