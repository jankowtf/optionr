\dontrun{

##------------------------------------------------------------------------------
## Basics //
##------------------------------------------------------------------------------

setAnywhereOption(id = "test", value = TRUE)
existsAnywhereOption(id = "test")

setAnywhereOption(id = "a/b/c", value = 10)
existsAnywhereOption(id = "a")
existsAnywhereOption(id = "a/b")
existsAnywhereOption(id = "a/b/c")
existsAnywhereOption(id = "a/b/c/d")

existsAnywhereOption(id = "c")
existsAnywhereOption(id = "c/d/e")
  
##------------------------------------------------------------------------------
## Strictness levels //
##------------------------------------------------------------------------------

## Empty ID //
existsAnywhereOption(id = character())
try(existsAnywhereOption(id = character(), strict = 1))
try(existsAnywhereOption(id = character(), strict = 2))

## Not-existing //  
existsAnywhereOption(id = "c/d/e")
try(existsAnywhereOption(id = "c/d/e", strict = 1))
try(existsAnywhereOption(id = "c/d/e", strict = 2))

##------------------------------------------------------------------------------
## Explicit `where` //
##------------------------------------------------------------------------------

where <- new.env()
setAnywhereOption(id = "a/b/c", value = 10, where = where)
existsAnywhereOption(id = "a/b/c", where = where)
existsAnywhereOption(id = "a/b/c/d", where = where)
existsAnywhereOption(id = "c/d/e", where = where)

}
