\dontrun{

##------------------------------------------------------------------------------
## Basics //
##------------------------------------------------------------------------------

setMetaValue(id = "test", value = TRUE)
existsMetaValue(id = "test")

setMetaValue(id = "a/b/c", value = 10)
existsMetaValue(id = "a")
existsMetaValue(id = "a/b")
existsMetaValue(id = "a/b/c")
existsMetaValue(id = "a/b/c/d")

existsMetaValue(id = "c")
existsMetaValue(id = "c/d/e")
  
##------------------------------------------------------------------------------
## Strictness levels //
##------------------------------------------------------------------------------

## Empty ID //
existsMetaValue(id = character())
try(existsMetaValue(id = character(), strict = 1))
try(existsMetaValue(id = character(), strict = 2))

## Not-existing //  
existsMetaValue(id = "c/d/e")
try(existsMetaValue(id = "c/d/e", strict = 1))
try(existsMetaValue(id = "c/d/e", strict = 2))

##------------------------------------------------------------------------------
## Explicit `where` //
##------------------------------------------------------------------------------

where <- new.env()
setMetaValue(id = "a/b/c", value = 10, where = where)
existsMetaValue(id = "a/b/c", where = where)
existsMetaValue(id = "a/b/c/d", where = where)
existsMetaValue(id = "c/d/e", where = where)

}
