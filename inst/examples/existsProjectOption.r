\dontrun{

##------------------------------------------------------------------------------
## Basics //
##------------------------------------------------------------------------------

setProjectOption(id = "test", value = TRUE)
existsProjectOption(id = "test")

setProjectOption(id = "a/b/c", value = 10)
existsProjectOption(id = "a")
existsProjectOption(id = "a/b")
existsProjectOption(id = "a/b/c")
existsProjectOption(id = "a/b/c/d")

existsProjectOption(id = "c")
existsProjectOption(id = "c/d/e")
  
##------------------------------------------------------------------------------
## Strictness levels //
##------------------------------------------------------------------------------

## Empty ID //
existsProjectOption(id = character())
try(existsProjectOption(id = character(), strict = 1))
try(existsProjectOption(id = character(), strict = 2))

## Not-existing //  
existsProjectOption(id = "c/d/e")
try(existsProjectOption(id = "c/d/e", strict = 1))
try(existsProjectOption(id = "c/d/e", strict = 2))

##------------------------------------------------------------------------------
## Explicit `where` //
##------------------------------------------------------------------------------

where <- new.env()
setProjectOption(id = "a/b/c", value = 10, where = where)
existsProjectOption(id = "a/b/c", where = where)
existsProjectOption(id = "a/b/c/d", where = where)
existsProjectOption(id = "c/d/e", where = where)

##------------------------------------------------------------------------------
## Sub ID //
##------------------------------------------------------------------------------

where <- new.env()
setProjectOption(id = "a/b/c", value = 10, where = where, sub_id = "sub")
existsProjectOption(id = "a/b/c", where = where, sub_id = "sub")
existsProjectOption(id = "a/b/c/d", where = where, sub_id = "sub")
existsProjectOption(id = "c/d/e", where = where, sub_id = "sub")

}
