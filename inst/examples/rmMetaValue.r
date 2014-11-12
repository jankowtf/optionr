\dontrun{

##------------------------------------------------------------------------------
## Default `where` //
##------------------------------------------------------------------------------

container <- initializeOptionContainer(overwrite = TRUE)
setMetaValue(id = "a", value = TRUE)
rmMetaValue(id = "a")
exists("a", container$.meta, inherits = FALSE)

setMetaValue(id = "a/b/c", value = 10, gap = TRUE)
rmMetaValue(id = "a/b/c")
exists("c", container$.meta$a$b, inherits = FALSE)

##------------------------------------------------------------------------------
## Different `where` //
##------------------------------------------------------------------------------
  
where <- "test"
container <- initializeOptionContainer(id = where, overwrite = TRUE)
".test" %in% names(options())
setMetaValue(id = "a/b/c", value = 10, where = where, gap = TRUE)
rmMetaValue(id = "a/b/c", where = where)
exists("c", container$.meta$a$b, inherits = FALSE)
  
where <- structure(list(id = "test"), class = "OptionContext.Test")
container <- initializeOptionContainer(id = where, overwrite = TRUE)
".test" %in% names(options())
setMetaValue(id = "a/b/c", value = 10, where = where, gap = TRUE)
rmMetaValue(id = "a/b/c", where = where)
exists("c", container$.meta$a$b, inherits = FALSE)

##------------------------------------------------------------------------------
## Strictness //
##------------------------------------------------------------------------------
  
container <- initializeOptionContainer(overwrite = TRUE)
rmMetaValue(id = "a")
try(rmMetaValue(id = "a", strict = TRUE))

rmMetaValue(id = "a/b/c")
try(rmMetaValue(id = "a/b/c", strict = TRUE))

rmMetaValue(id = character())
try(rmMetaValue(id = character(), strict = TRUE))
    
}
