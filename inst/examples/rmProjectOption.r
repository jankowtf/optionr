\dontrun{

##------------------------------------------------------------------------------
## Default `where` //
##------------------------------------------------------------------------------

container <- initializeOptionContainer(overwrite = TRUE)
setProjectOption(id = "a", value = TRUE)
rmProjectOption(id = "a")
exists("a", container$options, inherits = FALSE)

setProjectOption(id = "a/b/c", value = 10, gap = TRUE)
rmProjectOption(id = "a/b/c")
exists("c", container$options$a$b, inherits = FALSE)

##------------------------------------------------------------------------------
## Different `where` //
##------------------------------------------------------------------------------
  
where <- "test"
container <- initializeOptionContainer(id = where, overwrite = TRUE)
".test" %in% names(options())
setProjectOption(id = "a/b/c", value = 10, where = where, gap = TRUE)
rmProjectOption(id = "a/b/c", where = where)
exists("c", container$options$a$b, inherits = FALSE)
  
where <- structure(list(id = "test"), class = "OptionContext.Test")
container <- initializeOptionContainer(id = where, overwrite = TRUE)
".test" %in% names(options())
setProjectOption(id = "a/b/c", value = 10, where = where, gap = TRUE)
rmProjectOption(id = "a/b/c", where = where)
exists("c", container$options$a$b, inherits = FALSE)

##------------------------------------------------------------------------------
## Strictness //
##------------------------------------------------------------------------------
  
container <- initializeOptionContainer(overwrite = TRUE)
rmProjectOption(id = "a")
try(rmProjectOption(id = "a", strict = TRUE))

rmProjectOption(id = "a/b/c")
try(rmProjectOption(id = "a/b/c", strict = TRUE))

rmProjectOption(id = character())
try(rmProjectOption(id = character(), strict = TRUE))
    
}

