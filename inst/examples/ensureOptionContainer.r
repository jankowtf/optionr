\dontrun{

##------------------------------------------------------------------------------
## Default ID //
##------------------------------------------------------------------------------

options(".optionr" = NULL)
res <- ensureOptionContainer()
".optionr" %in% names(options())
ensureOptionContainer(overwrite = TRUE)
identical(getOption(".optionr"), res)
## --> identical environment object is reused --> ensures pass-by-references
## keeps working even when `overwrite = TRUE`

## Clean up //
options(".optionr" = NULL)

##------------------------------------------------------------------------------
## Explicit ID //
##------------------------------------------------------------------------------

res <- ensureOptionContainer(id = "abcd")
".abcd" %in% names(options())
ensureOptionContainer(id = "abcd", overwrite = TRUE)
identical(getOption(".abcd"), res)
## --> identical environment object is reused --> ensures pass-by-references
## keeps working even when `overwrite = TRUE`

## Clean up //
options(".abcd" = NULL)

##------------------------------------------------------------------------------
## Sub ID //
##------------------------------------------------------------------------------

res <- ensureOptionContainer(id = "abcd", sub_id = "a")
"a" %in% ls(res)

## Clean up //
options(".abcd" = NULL)

##------------------------------------------------------------------------------
## Not hidden //
##------------------------------------------------------------------------------

options("optionr" = NULL)
res <- ensureOptionContainer(hide = FALSE)
"optionr" %in% names(options())
identical(getOption("optionr"), res)

## Clean up //
options("optionr" = NULL)

##------------------------------------------------------------------------------
## Check for existing options of same name/ID //
##------------------------------------------------------------------------------

options(".optionr" = NULL)
res <- ensureOptionContainer()
try(ensureOptionContainer(check = TRUE))

## Clean up //
options(".optionr" = NULL)

##------------------------------------------------------------------------------
## As interface //
##------------------------------------------------------------------------------
  
id <- structure(list(id = "test"), class = "OptionContext.Test")
options("test" = NULL)

res <- ensureOptionContainer(id, hide = FALSE)
"test" %in% names(options())
identical(getOption(id$id), res)  

## Clean up //
options("test" = NULL)

}
