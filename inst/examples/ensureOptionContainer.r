\dontrun{

##------------------------------------------------------------------------------
## Default `id` //
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
