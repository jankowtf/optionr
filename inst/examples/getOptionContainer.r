\dontrun{

##------------------------------------------------------------------------------
## Default `id` //
##------------------------------------------------------------------------------

res <- ensureOptionContainer(overwrite = TRUE)
identical(getOptionContainer(), res)
options(".optionr" = NULL)
  
##------------------------------------------------------------------------------
## Hidden //
##------------------------------------------------------------------------------

res <- ensureOptionContainer(hidden = FALSE, overwrite = TRUE)
identical(getOptionContainer(hidden = FALSE), res)
options("optionr" = NULL)
  
##------------------------------------------------------------------------------
## As interface //
##------------------------------------------------------------------------------
  
id <- structure(list(id = "test"), class = "OptionContext.Test")
res <- ensureOptionContainer(id, hidden = FALSE, overwrite = TRUE)
identical(getOptionContainer(id, hidden = FALSE), res)
options("test" = NULL)
  
}
