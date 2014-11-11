\dontrun{

##------------------------------------------------------------------------------
## Default `id` //
##------------------------------------------------------------------------------
  
## Make sure initial options are `NULL` //
options("optionr" = NULL)

initializeOptionContainer()
opts <- getOption("optionr")
opts$options  
opts$.meta  
opts$.registry  

## Clean up //
options("optionr" = NULL)

##------------------------------------------------------------------------------
## Explicit character `id` //
##------------------------------------------------------------------------------
  
id <- "test"
options(id = NULL)

initializeOptionContainer(id)
opts <- getOption(id)
opts$options  
opts$.meta  
opts$.registry

## Clean up //
options(id = NULL)
  
##------------------------------------------------------------------------------
## Partial 
##------------------------------------------------------------------------------

id <- "test"
options(id = NULL)
initializeOptionContainer(id, components = "options")
opts <- getOption(id)
opts$options  
opts$.meta
## --> not created
opts$.registry
## --> not created

initializeOptionContainer(id, components = c("options", ".meta"), 
  overwrite = TRUE)
opts <- getOption(id)
opts$options  
opts$.meta
opts$.registry
## --> not created

## Condition handling //
initializeOptionContainer(id, components = c("nonexisting", "options"), 
  overwrite = TRUE)
opts <- getOption(id)
ls(opts, all.names = TRUE)

## Clean up //
options(id = NULL)
  
##------------------------------------------------------------------------------
## As interface //
##------------------------------------------------------------------------------

## Example of how custom classes can be used for creating custom methods //
id <- structure(list(id = "test"), class = "OptionContext.Test")
id

options("test" = NULL)
initializeOptionContainer(id)
## --> calls `initializeOptions()`, `initializeMeta()` and `initializeRegistry()`
## and for each of which methods for signature `id` can be defined

getOption(id$id)$options  
getOption(id$id)$.meta  
getOption(id$id)$.registry  
options("test" = NULL)

}
