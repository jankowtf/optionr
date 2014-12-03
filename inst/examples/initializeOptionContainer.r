\dontrun{

##------------------------------------------------------------------------------
## Default ID //
##------------------------------------------------------------------------------
  
## Make sure initial options are `NULL` //
options("optionr" = NULL)

id_hidden <- paste0(".", devtools::as.package(".")$package)
initializeOptionContainer()
opts <- getOption(id_hidden)
opts$options  
opts$.meta  
opts$.registry  

## Clean up //
options(id_hidden = NULL)

##------------------------------------------------------------------------------
## Explicit ID //
##------------------------------------------------------------------------------
  
id <- "test"
id_hidden <- paste0(".", id)
options(id = NULL)

initializeOptionContainer(id)
opts <- getOption(id_hidden)
opts$options  
opts$.meta  
opts$.registry

## Clean up //
options(id_hidden = NULL)

##------------------------------------------------------------------------------
## Sub ID //
##------------------------------------------------------------------------------
  
id <- "test"
id_hidden <- paste0(".", id)
sub_id <- "a"
options(id = NULL)

initializeOptionContainer(id = id, sub_id = sub_id)
opts <- getOption(id_hidden)
"a" %in% ls(opts)
opts[[sub_id]]$options  
opts[[sub_id]]$.meta  
opts[[sub_id]]$.registry

## Clean up //
options(id_hidden = NULL)
  
##------------------------------------------------------------------------------
## Partial 
##------------------------------------------------------------------------------

id <- "test"
id_hidden <- paste0(".", id)
options(id_hidden = NULL)
initializeOptionContainer(id, components = "options")
opts <- getOption(id_hidden)
opts$options  
opts$.meta
## --> not created
opts$.registry
## --> not created

initializeOptionContainer(id, components = c("options", ".meta"), 
  overwrite = TRUE)
opts <- getOption(id_hidden)
opts$options  
opts$.meta
opts$.registry
## --> not created

## Condition handling //
initializeOptionContainer(id, components = c("nonexisting", "options"), 
  overwrite = TRUE)
opts <- getOption(id_hidden)
ls(opts, all.names = TRUE)

## Clean up //
options(id_hidden = NULL)
  
##------------------------------------------------------------------------------
## As interface //
##------------------------------------------------------------------------------

## Example of how custom classes can be used for creating custom methods //
id <- structure(list(id = "test"), class = "OptionContext.Test")
id
id_hidden <- paste0(".", id$id)

options(id_hidden = NULL)
initializeOptionContainer(id)
## --> calls `initializeProjectOptions()`, `initializeMeta()` and `initializeRegistry()`
## and for each of which methods for signature ID can be defined

getOption(id_hidden)$options  
getOption(id_hidden)$.meta  
getOption(id_hidden)$.registry  
options(id_hidden = NULL)

}
