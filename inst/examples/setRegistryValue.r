\dontrun{

##------------------------------------------------------------------------------
## Default location of option container //
##------------------------------------------------------------------------------

## NOTE
## Obmitting `where` (and `id` in `initializeOptionContainer()`) in the all 
## calls will assign the overall option container to the package/project 
## name (if `hidden = TRUE` it is additionally prepended by a `.`)

initializeOptionContainer(overwrite = TRUE)
setRegistryValue(id = "x_1", value = 10)
getRegistryValue(id = "x_1")

##------------------------------------------------------------------------------
## Explicit location of option container //
##------------------------------------------------------------------------------

initializeOptionContainer(id = "test", overwrite = TRUE)
setRegistryValue(id = "x_1", value = 10, where = "test")
getRegistryValue(id = "x_1", where = "test")

##------------------------------------------------------------------------------
## Sub ID //
##------------------------------------------------------------------------------

initializeOptionContainer(sub_id = "a", overwrite = TRUE)
setRegistryValue(id = "x_1", value = 10, sub_id = "a")
getRegistryValue(id = "x_1", sub_id = "a")

}
