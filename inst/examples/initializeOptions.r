\dontrun{

## Default `id` //  
container <- ensureOptionContainer(overwrite = TRUE)  
initializeOptions(where = container)
exists("options", container)

## Different `id` //
container <- ensureOptionContainer(overwrite = TRUE)  
initializeOptions(id = "OPTIONS", where = container)
exists("OPTIONS", container)

}
