\dontrun{

## Default `id` //  
container <- ensureOptionContainer(overwrite = TRUE)  
initializeRegistry(where = container)
exists(".registry", container)

## Different `id` //
container <- ensureOptionContainer(overwrite = TRUE)  
initializeRegistry(id = "REGISTRY", where = container)
exists("REGISTRY", container)

}
