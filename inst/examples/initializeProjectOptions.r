\dontrun{

## Default `id` //  
container <- ensureOptionContainer(overwrite = TRUE)  
initializeProjectOptions(where = container)
exists("options", container)

## Different `id` //
container <- ensureOptionContainer(overwrite = TRUE)  
initializeProjectOptions(id = "OPTIONS", where = container)
exists("OPTIONS", container)

}
