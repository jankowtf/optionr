\dontrun{

## Default `id` //  
container <- ensureOptionContainer(overwrite = TRUE)  
initializeMeta(where = container)
exists(".meta", container)

## Different `id` //
container <- ensureOptionContainer(overwrite = TRUE)  
initializeMeta(id = "META", where = container)
exists("META", container)

}
