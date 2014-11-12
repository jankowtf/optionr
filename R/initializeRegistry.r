#' @title
#' Initialize Registry (generic)
#'
#' @description 
#' Convenience function to initialize a registry environment inside an 
#' option container.
#'   	
#' @param id \strong{Signature argument}.
#'    Object containing ID information.
#' @param where \strong{Signature argument}.
#'    Object containing location information.
#' @template threedots
#' @example inst/examples/initializeRegistry.r
#' @seealso \code{
#'   	\link[optionr]{initializeRegistry-char-env-method},
#'    \link[optionr]{setRegistryValue},
#'    \link[optionr]{getRegistryValue},
#'    \link[optionr]{rmRegistryValue}
#' }
#' @template author
#' @template references
setGeneric(
  name = "initializeRegistry",
  signature = c(
    "id",
    "where"
  ),
  def = function(
    id = ".registry",
    where,
    ...
  ) {
    standardGeneric("initializeRegistry")       
  }
)

#' @title
#' Initialize Registry (miss-miss)
#'
#' @description 
#' See generic: \code{\link[optionr]{initializeRegistry}}
#'      
#' @inheritParams initializeRegistry
#' @param id \code{\link{missing}}.
#' @param where \code{\link{missing}}.
#' @return See method
#'    \code{\link[optionr]{initializeRegistry-char-env-method}}.
#' @example inst/examples/initializeRegistry.r
#' @seealso \code{
#'    \link[optionr]{initializeRegistry}
#' }
#' @template author
#' @template references
#' @aliases initializeRegistry-miss-miss-method
#' @export
setMethod(
  f = "initializeRegistry", 
  signature = signature(
    id = "missing",
    where= "missing"
  ), 
  definition = function(
    id,
    where,
    ...
  ) {
    
  return(initializeRegistry(
    id = id,
    where = where,
    ...
  ))    
  
  }
)

#' @title
#' Initialize Registry (miss-envir)
#'
#' @description 
#' See generic: \code{\link[optionr]{initializeRegistry}}
#'      
#' @inheritParams initializeRegistry
#' @param id \code{\link{missing}}.
#' @param where \code{\link{environment}}.
#' @return See method
#'    \code{\link[optionr]{initializeRegistry-char-env-method}}.
#' @example inst/examples/initializeRegistry.r
#' @seealso \code{
#'    \link[optionr]{initializeRegistry}
#' }
#' @template author
#' @template references
#' @aliases initializeRegistry-miss-env-method
#' @export
setMethod(
  f = "initializeRegistry", 
  signature = signature(
    id = "missing",
    where= "environment"
  ), 
  definition = function(
    id,
    where,
    ...
  ) {
    
  return(initializeRegistry(
    id = id,
    where = where,
    ...
  ))    
  
  }
)

#' @title
#' Initialize Registry (any-envir)
#'
#' @description 
#' See generic: \code{\link[optionr]{initializeRegistry}}
#'      
#' @inheritParams initializeRegistry
#' @param id \code{\link{ANY}}.
#' @param where \code{\link{environment}}.
#' @return \code{\link{environment}}. Initialized registry.
#' @example inst/examples/initializeRegistry.r
#' @seealso \code{
#'    \link[optionr]{initializeRegistry}
#' }
#' @template author
#' @template references
#' @aliases initializeRegistry-any-env-method
#' @export
setMethod(
  f = "initializeRegistry", 
  signature = signature(
    id = "ANY",
    where= "environment"
  ), 
  definition = function(
    id,
    where,
    ...
  ) {
    
  assign(".registry", new.env(parent = emptyenv()), envir = where)
    
  }
)

#' @title
#' Initialize Registry (char-envir)
#'
#' @description 
#' See generic: \code{\link[optionr]{initializeRegistry}}
#'      
#' @inheritParams initializeRegistry
#' @param id \code{\link{character}}.
#' @param where \code{\link{environment}}.
#' @return \code{\link{environment}}. Initialized registry.
#' @example inst/examples/initializeRegistry.r
#' @seealso \code{
#'    \link[optionr]{initializeRegistry},
#'     \link[optionr]{getRegistry}
#' }
#' @template author
#' @template references
#' @aliases initializeRegistry-char-env-method
#' @export
setMethod(
  f = "initializeRegistry", 
  signature = signature(
    id = "character",
    where= "environment"
  ), 
  definition = function(
    id,
    where,
    ...
  ) {
  
  assign(id, new.env(parent = emptyenv()), envir = where)
    
  }
)

