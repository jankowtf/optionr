#' @title
#' Initialize Options (generic)
#'
#' @description 
#' Convenience function to initialize a package options environment below an 
#' option container.
#'   	
#' @param id \strong{Signature argument}.
#'    Object containing ID information.
#' @param where \strong{Signature argument}.
#'    Object containing location information.
#' @template threedots
#' @example inst/examples/initializeOptions.r
#' @seealso \code{
#'   	\link[reactr]{initializeOptions-NULL-method},
#'     \link[reactr]{getRegistry}
#' }
#' @template author
#' @template references
setGeneric(
  name = "initializeOptions",
  signature = c(
    "id",
    "where"
  ),
  def = function(
    id = "options",
    where,
    ...
  ) {
    standardGeneric("initializeOptions")       
  }
)

#' @title
#' Initialize Options (miss-miss)
#'
#' @description 
#' See generic: \code{\link[reactr]{initializeOptions}}
#'      
#' @inheritParams initializeOptions
#' @param id \code{\link{missing}}.
#' @param where \code{\link{missing}}.
#' @return See method
#'    \code{\link[reactr]{initializeOptions-NULL-method}}.
#' @example inst/examples/initializeOptions.r
#' @seealso \code{
#'    \link[reactr]{initializeOptions-NULL-method},
#'     \link[reactr]{getRegistry}
#' }
#' @template author
#' @template references
#' @export
#' @aliases initializeOptions-missing-missing-method
setMethod(
  f = "initializeOptions", 
  signature = signature(
    id = "missing",
    where= "missing"
  ), 
  definition = function(
    id,
    where,
    ...
  ) {
    
  return(initializeOptions(
    id = id,
    where = where,
    ...
  ))    
  
  }
)

#' @title
#' Initialize Options (miss-envir)
#'
#' @description 
#' See generic: \code{\link[reactr]{initializeOptions}}
#'      
#' @inheritParams initializeOptions
#' @param id \code{\link{missing}}.
#' @param where \code{\link{environment}}.
#' @return See method
#'    \code{\link[reactr]{initializeOptions-NULL-method}}.
#' @example inst/examples/initializeOptions.r
#' @seealso \code{
#'    \link[reactr]{initializeOptions-NULL-method},
#'     \link[reactr]{getRegistry}
#' }
#' @template author
#' @template references
#' @export
#' @aliases initializeOptions-missing-environment-method
setMethod(
  f = "initializeOptions", 
  signature = signature(
    id = "missing",
    where= "environment"
  ), 
  definition = function(
    id,
    where,
    ...
  ) {
    
  return(initializeOptions(
    id = id,
    where = where,
    ...
  ))    
  
  }
)

#' @title
#' Initialize Options (any-envir)
#'
#' @description 
#' See generic: \code{\link[reactr]{initializeOptions}}
#'      
#' @inheritParams initializeOptions
#' @param id \code{\link{ANY}}.
#' @param where \code{\link{environment}}.
#' @return \code{\link{environment}}. Environment containing the options.
#' @example inst/examples/initializeOptions.r
#' @seealso \code{
#'    \link[reactr]{initializeOptions},
#'     \link[reactr]{getRegistry}
#' }
#' @template author
#' @template references
#' @aliases initializeOptions-any-environment-method
#' @export
setMethod(
  f = "initializeOptions", 
  signature = signature(
    id = "ANY",
    where= "environment"
  ), 
  definition = function(
    id,
    where,
    ...
  ) {
  
  assign("options", new.env(parent = emptyenv()), envir = where)
    
  }
)

#' @title
#' Initialize Options (char-envir)
#'
#' @description 
#' See generic: \code{\link[reactr]{initializeOptions}}
#'      
#' @inheritParams initializeOptions
#' @param id \code{\link{character}}.
#' @param where \code{\link{environment}}.
#' @return \code{\link{environment}}. Environment containing the options.
#' @example inst/examples/initializeOptions.r
#' @seealso \code{
#'    \link[reactr]{initializeOptions},
#'     \link[reactr]{getRegistry}
#' }
#' @template author
#' @template references
#' @export
#' @aliases initializeOptions-character-environment-method
setMethod(
  f = "initializeOptions", 
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

