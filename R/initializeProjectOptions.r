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
#' @example inst/examples/initializeProjectOptions.r
#' @seealso \code{
#'   	\link[optionr]{initializeProjectOptions-char-env-method},
#'    \link[optionr]{setProjectOptions},
#'    \link[optionr]{getProjectOptions},
#'    \link[optionr]{rmProjectOptions}
#' }
#' @template author
#' @template references
setGeneric(
  name = "initializeProjectOptions",
  signature = c(
    "id",
    "where"
  ),
  def = function(
    id = "options",
    where,
    ...
  ) {
    standardGeneric("initializeProjectOptions")       
  }
)

#' @title
#' Initialize Options (miss-miss)
#'
#' @description 
#' See generic: \code{\link[optionr]{initializeProjectOptions}}
#'      
#' @inheritParams initializeProjectOptions
#' @param id \code{\link{missing}}.
#' @param where \code{\link{missing}}.
#' @return See method
#'    \code{\link[optionr]{initializeProjectOptions-char-env-method}}.
#' @example inst/examples/initializeProjectOptions.r
#' @seealso \code{
#'    \link[optionr]{initializeProjectOptions}
#' }
#' @template author
#' @template references
#' @aliases initializeProjectOptions-miss-miss-method
#' @export
setMethod(
  f = "initializeProjectOptions", 
  signature = signature(
    id = "missing",
    where= "missing"
  ), 
  definition = function(
    id,
    where,
    ...
  ) {
    
  return(initializeProjectOptions(
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
#' See generic: \code{\link[optionr]{initializeProjectOptions}}
#'      
#' @inheritParams initializeProjectOptions
#' @param id \code{\link{missing}}.
#' @param where \code{\link{environment}}.
#' @return See method
#'    \code{\link[optionr]{initializeProjectOptions-char-env-method}}.
#' @example inst/examples/initializeProjectOptions.r
#' @seealso \code{
#'    \link[optionr]{initializeProjectOptions}
#' }
#' @template author
#' @template references
#' @aliases initializeProjectOptions-miss-env-method
#' @export
setMethod(
  f = "initializeProjectOptions", 
  signature = signature(
    id = "missing",
    where= "environment"
  ), 
  definition = function(
    id,
    where,
    ...
  ) {
    
  return(initializeProjectOptions(
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
#' See generic: \code{\link[optionr]{initializeProjectOptions}}
#'      
#' @inheritParams initializeProjectOptions
#' @param id \code{\link{ANY}}.
#' @param where \code{\link{environment}}.
#' @return \code{\link{environment}}. Environment containing the options.
#' @example inst/examples/initializeProjectOptions.r
#' @seealso \code{
#'    \link[optionr]{initializeProjectOptions}
#' }
#' @template author
#' @template references
#' @aliases initializeProjectOptions-any-env-method
#' @export
setMethod(
  f = "initializeProjectOptions", 
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
#' See generic: \code{\link[optionr]{initializeProjectOptions}}
#'      
#' @inheritParams initializeProjectOptions
#' @param id \code{\link{character}}.
#' @param where \code{\link{environment}}.
#' @return \code{\link{environment}}. Environment containing the options.
#' @example inst/examples/initializeProjectOptions.r
#' @seealso \code{
#'    \link[optionr]{initializeProjectOptions}
#' }
#' @template author
#' @template references
#' @aliases initializeProjectOptions-char-env-method
#' @export
setMethod(
  f = "initializeProjectOptions", 
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

