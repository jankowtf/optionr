#' @title
#' Remove Registry Value (generic)
#'
#' @description 
#' Removes registry value from the respective environment containing registry
#' information (see \code{\link[optionr]{initializeOptionContainer}}).
#' 
#' @template path-like-ids
#'     
#' @param id \strong{Signature argument}.
#'    Object containing path-like name/ID information.
#' @param where \strong{Signature argument}.
#'    Object containing information about the location of the option container
#'    that is to be used. Typically, this either corresponds to the name/ID
#'    of a package/package project or an instance of a custom class for which
#'    suitable methods in the context of managing options are defined 
#'    (see other methods of this package that have signature arguments 
#'    \code{id} or \code{where}).  
#' @param strict \code{\link{logical}}. 
#'    \code{TRUE}: the following constellations trigger an error:
#'    \itemize{
#'        \item{\code{id} pointing to a non-existing option}
#'        \item{empty \code{id}}
#'    }
#'    \code{FALSE}: the stated constellations lead to the return value 
#'    being \code{FALSE}.
#' @template threedots
#' @example inst/examples/rmRegistryValue.r
#' @seealso \code{
#'   	\link[optionr]{rmRegistryValue-char-char-method},
#'     \link[optionr]{setRegistryValue},
#'     \link[optionr]{getRegistryValue}
#' }
#' @template author
#' @template references
#' @import devtools
#' @export 
setGeneric(
  name = "rmRegistryValue",
  signature = c(
    "id",
    "where"
  ),
  def = function(
    id,
    where = tryCatch(devtools::as.package(".")$package, error = function(cond) {
      stop("Invalid default value for `where`")
    }),
    strict = FALSE, 
    ...
  ) {
    standardGeneric("rmRegistryValue")       
  }
)

#' @title
#' Remove Registry Value (char-miss)
#'
#' @description 
#' See generic: \code{\link[optionr]{rmRegistryValue}}
#'      
#' @inheritParams rmRegistryValue
#' @param id \code{\link{character}}.
#' @param where \code{\link{missing}}.
#' @return See method 
#'    \code{\link[optionr]{rmRegistryValue-char-char-method}}
#' @example inst/examples/rmRegistryValue.r
#' @seealso \code{
#'    \link[optionr]{rmRegistryValue}
#' }
#' @template author
#' @template references
#' @aliases rmRegistryValue-char-miss-method
#' @export
setMethod(
  f = "rmRegistryValue", 
  signature = signature(
    id = "character",
    where = "missing"
  ), 
  definition = function(
    id,
    where,
    strict,
    ...
  ) {
 
  rmRegistryValue(
    id = id,
    where = where,
    strict = strict,
    ...
  )    
    
  }
)

#' @title
#' Remove Registry Value (char-any)
#'
#' @description 
#' See generic: \code{\link[optionr]{rmRegistryValue}}
#'      
#' @inheritParams rmRegistryValue
#' @param id \code{\link{character}}.
#' @param where \code{\link{ANY}}.
#' @return See method 
#'    \code{\link[optionr]{rmRegistryValue-char-char-method}}
#' @example inst/examples/rmRegistryValue.r
#' @seealso \code{
#'    \link[optionr]{rmRegistryValue}
#' }
#' @template author
#' @template references
#' @aliases rmRegistryValue-char-any-method
#' @import conditionr
#' @export
setMethod(
  f = "rmRegistryValue", 
  signature = signature(
    id = "character",
    where = "ANY"
  ), 
  definition = function(
    id,
    where,
    strict,
    ...
  ) {
 
  rmAnywhereOption(
    id = file.path(".registry", id),
    where = where$id,
    strict = strict,
    ...
  )    
    
  }
)

#' @title
#' Remove Registry Value (char-char)
#'
#' @description 
#' See generic: \code{\link[optionr]{rmRegistryValue}}
#'   	 
#' @inheritParams rmRegistryValue
#' @param id \code{\link{character}}.
#' @param where \code{\link{character}}.
#' @return \code{\link{ANY}}. Option value or for non-existing option 
#'    (i.e. wrong \code{id}): \code{NULL} if \code{strict = FALSE} and an error
#'    if \code{strict = TRUE}.
#' @example inst/examples/rmRegistryValue.r
#' @seealso \code{
#'    \link[optionr]{rmRegistryValue}
#' }
#' @template author
#' @template references
#' @aliases rmRegistryValue-char-miss-method
#' @export
setMethod(
  f = "rmRegistryValue", 
  signature = signature(
    id = "character",
    where = "character"
  ), 
  definition = function(
    id,
    where,
    strict,
    ...
  ) {

  rmAnywhereOption(
    id = file.path(".registry", id),
    where = where,
    strict = strict,
    ...
  ) 
    
  }
)
