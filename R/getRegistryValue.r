#' @title
#' Get Registry Value (generic)
#'
#' @description 
#' Retrieves registry value from the respective environment for registry 
#' information.
#' 
#' @details
#' Values for \code{id} are expected to be of structure \code{a/b/c/.../z},
#' i.e. being a path-like identifier with a slash used as separator. 
#' The identifier is transformed to \code{a$b$c$...$z} and then in turn to a
#' valid \emph{get} expression (\code{getRegistryValueContainer(...)$a$b$c$...$z}).
#'   	
#' @param id \strong{Signature argument}.
#'    Object containing path-like name/ID information.
#' @param where \strong{Signature argument}.
#'    Object containing information about the location of the option container
#'    that is to be used. Typically, this either corresponds to the name/ID
#'    of a package/package project or an instance of a custom class.  
#' @param default \code{\link{ANY}}. 
#'    Value to be returned if option does not exist. 
#'    See \code{\link[base]{getRegistryValue}}.
#' @param strict \code{\link{logical}}. 
#'    \code{TRUE}: \code{id} pointing to a non-existing option triggers
#'    error; \code{FALSE}: \code{id} pointing to a non-existing option leads
#'    to return value \code{NULL}.
#' @template threedots
#' @example inst/examples/getRegistryValue.r
#' @seealso \code{
#'   	\link[optionr]{getRegistryValue-character-method}
#' }
#' @template author
#' @template references
#' @import devtools
#' @export 
setGeneric(
  name = "getRegistryValue",
  signature = c(
    "id",
    "where"
  ),
  def = function(
    id,
    where = tryCatch(devtools::as.package(".")$package, error = function(cond) {
      stop("Invalid default value for `where`")
    }),
    default = NULL,
    strict = FALSE, 
    ...
  ) {
    standardGeneric("getRegistryValue")       
  }
)

#' @title
#' Get Option (char-miss)
#'
#' @description 
#' See generic: \code{\link[optionr]{getRegistryValue}}
#'      
#' @inheritParams getRegistryValue
#' @param id \code{\link{character}}.
#' @param where \code{\link{missing}}.
#' @return See method 
#'    \code{\link[optionr]{getRegistryValue-char-char-method}}
#' @example inst/examples/getRegistryValue.r
#' @seealso \code{
#'    \link[optionr]{getRegistryValue}
#' }
#' @template author
#' @template references
#' @aliases getRegistryValue-char-miss-method
#' @export
setMethod(
  f = "getRegistryValue", 
  signature = signature(
    id = "character",
    where = "missing"
  ), 
  definition = function(
    id,
    where,
    default,
    strict,
    ...
  ) {
 
  getRegistryValue(
    id = id,
    where = where,
    default = default,
    strict = strict,
    ...
  )    
    
  }
)

#' @title
#' Get Option (char-any)
#'
#' @description 
#' See generic: \code{\link[optionr]{getRegistryValue}}
#'      
#' @inheritParams getRegistryValue
#' @param id \code{\link{character}}.
#' @param where \code{\link{ANY}}.
#' @return See method 
#'    \code{\link[optionr]{getRegistryValue-char-char-method}}
#' @example inst/examples/getRegistryValue.r
#' @seealso \code{
#'    \link[optionr]{getRegistryValue}
#' }
#' @template author
#' @template references
#' @aliases getRegistryValue-char-any-method
#' @import conditionr
#' @export
setMethod(
  f = "getRegistryValue", 
  signature = signature(
    id = "character",
    where = "ANY"
  ), 
  definition = function(
    id,
    where,
    default,
    strict,
    ...
  ) {
 
  getFreeOption(
    id = file.path(".registry", id),
    where = where$id,
    default = default,
    strict = strict,
    ...
  )    
    
  }
)

#' @title
#' Get Option (char-char)
#'
#' @description 
#' See generic: \code{\link[optionr]{getRegistryValue}}
#'   	 
#' @inheritParams getRegistryValue
#' @param id \code{\link{character}}.
#' @param where \code{\link{character}}.
#' @return \code{\link{ANY}}. Option value or for non-existing option 
#'    (i.e. wrong \code{id}): \code{NULL} if \code{strict = FALSE} and an error
#'    if \code{strict = TRUE}.
#' @example inst/examples/getRegistryValue.r
#' @seealso \code{
#'    \link[optionr]{getRegistryValue}
#' }
#' @template author
#' @template references
#' @aliases getRegistryValue-char-miss-method
#' @export
setMethod(
  f = "getRegistryValue", 
  signature = signature(
    id = "character",
    where = "character"
  ), 
  definition = function(
    id,
    where,
    default,
    strict,
    ...
  ) {

  getFreeOption(
    id = file.path(".registry", id),
    where = where,
    default = default,
    strict = strict,
    ...
  ) 
    
  }
)
