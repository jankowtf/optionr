#' @title
#' Get Anywhere Option (generic)
#'
#' @description 
#' Retrieves option from anywhere inside an option container 
#' based on a path-like identifier (e.g. \code{"container/subcontainer/option_name"}.
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
#' @param default \code{\link{ANY}}. 
#'    Value to be returned if option does not exist. 
#'    See \code{\link[base]{getOption}} and \code{\link[nestr]{getNested}}.
#' @param strict \code{\link{logical}}.
#'     Controls what happens when \code{id} points to a non-existing option object:
#'    \itemize{
#'   		\item{0: }{ignore and return \code{FALSE} to signal that the 
#' 				assignment process was not successful or \code{fail_value} depending
#' 				on the value of \code{return_status}} 
#' 			\item{1: }{ignore and with warning and return \code{FALSE}}
#' 			\item{2: }{ignore and with error}
#'   	}
#' @template threedots
#' @example inst/examples/getAnywhereOption.r
#' @seealso \code{
#'   	\link[optionr]{getAnywhereOption-char-char-method},
#'     \link[optionr]{setAnywhereOption},
#'     \link[optionr]{existsAnywhereOption},
#'     \link[optionr]{rmAnywhereOption}
#' }
#' @template author
#' @template references
#' @import devtools
#' @export 
setGeneric(
  name = "getAnywhereOption",
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
    strict = c(0, 1, 2), 
    ...
  ) {
    standardGeneric("getAnywhereOption")       
  }
)

#' @title
#' Get Anywhere Option (char-miss)
#'
#' @description 
#' See generic: \code{\link[optionr]{getAnywhereOption}}
#'      
#' @inheritParams getAnywhereOption
#' @param id \code{\link{character}}.
#' @param where \code{\link{missing}}.
#' @return See method 
#'    \code{\link[optionr]{getAnywhereOption-char-char-method}}
#' @example inst/examples/getAnywhereOption.r
#' @seealso \code{
#'    \link[optionr]{getAnywhereOption}
#' }
#' @template author
#' @template references
#' @aliases getAnywhereOption-char-miss-method
#' @export
setMethod(
  f = "getAnywhereOption", 
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
 
  getAnywhereOption(
    id = id,
    where = where,
    default = default,
    strict = strict,
    ...
  )    
    
  }
)

#' @title
#' Get Anywhere Option (char-any)
#'
#' @description 
#' See generic: \code{\link[optionr]{getAnywhereOption}}
#'      
#' @inheritParams getAnywhereOption
#' @param id \code{\link{character}}.
#' @param where \code{\link{ANY}}.
#' @return See method 
#'    \code{\link[optionr]{getAnywhereOption-char-char-method}}
#' @example inst/examples/getAnywhereOption.r
#' @seealso \code{
#'    \link[optionr]{getAnywhereOption}
#' }
#' @template author
#' @template references
#' @aliases getAnywhereOption-char-any-method
#' @import conditionr
#' @export
setMethod(
  f = "getAnywhereOption", 
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
 
  if (is.null(where$id)) {
    conditionr::signalCondition(
      condition = "MissingIdField",
      msg = c(
        Reason = "name/ID field is missing, can not determine determine parent option"
      ),
      ns = "optionr",
      type = "error"
    )
  }        
    
  getAnywhereOption(
    id = id,
    where = where$id,
    default = default,
    strict = strict,
    ...
  )    
    
  }
)

#' @title
#' Get Anywhere Option (char-char)
#'
#' @description 
#' See generic: \code{\link[optionr]{getAnywhereOption}}
#'   	 
#' @inheritParams getAnywhereOption
#' @param id \code{\link{character}}.
#' @param where \code{\link{character}}.
#' @return \code{\link{ANY}}. Component value or for invalid argument input 
#' 		and non-existing component the value of \code{default} unless 
#' 		\code{strict == 1} in which case a warning is issued or
#' 		\code{strict == 2} in which case an error is thrown.
#' @example inst/examples/getAnywhereOption.r
#' @seealso \code{
#'    \link[optionr]{getAnywhereOption}
#' }
#' @template author
#' @template references
#' @aliases getAnywhereOption-char-miss-method
#' @export
setMethod(
  f = "getAnywhereOption", 
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

  container <- ensureOptionContainer(id = where, check = FALSE)  
  getNested(
    id = id, 
    where = container, 
    default = default,
    strict = strict
  )  
    
  }
)
