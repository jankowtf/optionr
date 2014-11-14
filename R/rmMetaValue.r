#' @title
#' Remove Meta Value (generic)
#'
#' @description 
#' Removes meta value from the respective environment containing meta 
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
#'     Controls what happens when \code{id} points to a non-existing meta object:
#'    \itemize{
#'       \item{0: }{ignore and return \code{FALSE} to signal that the 
#' 				assignment process was not successful or \code{fail_value} depending
#' 				on the value of \code{return_status}} 
#' 			\item{1: }{ignore and with warning and return \code{FALSE}}
#' 			\item{2: }{ignore and with error}
#'   	}
#' @template threedots
#' @example inst/examples/rmMetaValue.r
#' @seealso \code{
#'   	\link[optionr]{rmMetaValue-char-char-method},
#'     \link[optionr]{setMetaValue},
#'     \link[optionr]{getMetaValue},
#'     \link[optionr]{existsMetaValue}
#' }
#' @template author
#' @template references
#' @import devtools
#' @export 
setGeneric(
  name = "rmMetaValue",
  signature = c(
    "id",
    "where"
  ),
  def = function(
    id,
    where = tryCatch(devtools::as.package(".")$package, error = function(cond) {
      stop("Invalid default value for `where`")
    }),
    strict = c(0, 1, 2), 
    ...
  ) {
    standardGeneric("rmMetaValue")       
  }
)

#' @title
#' Remove Meta Value (char-miss)
#'
#' @description 
#' See generic: \code{\link[optionr]{rmMetaValue}}
#'      
#' @inheritParams rmMetaValue
#' @param id \code{\link{character}}.
#' @param where \code{\link{missing}}.
#' @return See method 
#'    \code{\link[optionr]{rmMetaValue-char-char-method}}
#' @example inst/examples/rmMetaValue.r
#' @seealso \code{
#'    \link[optionr]{rmMetaValue}
#' }
#' @template author
#' @template references
#' @aliases rmMetaValue-char-miss-method
#' @export
setMethod(
  f = "rmMetaValue", 
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
 
  rmMetaValue(
    id = id,
    where = where,
    strict = strict,
    ...
  )    
    
  }
)

#' @title
#' Remove Meta Value (char-any)
#'
#' @description 
#' See generic: \code{\link[optionr]{rmMetaValue}}
#'      
#' @inheritParams rmMetaValue
#' @param id \code{\link{character}}.
#' @param where \code{\link{ANY}}.
#' @return See method 
#'    \code{\link[optionr]{rmMetaValue-char-char-method}}
#' @example inst/examples/rmMetaValue.r
#' @seealso \code{
#'    \link[optionr]{rmMetaValue}
#' }
#' @template author
#' @template references
#' @aliases rmMetaValue-char-any-method
#' @import conditionr
#' @export
setMethod(
  f = "rmMetaValue", 
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
    id = file.path(".meta", id),
    where = where$id,
    strict = strict,
    ...
  )    
    
  }
)

#' @title
#' Remove Meta Value (char-char)
#'
#' @description 
#' See generic: \code{\link[optionr]{rmMetaValue}}
#'   	 
#' @inheritParams rmMetaValue
#' @param id \code{\link{character}}.
#' @param where \code{\link{character}}.
#' @return \code{\link{ANY}}. Component value or for invalid argument input 
#' 		and non-existing component the value of \code{default} unless 
#' 		\code{strict == 1} in which case a warning is issued or
#' 		\code{strict == 2} in which case an error is thrown.
#' @example inst/examples/rmMetaValue.r
#' @seealso \code{
#'    \link[optionr]{rmMetaValue}
#' }
#' @template author
#' @template references
#' @aliases rmMetaValue-char-miss-method
#' @export
setMethod(
  f = "rmMetaValue", 
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
    id = file.path(".meta", id),
    where = where,
    strict = strict,
    ...
  ) 
    
  }
)
