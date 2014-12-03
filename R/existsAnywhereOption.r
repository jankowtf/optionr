#' @title
#' Check Existence of an Anywhere Option (generic)
#'
#' @description 
#' Checks if an option exists anywhere inside of an option container
#' based on a path-like \code{id} with the last ID component being the 
#' actual object name that the function looks for.
#' See \code{\link[optionr]{getOptionContainer}}.
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
#'     Controls what happens when \code{id} points to a non-existing option object:
#'    \itemize{
#'   		\item{0: }{ignore and return \code{FALSE} to signal that the 
#' 				assignment process was not successful or \code{fail_value} depending
#' 				on the value of \code{return_status}} 
#' 			\item{1: }{ignore and with warning and return \code{FALSE}}
#' 			\item{2: }{ignore and with error}
#'   	}
#' @template threedots
#' @example inst/examples/existsAnywhereOption.r
#' @seealso \code{
#'   	\link[optionr]{existsAnywhereOption-char-char-method},
#'     \link[optionr]{setAnywhereOption},
#'     \link[optionr]{getAnywhereOption},
#'     \link[optionr]{rmAnywhereOption}
#' }
#' @template author
#' @template references
#' @import devtools
#' @export 
setGeneric(
  name = "existsAnywhereOption",
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
    standardGeneric("existsAnywhereOption")       
  }
)

#' @title
#' Check Existence of an Anywhere Option (char-miss)
#'
#' @description 
#' See generic: \code{\link[optionr]{existsAnywhereOption}}
#'      
#' @inheritParams existsAnywhereOption
#' @param id \code{\link{character}}.
#' @param where \code{\link{missing}}.
#' @return See method 
#'    \code{\link[optionr]{existsAnywhereOption-char-char-method}}
#' @example inst/examples/existsAnywhereOption.r
#' @seealso \code{
#'    \link[optionr]{existsAnywhereOption}
#' }
#' @template author
#' @template references
#' @aliases existsAnywhereOption-char-miss-method
#' @export
setMethod(
  f = "existsAnywhereOption", 
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
 
  existsAnywhereOption(
    id = id,
    where = where,
    strict = strict,
    ...
  )    
    
  }
)

#' @title
#' Check Existence of an Anywhere Option (char-any)
#'
#' @description 
#' See generic: \code{\link[optionr]{existsAnywhereOption}}
#'      
#' @inheritParams existsAnywhereOption
#' @param id \code{\link{character}}.
#' @param where \code{\link{ANY}}.
#' @return See method 
#'    \code{\link[optionr]{existsAnywhereOption-char-env-method}}
#' @example inst/examples/existsAnywhereOption.r
#' @seealso \code{
#'    \link[optionr]{existsAnywhereOption}
#' }
#' @template author
#' @template references
#' @aliases existsAnywhereOption-char-any-method
#' @import conditionr
#' @export
setMethod(
  f = "existsAnywhereOption", 
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
 
  if (is.null(where$id)) {
    conditionr::signalCondition(
      condition = "Invalid",
      msg = c(
        Reason = "cannot determine value for `where`"
      ),
      ns = "optionr",
      type = "error"
    )
  }        
    
  existsAnywhereOption(
    id = id,
    where = where$id,
    strict = strict,
    ...
  )  
    
  }
)

#' @title
#' Check Existence of an Anywhere Option (char-env)
#'
#' @description 
#' See generic: \code{\link[optionr]{existsAnywhereOption}}
#'      
#' @inheritParams existsAnywhereOption
#' @param id \code{\link{character}}.
#' @param where \code{\link{environment}}.
#' @return See method 
#'    \code{\link[optionr]{existsAnywhereOption-char-env-method}}
#' @example inst/examples/existsAnywhereOption.r
#' @seealso \code{
#'    \link[optionr]{existsAnywhereOption}
#' }
#' @template author
#' @template references
#' @aliases existsAnywhereOption-char-env-method
#' @import conditionr
#' @export
setMethod(
  f = "existsAnywhereOption", 
  signature = signature(
    id = "character",
    where = "environment"
  ), 
  definition = function(
    id,
    where,
    strict,
    ...
  ) {
      
  existsNested(
    id = id, 
    where = where, 
    strict = strict
  ) 
    
  }
)

#' @title
#' Check Existence of an Anywhere Option (char-char)
#'
#' @description 
#' See generic: \code{\link[optionr]{existsAnywhereOption}}
#'   	 
#' @inheritParams existsAnywhereOption
#' @param id \code{\link{character}}.
#' @param where \code{\link{character}}.
#' @return \code{\link{ANY}}. Component value or for invalid argument input 
#' 		and non-existing component the value of \code{default} unless 
#' 		\code{strict == 1} in which case a warning is issued or
#' 		\code{strict == 2} in which case an error is thrown.
#' @example inst/examples/existsAnywhereOption.r
#' @seealso \code{
#'    \link[optionr]{existsAnywhereOption}
#' }
#' @template author
#' @template references
#' @aliases existsAnywhereOption-char-miss-method
#' @export
setMethod(
  f = "existsAnywhereOption", 
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

  container <- ensureOptionContainer(id = where, check = FALSE)  
  existsNested(
    id = id, 
    where = container, 
    strict = strict
  )  
    
  }
)
