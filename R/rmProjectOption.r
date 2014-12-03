#' @title
#' Remove Project Option (generic)
#'
#' @description 
#' Removes project option from the respective environment containing project 
#' option information (see \code{\link[optionr]{initializeOptionContainer}}).
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
#' @param sub_id \code{\link{character}}.
#'    Optional ID for a sub layer. Useful for a hub-like option container 
#'    structure.
#' @param strict \code{\link{logical}}.
#'     Controls what happens when \code{id} points to a non-existing option object:
#'    \itemize{
#'       \item{0: }{ignore and return \code{FALSE} to signal that the 
#' 				assignment process was not successful or \code{fail_value} depending
#' 				on the value of \code{return_status}} 
#' 			\item{1: }{ignore and with warning and return \code{FALSE}}
#' 			\item{2: }{ignore and with error}
#'   	}
#' @template threedots
#' @example inst/examples/rmProjectOption.r
#' @seealso \code{
#'   	\link[optionr]{rmProjectOption-char-char-method},
#'     \link[optionr]{setProjectOption},
#'     \link[optionr]{getProjectOption},
#'     \link[optionr]{existsProjectOption}
#' }
#' @template author
#' @template references
#' @import devtools
#' @export 
setGeneric(
  name = "rmProjectOption",
  signature = c(
    "id",
    "where"
  ),
  def = function(
    id,
    where = tryCatch(devtools::as.package(".")$package, error = function(cond) {
      stop("Invalid default value for `where`")
    }),
    sub_id = character(),
    strict = c(0, 1, 2), 
    ...
  ) {
    standardGeneric("rmProjectOption")       
  }
)

#' @title
#' Remove Project Option (char-miss)
#'
#' @description 
#' See generic: \code{\link[optionr]{rmProjectOption}}
#'      
#' @inheritParams rmProjectOption
#' @param id \code{\link{character}}.
#' @param where \code{\link{missing}}.
#' @return See method 
#'    \code{\link[optionr]{rmProjectOption-char-char-method}}
#' @example inst/examples/rmProjectOption.r
#' @seealso \code{
#'    \link[optionr]{rmProjectOption}
#' }
#' @template author
#' @template references
#' @aliases rmProjectOption-char-miss-method
#' @export
setMethod(
  f = "rmProjectOption", 
  signature = signature(
    id = "character",
    where = "missing"
  ), 
  definition = function(
    id,
    where,
    sub_id,
    strict,
    ...
  ) {
 
  rmProjectOption(
    id = id,
    where = where,
    sub_id = sub_id,
    strict = strict,
    ...
  )    
    
  }
)

#' @title
#' Remove Project Option (char-any)
#'
#' @description 
#' See generic: \code{\link[optionr]{rmProjectOption}}
#'      
#' @inheritParams rmProjectOption
#' @param id \code{\link{character}}.
#' @param where \code{\link{ANY}}.
#' @return See method 
#'    \code{\link[optionr]{rmProjectOption-char-any-method}}
#' @example inst/examples/rmProjectOption.r
#' @seealso \code{
#'    \link[optionr]{rmProjectOption}
#' }
#' @template author
#' @template references
#' @aliases rmProjectOption-char-any-method
#' @import conditionr
#' @export
setMethod(
  f = "rmProjectOption", 
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
        Reason = "cannot determine value of `where`"
      ),
      ns = "optionr",
      type = "error"
    )
  }        
    
  rmProjectOption(
    id = id,
    where = where$id,
    strict = strict,
    ...
  )    
    
  }
)

#' @title
#' Remove Project Option (char-env)
#'
#' @description 
#' See generic: \code{\link[optionr]{rmProjectOption}}
#'      
#' @inheritParams rmProjectOption
#' @param id \code{\link{character}}.
#' @param where \code{\link{environment}}.
#' @return See method 
#'    \code{\link[optionr]{rmProjectOption-char-char-method}}
#' @example inst/examples/rmProjectOption.r
#' @seealso \code{
#'    \link[optionr]{rmProjectOption}
#' }
#' @template author
#' @template references
#' @aliases rmProjectOption-char-env-method
#' @import conditionr
#' @export
setMethod(
  f = "rmProjectOption", 
  signature = signature(
    id = "character",
    where = "environment"
  ), 
  definition = function(
    id,
    where,
    sub_id,
    strict,
    ...
  ) {
 
  sub_id <- as.character(sub_id)
  rmAnywhereOption(
    id = if (!length(sub_id)) {
      file.path("options", id)
    } else {
      file.path(sub_id, "options", id)
    },
    where = where,
    strict = strict,
    ...
  )    
    
  }
)

#' @title
#' Remove Project Option (char-char)
#'
#' @description 
#' See generic: \code{\link[optionr]{rmProjectOption}}
#'   	 
#' @inheritParams rmProjectOption
#' @param id \code{\link{character}}.
#' @param where \code{\link{character}}.
#' @return \code{\link{ANY}}. Component value or for invalid argument input 
#' 		and non-existing component the value of \code{default} unless 
#' 		\code{strict == 1} in which case a warning is issued or
#' 		\code{strict == 2} in which case an error is thrown.
#' @example inst/examples/rmProjectOption.r
#' @seealso \code{
#'    \link[optionr]{rmProjectOption}
#' }
#' @template author
#' @template references
#' @aliases rmProjectOption-char-miss-method
#' @export
setMethod(
  f = "rmProjectOption", 
  signature = signature(
    id = "character",
    where = "character"
  ), 
  definition = function(
    id,
    where,
    sub_id,
    strict,
    ...
  ) {

  sub_id <- as.character(sub_id)
  rmAnywhereOption(
    id = if (!length(sub_id)) {
      file.path("options", id)
    } else {
      file.path(sub_id, "options", id)
    },
    where = where,
    strict = strict,
    ...
  ) 
    
  }
)
