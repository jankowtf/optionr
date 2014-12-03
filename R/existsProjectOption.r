#' @title
#' Check Existence of Project Option (generic)
#'
#' @description 
#' Checks if a project option object exists inside of the respective environment 
#' for such information within an option container
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
#' @param sub_id \code{\link{character}}.
#'    Optional ID for a sub layer. Useful for a hub-like option container 
#'    structure. 
#' @param default \code{\link{ANY}}. 
#'    Value to be returned if option does not exist. 
#'    See \code{\link[base]{getOption}} and \code{\link[nestr]{getNested}}.
#' @param strict \code{\link{logical}}.
#'     Controls what happens when \code{id} points to a non-existing option object:
#'    \itemize{
#'     	\item{0: }{ignore and return \code{FALSE} to signal that the 
#' 				assignment process was not successful or \code{fail_value} depending
#' 				on the value of \code{return_status}} 
#' 			\item{1: }{ignore and with warning and return \code{FALSE}}
#' 			\item{2: }{ignore and with error}
#'   	}
#' @template threedots
#' @example inst/examples/existsProjectOption.r
#' @seealso \code{
#'   	\link[optionr]{existsProjectOption-char-char-method},
#'    \link[optionr]{setProjectOption},
#'    \link[optionr]{getProjectOption},
#'    \link[optionr]{rmProjectOption}
#' }
#' @template author
#' @template references
#' @import devtools
#' @export 
setGeneric(
  name = "existsProjectOption",
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
    default = NULL,
    strict = c(0, 1, 2), 
    ...
  ) {
    standardGeneric("existsProjectOption")       
  }
)

#' @title
#' Check Existence of Project Option (char-miss)
#'
#' @description 
#' See generic: \code{\link[optionr]{existsProjectOption}}
#'      
#' @inheritParams existsProjectOption
#' @param id \code{\link{character}}.
#' @param where \code{\link{missing}}.
#' @return See method 
#'    \code{\link[optionr]{existsProjectOption-char-char-method}}
#' @example inst/examples/existsProjectOption.r
#' @seealso \code{
#'    \link[optionr]{existsProjectOption}
#' }
#' @template author
#' @template references
#' @aliases existsProjectOption-char-miss-method
#' @export
setMethod(
  f = "existsProjectOption", 
  signature = signature(
    id = "character",
    where = "missing"
  ), 
  definition = function(
    id,
    where,
    sub_id,
    default,
    strict,
    ...
  ) {
 
  existsProjectOption(
    id = id,
    where = where,
    sub_id = sub_id,
    default = default,
    strict = strict,
    ...
  )    
    
  }
)

#' @title
#' Check Existence of Project Option (char-any)
#'
#' @description 
#' See generic: \code{\link[optionr]{existsProjectOption}}
#'      
#' @inheritParams existsMetaValuen
#' @param id \code{\link{character}}.
#' @param where \code{\link{ANY}}.
#' @return See method 
#'    \code{\link[optionr]{existsProjectOption-char-env-method}}
#' @example inst/examples/existsProjectOption.r
#' @seealso \code{
#'    \link[optionr]{existsProjectOption}
#' }
#' @template author
#' @template references
#' @aliases existsProjectOption-char-any-method
#' @import conditionr
#' @export
setMethod(
  f = "existsProjectOption", 
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
    
  existsProjectOption(
    id = id,
    where = where$id,
    strict = strict,
    ...
  )  
    
  }
)

#' @title
#' Check Existence of Project Option (char-env)
#'
#' @description 
#' See generic: \code{\link[optionr]{existsProjectOption}}
#'      
#' @inheritParams existsProjectOption
#' @param id \code{\link{character}}.
#' @param where \code{\link{environment}}.
#' @return See method 
#'    \code{\link[optionr]{existsProjectOption-char-char-method}}
#' @example inst/examples/existsProjectOption.r
#' @seealso \code{
#'    \link[optionr]{existsProjectOption}
#' }
#' @template author
#' @template references
#' @aliases existsProjectOption-char-env-method
#' @import conditionr
#' @export
setMethod(
  f = "existsProjectOption", 
  signature = signature(
    id = "character",
    where = "environment"
  ), 
  definition = function(
    id,
    where,
    sub_id,
    default,
    strict,
    ...
  ) {
 
  sub_id <- as.character(sub_id)    
  existsAnywhereOption(
    id = if (!length(sub_id)) {
      file.path("options", id)
    } else {
      file.path(sub_id, "options", id)
    },
    where = where,
    default = default,
    strict = strict,
    ...
  )    
    
  }
)

#' @title
#' Check Existence of Project Option (char-char)
#'
#' @description 
#' See generic: \code{\link[optionr]{existsProjectOption}}
#'   	 
#' @inheritParams existsProjectOption
#' @param id \code{\link{character}}.
#' @param where \code{\link{character}}.
#' @return \code{\link{ANY}}. Component value or for invalid argument input 
#' 		and non-existing component the value of \code{default} unless 
#' 		\code{strict == 1} in which case a warning is issued or
#' 		\code{strict == 2} in which case an error is thrown.
#' @example inst/examples/existsProjectOption.r
#' @seealso \code{
#'    \link[optionr]{existsProjectOption}
#' }
#' @template author
#' @template references
#' @aliases existsProjectOption-char-miss-method
#' @export
setMethod(
  f = "existsProjectOption", 
  signature = signature(
    id = "character",
    where = "character"
  ), 
  definition = function(
    id,
    where,
    sub_id,
    default,
    strict,
    ...
  ) {

  sub_id <- as.character(sub_id)    
  existsAnywhereOption(
    id = if (!length(sub_id)) {
      file.path("options", id)
    } else {
      file.path(sub_id, "options", id)
    },
    where = where,
    default = default,
    strict = strict,
    ...
  ) 
    
  }
)
