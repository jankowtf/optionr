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
#' @param strict \code{\link{logical}}. 
#'    \code{TRUE}: the following constellations trigger an error:
#'    \itemize{
#'        \item{\code{id} pointing to a non-existing option}
#'        \item{empty \code{id}}
#'    }
#'    \code{FALSE}: the stated constellations lead to the return value 
#'    being \code{FALSE}.
#' @template threedots
#' @example inst/examples/rmProjectOption.r
#' @seealso \code{
#'   	\link[optionr]{rmProjectOption-char-char-method},
#'     \link[optionr]{setProjectOption},
#'     \link[optionr]{getProjectOption}
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
    strict = FALSE, 
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
    strict,
    ...
  ) {
 
  rmProjectOption(
    id = id,
    where = where,
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
#'    \code{\link[optionr]{rmProjectOption-char-char-method}}
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
 
  rmAnywhereOption(
    id = file.path("options", id),
    where = where$id,
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
#' @return \code{\link{ANY}}. Option value or for non-existing option 
#'    (i.e. wrong \code{id}): \code{NULL} if \code{strict = FALSE} and an error
#'    if \code{strict = TRUE}.
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
    strict,
    ...
  ) {

  rmAnywhereOption(
    id = file.path("options", id),
    where = where,
    strict = strict,
    ...
  ) 
    
  }
)
