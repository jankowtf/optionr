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
#'    See \code{\link[base]{getOption}}.
#' @param strict \code{\link{logical}}. 
#'    \code{TRUE}: \code{id} pointing to a non-existing option triggers
#'    error; \code{FALSE}: \code{id} pointing to a non-existing option leads
#'    to return value \code{NULL}.
#' @template threedots
#' @example inst/examples/getAnywhereOption.r
#' @seealso \code{
#'   	\link[optionr]{getAnywhereOption-char-char-method},
#'     \link[optionr]{setAnywhereOption},
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
    strict = FALSE, 
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
#' @return \code{\link{ANY}}. Option value or for non-existing option 
#'    (i.e. wrong \code{id}): \code{NULL} if \code{strict = FALSE} and an error
#'    if \code{strict = TRUE}.
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

  if (!length(id)) {
    if (!strict) {
      out <- NULL
    } else {
      conditionr::signalCondition(
        condition = "InvalidOption",
        msg = c(
          Reason = "Empty ID"
        ),
        ns = "optionr",
        type = "error"
      )
    }
  } else {
    container <- getOptionContainer(id = where)
    envir_name <- "container"
# print(missing(default))    
#     if (missing(default)) {
#       path <- paste0("[[\"", gsub("/", "\"]][[\"", id), "\"]]")
#       expr <- paste0(envir_name, path)
#       out <- eval(parse(text = expr))  
#     } else {
      path <- if (grepl("^\\./", id) || dirname(id) != ".") {
        paste0("[[\"", gsub("/", "\"]][[\"", dirname(id)), "\"]]")
      }
      where <- eval(parse(text = paste0(envir_name, path)))
      if (  is.null(where) ||
            !exists(basename(id), envir = where, inherits = FALSE)) {
        out <- default
      } else {
        out <- get(basename(id), envir = where, inherits = FALSE)
      }
#     }

    if (is.null(out)) {
      if (!strict) {
        out <- out
      } else {
        conditionr::signalCondition(
          condition = "InvalidOption",
          msg = c(
            "Invalid option",
            Reason = "no such option",
            ID = id
          ),
          ns = "optionr",
          type = "error"
        )
      }
    }
  }
  
  return(out)
    
  }
)
