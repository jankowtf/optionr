#' @title
#' Get Generic Option (generic)
#'
#' @description 
#' Retrieves option from the an option container or
#' any of it subcontainers based on a path-like identifier 
#' (e.g. \code{"container/subcontainer/option_name"}.
#' 
#' @details
#' Values for \code{id} are expected to be of structure \code{a/b/c/.../z},
#' i.e. being a path-like identifier with a slash used as separator. 
#' The identifier is transformed to \code{a$b$c$...$z} and then in turn to a
#' valid \emph{get} expression (\code{getOptionContainer(...)$a$b$c$...$z}).
#'   	
#' @param id \strong{Signature argument}.
#'    Object containing path-like name/ID information.
#' @param where \strong{Signature argument}.
#'    Object containing information about the location of the option container
#'    that is to be used. Typically, this either corresponds to the name/ID
#'    of a package/package project or an instance of a custom class.  
#' @param default \code{\link{ANY}}. 
#'    Value to be returned if option does not exist. 
#'    See \code{\link[base]{getOption}}.
#' @param strict \code{\link{logical}}. 
#'    \code{TRUE}: \code{id} pointing to a non-existing option triggers
#'    error; \code{FALSE}: \code{id} pointing to a non-existing option leads
#'    to return value \code{NULL}.
#' @template threedots
#' @example inst/examples/getFreeOption.r
#' @seealso \code{
#'   	\link[optionr]{getFreeOption-character-method}
#' }
#' @template author
#' @template references
#' @import devtools
#' @export 
setGeneric(
  name = "getFreeOption",
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
    standardGeneric("getFreeOption")       
  }
)

#' @title
#' Get Generic Option (char-miss)
#'
#' @description 
#' See generic: \code{\link[optionr]{getFreeOption}}
#'      
#' @inheritParams getFreeOption
#' @param id \code{\link{character}}.
#' @param where \code{\link{missing}}.
#' @return See method 
#'    \code{\link[optionr]{getFreeOption-char-char-method}}
#' @example inst/examples/getFreeOption.r
#' @seealso \code{
#'    \link[optionr]{getFreeOption}
#' }
#' @template author
#' @template references
#' @aliases getFreeOption-char-miss-method
#' @export
setMethod(
  f = "getFreeOption", 
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
 
  getFreeOption(
    id = id,
    where = where,
    default = default,
    strict = strict,
    ...
  )    
    
  }
)

#' @title
#' Get Generic Option (char-any)
#'
#' @description 
#' See generic: \code{\link[optionr]{getFreeOption}}
#'      
#' @inheritParams getFreeOption
#' @param id \code{\link{character}}.
#' @param where \code{\link{ANY}}.
#' @return See method 
#'    \code{\link[optionr]{getFreeOption-char-char-method}}
#' @example inst/examples/getFreeOption.r
#' @seealso \code{
#'    \link[optionr]{getFreeOption}
#' }
#' @template author
#' @template references
#' @aliases getFreeOption-char-any-method
#' @import conditionr
#' @export
setMethod(
  f = "getFreeOption", 
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
    
  getFreeOption(
    id = id,
    where = where$id,
    default = default,
    strict = strict,
    ...
  )    
    
  }
)

#' @title
#' Get Generic Option (char-char)
#'
#' @description 
#' See generic: \code{\link[optionr]{getFreeOption}}
#'   	 
#' @inheritParams getFreeOption
#' @param id \code{\link{character}}.
#' @param where \code{\link{character}}.
#' @return \code{\link{ANY}}. Option value or for non-existing option 
#'    (i.e. wrong \code{id}): \code{NULL} if \code{strict = FALSE} and an error
#'    if \code{strict = TRUE}.
#' @example inst/examples/getFreeOption.r
#' @seealso \code{
#'    \link[optionr]{getFreeOption}
#' }
#' @template author
#' @template references
#' @aliases getFreeOption-char-miss-method
#' @export
setMethod(
  f = "getFreeOption", 
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
        ns = "rapp",
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
          ns = "rapp",
          type = "error"
        )
      }
    }
  }
  
  return(out)
    
  }
)
