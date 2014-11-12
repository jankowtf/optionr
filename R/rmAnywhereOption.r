#' @title
#' Remove Free Option (generic)
#'
#' @description 
#' Removes an option from anywhere within option container or
#' any of it subcontainers based on a path-like identifier 
#' (e.g. \code{"container/subcontainer/option_name"}.
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
#' @example inst/examples/rmAnywhereOption.r
#' @seealso \code{
#'   	\link[optionr]{rmAnywhereOption-char-char-method},
#'     \link[optionr]{setAnywhereOption},
#'     \link[optionr]{getAnywhereOption}
#' }
#' @template author
#' @template references
#' @import devtools
#' @export 
setGeneric(
  name = "rmAnywhereOption",
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
    standardGeneric("rmAnywhereOption")       
  }
)

#' @title
#' Remove Free Option (char-miss)
#'
#' @description 
#' See generic: \code{\link[optionr]{rmAnywhereOption}}
#'      
#' @inheritParams rmAnywhereOption
#' @param id \code{\link{character}}.
#' @param where \code{\link{missing}}.
#' @return See method 
#'    \code{\link[optionr]{rmAnywhereOption-char-char-method}}
#' @example inst/examples/rmAnywhereOption.r
#' @seealso \code{
#'    \link[optionr]{rmAnywhereOption}
#' }
#' @template author
#' @template references
#' @aliases rmAnywhereOption-char-miss-method
#' @export
setMethod(
  f = "rmAnywhereOption", 
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
 
  rmAnywhereOption(
    id = id,
    where = where,
    strict = strict,
    ...
  )    
    
  }
)

#' @title
#' Remove Free Option (char-any)
#'
#' @description 
#' See generic: \code{\link[optionr]{rmAnywhereOption}}
#'      
#' @inheritParams rmAnywhereOption
#' @param id \code{\link{character}}.
#' @param where \code{\link{ANY}}.
#' @return See method 
#'    \code{\link[optionr]{rmAnywhereOption-char-char-method}}
#' @example inst/examples/rmAnywhereOption.r
#' @seealso \code{
#'    \link[optionr]{rmAnywhereOption}
#' }
#' @template author
#' @template references
#' @aliases rmAnywhereOption-char-any-method
#' @import conditionr
#' @export
setMethod(
  f = "rmAnywhereOption", 
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
      condition = "MissingIdField",
      msg = c(
        Reason = "name/ID field is missing, can not determine determine parent option"
      ),
      ns = "optionr",
      type = "error"
    )
  }        
    
  rmAnywhereOption(
    id = id,
    where = where$id,
    strict = strict,
    ...
  )    
    
  }
)

#' @title
#' Remove Free Option (char-char)
#'
#' @description 
#' See generic: \code{\link[optionr]{rmAnywhereOption}}
#'   	 
#' @inheritParams rmAnywhereOption
#' @param id \code{\link{character}}.
#' @param where \code{\link{character}}.
#' @return \code{\link{logical}}. 
#'    \code{TRUE}: removal successful;
#'    \code{FALSE}: removal failed.
#' @example inst/examples/rmAnywhereOption.r
#' @seealso \code{
#'    \link[optionr]{rmAnywhereOption}
#' }
#' @template author
#' @template references
#' @aliases rmAnywhereOption-char-miss-method
#' @export
setMethod(
  f = "rmAnywhereOption", 
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

  if (!length(id)) {
    if (!strict) {
      out <- FALSE
    } else {
      conditionr::signalCondition(
        condition = "RemovalFailed",
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

    path <- if (grepl("^\\./", id) || dirname(id) != ".") {
      paste0("[[\"", gsub("/", "\"]][[\"", dirname(id)), "\"]]")
    }
    where <- eval(parse(text = paste0(envir_name, path)))
    if (  is.null(where) ||
          !exists(basename(id), envir = where, inherits = FALSE)) {
      out <- NULL
    } else {
      rm(list = basename(id), envir = where, inherits = FALSE)
      out <- TRUE
    }
    if (is.null(out)) {
      if (!strict) {
        out <- FALSE
      } else {
        conditionr::signalCondition(
          condition = "RemovalFailed",
          msg = c(
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
