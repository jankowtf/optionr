#' @title
#' Get Option Container (generic)
#'
#' @description 
#' Retrieves the \code{environment} that serves as an option
#' container from the list of R options. 
#'   	
#' @param id \strong{Signature argument}.
#'    Object containing suitable information to control the actual retrieval
#'    process.
#'    In the simplest case, this corresponds
#'    to the name/ID of a package/package project. But it can also be an 
#'    instance of a custom class for which a suitable methods are defined.
#' @param hidden \code{\link{logical}}.
#'    \code{TRUE}: make sure name/ID information in \code{id} 
#'    is preprended with a dot as the container was assigned with \code{hidden = TRUE}; 
#'    \code{FALSE}: use name/ID information in \code{id} as is.
#' @template threedots
#' @example inst/examples/getOptionContainer.r
#' @seealso \code{
#'   	\link[optionr]{getOptionContainer-char-method},
#'     \link[optionr]{ensureOptionContainer},
#'     \link[optionr]{initializeOptionContainer}
#' }
#' @template author
#' @template references
#' @import devtools
#' @export 
setGeneric(
  name = "getOptionContainer",
  signature = c(
    "id"
  ),
  def = function(
    id = tryCatch(devtools::as.package(".")$package, error = function(cond) {
      stop("Invalid default value for `id`")
    }),
    hidden = TRUE,
    ...
  ) {
    standardGeneric("getOptionContainer")       
  }
)

#' @title
#' Get Option Container (miss)
#'
#' @description 
#' See generic: \code{\link[optionr]{getOptionContainer}}
#'   	 
#' @inheritParams getOptionContainer
#' @param id \code{\link{missing}}.
#' @return \code{\link{logical}}. \code{TRUE}.
#' @example inst/examples/getOptionContainer.r
#' @seealso \code{
#'    \link[optionr]{getOptionContainer}
#' }
#' @template author
#' @template references
#' @aliases getOptionContainer-miss-method
#' @export
setMethod(
  f = "getOptionContainer", 
  signature = signature(
    id = "missing"
  ), 
  definition = function(
    id,
    hidden,
    ...
  ) {
    
  getOptionContainer(
    id = id,
    hidden = hidden,
    ...
  )
  
  }
)

#' @title
#' Get Option Container (any)
#'
#' @description 
#' See generic: \code{\link[optionr]{getOptionContainer}}
#'      
#' @inheritParams getOptionContainer
#' @param id \code{\link{ANY}}.
#' @return \code{\link{logical}}. \code{TRUE}.
#' @example inst/examples/getOptionContainer.r
#' @seealso \code{
#'    \link[optionr]{getOptionContainer}
#' }
#' @template author
#' @template references
#' @aliases getOptionContainer-any-method
#' @import conditionr
#' @export
setMethod(
  f = "getOptionContainer", 
  signature = signature(
    id = "ANY"
  ), 
  definition = function(
    id,
    hidden,
    ...
  ) {
  
  if (is.null(id$id)) {
    conditionr::signalCondition(
      condition = "MissingIdField",
      msg = c(
        Reason = "name/ID field is missing"
      ),
      ns = "optionr",
      type = "error"
    )
  }    
    
  if (hidden) {
    id$id <- paste0(".", id$id)
  }   
  return(getOption(id$id))
    
  }
)

#' @title
#' Get Option Container (char)
#'
#' @description 
#' See generic: \code{\link[optionr]{getOptionContainer}}
#'      
#' @inheritParams getOptionContainer
#' @param id \code{\link{character}}.
#' @return \code{\link{logical}}. \code{TRUE}.
#' @example inst/examples/getOptionContainer.r
#' @seealso \code{
#'    \link[optionr]{getOptionContainer}
#' }
#' @template author
#' @template references
#' @aliases getOptionContainer-char-method
#' @import conditionr
#' @export
setMethod(
  f = "getOptionContainer", 
  signature = signature(
    id = "character"
  ), 
  definition = function(
    id,
    hidden,
    ...
  ) {
    
  if (hidden) {
    id <- paste0(".", id)
  }    
  return(getOption(id))
  
  }
)
