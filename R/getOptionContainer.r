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
#'   	\link[rapp]{getOptionContainer-missing-method}
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
#' Ensure Option Container (miss-miss)
#'
#' @description 
#' See generic: \code{\link[rapp]{getOptionContainer}}
#'   	 
#' @inheritParams getOptionContainer
#' @param id \code{\link{missing}}.
#' @return \code{\link{logical}}. \code{TRUE}.
#' @example inst/examples/getOptionContainer.r
#' @seealso \code{
#'    \link[rapp]{getOptionContainer}
#' }
#' @template author
#' @template references
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
#' Ensure Option Container (any-miss)
#'
#' @description 
#' See generic: \code{\link[rapp]{getOptionContainer}}
#'      
#' @inheritParams getOptionContainer
#' @param id \code{\link{ANY}}.
#' @return \code{\link{logical}}. \code{TRUE}.
#' @example inst/examples/getOptionContainer.r
#' @seealso \code{
#'    \link[rapp]{getOptionContainer}
#' }
#' @template author
#' @template references
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
    
  getOptionContainer(
    id = id,
    hidden = hidden,
    ...
  )
  
  }
)

#' @title
#' Ensure Option Container (any-env)
#'
#' @description 
#' See generic: \code{\link[rapp]{getOptionContainer}}
#'      
#' @inheritParams getOptionContainer
#' @param id \code{\link{ANY}}.
#' @return \code{\link{logical}}. \code{TRUE}.
#' @example inst/examples/getOptionContainer.r
#' @seealso \code{
#'    \link[rapp]{getOptionContainer}
#' }
#' @template author
#' @template references
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
#' Ensure Option Container (char-env)
#'
#' @description 
#' See generic: \code{\link[rapp]{getOptionContainer}}
#'      
#' @inheritParams getOptionContainer
#' @param id \code{\link{character}}.
#' @return \code{\link{logical}}. \code{TRUE}.
#' @example inst/examples/getOptionContainer.r
#' @seealso \code{
#'    \link[rapp]{getOptionContainer}
#' }
#' @template author
#' @template references
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
