#' @title
#' Initialize Option Container (generic)
#'
#' @description 
#' Convenience function to initialize the option container that is then stored 
#' as an R option according to the name/ID provided by \code{id}.
#'   	
#' @param id \strong{Signature argument}.
#'    Object containing suitable information to control the distinct creation
#'    process.
#'    In the simplest case, this corresponds
#'    to the name/ID of a package/package project. But it can also be an 
#'    instance of a class for which methods for 
#'    \code{\link[optionr]{initializeOptions}}, 
#'    \code{\link[optionr]{initializeMeta}} and 
#'    \code{\link[optionr]{initializeRegistry}} exist.
#' @param check \code{\link{logical}}.
#'    \code{TRUE}: check if an R option with name/ID according to the information
#'    in \code{id} already exists (in which case an error is thrown); 
#'    \code{FALSE}: no check for existing R options.
#'    Note that \code{overwrite} will overrule \code{check}.
#' @param hidden \code{\link{logical}}.
#'    \code{TRUE}: make sure name/ID information in \code{id} 
#'    is preprended with a dot to hide it; 
#'    \code{FALSE}: use name/ID information in \code{id} as is.
#'    The former reduces the risk of accidentially overwriting existing R 
#'    options and thus is used by default.
#' @param overwrite \code{\link{logical}}.
#'    \code{TRUE}: overwrite existing container;
#'    \code{FALSE}: create only if no container exists yet.
#' @template threedots
#' @example inst/examples/initializeOptionContainer.r
#' @seealso \code{
#'   	\link[optionr]{initializeOptionContainer-NULL-method},
#'     \link[optionr]{getRegistry}
#' }
#' @template author
#' @template references
setGeneric(
  name = "initializeOptionContainer",
  signature = c(
    "id"
  ),
  def = function(
    id = tryCatch(devtools::as.package(".")$package, error = function(cond) {
      stop("Invalid default value for `id`")
    }),
    components = c("options", ".meta", ".registry"),
    check = TRUE,
    hidden = TRUE,
    overwrite = FALSE,
    ...
  ) {
    standardGeneric("initializeOptionContainer")       
  }
)

#' @title
#' Initialize Option Container (miss)
#'
#' @description 
#' See generic: \code{\link[optionr]{initializeOptionContainer}}
#'      
#' @inheritParams initializeOptionContainer
#' @param id \code{\link{missing}}.
#' @return See method
#'    \code{\link[optionr]{initializeOptionContainer-NULL-method}}.
#' @example inst/examples/initializeOptionContainer.r
#' @seealso \code{
#'    \link[optionr]{initializeOptionContainer-NULL-method},
#'     \link[optionr]{getRegistry}
#' }
#' @template author
#' @template references
#' @export
#' @aliases initializeOptionContainer-missing-method
setMethod(
  f = "initializeOptionContainer", 
  signature = signature(
    id = "missing"
  ), 
  definition = function(
    id,
    components,
    check,
    hidden,
    overwrite,
    ...
  ) {
    
  return(initializeOptionContainer(
    id = id,
    components = components,
    check = check,
    hidden = hidden,
    overwrite = overwrite,
    ...
  ))    
  
  }
)

#' @title
#' Initialize Option Container (any)
#'
#' @description 
#' See generic: \code{\link[optionr]{initializeOptionContainer}}
#'      
#' @inheritParams initializeOptionContainer
#' @param id \code{\link{ANY}}.
#' @return \code{\link{environment}}. The option container.
#' @example inst/examples/initializeOptionContainer.r
#' @seealso \code{
#'    \link[optionr]{initializeOptionContainer},
#'     \link[optionr]{getRegistry}
#' }
#' @template author
#' @template references
#' @aliases initializeOptionContainer-any-method
#' @import conditionr
#' @export
setMethod(
  f = "initializeOptionContainer", 
  signature = signature(
    id = "ANY"
  ), 
  definition = function(
    id,
    components,
    check,
    hidden,
    overwrite,
    ...
  ) {
  
  components <- match.arg(components, c("options", ".meta", ".registry"),
    several = TRUE)    
  
  if (is.null(id$id)) {
    conditionr::signalCondition(
      condition = "MissingIdField",
      msg = c(
        Reason = "name/ID field is missing, can not determine determine parent option"
      ),
      ns = "optionr",
      type = "error"
    )
  }
  
  id_char_0 <- id$id
  id_char <- id_char_0
  if (hidden) {
    id_char <- paste0(".", id$id)
  } 
    
  out <- if (is.null(getOption(id_char))) {
    opts <- ensureOptionContainer(id = id, check = check, hidden = hidden)
    if ("options" %in% components) {
      initializeOptions(id = id, where = opts)
    }
    if (".meta" %in% components) {
      initializeMeta(id = id, where = opts)
    }
    if (".registry" %in% components) {
      initializeRegistry(id = id, where = opts)
    }
    opts
  } else {
    ## This ensures that pass-by-reference still works as the same environment
    ## object is re-used again //
    if (overwrite) {
      opts <- getOption(id_char)  
      rm(list = ls(opts, all.names = TRUE), envir = opts)
      if ("options" %in% components) {
        initializeOptions(id = id, where = opts)
      }
      if (".meta" %in% components) {
        initializeMeta(id = id, where = opts)
      }
      if (".registry" %in% components) {
        initializeRegistry(id = id, where = opts)
      }
      opts
    } else {
      getOption(id_char)  
    }
  }
  out
    
  }
)

#' @title
#' Initialize Option Container (char)
#'
#' @description 
#' See generic: \code{\link[optionr]{initializeOptionContainer}}
#'      
#' @inheritParams initializeOptionContainer
#' @param id \code{\link{character}}.
#' @return \code{\link{environment}}. The option container.
#' @example inst/examples/initializeOptionContainer.r
#' @seealso \code{
#'    \link[optionr]{initializeOptionContainer},
#'     \link[optionr]{getRegistry}
#' }
#' @template author
#' @template references
#' @export
#' @aliases initializeOptionContainer-character-method
setMethod(
  f = "initializeOptionContainer", 
  signature = signature(
    id = "character"
  ), 
  definition = function(
    id,
    components,
    check,
    hidden,
    overwrite,
    ...
  ) {
  
  components <- match.arg(components, c("options", ".meta", ".registry"),
    several.ok = TRUE)    
    
  id_0 <- id
  if (hidden) {
    id <- paste0(".", id)
  } 
  
  out <- if (is.null(getOption(id))) {
    opts <- ensureOptionContainer(id = id_0, check = check, hidden = hidden)
    if ("options" %in% components) {
      initializeOptions(where = opts)
    }
    if (".meta" %in% components) {
      initializeMeta(where = opts)
    }
    if (".registry" %in% components) {
      initializeRegistry(where = opts)
    }
    opts
  } else {
    ## This ensures that pass-by-reference still works as the same environment
    ## object is re-used again //
    if (overwrite) {
      opts <- getOption(id)  
      rm(list = ls(opts, all.names = TRUE), envir = opts)
      if ("options" %in% components) {
        initializeOptions(where = opts)
      }
      if (".meta" %in% components) {
        initializeMeta(where = opts)
      }
      if (".registry" %in% components) {
        initializeRegistry(where = opts)
      }
      opts
    } else {
      getOption(id)  
    }
  }
  out
    
  }
)

