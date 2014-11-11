#' @title
#' Initialize Runtime Options (generic)
#'
#' @description 
#' Ensures initial options as expected by the \code{rapp} framework and its 
#' associated packages. The options are stored as \code{".rapp"} in 
#' \code{\link[base]{options}}.
#'   	
#' @param ns \strong{Signature argument}.
#'    Object containing namespace information.
#' @template threedots
#' @example inst/examples/initializeRuntimeOptions.r
#' @seealso \code{
#'   	\link[rapp]{initializeRuntimeOptions-missing-method}
#' }
#' @template author
#' @template references
#' @export 
setGeneric(
  name = "initializeRuntimeOptions",
  signature = c(
    "ns"
  ),
  def = function(
    ns,
    ...
  ) {
    standardGeneric("initializeRuntimeOptions")       
  }
)

#' @title
#' Initialize Runtime Options (miss)
#'
#' @description 
#' See generic: \code{\link[rapp]{initializeRuntimeOptions}}
#'   	 
#' @inheritParams initializeRuntimeOptions
#' @param ns \code{\link{missing}}. Default namespace.
#' @return \code{\link{environment}}. The options container as stored in 
#'    \code{options(".rapp")}.
#' @example inst/examples/initializeRuntimeOptions.r
#' @seealso \code{
#'    \link[rapp]{initializeRuntimeOptions}
#' }
#' @template author
#' @template references
#' @import reactr
#' @export
setMethod(
  f = "initializeRuntimeOptions", 
  signature = signature(
    ns = "missing"
  ), 
  definition = function(
    ns,
    ...
  ) {
    
  container <- ensureRappOptionsContainer(overwrite = TRUE)
  
  setRuntimeOption(
    id = "./id",
    value = NA_character_,
    branch_gap = TRUE,
    reactive = TRUE
  )
  setRuntimeOption(
    id = "./version",
    value = NA_character_,
    branch_gap = TRUE,
    reactive = TRUE
  )
  setRuntimeOption(
    id = "./global_dir",
    value = file.path(Sys.getenv("HOME"), "rapp"),
    branch_gap = TRUE,
    reactive = TRUE
  )
  setRuntimeOption(
    id = "./repos_root",
    value = NA_character_,
    reactive = TRUE
  )
  setRuntimeOption(
    id = "./repos_global",
    value = NA_character_,
    reactive = TRUE
  )
  setRuntimeOption(
    id = "./repos_pkgs",
    value = NA_character_,
    reactive = TRUE
  )
  setRuntimeOption(
    id = "./runtime_mode",
    value = "dev",
    branch_gap = TRUE,
    reactive = TRUE
  )
  
  ## Prime stuff //
  if (!isRapp()) {
    setRuntimeOption(
      id = "./ns_super",
      value = devtools::as.package(".")$package,
      branch_gap = TRUE,
      reactive = TRUE
    )
#     setRuntimeOption(
#       id = "./ns_super_type",
#       value = switch(!isRapp(), "TRUE" = "package", "FALSE" = "rapp"),
#       branch_gap = TRUE
#     )
    setRuntimeOption(
      id = "./wd_super",
      value = getwd(),
      branch_gap = TRUE,
      reactive = TRUE
    )
  }
  
  return(container)
    
  }
)
