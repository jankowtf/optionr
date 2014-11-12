#' @title
#' setRegistryValue (generic)
#'
#' @description 
#' Sets registry value inside the respective environment for registry information.
#' 
#' @template path-like-ids
#'   	
#' @param id \strong{Signature argument}.
#'    Object containing path-like ID information.
#' @param value \strong{Signature argument}.
#'    Object containing value information.
#' @param where \strong{Signature argument}.
#'    Object containing information about the location of the option container
#'    that is to be used. Typically, this either corresponds to the name/ID
#'    of a package/package project or an instance of a custom class for which
#'    suitable methods in the context of managing options are defined 
#'    (see other methods of this package that have signature arguments 
#'    \code{id} or \code{where}).  
#' @param must_exist \code{\link{logical}}. 
#'    \code{TRUE}: \code{id} pointing to a non-existing option either triggers
#'    an error or results in return value \code{FALSE} (depending on \code{strict}); 
#'    \code{FALSE}: option that \code{id} points to is set.
#' @param typed \code{\link{logical}}. 
#'    Implies that \code{must_exist} is automatically set to \code{TRUE}.
#'    \code{TRUE}: \code{class(value)} must match the class of the existing 
#'    option value; 
#'    \code{FALSE}: option that \code{id} points to is set without class check.
#' @param force \code{\link{logical}}. 
#'    \code{TRUE}: when \code{dirname(id)} points to a \emph{leaf} instead of a 
#'    \emph{branch} (i.e. \code{dirname(id)} is not an \code{environment}), 
#'    overwrite it to turn it into a branch;
#'    \code{FALSE}: either return with \code{FALSE} or throw error in such cases
#'    (depending on value of \code{strict}); 
#' @param gap \code{\link{logical}}. 
#'    \code{TRUE}: when \code{dirname(id)} points to a non-existing parent
#'    branch or if there are any missing branches in the path tree, 
#'    then auto-create all missing branches; 
#'    \code{FALSE}: either return with \code{FALSE} or throw error in such cases
#'    (depending on \code{strict}); 
#' @param strict \code{\link{logical}}. 
#'    \code{TRUE}: \code{id} pointing to a non-existing option triggers
#'    error; \code{FALSE}: \code{id} pointing to a non-existing option leads
#'    to return value \code{NULL}.
#' @param reactive \code{\link{logical}}. 
#'    \code{TRUE}: set reactive option via 
#'    \code{\link[optionr]{setReactive}} or \code{\link[optionr]{setShinyReactive}}.
#'    \code{FALSE}: set regular/non-reactive option.
#'    Note that if \code{value = reactiveOption()}, \code{reactive} is automatically
#'    set to \code{TRUE}.
#' @param Further arguments to be passed along to subsequent functions.
#'    In particular: 
#'    \code{\link[optionr]{setShinyReactive}}.
#' @example inst/examples/setRegistryValue.r
#' @seealso \code{
#'   	\link[optionr]{setRegistryValue-char-any-char-method},
#'     \link[optionr]{getRegistryValue},
#'     \link[optionr]{rmRegistryValue}
#' }
#' @template author
#' @template references
#' @import devtools
#' @export 
setGeneric(
  name = "setRegistryValue",
  signature = c(
    "id",
    "value",
    "where"
  ),
  def = function(
    id,
    value,
    where = tryCatch(devtools::as.package(".")$package, error = function(cond) {
      stop("Invalid default value for `where`")
    }),
    must_exist = FALSE, 
    typed = FALSE,
    force = FALSE,
    gap = FALSE,
    strict = FALSE,
    reactive = FALSE,
    ...
  ) {
    standardGeneric("setRegistryValue")       
  }
)

#' @title
#' setRegistryValue (char-any-miss)
#'
#' @description 
#' See generic: \code{\link[optionr]{setRegistryValue}}
#'      
#' @inheritParams setRegistryValue
#' @param id \code{\link{character}}.
#' @param value \code{\link{ANY}}.
#' @param where \code{\link{missing}}.
#' @return See method
#'    \code{\link{setRegistryValue-char-any-char-method}}.
#' @example inst/examples/setRegistryValue.r
#' @seealso \code{
#'    \link[optionr]{setRegistryValue}
#' }
#' @template author
#' @template references
#' @aliases setRegistryValue-char-any-miss-method
#' @export
setMethod(
  f = "setRegistryValue", 
  signature = signature(
    id = "character",
    value = "ANY",
    where = "missing"
  ), 
  definition = function(
    id,
    value,
    where,
    must_exist,
    typed,
    force,
    gap,
    strict,
    reactive,
    ...
  ) {
    
  setRegistryValue(
    id = id,
    value = value,
    where = where,
    must_exist = must_exist,
    typed = typed,
    force = force,
    gap = gap,
    strict = strict,
    reactive = reactive,
    ...
  )    
    
  }
)

#' @title
#' setRegistryValue (char-any-any)
#'
#' @description 
#' See generic: \code{\link[optionr]{setRegistryValue}}
#'      
#' @inheritParams setRegistryValue
#' @param id \code{\link{character}}.
#' @param value \code{\link{ANY}}.
#' @param where \code{\link{ANY}}.
#' @return See method
#'    \code{\link{setRegistryValue-char-any-char-method}}.
#' @example inst/examples/setRegistryValue.r
#' @seealso \code{
#'    \link[optionr]{setRegistryValue}
#' }
#' @template author
#' @template references
#' @aliases setRegistryValue-char-any-any-method
#' @import conditionr
#' @export
setMethod(
  f = "setRegistryValue", 
  signature = signature(
    id = "character",
    value = "ANY",
    where = "ANY"
  ), 
  definition = function(
    id,
    value,
    where,
    must_exist,
    typed,
    force,
    gap,
    strict,
    reactive,
    ...
  ) {

  setAnywhereOption(
    id = file.path(".registry", id),
    value = value,
    where = where$id,
    must_exist = must_exist,
    typed = typed,
    force = force,
    gap = gap,
    strict = strict,
    reactive = reactive,
    ...
  )    
    
  }
)

#' @title
#' setRegistryValue (char-any-char)
#'
#' @description 
#' See generic: \code{\link[optionr]{setRegistryValue}}
#'   	 
#' @inheritParams setRegistryValue
#' @param id \code{\link{character}}.
#' @param value \code{\link{ANY}}.
#' @param where \code{\link{character}}.
#' @return \code{\link{logical}}. \code{TRUE}.
#' @example inst/examples/setRegistryValue.r
#' @seealso \code{
#'    \link[optionr]{setRegistryValue}
#' }
#' @template author
#' @template references
#' @aliases setRegistryValue-char-any-char-method
#' @import reactr
#' @export
setMethod(
  f = "setRegistryValue", 
  signature = signature(
    id = "character",
    value = "ANY",
    where = "character"
  ), 
  definition = function(
    id,
    value,
    where,
    must_exist,
    typed,
    force,
    gap,
    strict,
    reactive,
    ...
  ) {
    
  setAnywhereOption(
    id = file.path(".registry", id),
    value = value,
    where = where,
    must_exist = must_exist,
    typed = typed,
    force = force,
    gap = gap,
    strict = strict,
    reactive = reactive,
    ...
  )   
  
  }
)
