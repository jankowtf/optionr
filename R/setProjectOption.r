#' @title
#' Set Project Option (generic)
#'
#' @description 
#' Sets generic option inside an option container created via 
#' \code{\link[optionr]{initializeOptionContainer}} or any of its subcontainers.
#' 
#' @details
#' Values for \code{id} are expected to be of structure \code{a/b/c/.../z},
#' i.e. being a path-like identifier with a slash used as separator. 
#' The identifier is transformed to \code{a$b$c$...$z} and then in turn to a
#' valid \code{assign} expression: 
#' \code{assign("z", value = value, envir = getOptionContainer(...)$a$b$c$...)}.
#' 
#' @note
#' Note that if \code{id = "a/b/d"}, the function expects that there exists an 
#' environment at \code{getOption(...)$a$b} (checked via 
#' \code{getFreeOption(id = "a/b")}).
#'   	
#' @param id \strong{Signature argument}.
#'    Object containing path-like ID information.
#' @param value \strong{Signature argument}.
#'    Object containing value information.
#' @param where \strong{Signature argument}.
#'    Object containing information about the location of the option container
#'    that is to be used. Typically, this either corresponds to the name/ID
#'    of a package/package project or an instance of a custom class.    
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
#'    \code{\link[reactr]{setReactive}} or \code{\link[reactr]{setShinyReactive}}.
#'    \code{FALSE}: set regular/non-reactive option.
#'    Note that if \code{value = reactiveOption()}, \code{reactive} is automatically
#'    set to \code{TRUE}.
#' @param Further arguments to be passed along to subsequent functions.
#'    In particular: 
#'    \code{\link[reactr]{setShinyReactive}}.
#' @example inst/examples/setProjectOption.r
#' @seealso \code{
#'   	\link[optionr]{setProjectOption-character-method}
#' }
#' @template author
#' @template references
#' @import devtools
#' @export 
setGeneric(
  name = "setProjectOption",
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
    standardGeneric("setProjectOption")       
  }
)

#' @title
#' Set Project Option (char-any-miss)
#'
#' @description 
#' See generic: \code{\link[optionr]{setProjectOption}}
#'      
#' @inheritParams setProjectOption
#' @param id \code{\link{character}}.
#' @param value \code{\link{ANY}}.
#' @param where \code{\link{missing}}.
#' @return See method
#'    \code{\link{setProjectOption-char-any-char-method}}.
#' @example inst/examples/setProjectOption.r
#' @seealso \code{
#'    \link[optionr]{setProjectOption}
#' }
#' @template author
#' @template references
#' @aliases setProjectOption-char-any-miss-method
#' @export
setMethod(
  f = "setProjectOption", 
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
    
  setProjectOption(
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
#' Set Project Option (char-any-any)
#'
#' @description 
#' See generic: \code{\link[optionr]{setProjectOption}}
#'      
#' @inheritParams setProjectOption
#' @param id \code{\link{character}}.
#' @param value \code{\link{ANY}}.
#' @param where \code{\link{ANY}}.
#' @return See method
#'    \code{\link{setProjectOption-char-any-char-method}}.
#' @example inst/examples/setProjectOption.r
#' @seealso \code{
#'    \link[optionr]{setProjectOption}
#' }
#' @template author
#' @template references
#' @aliases setProjectOption-char-any-any-method
#' @import conditionr
#' @export
setMethod(
  f = "setProjectOption", 
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

  setFreeOption(
    id = file.path("options", id),
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
#' Set Project Option (char-any-char)
#'
#' @description 
#' See generic: \code{\link[optionr]{setProjectOption}}
#'   	 
#' @inheritParams setProjectOption
#' @param id \code{\link{character}}.
#' @param value \code{\link{ANY}}.
#' @param where \code{\link{character}}.
#' @return \code{\link{logical}}. \code{TRUE}.
#' @example inst/examples/setProjectOption.r
#' @seealso \code{
#'    \link[optionr]{setProjectOption}
#' }
#' @template author
#' @template references
#' @aliases setProjectOption-char-any-char-method
#' @import reactr
#' @export
setMethod(
  f = "setProjectOption", 
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
    
  setFreeOption(
    id = file.path("options", id),
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
