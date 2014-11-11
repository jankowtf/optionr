#' @title
#' Set Free Option (generic)
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
#' @example inst/examples/setFreeOption.r
#' @seealso \code{
#'   	\link[optionr]{setFreeOption-character-method}
#' }
#' @template author
#' @template references
#' @import devtools
#' @export 
setGeneric(
  name = "setFreeOption",
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
    standardGeneric("setFreeOption")       
  }
)

#' @title
#' Set Free Option (char-any-miss)
#'
#' @description 
#' See generic: \code{\link[optionr]{setFreeOption}}
#'      
#' @inheritParams setFreeOption
#' @param id \code{\link{character}}.
#' @param value \code{\link{ANY}}.
#' @param where \code{\link{missing}}.
#' @return See method
#'    \code{\link{setFreeOption-char-any-char-method}}.
#' @example inst/examples/setFreeOption.r
#' @seealso \code{
#'    \link[optionr]{setFreeOption}
#' }
#' @template author
#' @template references
#' @aliases setFreeOption-char-any-miss-method
#' @export
setMethod(
  f = "setFreeOption", 
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
    
  setFreeOption(
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
#' Set Free Option (char-any-any)
#'
#' @description 
#' See generic: \code{\link[optionr]{setFreeOption}}
#'      
#' @inheritParams setFreeOption
#' @param id \code{\link{character}}.
#' @param value \code{\link{ANY}}.
#' @param where \code{\link{ANY}}.
#' @return See method
#'    \code{\link{setFreeOption-char-any-char-method}}.
#' @example inst/examples/setFreeOption.r
#' @seealso \code{
#'    \link[optionr]{setFreeOption}
#' }
#' @template author
#' @template references
#' @aliases setFreeOption-char-any-any-method
#' @import conditionr
#' @export
setMethod(
  f = "setFreeOption", 
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
    
  setFreeOption(
    id = id,
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
#' Set Free Option (char-any-char)
#'
#' @description 
#' See generic: \code{\link[optionr]{setFreeOption}}
#'   	 
#' @inheritParams setFreeOption
#' @param id \code{\link{character}}.
#' @param value \code{\link{ANY}}.
#' @param where \code{\link{character}}.
#' @return \code{\link{logical}}. \code{TRUE}.
#' @example inst/examples/setFreeOption.r
#' @seealso \code{
#'    \link[optionr]{setFreeOption}
#' }
#' @template author
#' @template references
#' @aliases setFreeOption-char-any-char-method
#' @import reactr
#' @export
setMethod(
  f = "setFreeOption", 
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
    
  out <- TRUE
  container <- ensureOptionContainer(id = where, check = FALSE)
  envir_name <- "container"
  
  ## Adjustments //
#   if (typed) {
#     must_exist <- TRUE
#   }
  if (inherits(value, "ReactiveOption")) {
    reactive <- TRUE
  }
  
  ## Direct parent check //
  id_branch <- dirname(id)
  if (!grepl("^\\./", id) && id_branch == ".") {
    branch_value <- container
  } else {
    branch_value <- tryCatch(
      getFreeOption(id = id_branch, where = where, strict = FALSE),
      error = function(cond) {
        NULL
      }
    )
  }
  
  ## Handling branch gaps //
  if (is.null(branch_value)) {
    if (gap) {
      ## Check how much to fill //
      id_branch_spl <- unlist(strsplit(id_branch, split = "/"))
      id_branch_tree <- NULL
      expr_get <- NULL
      expr_set <- NULL
      for (ii in 1:length(id_branch_spl)) {
        expr_get <- c(expr_get, 
          paste0(envir_name, "[[\"", paste(id_branch_spl[1:ii], collapse = "\"]][[\""),
             "\"]]"))
        expr_set <- c(expr_set, 
          paste0(envir_name, "[[\"", paste(id_branch_spl[1:ii], collapse = "\"]][[\""),
          "\"]] <- new.env()"))
        id_branch_tree <- c(id_branch_tree, paste(id_branch_spl[1:ii], collapse = "/"))
      }
      
      ## Determine component types //
      ## * yes --> branch
      ## * no --> leaf or not existing
      ## * error --> error
      idx <- sapply(expr_get, function(ii) {
        tryCatch({
          tmp <- switch(
            as.character(inherits(eval(parse(text = ii)), "environment")),
            "TRUE" = "yes",
            "FALSE" = "no"
          )},
          error = function(cond) {
            "error"
          }
        )
      }) 

      ## Invalid branch(es) //
      if (any(idx == "no") & any(idx == "error")) {
        idx_no <- which(idx == "no")
        if (length(idx_no)) {
          if (force) {
          ## Ensure that leafs are transformed to branches //            
            setFreeOption(
              id = id_branch_tree[idx_no],
              value = new.env(),
              where = where
            )
            
            ## Update `idx` and `expr_set` //
            idx <- idx[-idx_no]
            expr_set <- expr_set[-idx_no]
            
            ## Remove error entry //
            idx[which(idx == "error")] <- "no"
          } else {
            if (!strict) {
              out <- FALSE
            } else {
              conditionr::signalCondition(
                condition = "InvalidBranchConstellation",
                msg = c(
                  "Parent branch is not an environment",
                  ID = id,
                  "ID branch" = id_branch_tree[idx_no]
                ),
                ns = "optionr",
                type = "error"
              )  
            }
          }
        }
      }
      
      ## Gap not-yet-existing branch(es) //
      idx_no <- which(idx == "no")
      if (out) {
        if (length(idx_no)) {
          run_scope <- idx_no[1]:length(expr_set)
#         } else {
#           run_scope <- 1:length(expr_set)
#         }
        
#         if (length(run_scope)) {
          sapply(run_scope, function(ii) {
            eval(parse(text = expr_set[ii]))
          })  
          branch_value <- getFreeOption(id = id_branch, 
            where = where, strict = FALSE)
        }
      }
    } else {
      if (!strict) {
        out <- FALSE
      } else {
        conditionr::signalCondition(
          condition = "InvalidBranchConstellation",
          msg = c(
            "Branch gap",
            ID = id
          ),
          ns = "optionr",
          type = "error"
        )
      }
    }
  }

  ## Early exit //
  if (!out) {
    return(out)
  }
  
  ## Parent branch is no environment //
  if (!inherits(branch_value, "environment")) {
    if (force) {
    ## Transform to branch //
      expr_set <- paste0(envir_name, "$", gsub("/", 
        "$", id_branch), " <- new.env()")
      eval(parse(text = expr_set))
    } else {
      if (!strict) {
        out <- FALSE
      } else {
        conditionr::signalCondition(
          condition = "InvalidBranchConstellation",
          msg = c(
            "Parent branch is not an environment",
            ID = id,
            "ID branch" = id_branch,
            "Class branch" = class(branch_value)
          ),
          ns = "optionr",
          type = "error"
        )
      }
    }
  }

  ## Early exit //
  if (!out) {
    return(out)
  }

  ## Must exist //
  if (must_exist) {
    if (!exists(basename(id), envir = branch_value, inherits = FALSE)) {
      if (!strict) {
        out <- FALSE
      } else {
        conditionr::signalCondition(
          condition = "OptionPrerequisitesNotMet",
          msg = c(
            "Option does not exist yet",
            ID = id
          ),
          ns = "optionr",
          type = "error"
        )
      }
    }
  }

  ## Early exit //
  if (!out) {
    return(out)
  }
  
  ## Match class for typed options //
## LEGACY, keep as reference
#   if (typed) {
#     value_0 <- get(basename(id), envir = branch_value, inherits = FALSE)
#     cl_0 <- class(value_0)
#     cl_1 <- class(value)
#     if (!inherits(value_0, cl_1)) {
#       if (!strict) {
#         out <- FALSE
#       } else {
#         conditionr::signalCondition(
#           condition = "OptionPrerequisitesNotMet",
#           msg = c(
#             "New value has invalid class",
#             ID = id,
#             "Class expected" = cl_0,
#             "Class provided" = cl_1
#           ),
#           ns = "optionr",
#           type = "error"
#         )
#       }
#     }
#   }

  ## Early exit //
  if (!out) {
    return(out)
  }

  ## Auto-check if reactive //
  ## This significantly speeds up the assignment process for reactives that 
  ## already exist as `setShinyReactive()` does not need to be called again
  path <- if (grepl("^\\./", id) || dirname(id) != ".") {
    paste0("[[\"", gsub("/", "\"]][[\"", dirname(id)), "\"]]")
  }
  where <- eval(parse(text = paste0(envir_name, path)))
  reactive_exist <- isReactive(id = basename(id), where = where)
  
  if (!reactive && !typed || reactive_exist && !typed) {  
    path <- paste0("[[\"", gsub("/", "\"]][[\"", id), "\"]]")
    expr <- paste0(envir_name, path, " <- value")
    eval(parse(text = expr))  
  } else {
#     path <- if (grepl("^\\./", id) || dirname(id) != ".") {
#       paste0("[[\"", gsub("/", "\"]][[\"", dirname(id)), "\"]]")
#     }
#     where <- eval(parse(text = paste0(envir_name, path)))
    setShinyReactive(id = basename(id), value = value, 
      where = where, typed = typed, ...)
  }

  return(out)
  
  }
)
