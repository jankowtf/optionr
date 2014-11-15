##------------------------------------------------------------------------------
## Define //
##------------------------------------------------------------------------------

setGeneric(
  name = "setAnywhereOption",
  signature = "id",
  def = function(id, ...) standardGeneric("setAnywhereOption")      
)
setMethod(
  f = "setAnywhereOption", 
  signature = signature(id = "character"), 
 definition = function(id, ...) setNested(id = id)
  ## --> works
#  definition = function(id, ...) setNested(id = id, ...)
  ## --> works
#   definition = function(id, ...) setNested(id = id,)
  ## --> this leads to things get messed up with argument's default values
  ## --> so the trailing `,` was causing the problem!
)
setGeneric(
  name = "setNested",
  signature = "id",
  def = function(id, ...) standardGeneric("setNested")       
)
setMethod(
  f = "setNested", 
  signature = signature(id = "character"), 
  definition = function(id, ...) {
  
  if (FALSE) {  
    ## Omitted 
  } else {
    setShinyReactive(id = basename(id), ...)
  }
  
})
setShinyReactive <- function(
  id,
  lazy = FALSE,
  is_lazy = FALSE,
  push = FALSE,
  typed = FALSE,
  strict_set = c(0, 1, 2),
  ...
  ) {

  ###########
  ## DEBUG ##    
  ###########
  message("DEBUG/setShinyReactive/threedots")
  print(list(...))
  message("DEBUG/setShinyReactive/push")        
  print(push)    
  message("DEBUG/setShinyReactive/lazy")    
  try(print(lazy))
  ## --> strangely, R does not seem to like the name `lazy` 
  message("DEBUG/setShinyReactive/is_lazy")    
  print(is_lazy)
  ## --> this works
  lazy <- is_lazy
  message("DEBUG/setShinyReactive/lazy")    
  print(lazy)

  TRUE
  
}

##------------------------------------------------------------------------------
## Apply //
##------------------------------------------------------------------------------

setAnywhereOption(id = "test")
# DEBUG/setShinyReactive/threedots
# list()
# DEBUG/setShinyReactive/push
# [1] FALSE
# DEBUG/setShinyReactive/lazy
# Error in print(lazy) : argument is missing, with no default
# DEBUG/setShinyReactive/is_lazy
# [1] FALSE
# DEBUG/setShinyReactive/lazy
# [1] FALSE
# [1] TRUE

setAnywhereOption(id = "test", push = TRUE)
setAnywhereOption(id = "test", lazy = TRUE)

##------------------------------------------------------------------------------
## Solution //
##------------------------------------------------------------------------------

setMethod(
  f = "setAnywhereOption", 
  signature = signature(id = "character"), 
 definition = function(id, ...) setNested(id = id)
  ## --> works
#  definition = function(id, ...) setNested(id = id, ...)
  ## --> works
#   definition = function(id, ...) setNested(id = id,)
  ## --> this leads to things get messed up with argument's default values
  ## --> so the trailing `,` was causing the problem!
)

setAnywhereOption(id = "test")
# DEBUG/setShinyReactive/threedots
# list()
# DEBUG/setShinyReactive/push
# [1] FALSE
# DEBUG/setShinyReactive/lazy
# [1] FALSE
# DEBUG/setShinyReactive/is_lazy
# [1] FALSE
# DEBUG/setShinyReactive/lazy
# [1] FALSE
# [1] TRUE
