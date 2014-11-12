#' @title
#' Management of customized (and nested) project/package options
#'
#' @description
#' The package provides an extendable interface to conveniently 
#' create customized project/package options. Options can be specified and 
#' retrieved based on path-like names/identifiers (e.g.
#' \code{output/type/pdf = TRUE} will be translated into the following nested
#' environment structure: \code{output$type$pdf} with the value being \code{TRUE}).
#' 
#' Also, it allows to specify reactive options, i.e. options that are
#' dynamically linked to other options and thus automatically stay synced.
#' 
#' @section Core convenience functions:
#'
#'  \itemize{
#'    \item{\code{\link[optionr]{initializeOptionContainer}}: }{
#'
#'      Initialize an option container that serves as a central point of 
#'      reference for setting and retrieving project options and similar 
#'      information.
#'    }
#'    \item{\code{\link[optionr]{setPackageOption}} (\code{\link[optionr]{getPackageOption}}): }{
#'
#'      Set (retrieve) actual package options (from) a specific environment inside an option
#'      container.
#'    }
#'    \item{\code{\link[optionr]{setMetaValue}} (\code{\link[optionr]{getMetaValue}}): }{
#'
#'      Set (retrieve) meta information (from) a specific environment inside 
#'      an option container.
#'    }
#'    \item{\code{\link[optionr]{setRegistryValue}} (\code{\link[optionr]{getRegistryValue}}): }{
#'
#'      Set (retrieve) registry information (from) a specific environment inside 
#'      an option container.
#'    }
#'    \item{Removal functions: }{
#'
#'      Remove components from option container.
#'      
#'      \code{\link[optionr]{rmProjectOption}, \link[optionr]{rmMetaValue}, 
#'      \link[optionr]{rmRegistryValue}}
#'    }
#' }
#' 
#' @section Underlying core functions:
#' 
#'  \itemize{
#'    \item{\code{\link[optionr]{setAnywhereOption}} (\code{\link[optionr]{getAnywhereOption}}): }{
#'
#'      Set (retrieve) options (from) anywhere inside an option container.
#'      This function is called by most of the convenience functions listed above.
#'    }
#'    \item{Initialization functions: }{
#'
#'      Actual component initialization can be controlled by definining methods 
#'      for any of the following functions:
#'      
#'      \code{\link[optionr]{initializeOptionContainer},
#'      \link[optionr]{initializeProjectOptions}, 
#'      \link[optionr]{initializeMeta}, 
#'      \link[optionr]{initializeRegistry}}
#'      
#'    }
#' }
#'
#' @template author
#' @template references
#' @docType package
#' @name optionr
NULL
