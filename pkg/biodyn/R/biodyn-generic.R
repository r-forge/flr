#' Creates a list of biodyn
#'
#' @description Creates a list of biodyn biomass dynamic model classes.
#' @return list of biodyn objects
#' @export
#' @examples
#' \dontrun{biodyns()}
setGeneric('biodyns', function(object, ...) standardGeneric('biodyns'))

setGeneric('biodyn',   function(model,params,...)  standardGeneric('biodyn'))


setGeneric('msy',      function(object,params,...) standardGeneric('msy'))
setGeneric('fmsy',     function(object,params,...) standardGeneric('fmsy'))
setGeneric('bmsy',     function(object,params,...) standardGeneric('bmsy'))
setGeneric('refptSE',  function(object,params,...) standardGeneric('refptSE'))

if (!isGeneric("harvest")) setGeneric('harvest',  function(object,params,...) standardGeneric('harvest'))

if (!isGeneric("fwd"))       setGeneric("fwd",      function(object, ctrl, ...)    standardGeneric("fwd"))
if (!isGeneric("hcr"))       setGeneric("hcr",      function(object, ...)          standardGeneric("hcr"))
setGeneric("tac",      function(object, harvest, ...) standardGeneric("tac"))

if (!isGeneric("pella"))      setGeneric('pella',   function(object,index,...)  standardGeneric('pella'))
