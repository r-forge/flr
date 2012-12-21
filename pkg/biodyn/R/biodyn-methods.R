setMethod('harvest', signature(object='biodyn',catch="missing"),
          function(object) {
            res <- catch(object)/stock(object)[,dimnames(catch(object))$year]
            units(res) <- "hr"
            return(res)
          })

##############################################################
#' Calculates surplus production
#'
#' Calculates the surplus production for a biomass dynamic model given a level of stock biomass
#' 
#' @param  \code{object}, an object of class \code{biodyn} 
#'
#' @param \code{biomass}, stock biomaas, may be a \code{numerix},  \code{FLQuant} or missing. In the latte case the stock slot will be used.
#'
#' @return an \code{FLPar} object
#' 
#' @seealso \code{\link{plotSP}}
#' 
#' @export
#' @docType methods
#' @rdname sp
#'
#' @examples
#' \dontrun{ computeSP(bd,seq(0,params(bd)["k"])) }
#'  
setGeneric('computeSP',function(object,biomass,...) standardGeneric('computeSP'))
setMethod( 'computeSP', signature(object="biodyn",   biomass="missing"),     function(object,biomass=stock(object))  spFn(model(object),params(object),biomass))
setMethod( 'computeSP', signature(object="biodyn",   biomass="numeric"),     function(object,biomass)                spFn(model(object),params(object),biomass))
setMethod( 'computeSP', signature(object="biodyn",   biomass="FLQuant"),     function(object,biomass)                spFn(model(object),params(object),biomass))

setGeneric('kobe',       function(file,method,...)    standardGeneric('kobe'))
