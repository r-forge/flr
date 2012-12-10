setMethod('harvest', signature(object='biodym'),
          function(object) {
            res <- catch(object)/stock(object)[,dimnames(catch(object))$year]
            units(res) <- "hr"
            return(res)
          })

setGeneric('computeSP',function(object,biomass,...) standardGeneric('computeSP'))
setMethod( 'computeSP', signature(object="biodyn",   biomass="numeric"),     function(object,biomass)     spFn(model(object),params(object),biomass))
setMethod( 'computeSP', signature(object="biodyn",   biomass="FLQuant"),     function(object,biomass)     spFn(model(object),params(object),biomass))
setMethod( 'computeSP', signature(object="character",biomass="numeric"),     function(object,biomass,par) spFn(object,params,biomass))

