# accessors - FLAssess accesion and replacement methods
# FLAssess/R/accessors.R

# Copyright 2003-2009 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Finlay Scott, Cefas & Robert Scott, JRC
# $Id:  $

# accessors {{{
setMethod('catch.n', signature(object='FLAssess'),
    function(object) {
        slot(object, 'catch.n')
    }
)

setMethod('stock.n', signature(object='FLAssess'),
    function(object) {
        slot(object, 'stock.n')
    }
)

setMethod('harvest', signature(object='FLAssess', catch='missing'),
    function(object,catch) {
        slot(object, 'harvest')
    }
)

if(!isGeneric('index.name'))
    setGeneric('index.name', function(object, ...) standardGeneric('index.name'))

setMethod('index.name', signature(object='FLAssess'),
    function(object) {
        slot(object, 'index.name')
    }
)

if(!isGeneric('index.range'))
    setGeneric('index.range', function(object, ...) standardGeneric('index.range'))

setMethod('index.range', signature(object='FLAssess'),
    function(object) {
        slot(object, 'index.range')
    }
)

setMethod('index', signature(object='FLAssess'),
    function(object) {
        slot(object, 'index')
    }
)

if(!isGeneric('index.res'))
    setGeneric('index.res', function(object, ...) standardGeneric('index.res'))

setMethod('index.res', signature(object='FLAssess'),
    function(object) {
        slot(object, 'index.res')
    }
)

if(!isGeneric('index.hat'))
    setGeneric('index.hat', function(object, ...) standardGeneric('index.hat'))

setMethod('index.hat', signature(object='FLAssess'),
    function(object) {
        slot(object, 'index.hat')
    }
)

if(!isGeneric('index.var'))
    setGeneric('index.var', function(object, ...) standardGeneric('index.var'))

setMethod('index.var', signature(object='FLAssess'),
    function(object) {
        slot(object, 'index.var')
    }
) # }}}

# replacement {{{
setMethod('catch.n<-', signature(object='FLAssess', value = 'FLQuant'),
function(object,value){
    slot(object,'catch.n') <- value
    if(validObject(object)) object else stop("")
})

setMethod('stock.n<-', signature(object='FLAssess', value = 'FLQuant'),
function(object,value){
    slot(object,'stock.n') <- value
    if(validObject(object)) object else stop("")
})

setMethod('harvest<-', signature(object='FLAssess', value = 'FLQuant'),
function(object,value){
  slot(object,'harvest') <- value
  if(validObject(object)) object else stop("")
})

if(!isGeneric('index.name<-'))
  setGeneric('index.name<-', function(object, ..., value) standardGeneric('index.name<-'))

setMethod('index.name<-', signature(object='FLAssess', value = 'character'),
function(object,value){
    slot(object,'index.name') <- value
    if(validObject(object)) object else stop("")
})

if(!isGeneric('index.range<-'))
  setGeneric('index.range<-', function(object, ..., value)
    standardGeneric('index.range<-'))

setMethod('index.range<-', signature(object='FLAssess', value = 'list'),
function(object,value){
    slot(object,'index.range') <- value
    if(validObject(object)) object else stop("")
})

if(!isGeneric('index<-'))
  setGeneric('index<-', function(object, ..., value)
    standardGeneric('index<-'))

setMethod('index<-', signature(object='FLAssess', value = 'FLQuants'),
function(object,value){
    slot(object,'index') <- value
    if(validObject(object)) object else stop("")
})

if(!isGeneric('index.res<-'))
    setGeneric('index.res<-', function(object, ..., value) standardGeneric('index.res<-'))

setMethod('index.res<-', signature(object='FLAssess', value = 'FLQuants'),
function(object,value){
    slot(object,'index.res') <- value
    if(validObject(object)) object else stop("")
})

if(!isGeneric('index.hat<-'))
    setGeneric('index.hat<-', function(object, ..., value) standardGeneric('index.hat<-'))

setMethod('index.hat<-', signature(object='FLAssess', value = 'FLQuants'),
function(object,value){
    slot(object,'index.hat') <- value
    if(validObject(object)) object else stop("")
})

if(!isGeneric('index.var<-'))
    setGeneric('index.var<-', function(object, ..., value) standardGeneric('index.var<-'))

setMethod('index.var<-', signature(object='FLAssess', value = 'FLQuants'),
function(object,value){
    slot(object,'index.var') <- value
    if(validObject(object)) object else stop("")
})  # }}}
