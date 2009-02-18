# The slot accessors and replacement accessors
# Not automated, just cut 'n' paste


# FLAssess accessors

#catch.n
#stock.n
#harvest
#index.name
#index.range
#index
#index.res
#index.hat
#index.var
#call
#name
#desc
#range

# Accessors

if(!isGeneric('catch.n'))
    setGeneric('catch.n', function(object, ...) standardGeneric('catch.n'))

setMethod('catch.n', signature(object='FLAssess'),
    function(object) {
        slot(object, 'catch.n')
    }
)

if(!isGeneric('stock.n'))
    setGeneric('stock.n', function(object, ...) standardGeneric('stock.n'))

setMethod('stock.n', signature(object='FLAssess'),
    function(object) {
        slot(object, 'stock.n')
    }
)

if(!isGeneric('harvest'))
    setGeneric('harvest', function(object, catch, ...) standardGeneric('harvest'))

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

if(!isGeneric('index'))
    setGeneric('index', function(object, ...) standardGeneric('index'))

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
)

if(!isGeneric('name'))
    setGeneric('name', function(object, ...) standardGeneric('name'))

setMethod('name', signature(object='FLAssess'),
    function(object) {
        slot(object, 'name')
    }
)

if(!isGeneric('desc'))
    setGeneric('desc', function(object, ...) standardGeneric('desc'))

setMethod('desc', signature(object='FLAssess'),
    function(object) {
        slot(object, 'desc')
    }
)

#if(!isGeneric('range'))
#    setGeneric('range', function(object, ...) standardGeneric('range'))
#
#setMethod('range', signature(object='FLAssess'),
#    function(object) {
#        slot(object, 'range')
#    }
#)
#

## Replacement accessors
#
if(!isGeneric('catch.n<-'))
    setGeneric('catch.n<-', function(object, ..., value ) standardGeneric('catch.n<-'))

setMethod('catch.n<-', signature(object='FLAssess', value = 'FLQuant'),
function(object,value){
    slot(object,'catch.n') <- value
    if(validObject(object)) object else stop("")
})

if(!isGeneric('stock.n<-'))
    setGeneric('stock.n<-', function(object, ..., value) standardGeneric('stock.n<-'))

setMethod('stock.n<-', signature(object='FLAssess', value = 'FLQuant'),
function(object,value){
    slot(object,'stock.n') <- value
    if(validObject(object)) object else stop("")
})

if(!isGeneric('harvest<-'))
    setGeneric('harvest<-', function(object, ..., value) standardGeneric('harvest<-'))

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
    setGeneric('index.range<-', function(object, ..., value) standardGeneric('index.range<-'))

setMethod('index.range<-', signature(object='FLAssess', value = 'list'),
function(object,value){
    slot(object,'index.range') <- value
    if(validObject(object)) object else stop("")
})

if(!isGeneric('index<-'))
    setGeneric('index<-', function(object, ..., value) standardGeneric('index<-'))

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
})

if(!isGeneric('name<-'))
    setGeneric('name<-', function(object, ..., value) standardGeneric('name<-'))

setMethod('name<-', signature(object='FLAssess', value = 'character'),
function(object,value){
    slot(object,'name') <- value
    if(validObject(object)) object else stop("")
})

if(!isGeneric('desc<-'))
    setGeneric('desc<-', function(object, ..., value) standardGeneric('desc<-'))

setMethod('desc<-', signature(object='FLAssess', value = 'character'),
function(object,value){
    slot(object,'desc') <- value
    if(validObject(object)) object else stop("")
})

