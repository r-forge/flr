setGeneric('aspic',        function(object,...)   standardGeneric('aspic'))

# setGeneric('diagPlot',     function(object,...)   standardGeneric('diagPlot'))
# 
# setGeneric('survey',       function(object,...)   standardGeneric('survey'))
# setGeneric('cpue',         function(object,...)   standardGeneric('cpue'))
# 
setGeneric("readAspic",    function(object,...)   standardGeneric('readAspic'))
setGeneric("writeAspic",   function(object,...)   standardGeneric('writeAspic'))
# 
# setGeneric("cpue<-",       function(object,value,...) standardGeneric('cpue<-'))

#if (!isGeneric("fwd"))       setGeneric("fwd",      function(object, ctrl, ...) standardGeneric("fwd"))
#if (!isGeneric("hcr"))       setGeneric("hcr",      function(object, ctrl, ...) standardGeneric("hcr"))
#if (!isGeneric("tac"))       setGeneric("tac",      function(object, ctrl, ...) standardGeneric("tac"))

setGeneric('fit',   function(object,...)     standardGeneric('fit'))
setGeneric('boot',  function(object,...)     standardGeneric('boot'))
setGeneric('jk',    function(object,...)     standardGeneric('jk'))


