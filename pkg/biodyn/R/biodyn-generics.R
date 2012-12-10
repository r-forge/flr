setGeneric('biodyn',   function(model,params,...)  standardGeneric('biodyn'))

if (!isGeneric("msy"))       setGeneric('msy',      function(object,params,...) standardGeneric('msy'))
if (!isGeneric("fmsy"))      setGeneric('fmsy',     function(object,params,...) standardGeneric('fmsy'))
if (!isGeneric("bmsy"))      setGeneric('bmsy',     function(object,params,...) standardGeneric('bmsy'))
if (!isGeneric("refpts"))    setGeneric('refpts',   function(object,params,...) standardGeneric('refpts'))
if (!isGeneric("refptSE"))   setGeneric('refptSE',  function(object,params,...) standardGeneric('refptSE'))

if (!isGeneric("harvest"))   setGeneric('harvest',  function(object,params,...) standardGeneric('harvest'))

if (!isGeneric("fwd"))       setGeneric("fwd",      function(object, ctrl, ...) standardGeneric("fwd"))
if (!isGeneric("hcr"))       setGeneric("hcr",      function(object, ctrl, ...) standardGeneric("hcr"))
if (!isGeneric("tac"))       setGeneric("tac",      function(object, ctrl, ...) standardGeneric("tac"))




