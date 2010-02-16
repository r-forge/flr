# FLComp - «Short one line description»
# FLComp

# Copyright 2003-2008 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, Cefas
# $Id$

# sql {{{
setGeneric('sql', function(object, ...)
  standardGeneric('sql'))
# }}}

# unsql {{{
setGeneric('unsql', function(object, ...)
  standardGeneric('unsql'))

setMethod('unsql', signature(object='sqliteFLComp'),
  function(object) {
    .Call('selectFLComp', object@db, object@name, PACKAGE='SQLiteFL')
  }
)
# }}}

# summary {{{
setMethod('summary', signature(object='sqliteFLComp'),
  function(object)
  {
    rc <- .Call('summaryFLComp', object@db, object@name, PACKAGE='SQLiteFL')
  }
) # }}}

# dims  {{{
setMethod('dims', signature(obj='sqliteFLComp'),
  function(obj)
  {
    .Call('dimsFLComp', obj@db, obj@name, PACKAGE='SQLiteFL')
  }
) # }}}

# show

# iter  {{{
setMethod("iter", signature(object="sqliteFLComp"),
  function(object, iter=1, ...)
  {
    stop("TODO")
  }
)
# }}}

# iter<-

# ----

# updateFLComp  {{{
updateFLComp <- function(object, slot, value, ...)
{
  args <- list(...)

  # Standard dimension names
  qnames <- c('quant', 'year', 'unit', 'season', 'area', 'iter')
  
  # Find names not in standard list, assume they refer to quant
  idx <- names(args)%in%qnames

  # STOP if more than one 'quant'
  if(length(idx) - sum(idx) > 1)
    stop("Dimension names are incorrect: more than one name for 'quant'")

  # set quant name to 'quant'
  names(args)[!idx] <- 'quant'

  # Create list with given elements in right position ...
  update <- args[match(qnames, names(args))]

  # dims
  dims <- rep(0,6)
  idx <- !unlist(lapply(update, is.null))
  dims[idx] <- seq(1, sum(as.numeric(idx)))

  # reformat update
  update[!idx] <- '0'
  names(update) <- qnames
  update <- lapply(update, as.character)

  # resize value if appropriate
  # TODO what if args = list()?
  len <- prod(unlist(lapply(update[idx], length)))
  if(len %% length(value) != 0 && length(args) != 0)
    stop('number of items to replace is not a multiple of replacement length')
  else if(length(value) < len)
    value <- rep(value, len/length(value))

  # create stmt
  table <- paste(object@name, 'data', sep='_')
  stmt <- paste('UPDATE', paste('\"', table, '\"', sep=''), 'SET data = ? WHERE slot = ',
    paste('\"', slot, '\"',sep=''))
  # add AND statements if needed
  if(length(args) > 0)
    stmt <- paste(stmt, paste('AND', names(update[idx]), '= ?', collapse=' '), ';')
  else
    stmt <- paste(stmt, ';')
 
  invisible(.Call('updateFLComp', object@db, object@name, stmt, as.double(value),
    as.integer(dims), update[['quant']], update[['year']], update[['unit']], 
    update[['season']], update[['area']], update[['iter']], PACKAGE='SQLiteFL'))
} # }}}

# selectFromFLComp {{{
selectFromFLComp <- function(object, slot, ...)
  {
    quant  <- character(1)
    args <- list(...)
    if(length(args) == 0)
      return(.Call('selectSlotFLComp', object@db, object@name, slot, PACKAGE='SQLiteFL'))
    # Standard dimension names
    qnames <- c('quant', 'year', 'unit', 'season', 'area', 'iter')
    # Find names not in standard list, assume they refer to quant
    idx <- names(args)%in%qnames
    # Create list with given elements in right position ...
    select <- args[idx][match(qnames, names(args[idx]))]
    # ... unless a non-standard quant is used
    if(length(idx) - sum(idx) > 1)
      stop("More than one dimension provided for 'quant'")
    # If quant != 'quant', then place in select
    if(sum(idx) < length(idx))
    {
      select[1] <- args[!idx]
      quant <- names(args[!idx])
      names(select)[1] <- 'quant'
    }
    # get dims of new object
    dims <- unlist(lapply(select, length))
    # create SQL SELECT statements
    for(i in qnames[dims>0])
      select[[i]] <- paste('AND', i, 'IN (', paste(select[[i]], collapse=','), ')')
    select <- paste(unlist(select[dims>0]), collapse=' ')

    return(.Call('selectFromSlotFLComp', object@db, object@name, slot, select,
      as.integer(dims), quant, PACKAGE='SQLiteFL'))
} # }}}
