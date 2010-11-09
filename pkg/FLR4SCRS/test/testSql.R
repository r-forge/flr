library(DBI)
library(RSQLite)

#### Monte Carlo runs
## DB
SQLite(max.con = 16, fetch.default.rec = 500, force.reload = FALSE, shared.cache=FALSE)

## connect DB
dbMC  <-"c:/temp/t.dbf"
conMC <-dbConnect(dbDriver("SQLite"), dbname=dbMC)

dbWriteTable(conMC, "EastMC", data.frame(n=rep(1:4,25),a=rep(c("a","b","c","d"),each=25)), append=TRUE)

dbListTables(conMC)
file.info(dbMC)

setGeneric("sqlVar", function(object, ...)
	standardGeneric("sqlVar"))

setMethod("sqlVar", signature("character"),
  function(object, ...) paste("('",paste(object,collapse="','"),"')",sep=""))
    
setMethod("sqlVar", signature("numeric"),
  function(object, ...) paste("(",paste(object,collapse=","),")",sep=""))

sqlVar(c("b"))
sqlVar(2)

query<- paste("SELECT * FROM 'EastMC' WHERE n IN", sqlVar(2), "AND a IN", sqlVar(c("b")), "LIMIT 10")

x  <- dbGetQuery(conMC, query)

dbDisconnect(conMC)
