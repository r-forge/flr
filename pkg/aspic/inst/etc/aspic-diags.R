# dP=function(object){
#             
#             #
#             x    <- stock(    object)
#             y    <- index(    object)
#             hat  <- object@fitted
#             residual <- log(y/hat)
#             
#             dmns <- dimnames(x)
#             
#             residualLag <- FLQuant(NA, dimnames=dimnames(residual))
#             residualLag[,-dim(residual)[2]] <- residual[,-1]
#             
#             qq. <- qqnorm(c(residual),plot.it=FALSE)
#             qqx <- FLQuant(qq.$x,dimnames=dimnames(residual))
#             qqy <- FLQuant(qq.$y,dimnames=dimnames(residual))
#             
#             res <- drop(model.frame(mcf(FLQuants(x=x,y=y,hat=hat,residual=residual,residualLag=residualLag,qqx=qqx,qqy=qqy)),drop=T))
#             
#             p.=ggplot(getDiag(res,diagPlots()[[class(object)]])) + 
#               geom_point(aes(x,y))                           +
#               facet_wrap(~name,scale="free")                 +
#               geom_line(aes(x,hat,colour="red"))+geom_smooth(aes(x,y))
#             
#             print(p.)
#             
#             return(invisible(p.))}
