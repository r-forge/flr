#setMethod('kobeP', signature(b="FLStock",f="FLBRP"),
# function(b,f,rp="msy"){
#   res=model.frame(SSB    =ssb(    b),refpts(f)[rp,"ssb"],
#                   harvest=harvest(b),refpts(f)[rp,"harvest"],drop=TRUE)
#   
#   res=cbind(res,kobeP(res[,"SSB"],res[,"harvest"]))
#   
#   return(res)})


