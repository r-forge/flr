
jk=function(bd){
      mdply(1:dims(bd)$iter, function(x,bd){
           bd.      =iter(bd,x)
           index(bd.)=jacknife(index(bd.))
                            
           tmp       =admbBD(bd.)
           tmp2      =summaryStats(tmp)
           tmp3      =FLQuants(jackSummary(tmp2))
           
           model.frame(tmp3,drop=TRUE)},bd=bd)}

