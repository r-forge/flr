#cd /home/lkell/flr/experimental/FLSeine/admb
export ADMB_HOME='/usr/local/admb/'
sudo ln -s /usr/local/admb/bin/adlink /usr/local/bin/adlink

#pathNm="/home/lkell/flr/experimental/FLSeine/admb"


## Seine #########################################################################
breaks     =c(1990,1999,2004)

res=list()
res[["Med 1"]]   =seine(obs,ones,rng,breaks,grwMed)
res[["NAtl 1"]]  =seine(obs,ones,rng,breaks,grwNAtl)
res[["Med Var"]] =seine(obs, wts,rng,breaks,grwMed)
res[["NAtl Var"]]=seine(obs, wts,rng,breaks,grwNAtl)

rep=res[[1]]$rep
std=res[[1]]$std
    
hat=getStd(std,breaks,rng)

object=data.frame(y       =rep$obs,
                  x       =rep$minyr:rep$maxyr,
                  hat     =rep$hat,
                  residual=rep$Residuals)

                  
ggplot(jk)+geom_histogram(aes(value))+facet_wrap(~param,scale="free")
ggplot(jk)+geom_point(aes(X1,value))+facet_wrap(~param,scale="free")

wts2=wts
wts2[wts2>0]=1
seine(obs,wts2,grw,rng,breaks)
