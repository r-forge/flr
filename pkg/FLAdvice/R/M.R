#http://icesjms.oxfordjournals.org/content/66/9/1978.full
#http://onlinelibrary.wiley.com/doi/10.1111/j.1467-2979.2009.00350.x/full

gundersonDygert=function(params)
  0.03 + 1.68*params["gsi"]

## Constant values
pauly=function(params,t=10) { #winf,linf="missing",k="misssing",t=10) {
    pauly1=function(params,t)
       exp(-0.2107-0.0824*log(params["winf"])+0.6757*log(params["k"])+0.4627*log(t))
    
    pauly2=function(params,t)
       exp(-0.0066-0.279*log(params["linf"])+0.6543*log(params["k"])+0.4634*log(t))
    
    if ("winf" %in% dimnames(params)$params)
      pauly1(params,t=10)
    else if ("linf" %in% dimnames(params)$params)
      pauly2(params,t=10)}

hoenig=function(params,data=NULL) #amax)
   4.22/params["amax"]

jensen=function(params,data=NULL) #(k)
   1.5*params["k"]

richterEfanov=function(params) #(amat)
   1.52/(params["amat"]^0.72)-0.16 
  
petersonWroblewski=function(data) #(wt)
   1.29*data-0.25

lorenzen=function(data) #(wt)
   3.00*data-0.288

mcgurk=function(data) #(wt)
   0.00526*wt-0.25

## variable
gislason=function(params,data) #(l,linf,k) 
   exp(0.55-1.61*log(data) + 1.44*log(params["linf"]) + log(params["k"]))

chenWatanabe=function(params,data) { #(age,k,t0=-0.1){
   m =params["k"]/(1-exp(-params["k"]*(data-params["t0"])))

   tm =-(1/params["k"])*log(1-exp(params["k"]*params["t0"]))+params["t0"]
   print(tm)
   bit=exp(-params["k"]*(tm-params["t0"]))
   
   a0=1-bit
   a1=params["k"]*bit
   a2=-0.5*params["k"]^2*bit
   age.=age>tm
   m[age.] =params["k"]/(a0+a1*(age[age.]-tm)+a2*(age[age.]-tm)^2)
  
   return(m)}   

##########################################################################################################
##########################################################################################################
mlst=list("gunderson"         =gundersonDygert,
          "pauly"             =pauly,
          "hoenig"            =hoenig,
          "jensen"            =jensen,
          "richter"           =richterEfanov,
          "peterson"          =petersonWroblewski,
          "lorenzen"          =lorenzen,
          "mcgurk"            =mcgurk,
          "gislason"          =gislason,
          "chen"              =chenWatanabe)

rm(gundersonDygert,pauly,hoenig,jensen,richterEfanov,petersonWroblewski,lorenzen,mcgurk,gislason,chenWatanabe)

setGeneric('mFn', function(model,params,data, ...)
   standardGeneric('mFn'))
setMethod("mFn", signature(model="character",params="FLPar",data="ANY"),
   function(model,params,data="missing",...) {
     if (!missing(data))  
       if (is.FLQuant(data))
          data=ages(data)
         
      mlst[[model]](params,data,...)})

mFn("hoenig",FLPar(amax=40))

  

         