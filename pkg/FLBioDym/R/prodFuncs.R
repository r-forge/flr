#### Production functions ######################################################
prodFunc<-function(model,bio,r=.5,K=10,m=0.25,p=2,msy=0){
    fox<-function(bio,r,K){
        r*bio*(1-log(bio)/log(K))}

    schaefer<-function(bio,r,K){
        r*bio*(1-bio/K)}

    pellat<-function(bio,r,K,p=2){
        r*bio-r*(bio^p)/K}

    shepherd<-function(bio,r,K,m){
        r*bio/(1+bio/K)-m*bio}

    gulland<-function(bio,r,K){
        r*bio*(K-bio)}

    fletcher<-function(bio,K,msy,p){
        lambda<-(p^(p/(p-1)))/(p-1)
        
        lambda*msy*(bio/K)-lambda*msy*(bio/K)^p
        }

    res<-switch(model,
           fox     =fox(     bio,r,K),
           schaefer=schaefer(bio,r,K),
           gulland =gulland( bio,r,K),
           fletcher=fletcher(bio,  K,msy,p),
           pellat  =pellat(  bio,r,K,p),
           shepherd=shepherd(bio,r,K,m))

    return(res)
    }

