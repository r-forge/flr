
    # 1) Simulate data and fit all year 
    ## Harvest
    hvst=FLQuant(c(seq(0.0,1.0,length.out=20),seq(1.0,0.25,length.out=20)))*1.25*fmsy("pellat",FLPar(c(K=1000,r=0.5, p=2)))    
    
    sims<-rdply(10,function(){
                 bd=simBioDym("pellat",hvst,r=0.5,K=100,p=1)
           
                 as.data.frame(params(fit(bd)))
                 })
  ggplot(subset(sims, params %in% c("r","K","p","q","sigma","b0")))+geom_histogram(aes(data))+facet_wrap(~params,scale="free")
  
  # 2) fit every 5 years 
  sims5<-rdply(1000, 
               function(){
                   bd  =simBioDym("pellat",hvst,r=0.5,K=100,p=2)
                   mdply(data.frame(end=seq(20,40,5)), function(end) as.data.frame(params(fit(window(bd,end=end)))))
                   })
  ggplot(subset(sims5, params %in% c("K"))) + 
            geom_histogram(aes(data)) + 
            facet_grid(end~params,scale="free")+
            scale_x_continuous(limits=c(0,200))
 