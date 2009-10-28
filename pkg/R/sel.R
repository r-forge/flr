fapex   <-function(x) apply(harvest(x),2:6,max)
catchSel<-function(x,y=T) {if (y) sweep(harvest(x),2:6,fapex(x),"/")
                           else   sweep(harvest(x),2:6,fbar( x),"/")}
                           
landingsSel<-function(x,y=T) {catchSel(x,y)*landings.n(x)/(landings.n(x)+discards.n(x))}
discardsSel<-function(x,y=T) {catchSel(x,y)*discards.n(x)/(landings.n(x)+discards.n(x))}
