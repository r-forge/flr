### Scales figures ###########################################################
imageDef<-function(x,y,scale=200){
    ids           <-getImageDefs()

    ids$plotHeight=y*scale
    ids$dispHeight=y
    ids$plotWidth =x*scale
    ids$dispWidth =x

    setImageDefs(ids)}