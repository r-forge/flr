#' profile
#'
#' @description 
#' Performs a profile using residual sum of squares, fixes some parameters for a range of values 
#' and then estimate the others 
#'
#' @param fitted: an \code{aspic} object
#' @param which: \code{character} giving the parameters to do the profile for, i.e. to fix.
#' @param fixed: \code{character} any parameters that should be fixed, all others are extimated. 
#' @param maxsteps: \code{numeric} number of parameter values to vary, default is 11.
#' @param range; \code{numeric} how mucg to vary parameter values by, default [0.5,1.5]. 
#' @param fn: \code{function} that gives values to be profiled.
#' @param run: \code{logical} if \code{TRUE} then returns profile, otherwise it just sets the control object-
#' 
#' @return a \code{data frame} with results turned by \code{fn} by values in \code{which}. 
#' @seealso \code{\link{biodyn},\link{fit}}
#'
#' @export
#' @docType methods
#' @rdname profile
#'
#' @examples
#' /dontrun{
#' data(asp)
#' res=profile(asp,which="msy",fixed="b0",maxsteps=31,range=c(0.5,1.1))
#' ggplot(res)+geom_line(aes(k,rss))
#' }       
setMethod("profile", signature(fitted="aspic"),
      function(fitted,which,fixed=c(),
                   maxsteps=11, range=0.5,
                   fn   =function(x) cbind(model.frame(params(x)),model.frame(x@objFn)[,-3]),
                   run  =TRUE,...){
  
        if (dims(fitted)$iter>1) stop("can only be done for a single iter")
                 
        setControl(fitted)=params(fitted)
        fitted@control=propagate(fitted@control,maxsteps)
          
        if (length(range)==1) range=c(range,2-range)
        
        sq=list(seq(range[1],range[2],length.out=maxsteps))
        sq=do.call("expand.grid",sq[rep(1,length(which))])
        
        for (i in seq(length(which))){
           control(fitted)[which[i],"val"]=params(fitted)[which[i]]*sq[,i]
           control(fitted)[which[i],"min"]=min(control(fitted)[which[i],"val"])*.1
           control(fitted)[which[i],"max"]=max(control(fitted)[which[i],"val"])*10}
        
        fitted@control[c(fixed,which),"fit"]=0
          
        if (!run) return(fitted)
        
        res=fit(fitted)
        
        rtn=fn(res)
        
        return(rtn)})


# ### debugging stuff
# data(bd)
# fitted=biodyn(factor("pellat"),params(bd),catch=catch(bd))
# cpue=rlnorm(1,log(stock(bd)),.2)[,-60]
# setParams(fitted)     =cpue
# 
# 
# attach(list(maxsteps=11, range=0.5, ci=c(0.25, 0.5, 0.75, 0.95),
#             plot=TRUE,fixed=c()))
# which="r"
# fixed=c("p","b0")
# ###
# rtn=profile(swon[[1]],which="k",fixed="b0",maxsteps=31,range=c(.75,1.5))
# ggplot(rtn)+geom_line(aes(msy,rss))