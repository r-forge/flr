library(FLCore)
library(xtable)
data(ple4.index)
data(ple4.indices)
data(bt4)
data(ple4)
data(nsher)
flbiol <- as.FLBiol(ple4)
cls <- getClasses("package:FLCore")
FLCcls <- lapply(split(cls, cls), function(x){
	df0 <- data.frame(class=x, parent=NA, nSlots=NA, virtual=NA, child=NA, distance=NA)
	cls <- getClass(x)
	# superclass	
	if(length(cls@contains)!=0) df0$parent <- cls@contains[[1]]@superClass
	# virtual
	df0$virtual <- slot(cls, "virtual")	
	# number of slots
	if(slot(cls, "virtual")) NA else df0$nSlots <- length(slotNames(new(x)))
	# children
	if(length(slot(cls, "subclasses"))!=0) child <- names(slot(cls, "subclasses")) else child <- NA
	df0 <- df0[rep(1,length(child)),]
	df0$child <- child
	# distance
	if(length(slot(cls, "subclasses"))!=0) dist <- unlist(lapply(slot(cls, "subclasses"), slot, "distance")) else dist <- NA
	df0$distance <- dist 
	df0
})
FLCcls <- do.call("rbind", FLCcls)
FLCcls$child <- factor(FLCcls$child)
getClassMethods <- function(class, where){
    v <- showMethods(class=class, where=where, printTo=FALSE)->v
    unlist(lapply(strsplit(unlist(lapply(strsplit(v[substr(v, 1, 5) %in% "Funct"], " \\("), "[", 1)), ": "), "[", 2))
} 

# Set trellis arguments to plot in inverted colors
trellis.par.set(background=list(col="white"))


# Need to hack because of transparent background

pltSR <- function (x, main = "Functional form", log.resid = FALSE, 
        cex = 0.8) 
    {
        years <- as.numeric(dimnames(residuals(x))$year)
        scales <- list(y = list(rot = 90), tck = c(1, 0))
        trellis.device(new = FALSE)
        trellis.par.set(list(layout.heights = list(bottom.padding = -0.3, 
            axis.xlab.padding = 0.3, xlab = -0.3), layout.widths = list(left.padding = -0.3, 
            right.padding = -0.3, ylab.axis.padding = -0.3), background=list(col="white")))
        srpanel <- function(x, y, ...) {
            panel.xyplot(x, y, col = "black", cex = cex)
            panel.loess(x, y, col = "blue", lty = 4)
            panel.abline(a = 0, b = 0, lty = 2, col = "gray60")
        }
        respanel <- function(x, y, ...) {
            panel.xyplot(x, y, col = "black", cex = cex)
            panel.lmline(x, y, ..., col = "red")
            panel.abline(a = 0, b = 0, lty = 2, col = "gray60")
        }
        condnames <- names((fitted(x)))[c(3:5)][dim(fitted(x))[c(3:5)] != 
            1]
        cond <- paste(condnames, collapse = "+")
        if (cond != "") 
            cond <- paste("|", cond)
        ssb <- FLQuant(seq(0, max(ssb(x) + (ssb(x) * 0.15), na.rm = TRUE), 
            length = dim(ssb(x))[2]), dimnames = dimnames(ssb(x))[1:5])
        fitted <- predict(x, ssb = ssb)
        print(xyplot(formula(paste("fitted~ssb", cond)), ylab = "Recruits", 
            xlab = "SSB", model.frame(FLQuants(rec = propagate(x@rec, 
                dims(x)$iter), ssb = ssb, fitted = fitted)), 
            col = "red", main = main, xlim = c(0, max(ssb, na.rm = TRUE)), 
            ylim = c(0, max(x@rec, na.rm = TRUE) + (max(x@rec, 
                na.rm = TRUE)/10)), groups = iter, type = "l", 
            scales = scales), split = c(1, 1, 2, 3), more = TRUE)
        trellis.focus("panel", 1, 1)
        lpoints(x@ssb, x@rec, col = "black", cex = cex)
        llines(lowess(x)$ssb, lowess(x)$rec, col = "blue", lty = 4)
        trellis.unfocus()
        if (log.resid) 
            resid <- model.frame(FLQuants(resid = log(rec(x)/fitted(x))))
        else resid <- model.frame(FLQuants(resid = residuals(x)))
        print(xyplot(formula(paste("resid~year", cond)), ylab = "Residuals", 
            xlab = "", data = resid, scales = scales, panel = srpanel, 
            groups = iter, main = "Residuals by year"), split = c(2, 
            1, 2, 3), more = TRUE)
        print(xyplot(formula(paste("resid1~resid", cond)), ylab = "Residuals at t+1", 
            xlab = "Residuals at t", data = cbind(resid, resid1 = c(resid$resid[-1], 
                NA)), panel = respanel, main = "AR(1) Residuals", 
            scales = scales), split = c(1, 2, 2, 3), more = TRUE)
        print(xyplot(formula(paste("resid~ssb", cond)), ylab = "Residuals", 
            xlab = "SSB", data = cbind(resid, ssb = c(x@ssb)), 
            panel = srpanel, main = "Residuals by SSB", scales = scales), 
            split = c(2, 2, 2, 3), more = TRUE)
        print(qqmath(formula(paste("~resid", cond)), ylab = "Residuals", 
            xlab = "Sample Quantiles", data = resid, scales = scales, 
            panel = function(x, ...) {
                panel.qqmath(x, ..., , col = "gray40", cex = cex)
                panel.qqmathline(x, ..., col = "red")
            }, main = "Normal Q-Q Plot"), split = c(1, 3, 2, 
            3), more = TRUE)
        print(xyplot(formula(paste("resid~fitted", cond)), ylab = "Residuals", 
            xlab = "Recruits hat", data = cbind(resid, fitted = c(x@fitted)), 
            panel = srpanel, main = "Residuals by Estimated Recruits", 
            scales = scales), split = c(2, 3, 2, 3), more = FALSE)
        invisible()
    }

