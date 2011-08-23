# Expand along the iteration dimension
pleBRP <-propagate(pleBRP,iter=100)
# Put some noise on the natural mortality
m(pleBRP) <-m(pleBRP)*rlnorm(prod(dim(m(pleBRP))),0,0.3)
# Calculate reference points
refpts(pleBRP)<-computeRefpts(pleBRP)
# Remove the economic refpts
refpts(pleBRP) <- refpts(pleBRP)[-5,]
# 'ave a butchers
refpts(pleBRP)

# Turn refpts into a dataframe (makes plotting easier)
data<-cbind(expand.grid(dimnames(refpts(pleBRP)[,"harvest",])),val=c(refpts(pleBRP)[,"harvest",]))[,-2]

# Feast your eyes on this
histogram( ~ val | refpt, data = data,
            xlab = "Fishing Mortality", type = "density",
            panel = function(x, ...) {
            panel.histogram(x, ...)
            panel.mathdensity(dmath = dnorm, col = "black",
            args = list(mean=mean(x),sd=sd(x)))},
            scale="free", xlim=c(0,.5))


