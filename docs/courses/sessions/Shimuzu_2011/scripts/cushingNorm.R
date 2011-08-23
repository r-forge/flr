function () 
{
    logl <- function(a, b, rec, ssb) loglAR1(rec, a * 
        ssb^b)
    initial <- structure(function(rec, ssb) {
        a <- mean(rec/ssb)
        b <- 1
        return(FLPar(a = a, b = b))
    }, lower = c(-Inf, -Inf), upper = c(Inf, Inf))
    model <- rec ~ a * ssb^b
    return(list(logl = logl, model = model, initial = initial))
}
