msyDeriv<-list()
msyDeriv[["schaefer"]]<-list("msy","bmsy","fmsy")
msyDeriv[["schaefer"]][[ "msy"]]<-list("r","K")
msyDeriv[["schaefer"]][["bmsy"]]<-list("r","K")
msyDeriv[["schaefer"]][["fmsy"]]<-list("r","K")
msyDeriv[["schaefer"]][[ "msy"]][["r"]]<-function(r,K) return(K/4)
msyDeriv[["schaefer"]][[ "msy"]][["K"]]<-function(r,K) return(r/4)
msyDeriv[["schaefer"]][["bmsy"]][["r"]]<-function(r,K) return(0*r)
msyDeriv[["schaefer"]][["bmsy"]][["K"]]<-function(r,K) return((r/r)/2)
msyDeriv[["schaefer"]][["fmsy"]][["r"]]<-function(r,K) return((r/r)/2)
msyDeriv[["schaefer"]][["fmsy"]][["K"]]<-function(r,K) return(0*r)

msyDeriv[["fox"]]<-list("msy","bmsy","fmsy")
msyDeriv[["fox"]][[ "msy"]]<-list("r","K")
msyDeriv[["fox"]][["bmsy"]]<-list("r","K")
msyDeriv[["fox"]][["fmsy"]]<-list("r","K")
msyDeriv[["fox"]][[ "msy"]][["r"]]<-function(r,K) return(msyFox(r,K))
msyDeriv[["fox"]][[ "msy"]][["K"]]<-function(r,K){.expr2 <- exp(-1)
                                                .expr4 <- r * (K * .expr2)
                                                .expr5 <- log(K)
                                                .expr6 <- .expr5 - 1
                                                .expr8 <- 1 - .expr6/.expr5
                                                .expr12<- 1/K

                                                return(r * .expr2 * .expr8 - .expr4 * (.expr12/.expr5 -.expr6 * .expr12/.expr5^2))
                                                }

msyDeriv[["fox"]][["bmsy"]][["r"]]<-function(r,K) return(0*r)
msyDeriv[["fox"]][["bmsy"]][["K"]]<-function(r,K) return(exp(-1))
msyDeriv[["fox"]][["fmsy"]][["r"]]<-function(r,K) return(bmsyFox(r,K))
msyDeriv[["fox"]][["fmsy"]][["K"]]<-function(r,K){.expr1 <- log(K)
                                                .expr2 <- .expr1 - 1
                                                .expr6 <- 1/K

                                                return(-(r * (.expr6/.expr1 - .expr2 * .expr6/.expr1^2)))
                                                }

#### Pella Tomlinson
msyDeriv[["pellat"]]<-list("msy","bmsy","fmsy")
msyDeriv[["pellat"]][[ "msy"]]<-list("r","K","p")
msyDeriv[["pellat"]][["bmsy"]]<-list("r","K","p")
msyDeriv[["pellat"]][["fmsy"]]<-list("r","K","p")

#### BMSY
#deriv(~(K/p)^(1/(p-1)),"r")
msyDeriv[["pellat"]][["bmsy"]][["r"]]<-function(r,K,p) return(0)
#deriv(~(K/p)^(1/(p-1)),"K")
msyDeriv[["pellat"]][["bmsy"]][["K"]]<-function(r,K,p){
    .expr1 <- K/p
    .expr3 <- 1/(p - 1)
    .value <- .expr1^.expr3

    .grad <- .expr1^(.expr3 - 1) * (.expr3 * (1/p))

    return(.grad)
    }

#deriv(~(K/p)^(1/(p-1)),"p")
msyDeriv[["pellat"]][["bmsy"]][["p"]]<-function(r,K,p){
    .expr1 <- K/p
    .expr2 <- p - 1
    .expr3 <- 1/.expr2
    .expr4 <- .expr1^.expr3
    .value <- .expr4

    .grad <- -(.expr4 * (log(.expr1) * (1/.expr2^2)) +
        .expr1^(.expr3 - 1) * (.expr3 * (K/p^2)))

    return(.grad)
    }

#### Fmsy
#deriv(~r*(1-(K/p)^p),"r")
msyDeriv[["pellat"]][["fmsy"]][["r"]]<-function(r,K) return(0*r)
#deriv(~r*(1-(K/p)^p),"K")
msyDeriv[["pellat"]][["fmsy"]][["K"]]<-function(r,K,p){
    .expr1 <- K/p
    .expr3 <- 1/(p - 1)

    .grad <- .expr1^(.expr3 - 1) * (.expr3 * (1/p))

    return(.grad)
    }
#deriv(~r*(1-(K/p)^p),"p")
msyDeriv[["pellat"]][["fmsy"]][["p"]]<-function(r,K,p){
    .expr1 <- K/p
    .expr2 <- p - 1
    .expr3 <- 1/.expr2
    .expr4 <- .expr1^.expr3

    .grad <- -(.expr4 * (log(.expr1) * (1/.expr2^2)) +
        .expr1^(.expr3 - 1) * (.expr3 * (K/p^2)))

    return(.grad)
    }

#### MSY
#deriv(~r*(K/p)^(1/(p-1))*(1-(K/p)^p),"r")
msyDeriv[["pellat"]][["msy"]][["r"]]<-function(r,K,p){
    .expr1 <- K/p
    .expr4 <- .expr1^(1/(p - 1))
    .expr7 <- 1 - .expr1^p

    .grad <- .expr4 * .expr7

    return(.grad)
    }
#deriv(~r*(K/p)^(1/(p-1))*(1-(K/p)^p),"K")
msyDeriv[["pellat"]][["msy"]][["K"]]<-function(r,K,p){
    .expr1 <- K/p
    .expr2 <- p - 1
    .expr3 <- 1/.expr2
    .expr5 <- r * .expr1^.expr3
    .expr7 <- 1 - .expr1^p
    .expr11 <- 1/p

    .grad <- r * (.expr1^(.expr3 - 1) * (.expr3 * .expr11)) *
        .expr7 - .expr5 * (.expr1^.expr2 * (p * .expr11))

    return(.grad)
    }
#deriv(~r*(K/p)^(1/(p-1))*(1-(K/p)^p),"p")
msyDeriv[["pellat"]][["msy"]][["p"]]<-function(r,K,p){
    .expr1 <- K/p
    .expr2 <- p - 1
    .expr3 <- 1/.expr2
    .expr4 <- .expr1^.expr3
    .expr5 <- r * .expr4
    .expr6 <- .expr1^p
    .expr7 <- 1 - .expr6
    .expr9 <- log(.expr1)
    .expr13 <- K/p^2
    .value <- .expr5 * .expr7

    .grad <- -(.expr5 * (.expr6 * .expr9 - .expr1^.expr2 *
        (p * .expr13)) + r * (.expr4 * (.expr9 * (1/.expr2^2)) +
        .expr1^(.expr3 - 1) * (.expr3 * .expr13)) * .expr7)

    return(.grad)
    }

msyDeriv[["gulland"]]<-list("msy","bmsy","fmsy")
msyDeriv[["gulland"]][[ "msy"]]<-list("r","K")
msyDeriv[["gulland"]][["bmsy"]]<-list("r","K")
msyDeriv[["gulland"]][["fmsy"]]<-list("r","K")
msyDeriv[["gulland"]][[ "msy"]][["r"]]<-function(r,K) return(K^2/4)
msyDeriv[["gulland"]][[ "msy"]][["K"]]<-function(r,K) return(r*K/2)
msyDeriv[["gulland"]][["bmsy"]][["r"]]<-function(r,K) return(0*r)
msyDeriv[["gulland"]][["bmsy"]][["K"]]<-function(r,K) return((r/r)/2)
msyDeriv[["gulland"]][["fmsy"]][["r"]]<-function(r,K) return(r/2)
msyDeriv[["gulland"]][["fmsy"]][["K"]]<-function(r,K) return(K/2)

msyDeriv[["fletcher"]]<-list("msy","bmsy","fmsy")
msyDeriv[["fletcher"]][[ "msy"]]<-list("K","msy","p")
msyDeriv[["fletcher"]][["bmsy"]]<-list("K","msy","p")
msyDeriv[["fletcher"]][["fmsy"]]<-list("K","msy","p")
msyDeriv[["fletcher"]][[ "msy"]][["K"]]  <-function(K,msy,p) return(0*K)
msyDeriv[["fletcher"]][[ "msy"]][["msy"]]<-function(K,msy,p) return(K/K)
msyDeriv[["fletcher"]][[ "msy"]][["p"]]  <-function(K,msy,p) return(0*K)

### BMSY
#deriv(~K^(p-1)*(1/p)^(1/(p-1)),"K")
msyDeriv[["fletcher"]][["bmsy"]][["K"]]  <-function(K,msy,p){
    .expr1 <- p - 1
    .expr5 <- (1/p)^(1/.expr1)

    .grad <- K^(.expr1 - 1) * .expr1 * .expr5

    return(.grad)
    }
msyDeriv[["fletcher"]][[ "bmsy"]][["msy"]]<-function(K,msy,p) return(0*K)
#deriv(~K^(p-1)*(1/p)^(1/(p-1)),"p")
msyDeriv[["fletcher"]][["bmsy"]][["p"]]  <-function(K,msy,p){
    .expr1 <- p - 1
    .expr2 <- K^.expr1
    .expr3 <- 1/p
    .expr4 <- 1/.expr1
    .expr5 <- .expr3^.expr4

    .grad <- .expr2 * log(K) * .expr5 - .expr2 * (.expr5 *
        (log(.expr3) * (1/.expr1^2)) + .expr3^(.expr4 - 1) *
        (.expr4 * (1/p^2)))

    return(.grad)
    }
### FMSY
#deriv(~MSY/(K^(p-1)*(1/p)^(1/(p-1))),"K")
msyDeriv[["fletcher"]][["msy"]][["K"]]  <-function(K,msy,p){
    .expr1 <- p - 1
    .expr5 <- (1/p)^(1/.expr1)
    .expr6 <- K^.expr1 * .expr5

    .grad <- -(MSY * (K^(.expr1 - 1) * .expr1 * .expr5)/.expr6^2)

    return(.grad)
    }

#deriv(~MSY/(K^(p-1)*(1/p)^(1/(p-1))),"MSY")
msyDeriv[["fletcher"]][["msy"]][["msy"]]  <-function(K,msy,p){
    .expr1 <- p - 1
    .expr6 <- K^.expr1 * (1/p)^(1/.expr1)

    .grad <- 1/.expr6

    return(.grad)
    }

#deriv(~MSY/(K^(p-1)*(1/p)^(1/(p-1))),"p")
msyDeriv[["fletcher"]][["msy"]][["p"]]  <-function(K,msy,p){
    .expr1 <- p - 1
    .expr2 <- K^.expr1
    .expr3 <- 1/p
    .expr4 <- 1/.expr1
    .expr5 <- .expr3^.expr4
    .expr6 <- .expr2 * .expr5

    .grad <- -(MSY * (.expr2 * log(K) * .expr5 - .expr2 *
        (.expr5 * (log(.expr3) * (1/.expr1^2)) + .expr3^(.expr4 -
            1) * (.expr4 * (1/p^2))))/.expr6^2)

    return(.grad)
    }


msyDeriv[["shepherd"]]<-list("msy","bmsy","fmsy")
msyDeriv[["shepherd"]][[ "msy"]]<-list("r","K")
msyDeriv[["shepherd"]][["bmsy"]]<-list("r","K")
msyDeriv[["shepherd"]][["fmsy"]]<-list("r","K")

#### FMSY
#deriv(~K/((r-1)^.5),"K")
msyDeriv[["shepherd"]][[ "fmsy"]][["K"]]<-function(r,K,M){
    .expr2 <- (r - 1)^0.5
    .value <- K/.expr2
    .grad <- array(0, c(length(.value), 1L), list(NULL, c("K")))
    .grad <- 1/.expr2

    return(.grad)
    }
#deriv(~K/((r-1)^.5),"r")
msyDeriv[["shepherd"]][[ "fmsy"]][["r"]]<-function(r,K,M){
    .expr1 <- r - 1
    .expr2 <- .expr1^0.5
    .value <- K/.expr2
    .grad <- array(0, c(length(.value), 1L), list(NULL, c("r")))
    .grad <- -(K * (0.5 * .expr1^-0.5)/.expr2^2)

    return(.grad)
    }
#deriv(~K/((r-1)^.5),"M")
msyDeriv[["shepherd"]][[ "fmsy"]][["M"]]<-function(r,K,M){
    .value <- K/(r - 1)^0.5
    .grad <- array(0, c(length(.value), 1L), list(NULL, c("M")))
    .grad <- 0

    return(.grad)
    }
#### FMSY
#deriv(~r/(1+(K/((r-1)^.5))/K)-M/(K/((r-1)^.5)),"K")
msyDeriv[["shepherd"]][[ "fmsy"]][["K"]]<-function(r,K,M){
    .expr2 <- (r - 1)^0.5
    .expr3 <- K/.expr2
    .expr5 <- 1 + .expr3/K
    .expr9 <- 1/.expr2
    .value <- r/.expr5 - M/.expr3
    .grad <- array(0, c(length(.value), 1L), list(NULL, c("K")))
    .grad <- -(r * (.expr9/K - .expr3/K^2)/.expr5^2 -
        M * .expr9/.expr3^2)

    return(.grad)
    }
#deriv(~r/(1+(K/((r-1)^.5))/K)-M/(K/((r-1)^.5)),"r")
msyDeriv[["shepherd"]][[ "fmsy"]][["r"]]<-function(r,K,M){
    .expr1 <- r - 1
    .expr2 <- .expr1^0.5
    .expr3 <- K/.expr2
    .expr5 <- 1 + .expr3/K
    .expr14 <- K * (0.5 * .expr1^-0.5)/.expr2^2
    .value <- r/.expr5 - M/.expr3
    .grad <- array(0, c(length(.value), 1L), list(NULL, c("r")))
    .grad <- 1/.expr5 + r * (.expr14/K)/.expr5^2 - M *
        .expr14/.expr3^2

    return(.grad)
    }
#deriv(~r/(1+(K/((r-1)^.5))/K)-M/(K/((r-1)^.5)),"M")
msyDeriv[["shepherd"]][[ "fmsy"]][["M"]]<-function(r,K,M){
    .expr3 <- K/(r - 1)^0.5
    .value <- r/(1 + .expr3/K) - M/.expr3
    .grad <- array(0, c(length(.value), 1L), list(NULL, c("M")))
    .grad <- -(1/.expr3)

    return(.grad)
    }
#### MSY
#deriv(~r*(K/((r-1)^.5))/(1+(K/((r-1)^.5))/K)-M,"K")
msyDeriv[["shepherd"]][[ "msy"]][["K"]]<-function(r,K,M){
    .expr2 <- (r - 1)^0.5
    .expr3 <- K/.expr2
    .expr4 <- r * .expr3
    .expr6 <- 1 + .expr3/K
    .expr9 <- 1/.expr2
    .value <- .expr4/.expr6 - M
    .grad <- array(0, c(length(.value), 1L), list(NULL, c("K")))
    .grad <- r * .expr9/.expr6 - .expr4 * (.expr9/K -
        .expr3/K^2)/.expr6^2

    return(.grad)
    }
#deriv(~r*(K/((r-1)^.5))/(1+(K/((r-1)^.5))/K)-M,"r")
msyDeriv[["shepherd"]][[ "msy"]][["r"]]<-function(r,K,M){
    .expr1 <- r - 1
    .expr2 <- .expr1^0.5
    .expr3 <- K/.expr2
    .expr4 <- r * .expr3
    .expr6 <- 1 + .expr3/K
    .expr13 <- K * (0.5 * .expr1^-0.5)/.expr2^2
    .value <- .expr4/.expr6 - M
    .grad <- array(0, c(length(.value), 1L), list(NULL, c("r")))
    .grad <- (.expr3 - r * .expr13)/.expr6 + .expr4 *
        (.expr13/K)/.expr6^2

    return(.grad)
    }
#deriv(~r*(K/((r-1)^.5))/(1+(K/((r-1)^.5))/K)-M,"M")
msyDeriv[["shepherd"]][[ "msy"]][["M"]]<-function(r,K,M) return(1)
