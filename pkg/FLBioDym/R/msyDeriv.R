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

msyDeriv[["fox"]][[ "msy"]][["r"]]<-function(r,K){.expr3 <- K * exp(-1)
                                                  .expr5 <- log(K)
                                                  .expr8 <- 1 - (.expr5 - 1)/.expr5
                                                  .grad <- .expr3 * .expr8

                                                  return(.grad)
                                                  }

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
msyDeriv[["fox"]][["fmsy"]][["r"]]<-function(r,K) {.expr1 <- log(K)
                                                   .expr4 <- 1 - (.expr1 - 1)/.expr1
                                                   .grad <- .expr4

                                                   return(.grad)
                                                   }

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

#### Bmsy
#deriv(~(K/p)^(1/(p-1)),"r")
msyDeriv[["pellat"]][["bmsy"]][["r"]]<-function(r,K,p=2) return(0)
#deriv(~(K/p)^(1/(p-1)),"K")
msyDeriv[["pellat"]][["bmsy"]][["K"]]<-function(r,K,p=2){
    .expr1 <- K/p
    .expr3 <- 1/(p - 1)
    .value <- .expr1^.expr3

    .grad <- .expr1^(.expr3 - 1) * (.expr3 * (1/p))

    return(.grad)
    }

#deriv(~(K/p)^(1/(p-1)),"p")
msyDeriv[["pellat"]][["bmsy"]][["p"]]<-function(r,K,p=2){
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
#deriv(~r-r/K*(K/p),"r")
#deriv(~r-r/K*(K/p),"K")
#deriv(~r-r/K*(K/p),"p")
msyDeriv[["pellat"]][["fmsy"]][["r"]]<-function(r,K,p=2){
    .expr2 <- K/p

    .grad <- 1 - 1/K * .expr2

     return(.grad)
     }

msyDeriv[["pellat"]][["fmsy"]][["K"]]<-function(r,K,p=2){
    .expr1 <- r/K
    .expr2 <- K/p
    .value <- r - .expr1 * .expr2

    .grad <- -(.expr1 * (1/p) - r/K^2 * .expr2)
    
    return(.grad)
    }

msyDeriv[["pellat"]][["fmsy"]][["p"]]<-function(r,K,p=2){
    .expr1 <- r/K
    .value <- r - .expr1 * (K/p)

    .grad <- .expr1 * (K/p^2)

    return(.grad)
    }

#### msy
#deriv(~r*((K/p)^(1/(p-1)))-r/K*((K/p)^(p/(p-1))),"r")
msyDeriv[["pellat"]][["msy"]][["r"]]<-function(r,K,p=2){
    .expr1 <- K/p
    .expr2 <- p - 1
    .expr4 <- .expr1^(1/.expr2)
    .expr8 <- .expr1^(p/.expr2)

    .grad <- .expr4 - 1/K * .expr8

     return(.grad)
     }

#deriv(~r*(K/p)^(1/(p-1))-r/K*(K/p)^(p/(p-1)),"K")
msyDeriv[["pellat"]][["msy"]][["K"]]<-function(r,K,p=2){

    .expr1 <- K/p
    .expr2 <- p - 1
    .expr3 <- 1/.expr2
    .expr6 <- r/K
    .expr7 <- p/.expr2
    .expr8 <- .expr1^.expr7
    .expr13 <- 1/p

    .grad <- r * (.expr1^(.expr3 - 1) * (.expr3 * .expr13)) -
        (.expr6 * (.expr1^(.expr7 - 1) * (.expr7 * .expr13)) -
            r/K^2 * .expr8)

    return(.grad)
    }
#deriv(~r*((K/p)^(1/(p-1)))-r/K*((K/p)^(p/(p-1))),"p")
msyDeriv[["pellat"]][["msy"]][["p"]]<-function(r,K,p=2){
    .expr1 <- K/p
    .expr2 <- p - 1
    .expr3 <- 1/.expr2
    .expr4 <- .expr1^.expr3
    .expr6 <- r/K
    .expr7 <- p/.expr2
    .expr8 <- .expr1^.expr7
    .expr11 <- log(.expr1)
    .expr12 <- .expr2^2
    .expr19 <- K/p^2
    .value <- r * .expr4 - .expr6 * .expr8

    .grad <- -(r * (.expr4 * (.expr11 * (1/.expr12)) +
        .expr1^(.expr3 - 1) * (.expr3 * .expr19)) + .expr6 *
        (.expr8 * (.expr11 * (.expr3 - p/.expr12)) - .expr1^(.expr7 -
            1) * (.expr7 * .expr19)))

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
msyDeriv[["gulland"]][["fmsy"]][["r"]]<-function(r,K) return(K/2)
msyDeriv[["gulland"]][["fmsy"]][["K"]]<-function(r,K) return(r/2)

msyDeriv[["fletcher"]]<-list("msy","bmsy","fmsy")
msyDeriv[["fletcher"]][[ "msy"]]<-list("K","msy","p")
msyDeriv[["fletcher"]][["bmsy"]]<-list("K","msy","p")
msyDeriv[["fletcher"]][["fmsy"]]<-list("K","msy","p")
msyDeriv[["fletcher"]][[ "msy"]][["K"]]  <-function(K,msy,p) return(0*K)
msyDeriv[["fletcher"]][[ "msy"]][["msy"]]<-function(K,msy,p) return(K/K)
msyDeriv[["fletcher"]][[ "msy"]][["p"]]  <-function(K,msy,p) return(0*K)

### Bmsy
#deriv(~K^(p-1)*(1/p)^(1/(p-1)),"K")
msyDeriv[["fletcher"]][["bmsy"]][["K"]]  <-function(K,msy,p=2){

    .expr1 <- p - 1
    .expr5 <- (1/p)^(1/.expr1)

    .grad <- K^(.expr1 - 1) * .expr1 * .expr5

    return(.grad)
    }
msyDeriv[["fletcher"]][[ "bmsy"]][["msy"]]<-function(K,msy,p=2) return(0*K)

#deriv(~K*(1/p)^(1/(p-1)),"p")
msyDeriv[["fletcher"]][["bmsy"]][["p"]]  <-function(K,msy,p){
    .expr1 <- 1/p
    .expr2 <- p - 1
    .expr3 <- 1/.expr2
    .expr4 <- .expr1^.expr3
    .value <- K * .expr4

    .grad <- -(K * (.expr4 * (log(.expr1) * (1/.expr2^2)) +
        .expr1^(.expr3 - 1) * (.expr3 * (1/p^2))))

    return(.grad)
    }
### Fmsy
#deriv(~msy/(K^(p-1)*(1/p)^(1/(p-1))),"K")
msyDeriv[["fletcher"]][["fmsy"]][["K"]]  <-function(K,msy,p=2){
    .expr1 <- p - 1
    .expr5 <- (1/p)^(1/.expr1)
    .expr6 <- K^.expr1 * .expr5

    .grad <- -(msy * (K^(.expr1 - 1) * .expr1 * .expr5)/.expr6^2)

    return(.grad)
    }

#deriv(~msy/(K^(p-1)*(1/p)^(1/(p-1))),"msy")
msyDeriv[["fletcher"]][["fmsy"]][["msy"]]  <-function(K,msy,p=2){
    .expr1 <- p - 1
    .expr6 <- K^.expr1 * (1/p)^(1/.expr1)

    .grad <- 1/.expr6

    return(.grad)
    }

#deriv(~msy/(K*(1/p)^(1/(p-1))),"p")
msyDeriv[["fletcher"]][["fmsy"]][["p"]]  <-function(K,msy,p=2){
    .expr1 <- 1/p
    .expr2 <- p - 1
    .expr3 <- 1/.expr2
    .expr4 <- .expr1^.expr3
    .expr5 <- K * .expr4

    .grad <- msy * (K * (.expr4 * (log(.expr1) * (1/.expr2^2)) +
        .expr1^(.expr3 - 1) * (.expr3 * (1/p^2))))/.expr5^2

    return(.grad)
    }


msyDeriv[["shepherd"]]<-list("msy","bmsy","fmsy")
msyDeriv[["shepherd"]][[ "msy"]]<-list("r","K")
msyDeriv[["shepherd"]][["bmsy"]]<-list("r","K")
msyDeriv[["shepherd"]][["fmsy"]]<-list("r","K")

#### BMSY
#deriv(~(K*(r/m - 1))*((1+(r/m - 1))^.5-1)/(r/m - 1),"r")
#deriv(~(K*(r/m - 1))*((1+(r/m - 1))^.5-1)/(r/m - 1),"K")
#deriv(~(K*(r/m - 1))*((1+(r/m - 1))^.5-1)/(r/m - 1),"m")

msyDeriv[["shepherd"]][["bmsy"]][["r"]]  <-function(r,K,m){
    .expr2 <- r/m - 1
    .expr3 <- K * .expr2
    .expr4 <- 1 + .expr2
    .expr6 <- .expr4^0.5 - 1
    .expr7 <- .expr3 * .expr6
    .expr9 <- 1/m

    .grad <- (K * .expr9 * .expr6 + .expr3 * (0.5 * (.expr9 *
          .expr4^-0.5)))/.expr2 - .expr7 * .expr9/.expr2^2

    return(.grad)
    }
    
msyDeriv[["shepherd"]][["bmsy"]][["K"]]  <-function(r,K,m){
    .expr2 <- r/m - 1
    .expr6 <- (1 + .expr2)^0.5 - 1
    .value <- K * .expr2 * .expr6/.expr2
    .grad <- array(0, c(length(.value), 1L), list(NULL, c("K")))
    .grad <- .expr2 * .expr6/.expr2

    return(.grad)
    }

msyDeriv[["shepherd"]][["bmsy"]][["m"]]  <-function(r,K,m){
    .expr2 <- r/m - 1
    .expr3 <- K * .expr2
    .expr4 <- 1 + .expr2
    .expr6 <- .expr4^0.5 - 1
    .expr7 <- .expr3 * .expr6
    .expr10 <- r/m^2
    .value <- .expr7/.expr2
    .grad <- array(0, c(length(.value), 1L), list(NULL, c("m")))
    .grad <- -((.expr3 * (0.5 * (.expr10 * .expr4^-0.5)) +
        K * .expr10 * .expr6)/.expr2 - .expr7 * .expr10/.expr2^2)

    return(.grad)
    }


#### MSY
#deriv(~(r/m - 1)*m*((K*(r/m - 1))*((1+(r/m - 1))^.5-1)/(r/m - 1))*(1-((K*(r/m - 1))*((1+(r/m - 1))^.5-1)/(r/m - 1))/(K*(r/m - 1)))/(1+(r/m - 1))^.5,"r")
#deriv(~(r/m - 1)*m*((K*(r/m - 1))*((1+(r/m - 1))^.5-1)/(r/m - 1))*(1-((K*(r/m - 1))*((1+(r/m - 1))^.5-1)/(r/m - 1))/(K*(r/m - 1)))/(1+(r/m - 1))^.5,"K")
#deriv(~(r/m - 1)*m*((K*(r/m - 1))*((1+(r/m - 1))^.5-1)/(r/m - 1))*(1-((K*(r/m - 1))*((1+(r/m - 1))^.5-1)/(r/m - 1))/(K*(r/m - 1)))/(1+(r/m - 1))^.5,"m")

msyDeriv[["shepherd"]][["msy"]][["r"]]  <-function(r,K,m){
    .expr2 <- r/m - 1
    .expr3 <- .expr2 * m
    .expr4 <- K * .expr2
    .expr5 <- 1 + .expr2
    .expr6 <- .expr5^0.5
    .expr7 <- .expr6 - 1
    .expr8 <- .expr4 * .expr7
    .expr9 <- .expr8/.expr2
    .expr10 <- .expr3 * .expr9
    .expr12 <- 1 - .expr9/.expr4
    .expr13 <- .expr10 * .expr12
    .expr15 <- 1/m
    .expr18 <- K * .expr15
    .expr22 <- 0.5 * (.expr15 * .expr5^-0.5)
    .expr29 <- (.expr18 * .expr7 + .expr4 * .expr22)/.expr2 -
        .expr8 * .expr15/.expr2^2

    .grad <- ((.expr15 * m * .expr9 + .expr3 * .expr29) *
        .expr12 - .expr10 * (.expr29/.expr4 - .expr9 * .expr18/.expr4^2))/.expr6 -
        .expr13 * .expr22/.expr6^2

    return(.grad)
    }

msyDeriv[["shepherd"]][["msy"]][["K"]]  <-function(r,K,m){
    .expr2 <- r/m - 1
    .expr3 <- .expr2 * m
    .expr4 <- K * .expr2
    .expr6 <- (1 + .expr2)^0.5
    .expr7 <- .expr6 - 1
    .expr9 <- .expr4 * .expr7/.expr2
    .expr10 <- .expr3 * .expr9
    .expr12 <- 1 - .expr9/.expr4
    .expr16 <- .expr2 * .expr7/.expr2

    .grad <- (.expr3 * .expr16 * .expr12 - .expr10 * (.expr16/.expr4 -
        .expr9 * .expr2/.expr4^2))/.expr6

    return(.grad)
    }

msyDeriv[["shepherd"]][["msy"]][["m"]]  <-function(r,K,m){
    .expr2 <- r/m - 1
    .expr3 <- .expr2 * m
    .expr4 <- K * .expr2
    .expr5 <- 1 + .expr2
    .expr6 <- .expr5^0.5
    .expr7 <- .expr6 - 1
    .expr8 <- .expr4 * .expr7
    .expr9 <- .expr8/.expr2
    .expr10 <- .expr3 * .expr9
    .expr12 <- 1 - .expr9/.expr4
    .expr13 <- .expr10 * .expr12
    .expr16 <- r/m^2
    .expr22 <- 0.5 * (.expr16 * .expr5^-0.5)
    .expr24 <- K * .expr16
    .expr31 <- (.expr4 * .expr22 + .expr24 * .expr7)/.expr2 -
        .expr8 * .expr16/.expr2^2

    .grad <- (((.expr2 - .expr16 * m) * .expr9 - .expr3 *
        .expr31) * .expr12 + .expr10 * (.expr31/.expr4 - .expr9 *
        .expr24/.expr4^2))/.expr6 + .expr13 * .expr22/.expr6^2

    return(.grad)
    }



#### FMSY
#deriv(~(r/m - 1)*m**(1-((K*(r/m - 1))*((1+(r/m - 1))^.5-1)/(r/m - 1))/(K*(r/m - 1)))/(1+(r/m - 1))^.5,"r")
#deriv(~(r/m - 1)*m**(1-((K*(r/m - 1))*((1+(r/m - 1))^.5-1)/(r/m - 1))/(K*(r/m - 1)))/(1+(r/m - 1))^.5,"K")
#deriv(~(r/m - 1)*m**(1-((K*(r/m - 1))*((1+(r/m - 1))^.5-1)/(r/m - 1))/(K*(r/m - 1)))/(1+(r/m - 1))^.5,"m")
msyDeriv[["shepherd"]][["fmsy"]][["r"]]  <-function(r,K,m){
    .expr2 <- r/m - 1
    .expr3 <- .expr2 * m
    .expr4 <- K * .expr2
    .expr5 <- 1 + .expr2
    .expr6 <- .expr5^0.5
    .expr7 <- .expr6 - 1
    .expr8 <- .expr4 * .expr7
    .expr9 <- .expr8/.expr2
    .expr11 <- 1 - .expr9/.expr4
    .expr12 <- .expr3 * .expr11
    .expr14 <- 1/m
    .expr17 <- K * .expr14
    .expr21 <- 0.5 * (.expr14 * .expr5^-0.5)

    .grad <- (.expr14 * m * .expr11 - .expr3 * (((.expr17 *
        .expr7 + .expr4 * .expr21)/.expr2 - .expr8 * .expr14/.expr2^2)/.expr4 -
        .expr9 * .expr17/.expr4^2))/.expr6 - .expr12 * .expr21/.expr6^2

    return(.grad)
    }

msyDeriv[["shepherd"]][["fmsy"]][["K"]]  <-function(r,K,m){
    .expr2 <- r/m - 1
    .expr3 <- .expr2 * m
    .expr4 <- K * .expr2
    .expr6 <- (1 + .expr2)^0.5
    .expr7 <- .expr6 - 1
    .expr9 <- .expr4 * .expr7/.expr2

    .grad <- -(.expr3 * (.expr2 * .expr7/.expr2/.expr4 -
        .expr9 * .expr2/.expr4^2)/.expr6)

    return(.grad)
    }

msyDeriv[["shepherd"]][["fmsy"]][["m"]]  <-function(r,K,m){
    .expr2 <- r/m - 1
    .expr3 <- .expr2 * m
    .expr4 <- K * .expr2
    .expr5 <- 1 + .expr2
    .expr6 <- .expr5^0.5
    .expr7 <- .expr6 - 1
    .expr8 <- .expr4 * .expr7
    .expr9 <- .expr8/.expr2
    .expr11 <- 1 - .expr9/.expr4
    .expr12 <- .expr3 * .expr11
    .expr15 <- r/m^2
    .expr21 <- 0.5 * (.expr15 * .expr5^-0.5)
    .expr23 <- K * .expr15

    .grad <- ((.expr2 - .expr15 * m) * .expr11 + .expr3 *
        (((.expr4 * .expr21 + .expr23 * .expr7)/.expr2 - .expr8 *
            .expr15/.expr2^2)/.expr4 - .expr9 * .expr23/.expr4^2))/.expr6 +
        .expr12 * .expr21/.expr6^2

    return(.grad)
    }

