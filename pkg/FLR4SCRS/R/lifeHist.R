## eqn
lengthAtAge<-function(age=0,Linf=Linf,K=K,t0=t0) {
       lenAge = Linf*(1-exp(-K*(age-t0)))

       return(lenAge)}

## eqn
weightAtAge<-function(size=0,alpha=wtA,beta=wtB) {
       wtAge = alpha*(size^beta)

       return(wtAge)}

## eqn
kLarvae<-function(wtEgg=wtEgg,t0=t0) {
        W0       = weightAtAge(size=lengthAtAge(age=0,Linf=Linf,K=K,t0=t0),alpha=a_w,beta=b_w)*1000
        w0       = wtEgg
        days     = floor(abs(t0*365.25))
        kLarvae  = (log(W0)-log(w0))/days

        return(kLarvae) }

## eqn
massEggLarvae <- function(days,kLarvae=kLarvae,t0=t0) {
        daysMax = floor(abs(t0*365.25))

        if (days > daysMax)
            days <- daysMax

        massEggLarvae =  wtEgg*exp((kLarvae)*days)

        return(massEggLarvae)}

## eqn
MlEggJuv <- function(massEggLarvae=massEggLarvae) {
        if (massEggLarvae < (wc*4.76))
           return(mortEgg[1]*(massEggLarvae**mortEgg[2]))
        else
           return(mortLarvae[1]*(massEggLarvae**mortLarvae[2]))
        }

## eqn
Ma_eggjuv_fun <- function(days) {
        days_max = floor(abs(t0*365.25))
        if (days > days_max) days <- days_max
        k = kLarvae(wtEgg=wtEgg,t0=t0)
        tmp <- matrix(NA,nrow=days,ncol=2)
        for (i in 1:days) {
            tmp[i,1] <- massEggLarvae(days=i,kLarvae=k,t0=t0)
            tmp[i,2] <- MlEggJuv(massEggLarvae=tmp[i,1]) }
        Ma_eggjuv <- sum(tmp[,2])
        return(Ma_eggjuv)}

## eqn
pm_age_fun <- function(age=age,a50=a50,a50sigma=a50sigma) {
        pm_age = exp((age-a50)/a50sigma)/(1+(exp((age-a50)/a50sigma)))
        return(pm_age)  }

## eqn
M_rgamma_fun <- function(n,M_age_bar,M_age_nu) {
                  x <- vector("numeric",length=length(n))
                  alpha <- M_age_nu
                  beta <- M_age_bar/alpha
                  x = rgamma(n,shape=alpha,scale=beta)
                  return(x)}

## eqn
M_dgamma_fun <- function(x,M_age_bar,M_age_nu) {
                  y <- vector("numeric",length=length(x))
                  alpha <- M_age_nu
                  beta <- M_age_bar/alpha
                  y = dgamma(x,shape=alpha,scale=beta)
                  return(y)}

#   using linear interpolation within an age group, it is not the most efficient
## eqn
Surviv_func <- function(age,M_age_vec)  {
    if(age > (nages+1)) stop("Age is greater than last age group")
    ma <- c(0.0,cumsum(M_age_vec),20)  # get bounds for the survival of ages 0-1 and + group
    ia <- as.integer(age)
    x <- ma[ia+1]+(age-ia)*M_age_vec[ia+1]
    S_age = exp(-x)
      return(S_age)}

## eqn
spaw_freq_fun <- function(spaw_days,spaw_period) {
         spaw_freq = (365.25/spaw_days)*spaw_period
         return(spaw_freq)}

## eqn
Eggs_age_fun <- function(age,b0=b0_spaw_eggs_size,b1=b1_spaw_eggs_size) {
         Eggs_age <- max(0,(b0 + b1*lengthAtAge(age,Linf,K,t0)))
         return(Eggs_age) }

## eqn
w_bar_fun <- function(maxage=nages) {
         w_bar <- 0.0
         for (i in 1:maxage) {
            tmp <- Surviv_func(age=i,M_age_vec)*weightAtAge(size=lengthAtAge(age=i,Linf,K,t0),a_w,b_w)*pm_age_fun(age=i,a50,a50sigma)
            w_bar <- w_bar + tmp}
         return(w_bar) }

## eqn
RecruitsPB_fun <- function(age) {
        RecruitsPB <- vector("numeric",length=length(age))
        Ma <- Ma_eggjuv_fun(days=floor(abs(t0*365.25)))
        sf <- spaw_freq_fun(spaw_days,spaw_period)
        for(i in 1:length(age)){
           Recruits      <- exp(log(Eggs_age_fun(age[i],b0_spaw_eggs_size,b1_spaw_eggs_size))-Ma)*sf
           Wa            <- weightAtAge(lengthAtAge(age[i],Linf,K,t0),a_w,b_w)
           RecruitsPB[i] <- Recruits/Wa}

        return(RecruitsPB)}

## eqn
alpha_est_K <- function(age) {
        eps_age <- vector("numeric",length=length(age))
        wgt_age <- vector("numeric",length=length(age))
        w0 <- weightAtAge(size=lengthAtAge(age=0,Linf,K,t0),a_w,b_w)
        Sl <- exp(-Ma_eggjuv_fun(days=floor(abs(t0*365.25))))
        sf <- spaw_freq_fun(spaw_days,spaw_period)
        for(i in 1:Nf) {
          eps_age[i] <- Eggs_age_fun(age[i],b0_spaw_eggs_size,b1_spaw_eggs_size)
          wgt_age[i] <- weightAtAge(lengthAtAge(age[i],Linf,K,t0),a_w,b_w)}

        alpha_est <- w0*Sl*sf*(sum(eps_age)/sum(wgt_age))
       return(alpha_est)   }

## eqn
h_est_func <- function(alpha=alpha_ageAnk) {
     h_est <- vector("numeric",length=length(alpha))
     for(i in 1:length(alpha)){
        h_est[i] = (alpha[i]*p_fem*w_bar_fun(nages))/(4+alpha[i]*p_fem*w_bar_fun(nages))
        }
     return(h_est) }

## eqn
get_ageAnk_func <- function(x,M_age_vec) {
        ma <- c(0,cumsum(M_age_vec),28)    # need to define the boundaries for age 0 and above plus
        il <- 0; ih <-0;
        for(i in 1:(length(ma)-1)) {
            if(-log(x) >= ma[i]) {ih <- i} }
        il <- (ih - 1)
        ageX <- il +((-log(x)-(ma[ih]))/(ma[ih+1]-ma[ih]))
        return(ageX)}
