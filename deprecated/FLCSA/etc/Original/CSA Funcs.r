#  ----------------------------------------------------
#                 CSAo
###  Fitting Catch-Survey Analysis - Observation error
#     NEW (March 04): Post-recruits q searched as parameter
#      to simplify Jacobian used for approx. Var-Cov
#     Catch data now by stage
#     Missing data allowed, BUT treatment perhaps not ideal
# Version 15/03/2004
#  ----------------------------------------------------

# To run any suite of commands, highlight relevant (bits of) statements and press F10
#  !!WARNING!! do not run this script in one go (but by steps);
#   many lines are flagged to avoid unwanted damage, should this happen.

# You must first run (once !) the functions given at the end of this script
#  to 'install' them on your system

##      NOTE about missing indices:
#  - These should be coded as values =< 0, and preferably < 0 (negative),
#     NOT as NA !, in your external file or in the loaded dataframe.
#  - No limit is set on numbers of missing, but wise to have as few as possible
#  - Current treatment is to force residual to 0.0 (log[1.0]) wherever index is missing;
#     the alternative is to exclude these residuals; in the end it gives the same CSA results
#     but causes problems with plots of residual (mis-aligned with Years)
# Check:  CSdat$Year[CSdat$Urec <=0]    CSdat$Year[CSdat$Ufull <=0]
#nmiss <- length(CSdat$Year[CSdat$Urec <=0])+length(CSdat$Year[CSdat$Ufull <=0])

#
#  STEP 1: Import input data
#  ==========================
#   * Method 1: using SP2000 menu
#      Click File > Import Data > from file  in main Menu bar
#      Set file type (all) in drop menu
#      Select directory in browse pane, click file name (eg. sim_DAL20.dat)
#      .dat extension forces Gauss type; reset to ASCII text.
#      Check/adjust name of receiving Dataframe (eg. sim.DAL20)
#      In Options page: set Name row (3), Start row (4), End row (23), as.factors= never
#      If all OK, click Open button. A Data sheet pops up; check.
#      You may want to change the type of the Year column to Integer.

#   * Method 2: command mode, with read.table
#  First copy the input data file to a temporary file (eg. 'dummy.txt') using file manager
#  Open it in a text editor ; strip the first 2 lines and anything below the data table, 
#   including the M value; save and exit editor.
#  Specify your home (or data) directory, eg.:
#     Unix: (because ~ seems no longer recognised)
#homed <- "/home/...etc.../mydata/"
#     Windows:  need to use \\ for \
#homed <- paste("c:\\tempo\\")
#  Then full file name
#fich <- paste(homed,"dummy.txt",sep="")  #!! don't forget sep
#  Import (here assumes fields are comma-separated; otherwise change sep)
#sim.DAL20 <- read.table(fich,header=T,as.is=T,sep=",")

#
#  STEP 2: Copy data to generic object
#  ====================================
#  !! IMPORTANT: all functions refer to generic name CSdat
CSdat <- sim.DAL20

#  !! Moreover, all data items are identified by standard names (Watch case)
#    If you did not use these in your file, rename by running:
# names(CSdat) <- c("Year","CatRec","CatFull","Urec","Ufull","Wrec","Wfull","Srat")
 
#  !! And save original Srat, to preserve year specific relative values if given
Srat.ori <- CSdat$Srat

#  Optional: Rescale catches to '000  (to beautify plots)   !!!! Only do it ONCE  !!!
CSdat$CatRec <- CSdat$CatRec/1000
CSdat$CatFull <- CSdat$CatFull/1000

# !! ALWAYS !! : Sum up total catch into extra column
CSdat$CatchN <- CSdat$CatRec+CSdat$CatFull

#
#  STEP 3: Set constants
#  =====================
nyr <- length(CSdat[,1])  # nbr of Years
npar <- nyr+1             # nbr of parameters
nrt <- 2*nyr-1            # nbr of residual terms

#
#  STEP 4: Choose options for this CSA run (re-enter here for replays)
#  ========================================
# (i) Tau ; 0.5 for Catch discounted mid-year, or O for Catch at year start
tau <- 0.5

# (ii) Weight of error on Recruits indices, relative to error on FullRec indices
rwer <- 1.0

# (iii) M ; Column M is created if none exists, updated otherwise
natmor <- 0.2  
CSdat$M <- rep(natmor,nyr) 

# (iv) Catchability ratio s ; overwrites Srat column in CSdat
sfac <- 0.995                 # factor to scale original s values by year
CSdat$Srat <- sfac*Srat.ori   # !! make sure Srat.ori has been set first

# alternatively, for M & Srat, enter year-specific values using operator c()

#
#  STEP 5: Initialise & start fit
#  ===============================
#  Set starting parameter values for search
#   (kept old syntax, used by profiling in extra)
qini <- 1
vini <- CSini.f(CSdat,nyr,qini,tau)
CSmin <- nlregb(nres=nrt,start=vini,residuals=CSres.f,lower=rep(0,npar),
data=CSdat,tau=tau,lambda=rwer)
## IF error messages, input s ratio is probably inadequate;
#    readjust CSdat$Srat, then redo vini & CSmin commands
# Inspect CSmin for details, eg. CSmin$message  CSmin$objective  CSmin$residuals

# Now compute and display stock estimates of interest ; output is a list
#  First give a Title for your run: IMPORTANT: used later for run ID
rundesc <- "sim_DAL20 data - NO Q trend - tau= 0.5 - s= 0.995"
CSest <- CSout.f(CSmin,CSdat,tau,rwer,rundesc)
print(CSest,digits=5)

# Plot results     !! Watch: functions changed March 04 => re-load !!
#  This one plots residuals as bar-plots
CSplot1.f(CSest,CSmin)
#  But if you prefer line plots, try that one
CSplot2.f(CSest,CSmin)

# Save CSest to a unique object if you want to keep it and compare runs, e.g.
#DAL20.base <- CSest

# Optional: save CSA stock estimates to a file: 
#  re-uses same file specified in fich => rename it afterwards in a file manager window
# In SP2000, you can use File -> Export Data -> ... instead
# !this form is used because export.data does not work for list objects
#   and the format of files made by dump is not convenient
sink(fich)
print(CSest,digits=5)  # adjust digits to your taste
sink()


# Return to Step 4 for another run


###  Draft Jacobian on short example
Data8 <- CSdat[1:8,]
nyr <- 8
npar <- 9
nrt <- 15
w8 <- 10
vin8 <- CSini.f(Data8,nyr,qini,tau)
min8 <- nlregb(nres=nrt,start=vin8,residuals=CSres.f,lower=rep(0,npar),
data=Data8,tau=tau,lambda=w8)
rundesc <- "Data8 = 8 first years of sim_DAL20 - s= 0.995 - lambda= 10"
est8 <- CSout.f(min8,Data8,tau,w8,rundesc)

Jac <- CSjac.f(min8,est8)
print(Jac,digit=5)
# Unfortunately, Jac is singular => error in
solve(t(Jac) %*% Jac)
or in MASS function vcov.nlregb(min8)
# tried this, but gives silly CVs
ginverse(t(Jac) %*% Jac)
sqrt(min8$objective/(nrt-npar)* ginverse(t(Jac) %*% Jac))/min8$param


#      --------------------------------
##  Functions used:
	#      --------------------------------
#
CSini.f <- function(data,nyr,qini,tau){
	#   Version for q searched
	# Initialise CSA parameter vector given observed indices
	# replace missing (< 0) indices by mean of non-missing
	# rescale qini until Pop > Catch
	#
	v <- vector(length = nyr+1)
	lab <- vector(length = nyr+1)
	tmp <- rep(-1,nyr)
	ubar1 <- mean(data$Urec[data$Urec>0])
	ubar2 <- mean(data$Ufull[data$Ufull>0])
	qini <- qini*10
		while(any(tmp<0)) {
	qini <- qini/10
	v[1:nyr] <- (ifelse(data$Urec>0,data$Urec,ubar1))/(qini*data$Srat)  # Recruits
	v[nyr] <- (ifelse(data$Ufull[1]>0,data$Ufull[1],ubar2))/qini   # 1st year full
	tmp <- CSfwd.f(v,data,tau,nyr)
		}
	v[nyr+1] <- qini
	lab <- paste("R",1:nyr,sep="")
	lab[nyr] <- "N1"
	lab[nyr+1] <- "q"
	names(v) <- lab
	v
}

# ------
CSres.f <- function(Vpar,data,tau,lambda){
	#  NEW (March 04): version for q searched
	# Computes & returns CSA (log-)residuals 
	# Vpar is vector of 'current' parameter values, with RecruitsN in 
	#  positions [1:(nyr-1)], 1st year's FullN in position [nyr] and
	#  post-recruits q in position [nyr+1]
	# Version for nlregb: residuals not squared  (but should for ms())
	# Calls CSfwd.f => needs to pass tau
	#
	# Amended for missing (< 0) indices; 12/12/03
	# BUT nlregb() does NOT accept NA residuals => artificially forced to zero (log(1))
	# however, same CSA results obtained when corresponding residuals are excluded
	#
	nyr <- length(data$Year)

	nres <- 2*nyr-1
	vres <- vector(length = nres)
	
	w <- sqrt(lambda)
	Nfull <- CSfwd.f(Vpar,data,tau,nyr)
	qhat <- Vpar[nyr+1]
	tmp <- Vpar[1:nyr]   # for length consistent with Urec
	vres <- w*log(ifelse(data$Urec>0,(qhat*data$Srat*tmp/data$Urec),1))  # Recruits
	vres[nyr:nres] <- log(ifelse(data$Ufull>0,(qhat*Nfull/data$Ufull),1))   # Full
	vres
}

# ------
CSfwd.f <- function(Vpar,data,tau,nyr){
	# Project CSA equation forward to estimate fully recruited Ns 
	# given RecruitsN in Vpar[1 : nyr-1] and first year's FullN in Vpar[nyr]
	# and total catch in column $CatchN of df data
	#
	Nfull <- vector(length = nyr)
	Nfull[1] <- Vpar[nyr]
	for(i in 1:(nyr-1)) {
		natm <- data$M[i]
		x <- (Vpar[i] + Nfull[i])*exp(-natm)
		Nfull[i+1] <- x - data$CatchN[i]*exp(-natm*(1-tau))
	}
	Nfull
}

# ------
CSout.f <- function(liste,data,tau,lambda,title){
	#  NEW (March 04): version for q searched
	# Computes CSA stock estimates and harvest rates by stage
	# based on output of minimisation (in liste) and stuff in data;
	# Assembles output in a list
	# Calls CSfwd.f  (thus needs to pass tau)
	#
	# Amended for missing (< 0) indices (12/12/03)
	#
	nyr <- length(data$Year)
	RecN <- vector(length = nyr)
	FullN <- vector(length = nyr)
	Btot <- vector(length = nyr)
	HRrec <- vector(length = nyr)
	HRfull <- vector(length = nyr)
	#
	RecN <- liste$param[1:nyr]              # Recruits N (but last year's wrong)
	FullN <- CSfwd.f(liste$param,data,tau,nyr)   # Full N
	qhat <- liste$param[nyr+1]
	#  Use observed Urec (or mean if missing) for last recruitment
	ubar <- mean(data$Urec[data$Urec>0])
	RecN[nyr] <- ifelse(data$Urec[nyr]>0,data$Urec[nyr],ubar)/(qhat*data$Srat[nyr])
	Btot <- (RecN*data$Wrec)+(FullN*data$Wfull)
	HRrec <- data$CatRec/RecN
	HRfull <- data$CatFull/FullN
	Year <- data$Year
	Stock <- data.frame(Year,RecN,FullN,Btot,HRrec,HRfull)
	out <- list(descrip=title,tau=tau,lambda=lambda,M=data$M,Stock=Stock,qhat=qhat)
	out
}

# ------
CSplot1.f <- function(CSest,CSmin){
# Plots Biomass & Recruits N + Barplot of residuals - March 04
#
#parsave <- par()          # This is normally recommended but
#	on.exit(par(parsave))  #  gives a silly warning, known as bug in par() help
par(mfcol=c(2,2))
par(oma=c(1,1,1,1))
par(mar=c(4,4,2,2)+0.1)
# Recruits
plot(CSest$Stock$Year,CSest$Stock$RecN,type="b",xlab="",ylab="Recruits (M)",las=1)
# Biomass
plot(CSest$Stock$Year,CSest$Stock$Btot,type="b",xlab="Year",ylab="Biomass (,OOO t)",
las=1)   #,tck=0.02)
# Residuals
nyr <- length(CSest$Stock$Year)
nrt <- length(CSmin$resid)
## NB: Recruits residuals run up to LastYear-1, FullRecr to LastYear
#   x-axis labels every 5 years
xleg <- as.character(CSest$Stock$Year[1:(nyr-1)])
xleg[(1:(nyr-1)%%5)!=0] <- " "
barplot(CSmin$resid[1:(nyr-1)],names=xleg,
xlab="",ylab="Raw log-residuals Recruits",las=1,style="old")
xleg <- as.character(CSest$Stock$Year)
xleg[(1:nyr%%5)!=0] <- " "
barplot(CSmin$resid[nyr:nrt],names=xleg,
xlab="Year",ylab="Raw log-residuals FullRecr.",las=1,style="old")
mtext(CSest$descrip,side=3,outer=T)
}

# ------
CSplot2.f <- function(CSest,CSmin){
# Plots Biomass & Recruits N + Line plot of residuals - March 04
#
#parsave <- par()          # This is normally recommended but
#	on.exit(par(parsave))  #  gives a silly warning, known as bug in par() help
par(mfcol=c(2,2))
par(oma=c(1,1,1,1))
par(mar=c(4,4,2,2)+0.1)
# Recruits
plot(CSest$Stock$Year,CSest$Stock$RecN,type="b",xlab="",ylab="Recruits (M)",las=1)
# Biomass
plot(CSest$Stock$Year,CSest$Stock$Btot,type="b",xlab="Year",ylab="Biomass (,OOO t)",
las=1)   #,tck=0.02)
# Residuals
nyr <- length(CSest$Stock$Year)
nrt <- length(CSmin$resid)
# NB: Recruits residuals run up to LastYear-1, FullRecr to LastYear
plot(CSest$Stock$Year[1:(nyr-1)],CSmin$resid[1:(nyr-1)],type="p",pch=18,
xlab="",ylab="Raw log-residuals Recruits",las=1)
abline(h=0.0,lty=4)
plot(CSest$Stock$Year,CSmin$resid[nyr:nrt],type="p",pch=18,
xlab="Year",ylab="Raw log-residuals FullRecr.",las=1)
abline(h=0.0,lty=4)
mtext(CSest$descrip,side=3,outer=T)
}

# ------
CSjac.f <- function(min,estim) {
# Computes Jacobian (1st derivatives) of parameters
# Version for q searched
#
	nyr <- length(estim$Stock$Year)
	npar <- length(min$param)
	nrt <- length(min$resid)
	Jac <- matrix(0,nrow=nrt,ncol=npar)
	svec <- vector(length=nyr)
	
	diag(Jac) <- 1/min$param
	Jac[,npar] <- 1/min$param[npar]
	Jac[1:(nyr-1),] <- Jac[1:(nyr-1),]*sqrt(estim$lambda)
# Done with top part, now for derivs of FullRec residuals
#  first "tabulate" powers of exp(-M) in svec
	M <- mean(estim$M)   # Only works with single M => take mean
	tmp <- exp(-M)
	svec[1] <- 1
	for(i in 2:nyr) {svec[i] <- tmp*svec[i-1]}
# and make lower corner
	m <- 1
	for(k in (nyr+1):nrt) {
		m <- m+1
		ii <- 0
		for(j in m:2) {
			ii <- ii+1
			Jac[k,ii] <- svec[j]/estim$Stock$FullN[m]
			}
		}
	Jac[(nyr+1):nrt,npar-1] <- Jac[(nyr+1):nrt,1]
	Jac
}

# ------
