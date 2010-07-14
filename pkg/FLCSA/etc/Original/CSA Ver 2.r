#  ----------------------------------------------------
#                 CSAo - Version for R
###  Fitting Catch-Survey Analysis - Observation error
#
# Retrieved from S+ Version as of 30/03/2004
#     Post-recruits q computed as GM
#     Catch data now by stage
#     Missing data allowed, BUT treatment perhaps not ideal
#     Approximate variances of parameters based on Jacobian
#     Adapted for M variable by year & and new data file form with M
#  For R, changes needed were:
#   Use nls.lm instead of nlregb for NLLS minimisation => need library(minpack.lm) from CRAN
# Last update 02/02/2005
#  ----------------------------------------------------

# !! WATCH !! Make sure to (re-)SET THE RIGHT WORK. DIR. for your project
getwd()  # to check

# To run any suite of commands, highlight relevant (bits of) statements and Click Run_Selec or CTL-R
#  !!WARNING!! do not run this script in one go (but by steps);
#   many lines are flagged to avoid unwanted damage, should this happen.
Oops   # and this should stop it!

# You must first run (once !) the functions given at the end of this script
#  to 'install' them on your system, and IN EACH SESSION
library(minpack.lm)

#
#  STEP 1: Import input data
#  ==========================
#   See the CSA Manual for the accepted data format
#  There should be a first line with a title (for house-keeping),
#   a second line with first_year, last_year 
#   then a line of columns headers (see below),
#   followed by a bunch of data, each row being for a year; fields assumed comma-separated.

#  Specify the data directory (unless current is OK),e.g.:
#mydir <- paste(getwd(),"ALsims",sep="/")
#  Then full file name, e.g.:
#fich <- "sim_DAL20.dat"
#OR: fich <- paste(mydir,"sim_DAL20.dat",sep="/")
# Now read year range, then the data array
yrange <- as.numeric(read.table(fich, header=F, skip=1, nrows=1, sep=","))
Nlin <- yrange[2]-yrange[1]+1
sim.DAL20 <- read.table(fich, header=T, skip=2, nrows=Nlin, sep=",")
CSdat <- sim.DAL20

# OPTIONAL 
# The example files also include the True states; you may wish to upload them now
#  to check later how estimates compare with truth.
# !! There may be a need to adjust X in skip=Nlin+X (X= 8 here) if R complains !!
#   (seems that treatment of blank lines is not consistent)
true.pop <- read.table(fich, header=T, skip=Nlin+8, nrows=Nlin, sep=",")

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
#  STEP 2: Copy data to generic object
#  ====================================
#  !! IMPORTANT: all functions refer to generic name CSdat
CSdat <- sim.DAL20

#  !! Moreover, all data items are identified by standard names (Watch case)
#    If you did not use these in your file, rename by running:
# names(CSdat) <- c("Year","CatRec","CatFull","Urec","Ufull","Wrec","Wfull","Srat","M")
 
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
npar <- nyr               # nbr of parameters
#  Control for Marquardt minimiser
mqdt.control <- list(ftol=0.00001,ptol=0.00001,gtol=0,diag=numeric(),factor=100,
maxfev=100*(npar+1),nprint=0)
# epsfcn= see if needed
#
#  STEP 4: Choose options for this CSA run (re-enter here for replays)
#  ========================================
# (i) Tau ; 0.5 for Catch discounted mid-year, or O for Catch at year start
tau <- 0.5

# (ii) Weight of error on Recruits indices, relative to error on FullRec indices
rwer <- 1.0

# (iii) M ; Column M is created if none exists (old format), updated otherwise
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
qini <- 1
vini <- CSiniGM.f(CSdat,qini,tau)
CSmin <- nls.lm(vini,fn=CSresGM.f,control=mqdt.control,data=CSdat,tau=tau,lambda=rwer)
## IF error messages, input s ratio is probably inadequate;
#    readjust CSdat$Srat, then redo vini & CSmin commands
# Inspect CSmin for details, eg. CSmin$message  CSmin$fvec #(=residuals)

# Now compute and display stock estimates of interest
#  output is a list with estimates + useful parameters to pass for predictions (see extra)
#  First give a Title for your run: IMPORTANT: used later for run ID
rundesc <- "sim_DAL20 data - NO Q trend - tau= 0.5 - s= 0.995"
CSest <- CSoutGM.f(CSmin,CSdat,tau,rwer,rundesc)
print(CSest,digits=5)

# Plot results
#  This one plots residuals as bar-plots
CSplot1.f(CSest,CSmin)
#  But if you prefer line plots, try that one
CSplot2.f(CSest,CSmin)

# Save CSest to a unique object if you want to keep it and compare runs, e.g.
#DAL20.base <- CSest

# Optional: save CSA stock estimates to a file: 
#   TO BE REVISED with appropriate function in R (if available)
# 
fich2 <- "tempo.txt"
sink(fich2)
print(CSest,digits=5)  # adjust digits to your taste
sink()

###  Approximate variance-covariance, or CV of parameters
#     as suggested in Venables & Ripley
#   !! Do NOT use if you have missing indices !!   
Jac <- CSjacGM.f(CSdat,CSmin,CSest)
# Variance:  !! note df= nrt-npar when q computed !!
nrt <- 2*nyr-1            # nbr of residual terms
ss <- sum(CSmin$fvec^2)
VV <- diag(ss/(nrt-npar)* solve(t(Jac) %*% Jac))
names(VV) <- names(vini)
VV
# or CV%:
100*sqrt(VV)/CSmin$par
## Alternatively, take advantage of the Hessian computed by nls.lm
nrt <- 2*nyr-1            # nbr of residual terms
ss <- sum(CSmin$fvec^2)
100*sqrt(diag(ss/(nrt-npar)* solve(CSmin$hessian)))/CSmin$par

#

# Return to Step 4 for another run

### Everything above checked against S+ version on the simDAL20 examples on 27.Jan.05
###  Gave very precisely the same results
### The variance & CV stuff had been checked against Venables & Ripley function in S+

#      --------------------------------
##  Functions used:
#      --------------------------------
#
CSiniGM.f <- function(data,qini,tau){
	#  Version for q estimated as GM, not searched
	# Initialise CSA parameter vector given observed indices
	# replace missing (< 0) indices by mean of non-missing
	# rescale qini until Pop > Catch
	#
	nyr <- length(data$Year)
	v <- vector(length = nyr)
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
	lab <- paste("R",1:nyr,sep="")
	lab[nyr] <- "N1"
	names(v) <- lab
	# save final qini, and return v
	qini <<- qini
	v
}

# ------
CSresGM.f <- function(Vpar,data,tau,lambda){
	#  Version for q estimated as GM, not searched
	# Computes & returns CSA (log-)residuals 
	# Vpar is vector of 'current' parameter values, with RecruitsN in 
	#  positions [1:(nyr-1)] and 1st year's FullN in position [nyr]
	# Version for nlregb: residuals not squared  (but should for ms())
	# Calls CSfwd.f => needs to pass tau
	#
	# Amended for missing (< 0) indices; 12/12/03
	# BUT nlregb() does NOT accept NA residuals => artificially forced to zero (log(1))
	# however, same CSA results obtained when corresponding residuals are excluded
	# Wrong calculation of qhat modified 03/09/03 & 12/03/04 => now cumbersome
	#
	nyr <- length(data$Year)
	nres <- 2*nyr-1
	vres <- vector(length = nres)
	
	Nfull <- CSfwd.f(Vpar,data,tau,nyr)
	tmp <- data$Urec[1:(nyr-1)]/(data$Srat[1:(nyr-1)]*Vpar[1:(nyr-1)])
	w <- sum(ifelse(tmp>0,lambda,0))+ sum(ifelse(data$Ufull>0,1,0))
	qhat <- sum(lambda*log(ifelse(tmp>0,tmp,NA)),na.rm=T)
	qhat <- qhat+ sum(log(ifelse(data$Ufull>0,data$Ufull/Nfull,NA)),na.rm=T)
	qhat <- exp(qhat/w)
	w <- sqrt(lambda)  # Watch new use of w
	vres <- w*log(ifelse(data$Urec>0,(qhat*data$Srat*Vpar/data$Urec),1))  # Recruits
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
CSoutGM.f <- function(liste,data,tau,lambda,title){
	#  Version for q estimated as GM, not searched
	# Computes CSA stock estimates and harvest rates by stage
	# based on output of minimisation (in liste) and stuff in data;
	# Assembles output in a list
	# Calls CSfwd.f  (thus needs to pass tau)
	#
	# Amended for missing (< 0) indices (12/12/03)
	# Wrong calculation of qhat modified 03/09/03 & 12/03/04 => now cumbersome
	## This is Version for R with nls.lm (just change names in liste$..); Feb 05
	#
	nyr <- length(data$Year)
	RecN <- vector(length = nyr)
	FullN <- vector(length = nyr)
	Btot <- vector(length = nyr)
	HRrec <- vector(length = nyr)
	HRfull <- vector(length = nyr)
	#
	RecN <- liste$par              # Recruits N (but last year's wrong)
	names(RecN) <- NULL    # needed in R to remove unwanted 'row' labels in out
	FullN <- CSfwd.f(liste$par,data,tau,nyr)   # Full N
	tmp <- data$Urec[1:(nyr-1)]/(data$Srat[1:(nyr-1)]*RecN[1:(nyr-1)])
	w <- sum(ifelse(tmp>0,lambda,0))+ sum(ifelse(data$Ufull>0,1,0))
	qhat <- sum(lambda*log(ifelse(tmp>0,tmp,NA)),na.rm=T)
	qhat <- qhat+ sum(log(ifelse(data$Ufull>0,data$Ufull/FullN,NA)),na.rm=T)
	qhat <- exp(qhat/w)
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
##  This is Version for R with nls.lm (just change names in liste$..); Jan. 05
#
windows()  # for R
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
nrt <- length(CSmin$fvec)  # $fvec in nls.lm == $residuals in S+ nlregb
## NB: Recruits residuals run up to LastYear-1, FullRecr to LastYear
#   x-axis labels every 5 years
xleg <- as.character(CSest$Stock$Year[1:(nyr-1)])
xleg[(1:(nyr-1)%%5)!=0] <- " "
barplot(CSmin$fvec[1:(nyr-1)],names.arg=xleg,col=NA,
xlab="",ylab="Raw log-residuals Recruits",las=1)  #,style="old")
xleg <- as.character(CSest$Stock$Year)
xleg[(1:nyr%%5)!=0] <- " "
barplot(CSmin$fvec[nyr:nrt],names.arg=xleg,col="white",
xlab="Year",ylab="Raw log-residuals FullRecr.",las=1)  #,style="old")
mtext(CSest$descrip,side=3,outer=T)
}

# ------
CSplot2.f <- function(CSest,CSmin){
# Plots Biomass & Recruits N + Line plot of residuals
##  This is Version for R with nls.lm (just change names in liste$..); Jan. 05
#
windows()  # for R
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
nrt <- length(CSmin$fvec)  # $fvec in nls.lm == $residuals in S+ nlregb
# NB: Recruits residuals run up to LastYear-1, FullRecr to LastYear
plot(CSest$Stock$Year[1:(nyr-1)],CSmin$fvec[1:(nyr-1)],type="p",pch=18,
xlab="",ylab="Raw log-residuals Recruits",las=1)
abline(h=0.0,lty=4)
plot(CSest$Stock$Year,CSmin$fvec[nyr:nrt],type="p",pch=18,
xlab="Year",ylab="Raw log-residuals FullRecr.",las=1)
abline(h=0.0,lty=4)
mtext(CSest$descrip,side=3,outer=T)
}

# ------
CSjacGM.f <- function(data,min,estim) {
# Computes Jacobian (1st derivatives) of parameters
# Version for q computed as GM (more tedious)
#  and M variable by year (still more cumbersome)
# BUT does NOT handle missing indices
#
#  Version for R & nls.lm (change names in min$..) ; Jan 05
#
	nyr <- length(estim$Stock$Year)
	npar <- length(min$par)
		if(npar != nyr) {
			cat(" CSmin and CSest not consistent \n")
			stop
		}
		if(any(data$Urec<0) || any(data$Ufull<0)) {
			cat(" Missing data not accepted \n")
			stop
		}
	nrt <- length(min$fvec)
	Jac <- matrix(0,nrow=nrt,ncol=npar)
	stab <- matrix(0,nrow=nyr,ncol=npar-1)
	dlq <- vector(length=npar)

#  first "tabulate" powers of exp(-M), =dN/dR, in stab
#  If Jac was to be passed to the minimiser, stab should be set up once for all
#   in the calling program (not in each iteration) to save time
#   since it only depends on the Ms, not on the 'current' estimates
	stab[1,1] <- 1
	for(i in 1:(nyr-1)){
		tmp <- estim$M[i]
		for(j in (i+1):nyr) {
			stab[j,i] <- exp(-tmp)
			tmp <- tmp+ estim$M[j]
			}
		}
# Weight in GM q
	w <- sum(ifelse(data$Urec[1:(nyr-1)]>0,estim$lambda,0))
	w <- w + sum(ifelse(data$Ufull>0,1,0))
	w <- -1/w
# Now, dLog(q)/dRi
	for(ii in 1:(npar-1)) {
		tmp <- 0
		for(j in (ii+1):nyr) {
			tmp <- tmp+ stab[j,ii]/estim$Stock$FullN[j]
		}
		dlq[ii] <- w*(tmp+ estim$lambda/min$par[ii])
	}
# and dLog(q)/dN1  (same stab column as for R1)
	tmp <- 0
	for(j in 2:nyr) {
		tmp <- tmp+ stab[j,1]/estim$Stock$FullN[j]
	}
	dlq[npar] <- w*(tmp+ 1/min$par[npar])
# Replicate down each column
#	for(i in 1:nrt) {Jac[i,] <- dlq} # inelegant, or:
	Jac <- matrix(rep(dlq,nrt),nrow=nrt,byrow=T)
# Adjust diagonal and weight for derivs of Recruits residual terms
	diag(Jac) <- diag(Jac)+ 1/min$par
	Jac[1:(nyr-1),] <- Jac[1:(nyr-1),]*sqrt(estim$lambda)
# Done with top part, now for derivs of FullRec residuals
# and make lower corner
	m <- 1
	for(k in (nyr+1):nrt) {
		m <- m+1
		ii <- 0
		for(j in m:2) {
			ii <- ii+1
			Jac[k,ii] <- Jac[k,ii]+ stab[m,ii]/estim$Stock$FullN[m]
			}
		}
# deriv in N1, skip first year done with diag() above
	ii<- 1
	for(i in (nyr+1):nrt) {
		ii <- ii+1
		Jac[i,npar] <- Jac[i,npar]+ stab[ii,1]/estim$Stock$FullN[ii]
	}
	Jac
}

# ------
