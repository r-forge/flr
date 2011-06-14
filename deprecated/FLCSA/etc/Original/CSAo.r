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

	