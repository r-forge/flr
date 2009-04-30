# Outline of tests for FLash

# Setting up ctrl objects
# running projections
#	- does op match target (within tolerance)
#   - simple projection with R using found Fs should give same result (within tolerance)
# Is SRR being treated correctly
# Multiple iterations

# Dispatch on FLStock
# FLBiol and Fleet

# Seasons and areas etc...

# Targets to test
# ssb
# biomass
# catch
# landings
# discards
# f
# z
# f.landings
# f.discards
# mnsz
# Not revenue, cost, profit, effort yet





#*******************************************************************************
# Quick test

library(FLash)
data(ple4)
fwd.ple4 <- ple4
ctrl<-fwdControl(data.frame(year=1999:2001,val=25000,quantity="catch"))
fwd.ple4<-fwd(fwd.ple4,ctrl=ctrl,sr=list(model="mean",params=FLPar(333333)))

computeCatch(ple4)[,ac(1999:2001)]
fbar(ple4)[,ac(1999:2001)]

computeCatch(fwd.ple4)[,ac(1999:2001)]
fbar(fwd.ple4)[,ac(1999:2001)]

#****************** Test Control Constructor ***********************************

# Ignore this for the moment
# Might change the constructor

# Two parts:
#	Target array
#   Effort array

# Each of those has two parts
#   target
#   trgtArray (iterations)
#   effort
#   effArray (iterations)

# Effort part not always needed

#**** target *****

# It's a data.frame with columns:
# year quantity min val  max spp fleet metier rel

# Need to test several things:
# 1. Does the target data.frame match the data in the argument
# 2. Should the target be valid

# Simplest control
# Single iteration, single value (not min or max), singe year, single quantity
# Have to supply at least year (but surely val (or min or max) and quantity should also be spec)
# Is this a sensible fwdControl (it has only year, nothing else).
# Maybe so user can change it after creation. Sounds dangerous. COuld be invalid..
# Pass (should it)
res <- fwdControl(data.frame(year=1999))
# Fail
res <- fwdControl(data.frame(dummy="stupid"))

# Can put any old rubbish in the data.frame argument, but only those columns
# that match the target data.frame are copied across
# Pass (but only year used)
res <- fwdControl(data.frame(year = 1999, dummy="stupid"))

# Should fail
res <- fwdControl(data.frame(year = 1999, quantity="stupid"))

# # ssb
# biomass
# catch
# landings
# discards
# f
# z
# f.landings
# f.discards
# mnsz
# Test all quantities
res <- fwdControl(data.frame(year = 1999, quantity="ssb"))
res <- fwdControl(data.frame(year = 1999, quantity="biomass"))
res <- fwdControl(data.frame(year = 1999, quantity="catch"))
res <- fwdControl(data.frame(year = 1999, quantity="landings"))
res <- fwdControl(data.frame(year = 1999, quantity="discards"))
res <- fwdControl(data.frame(year = 1999, quantity="f"))
res <- fwdControl(data.frame(year = 1999, quantity="z"))
res <- fwdControl(data.frame(year = 1999, quantity="f.landings"))
res <- fwdControl(data.frame(year = 1999, quantity="f.discards"))
res <- fwdControl(data.frame(year = 1999, quantity="mnsz"))
# Should fail
res <- fwdControl(data.frame(year = 1999, quantity="effort"))
res <- fwdControl(data.frame(year = 1999, quantity="cost"))
res <- fwdControl(data.frame(year = 1999, quantity="revenue"))
res <- fwdControl(data.frame(year = 1999, quantity="profit"))



# range of years, range of quantities

# move into min and max

#**** target array ****

# Multiple iterations

#Multiple iterations and years

ctrl<-fwdControl(data.frame(year=1999:2001,val=25000,quantity="catch"))
# This should fail (instead quantity <- NA)
ctrl<-fwdControl(data.frame(year=1999:2001,val=25000,quantity="nonsense"))
# There is a validFwdControl - is it ever called?

# checkTarget repeats same checks as in fwdControl method (min max and val)


#**** Effort ****


#********************* FLash with FLStock **************************************

library(FLash)
data(ple4)

simple_project <- function(stock,yrs,sr)
{
	flsr <- FLSR()
	flsr@model <- sr[["model"]]
	flsr@params <- sr[["params"]]
	nages <- dims(stock)$age
	for (yr in yrs)
    	{
        	stock.n(stock)[2:nages,ac(yr)] <- stock.n(stock)[1:(nages-1),ac(yr-1)] * (exp(-m(stock)[1:(nages-1),ac(yr-1)] -harvest(stock)[1:(nages-1),ac(yr-1)]))
        	stock.n(stock)[nages,ac(yr)] <- stock.n(stock)[nages,ac(yr)] + stock.n(stock)[nages,ac(yr-1)] * (exp(-m(stock)[nages,ac(yr-1)] -harvest(stock)[nages,ac(yr-1)]))
			stock.n(stock)[1,ac(yr)] <- as.numeric(predict(flsr,ssb=ssb(stock)[,ac(yr-1)]))
			catch.n(stock)[1:nages,ac(yr)] <- (harvest(stock)[1:nages,ac(yr)] / (m(stock)[1:nages,ac(yr)] + harvest(stock)[1:nages,ac(yr)]))  * stock.n(stock)[1:nages,ac(yr)] * (1 - exp(-m(stock)[1:nages,ac(yr)] -harvest(stock)[1:nages,ac(yr)]))
    	}
   stock(stock) <- computeStock(stock)
   catch(stock) <- computeCatch(stock)
   return(stock)
}

	# Run some simple tests
	# quantity = f
	ctrl<-fwdControl(data.frame(year=1999:2001,val=c(0.3,0.3,0.3),quantity="f"))
	fwd.ple4<-fwd(ple4,ctrl=ctrl,sr=list(model="bevholt",params=FLPar(list(a=3.767816,b=3.298548))))
	fbar(fwd.ple4)[,ac(1999:2001)]
	# Are the resulting catches etc. consistent with the estimated Fs
	sim_test <- simple_project(fwd.ple4,1999:2001,sr=list(model=formula("rec ~ a * ssb/(b + ssb)"),params=FLPar(list(a=3.767816,b=3.298548))))
	# check catch
	computeCatch(fwd.ple4)[,ac(1999:2001)]
	catch(sim_test)[,ac(1999:2001)]
	# And ssb
	ssb(fwd.ple4)[,ac(1999:2001)]
	ssb(sim_test)[,ac(1999:2001)]
	
	# quantity = z
	ctrl<-fwdControl(data.frame(year=1999:2001,val=c(0.3,0.3,0.3),quantity="z"))
	fwd.ple4<-fwd(ple4,ctrl=ctrl,sr=list(model="bevholt",params=FLPar(list(a=3.767816,b=3.298548))))
	# check z
	apply((harvest(fwd.ple4)[,ac(1999:2001)] + m(fwd.ple4)[,ac(1999:2001)])[range(fwd.ple4)["minfbar"]:range(fwd.ple4)["maxfbar"],],2:6,mean)
	# Are the resulting catches etc. consistent with the estimated Fs
	sim_test <- simple_project(fwd.ple4,1999:2001,sr=list(model=formula("rec ~ a * ssb/(b + ssb)"),params=FLPar(list(a=3.767816,b=3.298548))))
	# check catch
	computeCatch(fwd.ple4)[,ac(1999:2001)]
	catch(sim_test)[,ac(1999:2001)]
	# And ssb
	ssb(fwd.ple4)[,ac(1999:2001)]
	ssb(sim_test)[,ac(1999:2001)]
	
	# quantity = catch
	ctrl<-fwdControl(data.frame(year=1999:2001,val=25000,quantity="catch"))
	fwd.ple4<-fwd(ple4,ctrl=ctrl,sr=list(model="bevholt",params=FLPar(list(a=3.767816,b=3.298548))))
	# Are the resulting catches etc. consistent with the estimated Fs
	sim_test <- simple_project(fwd.ple4,1999:2001,sr=list(model=formula("rec ~ a * ssb/(b + ssb)"),params=FLPar(list(a=3.767816,b=3.298548))))
	# check catch
	computeCatch(fwd.ple4)[,ac(1999:2001)]
	catch(sim_test)[,ac(1999:2001)]
	# And ssb
	ssb(fwd.ple4)[,ac(1999:2001)]
	ssb(sim_test)[,ac(1999:2001)]
	
	# Bio quantities are a little different - set F to achieve target in following year
	# quantity = ssb
	ctrl<-fwdControl(data.frame(year=1999:2001,val=250000,quantity="ssb"))
	fwd.ple4<-fwd(ple4,ctrl=ctrl,sr=list(model="bevholt",params=FLPar(list(a=3.767816,b=3.298548))))
	# Are the resulting catches etc. consistent with the estimated Fs
	sim_test <- simple_project(fwd.ple4,1999:2001,sr=list(model=formula("rec ~ a * ssb/(b + ssb)"),params=FLPar(list(a=3.767816,b=3.298548))))
	# check catch
	computeCatch(fwd.ple4)[,ac(1999:2001)]
	catch(sim_test)[,ac(1999:2001)]
	# And ssb
	ssb(fwd.ple4)[,ac(1999:2001)]
	ssb(sim_test)[,ac(1999:2001)]
	
	# quantity = biomass
	ctrl<-fwdControl(data.frame(year=1999:2001,val=300000,quantity="biomass"))
	fwd.ple4<-fwd(ple4,ctrl=ctrl,sr=list(model="bevholt",params=FLPar(list(a=3.767816,b=3.298548))))
	# Are the resulting catches etc. consistent with the estimated Fs
	sim_test <- simple_project(fwd.ple4,1999:2001,sr=list(model=formula("rec ~ a * ssb/(b + ssb)"),params=FLPar(list(a=3.767816,b=3.298548))))
	# check biomass
	computeStock(fwd.ple4)[,ac(1999:2001)]
	# check catch
	computeCatch(fwd.ple4)[,ac(1999:2001)]
	catch(sim_test)[,ac(1999:2001)]
	# And ssb
	ssb(fwd.ple4)[,ac(1999:2001)]
	ssb(sim_test)[,ac(1999:2001)]

# Need some work - cannot get these from my simple projection (unless same assumptions made as in FLash)
	# quantity = landings
	ctrl<-fwdControl(data.frame(year=1999:2001,val=300000,quantity="biomass"))
	fwd.ple4<-fwd(ple4,ctrl=ctrl,sr=list(model="bevholt",params=FLPar(list(a=3.767816,b=3.298548))))

	# quantity = discards
	ctrl<-fwdControl(data.frame(year=1999:2001,val=300000,quantity="biomass"))
	fwd.ple4<-fwd(ple4,ctrl=ctrl,sr=list(model="bevholt",params=FLPar(list(a=3.767816,b=3.298548))))

# landings.f, discards.f, mnsz

#*********************** Iterations *******************************************
# Ignore the control for the moment, just so long as op is OK

	nits <- 10
	# Propagate stock - shouldn't have to?
	prop_ple4 <- propagate(ple4,nits)
	# Should check that nits is same for trgtArray and stock

# Start with target F

	targ_obj <- data.frame(year=1999:2001,val=c(0.3,0.3,0.3),quantity="f")
	targ_array <- array(rnorm(3*nits,0.3,0.05),c(3,nits))
	ctrl<-fwdControl(object=targ_obj, trgtArray=list(val=targ_array))
	# no propagate on trgtArray
	ctrl<-fwdControl(object=targ_obj)

	fwd.ple4<-fwd(prop_ple4,ctrl=ctrl,sr=list(model="bevholt",params=FLPar(list(a=3.767816,b=3.298548))))

	fbar(fwd.ple4)[,ac(1999:2001)]
	# Are the resulting catches etc. consistent with the estimated Fs
	sim_test <- simple_project(fwd.ple4,1999:2001,sr=list(model=formula("rec ~ a * ssb/(b + ssb)"),params=FLPar(list(a=3.767816,b=3.298548))))
	# check catch
	computeCatch(fwd.ple4)[,ac(1999:2001)]
	catch(sim_test)[,ac(1999:2001)]
	# And ssb
	ssb(fwd.ple4)[,ac(1999:2001)]
	ssb(sim_test)[,ac(1999:2001)]
	
	
	