##########################################################################
### FLAdapt
### This file contains code just for the class FLAdapt

### 04-03-2005 implemented as a S4 Class by L T Kell
### using original Fortran code by Clay Porch 
#########################################################################
## There are two main classes the Adapt class and the control class
### classes ##############################################################

validFLAdaptControl<-function(object){
	# Everything is fine
	return(TRUE)}

defaults=function(type,x="missing"){
    if (x=="simplex"){
				##Simplex options
				res=unlist(list(seed =-991,   
				                maxit= 100,   
				                check=   3,   
		                    pdev = 0.4))  
            return(res)}
            				
    if (x=="q"){
				##Index options
				res=unlist(list(scale =1, 
				                cv    =1.0,    
			                  add   =0))    
            return(res)}
				
    if (x=="sel"){
				##Selectivity options
				res=unlist(list(penalty=3,  
				                sigma  =0.5,   
				                minage =NA, 
				                maxage =NA)) 
            return(res)}
				
    if (x=="sr"){
				##Stock recruit options
				res=unlist(list(pdf    =0,   
				                minyear=NA,   
				                maxyear=NA))
            return(res)}
				
    if (x=="rec"){
				##Recruitment options
				res=unlist(list(penalty=0,
                				 sigma  =0.5))
            return(res)}
				
    if (x=="catch"){
				##Catch options
				res=unlist(list(penalty=0,
				                sigma  =0.5,
				                pdf    =0))
            return(res)}
				
    if (x=="params"){
				##Parameter options as in the VPA2Box *.p file
				termage=data.frame()  #terminal ages
				fratio =data.frame()  #f ratio for oldest age or plusgroup
 			  sr     =data.frame()  #stock recruit parameters
				var    =data.frame()  #variance scaling parameters
				q      =data.frame()  #catchability parameters
        res=   rbind(termage,fratio,sr,var,q)
            
        return()}
				}

      
setClass("FLAdaptControl",
		representation(
				
        ##General options
				season		 ="numeric",   #Spawning season as fraction of a year
				optionF      ="logical",   #Option to use F's as terminal year parameters default is true, if false then use N's
			  ##Simplex options
				simplex      ="numeric",         
  			##Index options
				q            ="numeric",
				##Selectivity options
				sel          ="numeric",  
				##Stock recruit options
				sr          ="numeric",
				##Recruitment options
				rec         ="numeric",
				##Catch options
				catch       ="logical",   
				##Parameter options as in the VPA2Box *.p file
				params      ="data.frame"),
		prototype=prototype(
				season     =as.numeric(0.0),         
				optionF    =as.logical(TRUE),
				simplex    =defaults("simplex"),
				q          =defaults("q"),
				sel        =defaults("sel"),
				sr         =defaults("sr"),
				rec        =defaults("rec"),
				catch      =defaults("catch"),
				params     =defaults("params")),
		validity=validFLAdaptControl)

setValidity("FLAdaptControl", validFLAdaptControl)
remove(validFLAdaptControl)	# We do not need this function any more

is.FLAdaptControl = function(x)
	return(inherits(x, "FLAdaptControl"))



