validPro2boxControl <- function(object) {
  
  return(TRUE)}

fls=data.frame(type   =c(0,1,1,0,0,1,0,0,0,0),
               name   =c("quotas.txt","naa.out","faa.out","waa.txt","caa.txt","maa.out","sel.txt","recruit.txt","trans.txt","disc.txt"),
               comment=c("file with catch/effort limit scenarios","stock size in numbers","fishing mortality rate","","","","","stock-recruitment parameters","","proportion from each fishery that is discarded"),
               stringsAsFactors=FALSE)
  							
setClass('pro2boxControl', representation(
    "FLComp",
    model         ="character",
    params        ='FLPar',
    grw           ='FLPar',
    target        ='FLQuants',
    maxF          ='FLQuant',
    n             ="integer",
    ci            ="integer",
    seed          ="integer",
    patch         ="integer",
    selyrs        ="integer",
    files         =data.frame),
  prototype(
    range         =unlist(list(minyear=as.numeric(NA), maxyear=as.numeric(NA))),
    model         ="bevholt",
    params        =FLPar(NA,dimnames=list(param=c("s","vb","spr0"),iter=1)),
    n             =500,
    ci            =80,
    seed          =-911,
    patch         =0,
    files         =fls),
  validity=validpro2boxControl) 
  
is.pro2boxControl <- function(x)
  return(inherits(x, "pro2boxControl"))

Pro2boxControl

readPro2boxControl=function(object,file){}

writePro2boxControl=function(object,file){
cat("#---------------------------------------------------------------------------------											
     #	INPUT DATA FILE FOR VPA2-box PROJECT bluefin east 2010										
     # data entry is free-format.  Null lines and comments must be											
     # initiated here by pound signs (#) asterisks (*) or dashes (-).  Do not,											
     # however, interrupt a stream of vector inputs with a comment.											
     #---------------------------------------------------------------------------------											
     #---------------------------------------------------------------------------------											
     # Enter control information common to both areas/stocks											
     #---------------------------------------------------------------------------------\n",file=file)	

cat("1			 MODEL_TYPE  (1 = diffusion , 2 = overlap, 3 = sex-specific)\n							
     1			 N_BOX  {number of sexes/areas/stocks -- enter 1 or 2}\n",file=file)
    
# cat(n(object),		 "N_LOOPS {number of bootstrap projections, enter 0 if only a single deterministic run and a negative integer if you want the program to check the ASC-II file bad.out for bad runs}\n",		
#     ci(object),  	 "CI {confidence interval in percent (e.g.\n",
#     seed(object),	 "SEED {random number seed (should be a negative integer)}\n",						
#     patch(object), "PATCH (>0)=number of recruitments replace with estimates from spawner-recruit curve, (<=0)=don't do it\n",								
#     "1			YR_BOX (1)=maximize y/r and msy of sex/area/stock 1 alone, or (2) of sex/area/stock 2 alone, or (3) of both stocks combined\n",file=file)								
#     "0			SR_BOX recruitment depends on fecundity of (1) sex 1 only, (2) sex 2 only, or (3) both sexes combined\n",file=file)
#                  
# cat("1			SEX_FRACTION = fraction of population belonging to sex 1 at age of recruitment (immaterial if SR_BOX<1 or N_box=1)\n",
#     "1			WEIGHT_TYPE (0) use growth curve for all weights (1) use growth curve only for plus group projections, (2) do not use growth curve at all,\n",
#     "1950	2009	2040	{FIRST year, LAST year in data, Last year to PROJEC}\n"
#     "1	10	10	First age followed by last ages for sex/area/stock 1 and 2 (a total of three entries),\n",file=file)								
# 
# cat("#-------------------------------------------------------------------------\n
#      # Enter the selectivity option: two positive integers indicate the first and\n"				
#      #      last years for a geometric mean of the F's at age.  Any negative value indicates\n"								
#      #      that the user specified vector in the selectivity file are to be used (otherwise\n"			
#      #      the selectivity file values are used to modify the geometric mean)\n"
#      # 1997 1999  Values in 2002 Base\n",file=file)
# cat(selyrs(object),"\n",file=file)										
# 
#     
# cat("#-------------------------------------------------------------------------\n",
#      # Enter the first and last years to use for recruitment and the first and last years for ssb\n",file=file)											
#      1990	2004	2000	2004								
# cat("# Error type for recruitment projections and fitting spawner-recruit curves (1=lognormal, 2=normal)\n",file=file)		
# 1											
# cat(#-------------------------------------------------------------------------\n",
#     # Enter area specific growth information for area/stock1									\n",		
#     #-------------------------------------------------------------------------\n",file=file)											
#     1			N_GEARS (area 1)\n",file=file)								
#    -40			average age of the plus-group at the BEGINNING of the last year (-x) = compute from growth curve\n				
#     0	0	0	0.5	1	1	1	1	1	1		Maturity fraction,\n
#     # curve type (1=von Bertalanffy or Chapman Richards, 2=Gompertz),\n											
#     # |  Linfinity											\n
#     # |  |     k											  \n
#     # |  |     |      tO 								\n			
#     # |  |     |      |         Chapman-Richards skew parameter (m=1 for von Bert) - ignored if Gompertz used\n											
#     # |  |     |      |         |     weight parameter a of w=al^b											                     \n
#     # |  |     |      |         |     |          weight parameter b of w=al^b											           \n
#     # |  |     |      |         |     |          |       time of year for growth equation (in months)				 \n							
#     # |  |     |      |         |     |          |       |    offset (months) to be subtracted from vpa age to get growth curve age\n											
#    # |  |     |      |         |     |          |       |      |     											\n",file=file)
# 
# 1	318.9	0.093	-0.97	1	0.0000196	3.0092	6	4		SSB	
# 1	318.9	0.093	-0.97	1	0.0000196	3.0092	6	4		fishery	
# cat("#-------------------------------------------------------------------------\n											
#      # Enter area specific growth information for area/stock 2 (if any)				 \n							
#      #-------------------------------------------------------------------------\n											
#      #-------------------------------------------------------------------------\n											
#      # Enter file information										                               \n	
#      #-------------------------------------------------------------------------\n											 
#      # file type (0=asc-ii, default=VPA-2box binary format)										 \n	
#      # |   file name											                                     \n
#      # |   |											                                             \n",file=file)
# 
# cat(apply(files(object), 1, paste,sep=" ",collapse=" "),file=file)							
# 
# cat("# END OF FILE\n",file=file)
}

