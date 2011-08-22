#---------------------------------------------------------------------------------											
#	INPUT DATA FILE FOR VPA2-box PROJECT bluefin east 2010										
# data entry is free-format.  Null lines and comments must be											
# initiated here by pound signs (#) asterisks (*) or dashes (-).  Do not,											
# however, interrupt a stream of vector inputs with a comment.											
#---------------------------------------------------------------------------------											
#---------------------------------------------------------------------------------											
# Enter control information common to both areas/stocks											
#---------------------------------------------------------------------------------											
1			MODEL_TYPE  (1 = diffusion , 2 = overlap, 3 = sex-specific)								
1			N_BOX  {number of sexes/areas/stocks -- enter 1 or 2}								
500			N_LOOPS {number of bootstrap projections, enter 0 if only a single deterministic run and a negative integer if wyou want the program to check the ASC-II file bad.out for bad runs}								
80			   CI {confidence interval in percent (e.g., 80)}								
-911			SEED {random number seed (should be a negative integer)}								
3			PATCH (>0)=number of recruitments replace with estimates from spawner-recruit curve, (<=0)=don't do it								
1			YR_BOX (1)=maximize y/r and msy of sex/area/stock 1 alone, or (2) of sex/area/stock 2 alone, or (3) of both stocks combined								
0			SR_BOX recruitment depends on fecundity of (1) sex 1 only, (2) sex 2 only, or (3) both sexes combined								
1			SEX_FRACTION = fraction of population belonging to sex 1 at age of recruitment (immaterial if SR_BOX<1 or N_box=1)								
1			WEIGHT_TYPE (0) use growth curve for all weights (1) use growth curve only for plus group projections, (2) do not use growth curve at all								
1950	2009	2040	{FIRST year, LAST year in data, Last year to PROJECT}								
1	10	10	First age followed by last ages for sex/area/stock 1 and 2 (a total of three entries)								
#-------------------------------------------------------------------------											
# Enter the selectivity option: two positive integers indicate the first and											
#      last years for a geometric mean of the F's at age.  Any negative value indicates											
#      that the user specified vector in the selectivity file are to be used (otherwise											
#      the selectivity file values are used to modify the geometric mean)  										
# 1997 1999  Values in 2002 Base											
-2007	-2009										
#-------------------------------------------------------------------------											
# Enter the first and last years to use for recruitment and the first and last years for ssb											
# 1976 1996  1991 2001  Values in 2002 Base ? For benchmarks?											
# 1990 2001  2000 2004  Values in 2006 Base ? For benchmarks?, 2000,2004 is the hinge point time period											
1990	2004	2000	2004								
# Error type for recruitment projections and fitting spawner-recruit curves (1=lognormal, 2=normal)											
1											
#-------------------------------------------------------------------------											
# Enter area specific growth information for area/stock1											
#-------------------------------------------------------------------------											
1			N_GEARS (area 1)								
-40			average age of the plus-group at the BEGINNING of the last year (-x) = compute from growth curve  								
0	0	0	0.5	1	1	1	1	1	1		Maturity fraction
# curve type (1=von Bertalanffy or Chapman Richards, 2=Gompertz)											
# |  Linfinity											
# |  |     k											
# |  |     |      tO 											
# |  |     |      |         Chapman-Richards skew parameter (m=1 for von Bert) - ignored if Gompertz used											
# |  |     |      |         |     weight parameter a of w=al^b											
# |  |     |      |         |     |          weight parameter b of w=al^b											
# |  |     |      |         |     |          |       time of year for growth equation (in months)											
# |  |     |      |         |     |          |       |    offset (months) to be subtracted from vpa age to get growth curve age											
# |  |     |      |         |     |          |       |      |     											
1	318.9	0.093	-0.97	1	0.0000196	3.0092	6	4		SSB	
1	318.9	0.093	-0.97	1	0.0000196	3.0092	6	4		fishery	
#-------------------------------------------------------------------------											
# Enter area specific growth information for area/stock 2 (if any)											
#-------------------------------------------------------------------------											
#-------------------------------------------------------------------------											
# Enter file information											
#-------------------------------------------------------------------------											
# file type (0=asc-ii, default=VPA-2box binary format)											
# |   file name											
# |   |											
0	quotas.txt		       	file with catch/effort limit scenarios							
1	NAA.OUT		     	stock size in numbers							
1	FAA.OUT		      	fishing mortality rate							
0	waa.txt										
0	caa.txt										
1	MAA.OUT										
0	Sel.txt										
0	recruit.txt			stock-recruitment parameters							
0	Trans.txt										
0	disc.txt			proportion from each fishery that is discarded							
# END OF FILE											
