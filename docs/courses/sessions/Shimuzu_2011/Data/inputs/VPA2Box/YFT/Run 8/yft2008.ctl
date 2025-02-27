#-----------------------------------------------------------------------------	
#--               CONTROL FILE FOR PROGRAM VPA-2BOX, Version 3.0           ---	
#-----------------------------------------------------------------------------	
#  INSTRUCTIONS: the control options are entered in the order specified.	
#                Additional comment lines may be inserted anywhere in this 	
#                file provided they are preceded by a # symbol in the FIRST 	
#                column, otherwise the line is perceived as free-format data.	
#-----------------------------------------------------------------------------	
#	
#-----------------------------------------------------------------------------	
# TITLES AND FILE NAMES (MUST BE PLACED WITHIN SINGLE QUOTES)	
#-----------------------------------------------------------------------------	
#|--------must be 50 characters or less----------|	
'YFT 1970-2006 RUN2'                          TITLE OF RUN	
'yft2008.dat'                                      DATA FILE NAME (INPUT)	
'yft2008.par'                                      PARAMETER SPECIFICATION FILE (INPUT)	
'yft2008.rep'                                      RESULTS FILE NAME (OUTPUT)	
'yft2008.est'                                      PARAMETER ESTIMATE FILE NAME (OUTPUT)	
'yft2008.spd'                                      SPREADSHEET FRIENDLY RESULTS (OUTPUT)	
'none'                                              TAGGING DATA FILE (INPUT)	
#-----------------------------------------------------------------------------	
# MODEL TYPE OPTIONS	
#-----------------------------------------------------------------------------	
 1                         NUMBER OF ZONES (1 OR 2)	
 1                         MODEL_TYPE (1=DIFFUSION, 2=OVERLAP)	
#-----------------------------------------------------------------------------	
# TAGGING DATA SWITCH	
#-----------------------------------------------------------------------------	
# tagging data switch (0=do not use tagging data, 1=use tagging data)	
# |  weighting factor for modifying importance of tagging data in objective function	
# |  |     tag timing factors	
# |  |     |    	
  0  1.0   0     0           TAGGING MODEL CONTROLS	
#-----------------------------------------------------------------------------	
# SEARCH ALGORITHM CONTROLS	
#-----------------------------------------------------------------------------	
-911   RANDOM NUMBER SEED	
 50   MAXIMUM NUMBER OF AMOEBA SIMPLEX SEARCH RESTARTS 	
 3     NUMBER OF CONSECUTIVE RESTARTS THAT MUST VARY BY LESS THAN 1% TO STOP SEARCH  	
 0.4   PDEV (standard deviation controlling vertices for Initial simplex of each restart)	
#-----------------------------------------------------------------------------	
# INDEX WEIGHTING CONTROLS	
#-----------------------------------------------------------------------------	
 1      SCALE (DIVIDE INDEX VALUES BY THEIR MEAN)- ANY VALUE > 0 = YES	
 1.0    INDEX WEIGHTING:(0)INPUT CV's, (+)DEFAULT CV, (-)DEFAULT STD. DEV., (999)MLE	
 0      (0) MULTIPLICATIVE VARIANCE SCALING FACTOR or (1) ADDITIVE VARIANCE SCALING FACTOR 	
#-----------------------------------------------------------------------------	
# CONSTRAINT ON VulnerabilitY (PARTIAL RECRUITMENT) 	
#-----------------------------------------------------------------------------	
# apply this penalty to the last N years (SET N = 0 TO IGNORE)	
# |  standard deviation controlling the severity of the penalty	
# |  |  first age affected   	
# |  |  |  last age affected	
# |  |  |  | 	
 0  0.4  0  5    LINKS THE VULNERABILITIES IN THE LAST N YEARS	
#-----------------------------------------------------------------------------	
# CONSTRAINTS ON RECRUITMENT 	
#-----------------------------------------------------------------------------	
# apply this penalty to the last N years (SET N = 0 TO IGNORE)	
# |  standard deviation controlling the severity of the penalty	
 4   0.4      LINKS THE RECRUITMENTS IN THE LAST N YEARS 	
 0  0.1   1      LINKS THE RECRUITMENTS OF THE TWO STOCKS 	
#        |	
#        ratio of stock (sex) 1 to stock (sex) 2 {a value of 1 means a 1:1 ratio}	
#-----------------------------------------------------------------------------	
# CONSTRAINT ON SPAWNER-RECRUIT RELATIONSHIP	
#-----------------------------------------------------------------------------	
# PDF of spawner-recruit penalty: 0=none, 1=lognormal, 2=normal (-)=estimate sigma by MLE	
# |  first and last years to use in fitting (in terms of recruits)	
# |  |	
  0  1970 2006  PENALIZES DEPARTURES FROM BEVERTON AND HOLT STOCK-RECRUIT CURVE	
#	             (note: check the parameter file to make sure you are estimating the S/R 
#                           parameters when pdf not 0, or not estimating them when pdf=0))	
#-----------------------------------------------------------------------------	
# PARAMETER ESTIMATION OPTIONS	
#-----------------------------------------------------------------------------	
 1           OPTION TO USE (1) F'S OR (2) N'S AS TERMINAL YEAR PARAMETERS	
 -1             ESTIMATE Q IN (+) SEARCH or (<0) by concentrated MLE's	
#-----------------------------------------------------------------------------	
# BOOTSTRAP ANALYSES 	
#-----------------------------------------------------------------------------	
# Number of bootstraps to run (negative value = do a parametric bootstrap)	
# |   Use Stine correction to inflate bootstrap residuals (0=NO)	
# |   |	
  0   1           BOOTSTRAP OPTION 	
#-----------------------------------------------------------------------------	
# RETROSPECTIVE ANALYSES (CANNOT DO RETROSPECTIVE ANALYSES AND BOOTSTRAPS AT SAME TIME)	
#-----------------------------------------------------------------------------	
 0                NUMBER OF YEARS TO GO BACK FOR RETROSPECTIVE ANALYSES	
@@EOF@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@	
