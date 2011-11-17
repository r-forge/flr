#### FUNCTION to write ADAPT files (VPA + projections)

###########################################################################
########################  WRITE CONTROL FILE  #############################
###########################################################################
### changed PEDV because should be integer????

write_c1VPA <- function(dir_out,
                        years=c(1950,2009),
                        control_file_name=paste(dir_out,'bfte2010.c1',sep=""),
                        run_title=paste('BFT East ',years[1],'-',years[2],' Continuity Run ', sep=""),
                        data_file_name='bfte2010.d1',
                        parameter_file_name='bfte2010.p1',
                        result_file_name='bfte2010.r1',
                        parameter_estimate_file_name='bfte2010.e1',
                        f_spreadsheet_results='bfte2010.spd',
                        tagging_file_name='none',
                        ## MODEL TYPE OPTIONS
                        number_zone=1,      ## NUMBER OF ZONES (1 OR 2)
                        model_type=1,       ## MODEL_TYPE (1=DIFFUSION, 2=OVERLAP)
                        ### TAGGING DATA SWITCH
                        tagging_data_switch=0,  #(0=do not use tagging data, 1=use tagging data)
                        weighting_factor=1.0,  	# for modifying importance of tagging data in objective function
                        tag_timing_factor=c(1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1), # pDF OF TAG DATA,DOWNWEIGHTING DIVISOR, TIME OF YEAR WHEN FISHING SEASON BEGINS, DURATION OF FISHING SEASON-- (<=0) DO NOT USE TAGGING DATA
                        ### SEARCH ALGORITHM CONTROLS
                        random_seed=-911,   	## RANDOM NUMBER SEED
                        max_amoeba=100,   	## MAXIMUM NUMBER OF AMOEBA SIMPLEX SEARCH RESTARTS
                        number_restart=3,    	## NUMBER OF CONSECUTIVE RESTARTS THAT MUST VARY BY LESS THAN 1% TO STOP SEARCH  
                        PDEV=1,		## PDEV (standard deviation controlling vertices for Initial simplex of each restart)
                        ### INDEX WEIGHTING CONTROLS
                        scale=1,      		## SCALE (DIVIDE INDEX VALUES BY THEIR MEAN)- ANY VALUE > 0 = YES
                        index_weighting=1.0,    	## INDEX WEIGHTING:(0)INPUT CV's, (+)DEFAULT CV, (-)DEFAULT STD. DEV., (999)MLE
                        multiplicative_variance=0,    ## (0) MULTIPLICATIVE VARIANCE SCALING FACTOR or (1) ADDITIVE VARIANCE SCALING FACTOR 
                        ### CONSTRAINT ON VulnerabilitY (PARTIAL RECRUITMENT) 
                        penalty=3,		# apply this penalty to the last N years (SET N = 0 TO IGNORE)
                        sd_severity=.5,	# |  standard deviation controlling the severity of the penalty
                        first_age_affected=1,	# |  |  first age affected
                        last_age_affected=9,	# |  |  |  last age affected
                        ### CONSTRAINTS ON RECRUITMENT 
                        penalty_recruitment=c(2,0.5),		# apply this penalty to the last N years (SET N = 0 TO IGNORE)
                        sd_penalty_recruitment=c(0,.1,1),	# |  standard deviation controlling the severity of the penalty and ratio of stock (sex) 1 to stock (sex) 2 {a value of 1 means a 1:1 ratio}
                        ### CONSTRAINT ON SPAWNER-RECRUIT RELATIONSHIP
                        pdf_SR=0,			# PDF of spawner-recruit penalty: 0=none, 1=lognormal, 2=normal (-)=estimate sigma by MLE
                        fit_SR_year_start=1971,	# |  first and last years to use in fitting (in terms of recruits)
                        fit_SR_year_end=1996,		# |  first and last years to use in fitting (in terms of recruits)
                        ### PARAMETER ESTIMATION OPTIONS
                        option_parameter=1,	# OPTION TO USE (1) F'S OR (2) N'S AS TERMINAL YEAR PARAMETERS
                        estimate_q=-1,	# ESTIMATE Q IN (+) SEARCH or (<0) by concentrated MLE's
                        ### BOOTSTRAP ANALYSES 
                        number_bootstraps=10,		# Number of bootstraps to run (negative value = do a parametric bootstrap)
                        stine_correction=1,		# |   Use Stine correction to inflate bootstrap residuals (0=NO)
                        file_type_bootstraps=1,	# |   |   File type (0 = ASC-II, 1=Binary)
                        # RETROSPECTIVE ANALYSES (CANNOT DO RETROSPECTIVE ANALYSES AND BOOTSTRAPS AT SAME TIME)
                        number_year_retrospective_analyses=0){

cat("#-----------------------------------------------------------------------------
#--               CONTROL FILE FOR PROGRAM VPA-2BOX, Version 3.0           ---
#-----------------------------------------------------------------------------
#  INSTRUCTIONS: the control options are entered in the order specified.
#                Additional comment lines may be inserted anywhere in this 
#                file provided they are preceded by a # symbol in the FIRST 
#                column, otherwise the line is perceived as free-format data.
#-----------------------------------------------------------------------------
# Input file created automatically with the write_c1VPA R function on ", date(),
"\n#-----------------------------------------------------------------------------
# TITLES AND FILE NAMES (MUST BE PLACED WITHIN SINGLE QUOTES)
#-----------------------------------------------------------------------------
#|--------must be 50 characters or less----------|\n",
    paste("\'",run_title,"\'", sep=""), "\n",
    paste("\'",data_file_name,"\'", sep=""), "\n",
    paste("\'",parameter_file_name,"\'", sep=""),"\n",
    paste("\'",result_file_name,"\'", sep=""), "\n",
    paste("\'",parameter_estimate_file_name,"\'", sep=""), "\n",
    paste("\'",f_spreadsheet_results,"\'", sep=""),"\n",
    paste("\'",tagging_file_name,"\'", sep=""),"\n",
"#-----------------------------------------------------------------------------
# MODEL TYPE OPTIONS
#-----------------------------------------------------------------------------\n",
    number_zone,"\n",
    model_type, "\n",
"#-----------------------------------------------------------------------------
# TAGGING DATA SWITCH
#-----------------------------------------------------------------------------
# tagging data switch (0=do not use tagging data, 1=use tagging data)
# |  weighting factor for modifying importance of tagging data in objective function
# |  |     tag timing factors
# |  |     |    \n",
    tagging_data_switch," ",  #(0=do not use tagging data, 1=use tagging data)
    weighting_factor," ",  	# for modifying importance of tagging data in objective function
    paste(tag_timing_factor,"",sep=" "), # pDF OF TAG DATA,DOWNWEIGHTING DIVISOR, TIME OF YEAR WHEN FISHING SEASON BEGINS, DURATION OF FISHING SEASON-- (<=0) DO NOT USE TAGGING DATA      
"#\n-----------------------------------------------------------------------------
# SEARCH ALGORITHM CONTROLS
#-----------------------------------------------------------------------------\n",
    random_seed,"\n",     ## RANDOM NUMBER SEED
    max_amoeba,"\n",   	## MAXIMUM NUMBER OF AMOEBA SIMPLEX SEARCH RESTARTS
    number_restart,"\n",    	## NUMBER OF CONSECUTIVE RESTARTS THAT MUST VARY BY LESS THAN 1% TO STOP SEARCH  
    PDEV,"\n",		## PDEV (standard deviation controlling vertices for Initial simplex of each restart)
"#-----------------------------------------------------------------------------
# INDEX WEIGHTING CONTROLS
#-----------------------------------------------------------------------------\n",
    scale,"\n",        	## SCALE (DIVIDE INDEX VALUES BY THEIR MEAN)- ANY VALUE > 0 = YES
    index_weighting,"\n",    	## INDEX WEIGHTING:(0)INPUT CV's, (+)DEFAULT CV, (-)DEFAULT STD. DEV., (999)MLE
    multiplicative_variance,"\n",    ## (0) MULTIPLICATIVE VARIANCE SCALING FACTOR or (1) ADDITIVE VARIANCE SCALING FACTOR 
"#-----------------------------------------------------------------------------
# CONSTRAINT ON VulnerabilitY (PARTIAL RECRUITMENT) 
#-----------------------------------------------------------------------------
# apply this penalty to the last N years (SET N = 0 TO IGNORE)
# |  standard deviation controlling the severity of the penalty
# |  |  first age affected   
# |  |  |  last age affected
# |  |  |  | \n",
    penalty, " ",  	# apply this penalty to the last N years (SET N = 0 TO IGNORE)
    sd_severity, " ",	# |  standard deviation controlling the severity of the penalty
    first_age_affected, " ",	# |  |  first age affected
    last_age_affected, " ",	# |  |  |  last age affected
"#-----------------------------------------------------------------------------
# CONSTRAINTS ON RECRUITMENT 
#-----------------------------------------------------------------------------
# apply this penalty to the last N years (SET N = 0 TO IGNORE)
# |  standard deviation controlling the severity of the penalty\n",
    paste(penalty_recruitment,"",sep=" "),"\n",  	# apply this penalty to the last N years (SET N = 0 TO IGNORE)
    paste(sd_penalty_recruitment,"",sep=" "),"\n",	# |  standard deviation controlling the severity of the penalty and ratio of stock (sex) 1 to stock (sex) 2 {a value of 1 means a 1:1 ratio}
"#        |
#        ratio of stock (sex) 1 to stock (sex) 2 {a value of 1 means a 1:1 ratio}
#-----------------------------------------------------------------------------
# CONSTRAINT ON SPAWNER-RECRUIT RELATIONSHIP
#-----------------------------------------------------------------------------
# PDF of spawner-recruit penalty: 0=none, 1=lognormal, 2=normal (-)=estimate sigma by MLE
# |  first and last years to use in fitting (in terms of recruits)
# |  |\n",
    pdf_SR," ",  		# PDF of spawner-recruit penalty: 0=none, 1=lognormal, 2=normal (-)=estimate sigma by MLE
    fit_SR_year_start," ",	# |  first and last years to use in fitting (in terms of recruits)
    fit_SR_year_end,"\n",		# |  first and last years to use in fitting (in terms of recruits)
"#               (note: check the parameter file to make sure you are estimating the S/R 
#                           parameters when pdf not 0, or not estimating them when pdf=0))
#-----------------------------------------------------------------------------
# PARAMETER ESTIMATION OPTIONS
#----------------------------------------------------------------------------\n",
    option_parameter,"\n",  # OPTION TO USE (1) F'S OR (2) N'S AS TERMINAL YEAR PARAMETERS
    estimate_q,"\n",	# ESTIMATE Q IN (+) SEARCH or (<0) by concentrated MLE's
"#-----------------------------------------------------------------------------
# BOOTSTRAP ANALYSES 
#-----------------------------------------------------------------------------
# Number of bootstraps to run (negative value = do a parametric bootstrap)
# |   Use Stine correction to inflate bootstrap residuals (0=NO)
# |   |   File type (0 = ASC-II, 1=Binary)
# |   |   |\n",
    number_bootstraps," ",  	# Number of bootstraps to run (negative value = do a parametric bootstrap)
    stine_correction," ",		# |   Use Stine correction to inflate bootstrap residuals (0=NO)
    file_type_bootstraps,"\n",	# |   |   File type (0 = ASC-II, 1=Binary)
"#-----------------------------------------------------------------------------
# RETROSPECTIVE ANALYSES (CANNOT DO RETROSPECTIVE ANALYSES AND BOOTSTRAPS AT SAME TIME)
#-----------------------------------------------------------------------------\n",
    number_year_retrospective_analyses,"\n",
"@@EOF@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@",
    sep="", file=control_file_name)

print(paste("File ",dir_out,'bfte2010.c1 has been created!!!', sep=""))
}

###########################################################################
########################  WRITE DATA FILE  ################################
###########################################################################

###TO DO: calculate the partial catch, WAA and fecundity internally
write_d1VPA <- function(dir_data,
                        dir_out,
                        file="bfte2010.d1",
                        filename=paste(dir_out,file, sep=""),
                        years=c(1950,2009),
                        first_age=1, last_age=10, age_plus=10, expanded_group=10, CPUE_number=12,
                        cpue_number=12,
                        spawning_season=6,
                        maturity_at_age=c(0,0,0,0.5,rep(1,(age_plus-4))),
                        output_file_unknown='BFTE test',
                        pdf_catch=0,
                        sigma_catch=0.1,
                        title_pdf_sigma=0,
                         caa=read.csv(paste(dir_data,'CAA.csv', sep=""),sep=";" ,header=F),
                        cpue_to_use=1:12,  ## Choose which cpue you want to include
                        cpue_odd=rep(" ",cpue_number),
                        cpue_use=c(1,0,1,1,0,0,1,0,0,1,1,1),    ## INDEX PDF  (0= do not use,1=lognormal, 2=normal)
                        cpue_units=c(1,1,1,1,1,1,1,1,1,2,2,1),  		## UNITS (1 = numbers, 2 = biomass)
                        cpue_vulnerability=c(4,1,1,1,1,1,4,4,4,4,1,4), ## VULNERABILITY (1=fixed, 2=frac.catches, 3=part. catches, 4=Butt. & Gero.
                        cpue_timing=c(-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1),	## TIMING (-1=average, +integer = number of months elapased}
                        cpue_first_age=c(6,1,2,3,4,5,6,6,6,2,10,4),	## FIRST AGE
                        cpue_last_age=c(10,1,2,3,4,5,10,10,10,3,10,10),	## LAST AGE
                        cpue_title=c('ESPMarTrap', 'ESP BB1', 'ESP BB2', 'ESP BB3','ESP BB4', 'ESP BB5', 'JLL EastMed','MarTrap', 'ESP trap', 'ESP BB historic', 'Nor PS', 'JP LL NEA'),
                        partial_catch_cpue_number=4,		## number of partial catch
                        cpues=read.csv(paste(dir_data,'CPUE.csv', sep=""),sep=";" ,header=F),
                        partial_catch=read.csv(paste(dir_data,'partial_catches.csv', sep=""),sep=";" ,header=F),
                        waa=read.csv(paste(dir_data,'waa.csv', sep=""),sep=";" ,header=F),
                        faa=read.csv(paste(dir_data,'fecaa.csv', sep=""),sep=";" ,header=F),
                        scaling_parameter=1
                        ){
cat("##############################################################################
#  DATA FILE FOR PROGRAM VPA-2BOX, Version 3.01
#   The data and specifications are entered in the order indicated 
#      by the existing comments. Additional comments must be preceded by a # symbol
#      in the first column, otherwise the line is perceived as free format input.
#  This data file has been generated automatically from the writed1VPA R function on ", date(),"
##############################################################################
# DATA FILE FOR Continuity RUN XX\n", years,"\n",first_age, last_age, age_plus, expanded_group,
    "\n###############################################################################
# BEGIN INPUT FOR ZONE/STOCK 1
###############################################################################\n",
cpue_number,"\n",spawning_season,"\n",maturity_at_age,
"\n# 50 CHARACTER TITLE WITHIN SINGLE QUOTES   ----->]  PDF OF CATCH 
# |                                                    |       SIGMA CATCH
", paste('\'',output_file_unknown,'\'', sep=""), pdf_catch, sigma_catch,
"\n#============================================================================== 
# NOW ENTER THE CATCH-AT-AGE DATA. ROW=YEAR, COLUMN=AGE
#============================================================================== 
#   YEAR", 1:age_plus,"+ <--AGE\n", sep=" ", file=filename)

CAA=caa[which(caa[,1]>=years[1] & caa[,1]<=years[2]),]/scaling_parameter
write.table(CAA,file=filename,append=T,eol="\n", row.names=FALSE, col.names=FALSE, quote=F,sep='\t')

cat("-1
#============================================================================== 
# NOW ENTER IN THE ABUNDANCE INDEX SPECIFICATIONS
#============================================================================== 
# INDEX PDF  (0= do not use,1=lognormal, 2=normal)
# |     |    UNITS (1 = numbers, 2 = biomass)
# |     |    |      VULNERABILITY (1=fixed, 2=frac.catches, 3=part. catches, 4=Butt. & Gero.
# |     |    |      |     TIMING (-1=average, +integer = number of months elapased}
# |     |    |      |     |     FIRST AGE  LAST AGE   TITLE (IN SINGLE QUOTES)\n",
    sep=" ", file=filename, append=T)

    cpue_table <- cbind(cpue_odd,
                        cpue_to_use,
                        cpue_use,
                        cpue_units,
                        cpue_vulnerability,
                        cpue_timing,
                        cpue_first_age,
                        cpue_last_age,
                        paste("\'",cpue_title,"\'",sep="")
                        #partial_catch_cpue_number
                        )
    cpue_table <- cpue_table[cpue_to_use,]
    write.table(cpue_table, file=filename,sep=" ", append=T,eol="\n", row.names=FALSE, col.names=FALSE, quote=F)
cat("-1 
#============================================================================== 
# NOW ENTER IN THE INDICES OF ABUNDANCE
#============================================================================== 
#INDEX
#ID   YEAR      VALUE   CV (or STD ERROR)\n", sep=" ", file=filename, append=T)
cpues <- cpues[which(cpues[,1] %in% cpue_to_use),]
write.table(cpues, file=filename,sep="\t", append=T,eol="\n", row.names=FALSE, col.names=FALSE, quote=F)
cat("-1                                       
#============================================================================== 
# NOW ENTER IN THE VULNERABILITIES OR PARTIAL CATCHES FOR THE INDICES OF ABUNDANCE
#============================================================================== 
#INDEX YEAR   AGE 1-10\n", sep=" ", file=filename, append=T)
write.table(partial_catch, file=filename,sep="\t", append=T,eol="\n", row.names=FALSE, col.names=FALSE, quote=F)
cat("-1 -1 
#============================================================================== 
# NOW ENTER IN THE WEIGHTS AT AGE FOR THE INDICES OF ABUNDANCE (row=year, col=age)          
#============================================================================== 
#Index year  ages 1-10", sep=" ", file=filename, append=T)
write.table(waa, file=filename,sep="\t", append=T,eol="\n", row.names=FALSE, col.names=FALSE, quote=F)
cat("-1
#============================================================================== 
# NOW ENTER IN THE FECUNDITY AT AGE FOR THE SPAWNING STOCK BIOMASS (row=year, col=age)       
#==============================================================================\n", sep=" ", file=filename, append=T)
write.table(waa, file=filename,sep="\t", append=T,eol="\n", row.names=FALSE, col.names=FALSE, quote=F)
cat("-1", sep=" ", file=filename, append=T)
}

###########################################################################
########################  WRITE PARAMETER FILE  ###########################
###########################################################################

### TO DO: to relate to the age plus
### how the number of variance scaling parameter is set?
write_p1VPA <- function(#######  TERMINAL F PARAMETERS  ##########
                        dir_out,
                        file='bfte2010.p1',
                        parameter_file_name=paste(dir_out,file,sep=""),
                        age_plus=10,
                        min_bound_terminal_F_vector=c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.5),
                        terminal_F_vector=c(0.75, 0.3, 0.3, 1.2, 1.6, 0.3, 1, 0.3, rep(1.5,(age_plus-9))),
                        max_bound_terminal_F_vector=c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.3,0.5,  rep(0.3,(age_plus-9))),
                        indicator=c(0,rep(1, (age_plus-2))),
                        standard_deviation_prior=c(0.2, 0.01, 0.01, 0.3, 0.3, 0.01, 0.6, 0.01, rep(0.8, (age_plus-9))),
                        reference_age=seq(1, (age_plus-1)),
                        #####  TERMINAL F FINAL PARAMETERS  ######
                        number_period=3,
                        periods_duration=c(20,15,10,15),
                        #D-01
                        min_bound_terminal_F_vector_final=c(1, 1, 1, 1),
                        #D-00
                        terminal_F_vector_final=c(0.7, 1, 0.6,1.2),
                        #D+01
                        max_bound_terminal_F_vector_final=c(0.5, 0.5, 0.5, 0.5),
                        indicator_final=c(0,0,0,0),
                        #D+01
                        standard_deviation_prior_final=rep(0.02, number_period),
                        reference_age_final=rep(age_plus, number_period),
                        #####  NATURAL MORTALITY PARAMETERS  #####
                        min_bound_terminal_M_vector=c(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0, 0., rep(0,(age_plus-9))),
                        terminal_M_vector=c(0.49, 0.24, 0.24, 0.24, 0.24, 0.2, 0.175, 0.15, 0.125, rep(0.1,(age_plus-9))),
                        max_bound_terminal_M_vector=c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, rep(0.1,(age_plus-9))),
                        indicator_M=rep(0, age_plus),
                        standard_deviation_prior_M=c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, rep(0.1, (age_plus-8))),
                        reference_age_M=seq(45, (45+(age_plus-1))),
                        # MIXING PARAMETERS: one parameter (set of specifications) for each age
                        mixing_parameter_1=10,
                        min_bound_mixing_parameter=0,
                        best_mixing_parameter=0,
                        max_bound_mixing_parameter=0.1,
                        indicator_mixing_parameter=0,
                        standard_deviation_prior_parameter=0.1,
                        reference_age_parameter=(46+(age_plus-1)),
                        #####  STOCK-RECRUITMENT PARAMETERS  #####
                        min_bound_SR=c(0.0, 0.0, 0.0, 0.0, 0.0),
                        SR=c(0.2507, 0.1660, 0.1580, 0.1, 0.1),
                        D_best_SR=c(6,5,0,1,1),
                        max_bound_SR=c(0.1, 0.1, 0.9, 0.2, 0.2),
                        D_max_SR=c(21,21,0,1,1),
                        indicator_SR=c(0,0,0,0,0),
                        standard_deviation_prior_SR=c(0.4,0,0,0,0),
                        reference_age_parameter_SR=seq(((age_plus)+reference_age_parameter),((age_plus)+reference_age_parameter+4)),
                        #####  VARIANCE SCALING PARAMETER  #######
                        min_bound_VAR=c(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,0.0,0.0,0.0,0.0),
                        VAR=c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1,0.1,0.1,0.1,0.1),
                        D_best_VAR=c(1,1,1,1,1,1,1,1,1,1,1,1),
                        max_bound_VAR=c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1),
                        D_max_VAR=c(21,21,21,21,21,21,21,21,21,21,21,21),
                        indicator_VAR=c(1,-0.1,-0.1,-0.1,-0.1,-0.1,-0.1,-0.1,-0.1,-0.1,-0.1,-0.1),
                        standard_deviation_prior_VAR=c(0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4),
                        reference_age_parameter_VAR=seq(((age_plus)+reference_age_parameter+5),((age_plus)+reference_age_parameter+4+length(VAR)))
) {
  
cat("# PARAMETER FILE FOR PROGRAM VPA_2BOX, Version 3.0
#        The specifications are entered in the order indicated 
#        by the existing comments. Additional comments must be preceded by a number symbol
#        in the first column, otherwise the line is perceived as free format input.
#
#      	Each parameter in the model must have its own specification lines unless a dollar 
#      	symbol is placed in the first column followed by an integer value (n), which 
#      	tells the program that the next n parameters abide by the same specifications.
#
#      	The format of each specification line is as follows
#
#      	column 1
#      	|   number of parameters to which these specifications apply
#      	|   |    lower bound
#      	|   |    |       best estimate (prior expectation)
#      	|   |    |       |       upper bound
#      	|   |    |       |       |       method of estimation
#      	|   |    |       |       |       |      standard deviation of prior 
#      	$   5    0       1.2     2.0     1      0.1
#
#	The methods of estimation include:
#	0  	set equal to the value given for the best estimate (a fixed constant)
#	1	estimate in the usual frequentist (non-Bayesian) sense 
#	2(0.1)	estimate as a random deviation from the previous parameter
#	3(0.2)	estimate as a random deviation from the previous constant or type 1 parameter
#	4(0.3)	estimate as random deviation from the best estimate.
#	-0.1  	set equal to the value of the closest previous estimated parameter
#	-n  	set equal to the value of the nth parameter in the list (estimated or not)
#-----------------------------------------------------------------------------
#=============================================================================
# TERMINAL F PARAMETERS: (lower bound, best estimate, upper bound, indicator, reference age)
#   	Note 1: the method indicator for the terminal F parameters is unique in that if it is
#   	zero but the best estimate is set to a value < 9, then the 'best estimate'
#   	is taken to be the vulnerability relative to the reference age in the last 
#   	(fifth) column.  Otherwise these parameters are treated the same as the
#    	others below and the fifth column is the standard deviation of the prior.        
# 	Note 2: the last age is represented by an F-ratio parameter (below), so the number
#     	of entries here should be 1 fewer than the number of ages 
#-----------------------------------------------------------------------------
## Input file created automatically with the write_p1VPA R function on ", date(),
"\n#=============================================================================
# terminal age structure of population
#-----------------------------------------------------------------------------\n",
    sep="", file=parameter_file_name)

    terminal_Fs_final <- cbind(paste(
            "  ",sprintf('%.4f',min_bound_terminal_F_vector),"D-01",
            "   ", sprintf('%.4f',terminal_F_vector), "D+00",
            "   ", sprintf('%.4f',max_bound_terminal_F_vector),"D+01",
            "      ",sprintf('%.1f',indicator),
            "   ", sprintf('%.4f',standard_deviation_prior),"D+01",
            "     ",sprintf('%.0f',reference_age),
                    sep=""))  
    write.table(terminal_Fs_final,file=parameter_file_name,eol="\n", row.names=FALSE, col.names=FALSE, quote=F,sep='',append=T)

cat("# F-RATIO PARAMETERS F{oldest}/F{oldest-1} one parameter (set of specifications) for each year
#-----------------------------------------------------------------------------\n",
    sep="", file=parameter_file_name, append=T)

    terminal_Fs_final <- cbind(paste(
      "$ ",sprintf('%.0f',periods_duration),
      "  ", sprintf('%.4f',min_bound_terminal_F_vector_final),"D-01",
      "   ", sprintf('%.4f',terminal_F_vector_final), "D+00",
      "   ", sprintf('%.4f',max_bound_terminal_F_vector_final),"D+01",
      "      ",sprintf('%.1f',indicator_final),
      "   ", sprintf('%.4f',standard_deviation_prior_final),"D+01",
      "    ",sprintf('%.0f',reference_age_final),
                          sep=""))
    write.table(terminal_Fs_final,file=parameter_file_name,eol="\n", row.names=FALSE, col.names=FALSE, quote=F, append=T)

cat("#=============================================================================
# NATURAL MORTALITY PARAMETERS: one parameter (set of specifications) for each age
#-----------------------------------------------------------------------------\n", 
    sep="", file=parameter_file_name, append=T)
  
    terminal_Ms <- cbind(paste(
      "  ",sprintf('%.4f',min_bound_terminal_M_vector),"D+00",
      "   ", sprintf('%.4f',terminal_M_vector), "D+00",
      "   ", sprintf('%.4f',max_bound_terminal_M_vector),"D+01",
      "      ",sprintf('%.1f',indicator_M),
      "   ", sprintf('%.4f',standard_deviation_prior_M),"D+00",
      "    ",sprintf('%.0f',reference_age_M)
                    ,sep=""))
    write.table(terminal_Ms,file=parameter_file_name,eol="\n", row.names=FALSE, col.names=FALSE, quote=F, append=T)
cat("#=============================================================================
# MIXING PARAMETERS: one parameter (set of specifications) for each age
#-----------------------------------------------------------------------------\n",
  sep="", file=parameter_file_name, append=T)
    
    mixing_parameter <- cbind(paste(
      "$ ",sprintf('%.0f',mixing_parameter_1),
      "  ", sprintf('%.4f',min_bound_mixing_parameter),"D+00",
      "   ", sprintf('%.4f',best_mixing_parameter), "D+00",
      "   ", sprintf('%.4f',max_bound_mixing_parameter),"D+01",
      "      ",sprintf('%.1f',indicator_mixing_parameter),
      "   ", sprintf('%.4f',standard_deviation_prior_parameter),"D+00",
      "    ",sprintf('%.0f',reference_age_parameter),
                        sep=""))
    write.table(mixing_parameter,file=parameter_file_name,eol="\n", row.names=FALSE, col.names=FALSE, quote=F, append=T)
cat("#=============================================================================
# STOCK-RECRUITMENT PARAMETERS: five parameters so 5 sets of specifications
#-----------------------------------------------------------------------------\n",
    sep="", file=parameter_file_name, append=T)

    SR_parameter <- cbind(paste(
      "  ", sprintf('%.4f',min_bound_SR),"D+00",
      "   ", sprintf('%.4f',SR),
        paste("D+",sprintf('%.2i',D_best_SR),sep=""),
      "   ", sprintf('%.4f',max_bound_SR),
        paste("D+",sprintf('%.2i',D_max_SR),sep=""),
      "      ",sprintf('%.1f',indicator_SR),"   ",
      sprintf('%.4f',standard_deviation_prior_SR),"D+00",
      "    ",sprintf('%.0f',reference_age_parameter_SR),
                    sep=""))
    write.table(SR_parameter,file=parameter_file_name,eol="\n", row.names=FALSE, col.names=FALSE, quote=F, append=T)
cat("#=============================================================================
# VARIANCE SCALING PARAMETER (lower bound, best estimate, upper bound, indicator, std. dev.) 
#   this parameter scales the input variance up or down as desired
#   In principal, if you estimate this you should obtain more accurate estimates of the
#   magnitude of the parameter variances-- all other things being equal.
#-----------------------------------------------------------------------------\n",
  sep="", file=parameter_file_name, append=T)

  VAR_parameter <- cbind(paste(
    "  ", sprintf('%.4f',min_bound_VAR),"D+00",
    "   ", sprintf('%.4f',VAR),
      paste("D+",sprintf('%.2i',D_best_VAR),sep=""),
    "   ", sprintf('%.4f',max_bound_VAR),
      paste("D+",sprintf('%.2i',D_max_VAR),sep=""),
    "      ",sprintf('%.1f',indicator_VAR),
    "   ", sprintf('%.4f',standard_deviation_prior_VAR),"D+00",
    "    ",sprintf('%.0f',reference_age_parameter_VAR),
                    sep=""))
  write.table(VAR_parameter,file=parameter_file_name,eol="\n", row.names=FALSE, col.names=FALSE, quote=F, append=T)
cat("@ END PARAMETER INPUT",sep="", file=parameter_file_name, append=T)
print(paste("File ",parameter_file_name,' has been created!!!', sep=""))
  
}

###########################################################################
########################  WRITE QUOTA FILE  ###############################
###########################################################################

write_quotas <- function(dir_out,
                         file="quotas.txt",
                         file_out=paste(dir_out,file, sep=""),
                         scen_num=12, ### !!! 3 are then added for F0.1, FMSY and FMAX
                         area_stock=1,
                         fishery=1,
                         current_quota=13500,
                         futur_quotas=c(seq(0,12000,2000),
                                        current_quota,
                                        seq(14000,20000,2000)),
                         current_F=3,
                         futur_F=3
                         ){
cat("***Enter the number of catch/effort scenarios to run.\n",
  scen_num,"\n", sep="",file=file_out)

for (i in 1:scen_num){
  cat("#-------------------------------------------------------------------------
**",i,".  ", futur_quotas[i],
"\n#-------------------------------------------------------------------------
**Enter the total allowable catch (mt) for each projection year
# area/stock
# | fishery
# | | quotas for each projection year
# | | |\n",
paste(area_stock, fishery, current_quota, paste("100*",futur_quotas[i],sep=""),sep="\t"),
"\n-1  	
**Enter the fixed fully-selected fishing mortality rates by year\n",
paste(area_stock, fishery, current_F, paste("100*",futur_F,sep=""),sep="\t"),
"\n-1 \n",   	
    sep="",file=file_out,append=T)
}

cat("#-------------------------------------------------------------------------
**",scen_num+1,". F01
#-------------------------------------------------------------------------
**Enter the total allowable catch (mt) for each projection year
# area/stock
# | fishery
# | | quotas for each projection year
# | | |\n",
paste(area_stock, fishery, current_quota,"100*2000000",sep="\t"),
"\n-1 
**Enter the fixed fully-selected fishing mortality rates by year\n",
paste(area_stock, fishery, current_F, "100*-0.1",sep="\t"),
"\n-1 
#-------------------------------------------------------------------------
**",scen_num+2,". Fmsy
#-------------------------------------------------------------------------
**Enter the total allowable catch (mt) for each projection year
# area/stock
# | fishery
# | | quotas for each projection year
# | | |\n",
paste(area_stock, fishery, current_quota, "100*2000000",sep="\t"),
"\n-1 
**Enter the fixed fully-selected fishing mortality rates by year\n",
paste(area_stock, fishery, current_F, "100*-2",sep="\t"),
"\n-1 
#-------------------------------------------------------------------------
**",scen_num+3,". Fmax
#-------------------------------------------------------------------------
**Enter the total allowable catch (mt) for each projection year
# area/stock
# | fishery
# | | quotas for each projection year
# | | |\n",
paste(area_stock, fishery, current_quota, "100*2000000",sep="\t"),
"\n-1 
**Enter the fixed fully-selected fishing mortality rates by year\n",
paste(area_stock, fishery, current_F, "100*-2",sep="\t"),
"\n-1 ",
    sep="",file=file_out,append=T)  
  
}

###########################################################################
###########################  WRITE DISC FILE  #############################
###########################################################################
write_disc <- function(dir_out,
                       file="disc.txt",
                       file_out=paste(dir_out,file, sep=""),
                       years=c(1950,2009),
                       age_plus=10,
                       v1=rep(1,length=(years[2]-years[1]+1)),
                       v2=rep(1,length=(years[2]-years[1]+1)),
                       exch=matrix(0,nrow=(years[2]-years[1]+1),ncol=age_plus)
                       ){
  mat_tot <- cbind(v1,v2,years[1]:years[2],exch)
  write.table(mat_tot,file=file_out,append=T,eol="\n", row.names=FALSE,
          col.names=FALSE, quote=F,sep='\t')
}
###########################################################################
###########################  WRITE Prj10.ctl FILE  ########################
###########################################################################
write_prjctlVPA <- function(dir_out,
                            file="Prj10.ctl",
                            file_out=paste(dir_out,file, sep=""),
                            model_type=1,
                            n_box=1,
                            n_loops=500,
                            ci=80,
                            seed=-911,
                            patch=3,
                            yr_box=1,
                            sr_box=0,
                            sex_fraction=1,
                            weight_type=1,
                            years=c(1950,2009,2040),
                            ages=c(1,10,10),
                            selec_F_years=c(2007,2009),
                            recruit_ssb_years=c(1990,2004,2000,2004),
                            err_type_ssb_r=1,
                            n_gears=1,
                            ave_age_plus_group=-40,
                            maturity=c(0,0,0,0.5,1,1,1,1,1,1),
                            curve_type=1,
                            L_inf=318.9,
                            k=0.093,
                            t0=-0.97,
                            Chap_Rich=1,
                            a_weight=0.0000196,
                            b_weight=3.0092,
                            time_growth=6,
                            offset_vpa=6,
                            file_types=c(0,1,1,0,0,1,0,0,0,0),
                            file_names=c("quotas.txt","NAA.OUT","FAA.OUT",
                                        "waa.txt", "caa.txt","MAA.OUT",
                                        "Sel.txt","recruit.txt","Trans.txt",
                                        "disc.txt")                       
                            ){
cat("#---------------------------------------------------------------------------------      								
#	INPUT DATA FILE FOR VPA2-box PROJECT bluefin east 2010										
# data entry is free-format.  Null lines and comments must be											
# initiated here by pound signs (#) asterisks (*) or dashes (-).  Do not,											
# however, interrupt a stream of vector inputs with a comment.											
#---------------------------------------------------------------------------------											
#---------------------------------------------------------------------------------											
# Enter control information common to both areas/stocks											
#---------------------------------------------------------------------------------\n",
    model_type, "\t\tMODEL_TYPE  (1 = diffusion , 2 = overlap, 3 = sex-specific)\n",
    n_box,"\t\tN_BOX  {number of sexes/areas/stocks -- enter 1 or 2}\n",
    n_loops,"\t\tN_LOOPS {number of bootstrap projections, enter 0 if only a single deterministic run and a negative integer if you want the program to check the ASC-II file bad.out for bad runs}\n",
    ci, "\t\tCI {confidence interval in percent (e.g., 80)}\n",
    seed,"\t\tSEED {random number seed (should be a negative integer)}\n",
    patch,"\t\tPATCH (>0)=number of recruitments replace with estimates from spawner-recruit curve, (<=0)=don t do it\n",
    yr_box,"\t\tYR_BOX (1)=maximize y/r and msy of sex/area/stock 1 alone, or (2) of sex/area/stock 2 alone, or (3) of both stocks combined\n",
    sr_box,"\t\tSR_BOX recruitment depends on fecundity of (1) sex 1 only, (2) sex 2 only, or (3) both sexes combined\n",	
    sex_fraction,"\t\tSEX_FRACTION = fraction of population belonging to sex 1 at age of recruitment (immaterial if SR_BOX<1 or N_box=1)\n",
    weight_type,"\t\tWEIGHT_TYPE (0) use growth curve for all weights (1) use growth curve only for plus group projections, (2) do not use growth curve at all\n",
    paste(years,"",sep="\t"),"\t{FIRST year, LAST year in data, Last year to PROJECT}\n",								
    paste(ages,"",sep="\t"),"\tFirst age followed by last ages for sex/area/stock 1 and 2 (a total of three entries)
#-------------------------------------------------------------------------											
# Enter the selectivity option: two positive integers indicate the first and 
#      last years for a geometric mean of the F's at age.  Any negative value indicates
#      that the user specified vector in the selectivity file are to be used (otherwise
#      the selectivity file values are used to modify the geometric mean)				
# 1997 1999  Values in 2002 Base\n",											
paste(selec_F_years,"",sep="\t"),
"\n#-------------------------------------------------------------------------											
# Enter the first and last years to use for recruitment and the first and last years for ssb
# 1976 1996  1991 2001  Values in 2002 Base ? For benchmarks?											
# 1990 2001  2000 2004  Values in 2006 Base ? For benchmarks?, 2000,2004 is the hinge point time period\n",
recruit_ssb_years,
"\n# Error type for recruitment projections and fitting spawner-recruit curves (1=lognormal, 2=normal)\n",
err_type_ssb_r,											
"#-------------------------------------------------------------------------											
# Enter area specific growth information for area/stock1											
#-------------------------------------------------------------------------\n",
n_gears,"			N_GEARS (area 1)\n",
ave_age_plus_group,"			average age of the plus-group at the BEGINNING of the last year (-x) = compute from growth curve\n",
paste(maturity,"",sep="\t"),"		Maturity fraction\n# curve type (1=von Bertalanffy or Chapman Richards, 2=Gompertz)
# |  Linfinity
# |  |     k
# |  |     |      tO
# |  |     |      |         Chapman-Richards skew parameter (m=1 for von Bert) - ignored if Gompertz used
# |  |     |      |         |     weight parameter a of w=al^b
# |  |     |      |         |     |          weight parameter b of w=al^b
# |  |     |      |         |     |          |       time of year for growth equation (in months)
# |  |     |      |         |     |          |       |    offset (months) to be subtracted from vpa age to get growth curve age
# |  |     |      |         |     |          |       |      |\n",
paste(curve_type, L_inf, k, t0, Chap_Rich, a_weight, b_weight, time_growth, offset_vpa ,sep="\t"),"		SSB\n",
paste(curve_type, L_inf, k, t0, Chap_Rich, a_weight, b_weight, time_growth, offset_vpa ,sep="\t"),"  	fishery
#-------------------------------------------------------------------------
# Enter area specific growth information for area/stock 2 (if any)							
#-------------------------------------------------------------------------
#-------------------------------------------------------------------------
# Enter file information
#-------------------------------------------------------------------------
# file type (0=asc-ii, default=VPA-2box binary format)
# |   file name
# |   |\n",
    file=file_out, sep="")

    file_info <- cbind(file_types,file_names)
    write.table(file_info,file=file_out,append=T,eol="\n", row.names=FALSE,
          col.names=FALSE, quote=F,sep='\t')
cat("# END OF FILE",file=file_out, sep="",append=T)   
}

###########################################################################
###########################  WRITE Sel FILE  ##############################
###########################################################################
write_sel <- function(dir_out,
                       file="Sel.txt",
                       file_out=paste(dir_out,file, sep=""),
                       v1=c(1,1),
                       v2=rep(1,10)
                       ){
  vec_tot <- c(paste(v1,"",sep=""),paste(sprintf('%.1f',v2),"",sep=""))
  write.table(vec_tot,file=file_out,append=F,eol="\t", row.names=FALSE,
          col.names=FALSE, quote=F,sep='\t')
}

###########################################################################
#########################  WRITE Recruit FILE  ############################
###########################################################################
write_recruit <- function(dir_out,
                      file="recruit.txt",
                      file_out=paste(dir_out,file, sep=""),
                      v1=1,
                      v2=-7,
                      virgin_biomass=2046087.597,
                      steepness=0.99,
                      cv_recruit=0.42966422,
                      v3=0,
                      v4=0
                       ){
  vec_tot <- paste(v1,v2,virgin_biomass,steepness,cv_recruit,v3,v4, sep="\t")
  write.table(vec_tot,file=file_out,append=F,eol="\t", row.names=FALSE,
          col.names=FALSE, quote=F,sep='\t')
}

###########################################################################
#################################  END  ###################################
###########################################################################
