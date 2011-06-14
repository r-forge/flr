! Parameter definition module modified from EMAS batch version

! Adapted by L Kell from Coby Needle's ICA batch implementation.

! problem in C picking up global variables if dimensions defined as parameters

!ARRAYLEN 
    MODULE ARRAYLEN
        ! Maximum lengths of the staticly allocated matrices
		! Sme as in DIMS but there is aproblem reading them in C
	    integer NAges 	     ! maximum no of ages including + gp
        integer NYrs 		 ! maximum no of years in analysis
        integer MaxNSurveys  ! maximum no of age-structured surveys
        integer MaxNBSurveys ! maximum number of SSB surveys
        integer NData 		 ! maximum no of observations 2500
        integer NParm 		 ! maximum no of parameters
        integer NIters 		 ! maximum iterations in uncertainty eval.
   END MODULE ARRAYLEN

!DIMS 
    MODULE DIMS
        ! Maximum lengths of the staticly allocated matrices
	    integer maxage 	    ! maximum no of ages including + gp
        integer maxyear 	! maximum no of years in analysis
        integer maxsurvey 	! maximum no of age-structured surveys
        integer maxbsurv 	! maximum number of SSB surveys
        integer maxdata 	! maximum no of observations 2500
        integer maxparm 	! maximum no of parameters
        integer maxiters 	! maximum iterations in uncertainty eval.

        PARAMETER (maxage = 20, maxyear = 100, maxsurvey = 20, maxbsurv = 2, maxdata = 2500, maxparm = 150, maxiters = 2000)
		
		! Flag for missing data
		DOUBLE PRECISION  missing 
	END MODULE DIMS

!STCK these are the input population data and estimates
     MODULE STCK
        USE DIMS
	    
		! range of population age data
		integer :: firstage, lastage, firstyear, lastyear 
        
!		DOUBLE PRECISION CN(maxyear,maxage)			! Catch Number 
!		DOUBLE PRECISION NM(maxyear,maxage)         ! Natural Mortality
!		DOUBLE PRECISION SW(maxyear,maxage)         ! Stock Weight
!		DOUBLE PRECISION MO(maxyear,maxage)         ! Maturity Ogive
!		DOUBLE PRECISION CW(maxyear,maxage)         ! Stock Weight
!		DOUBLE PRECISION LA(maxyear)                ! Reported Landings 
!    	DOUBLE PRECISION N(maxyear,maxage)          ! Numbers-at-age
!    	DOUBLE PRECISION F(maxyear,maxage)          ! F-at-age

		DOUBLE PRECISION CN(100,20)			! Catch Number 
		DOUBLE PRECISION NM(100,20)         ! Natural Mortality
		DOUBLE PRECISION SW(100,20)         ! Stock Weight
		DOUBLE PRECISION MO(100,20)         ! Maturity Ogive
		DOUBLE PRECISION CW(100,20)         ! Stock Weight
		DOUBLE PRECISION LA(100)            ! Reported Landings 
    	DOUBLE PRECISION N(100,20)          ! Numbers-at-age
    	DOUBLE PRECISION F(100,20)          ! F-at-age


		DOUBLE PRECISION PF                         ! Proportions of F before spawning
		DOUBLE PRECISION PM                         ! Proportions of M before spawning
     END MODULE STCK

! CONTROL Parameters and control flags for fitting the separable model
     MODULE CONTROL
        USE DIMS
		INTEGER maxSep                                                                                                                 
        LOGICAL DLLFlag    ! Flag to indicate whether DLL, if true turns off cosole IO
        LOGICAL UseSep     ! not used
		LOGICAL writeout   ! whether objective function writes out residuals to disk
		LOGICAL Full       ! whether objective funtion recalculates whole VPA, or only as far back as is currently required 
        INTEGER NySep      ! NySep - Number of years for separable model             
		INTEGER RefAge     ! RefAge - Reference age for fitting the separable model  
        INTEGER	RWOpt
		
        ! Parameter list +/- standard deviation 
		! (all parameters are natural logs of F, S, N, Q, etc.)        
        DOUBLE PRECISION RelWt(maxage)   ! Relative weights by age 
		DOUBLE PRECISION YearWt(maxyear) ! Relative weights by year  

		DOUBLE PRECISION TermS           ! selection on last true reference age                                                       

       ! Weighting matrices for catch-at-age; for aged surveys; for SSB surveys                                    
        DOUBLE PRECISION W(maxyear,maxage)  
		
		LOGICAL TwoSel          ! whether to have 2 selection patterns       
        LOGICAL StepSel         ! whether change in selection is step or gradual                                                            
        INTEGER ChangeSel       ! last year for constant selection, if 2 sel patterns chosen                                                
        DOUBLE PRECISION TermS2      

	    ! Stock and recruitment parameters (from SRR.INC)
        LOGICAL  FitSRR            ! whether to fit a stock-recruit relation
        INTEGER  lag               ! time lag between spawning and firstage

!		DOUBLE PRECISION CNLAMBDA(maxyear,maxage)
		DOUBLE PRECISION CNLAMBDA(100,20)
		DOUBLE PRECISION SRRLAMBDA ! weight for the SRR term in the objective function
        
		INTEGER fychosen ! first and last years of choice for SRR relation
        INTEGER lychosen ! first and last years of choice for SRR relation

		DOUBLE PRECISION, dimension(maxsurvey)                :: IXcor
		
        DOUBLE PRECISION FmTol     !added to so that can be initialised by control file
		PARAMETER (maxSep = 15)
     END MODULE CONTROL

! STATS statistics
     MODULE STATS
        USE DIMS
		INTEGER Nxdata     ! NxData - Number of observations                         
		INTEGER Nxparm     ! NxParm - Number of parameters                             
      	INTEGER                               :: NoCdata, dfsep, NoSRRdata
		INTEGER, dimension(maxbsurv)          :: NoBdata
		INTEGER, dimension(maxsurvey,maxage)  :: NoAdata
		
        DOUBLE PRECISION Xbest(maxparm)
		DOUBLE PRECISION Xlow(maxparm)
		DOUBLE PRECISION Xhigh(maxparm)

     	DOUBLE PRECISION				               :: SRRVar                                          
	    DOUBLE PRECISION                               :: CVar,Cchi2,CPchi,CCV,Cskew,CKurt                                                                             
        DOUBLE PRECISION, dimension(maxbsurv)          :: Bvar,Bchi2,BPchi,BCV,BSkew,BKurt                                                                                
        DOUBLE PRECISION, dimension(maxsurvey,maxage)  :: Avar,Achi2,APchi,ACV,ASkew,AKurt
     END MODULE STATS

! CPUE Parameters and data for CPUE
     MODULE CPUE
        USE DIMS

		INTEGER					        :: nageix          ! number of age-structured surveys (max is 5)
        INTEGER						    :: nssbix		  ! Number of SSB surveys (max is 3)
        
		
		INTEGER                         :: fbyear, lbyear           ! first and last years of the SSB surveys  
        !INTEGER, dimension(maxsurvey)  :: fage, lage, fyear, lyear	! first and last years and ages
		INTEGER, dimension(20)          :: fage, lage, fyear, lyear	! first and last years and ages
		

		INTEGER, dimension(maxsurvey)   :: QAParm, QBParm            ! number of fitted parameters for catchability relationship
        INTEGER, dimension(maxsurvey)  :: xplusgp 
        DOUBLE PRECISION ALAMBDA(maxsurvey,maxage)  
		DOUBLE PRECISION BLAMBDA(maxbsurv) 
		
		LOGICAL                        :: RecalculatePopulations
        LOGICAL, dimension(maxsurvey)  :: plusgp ! plusgp(n) is whether the last age of the nth age-structured survey is a plus-group
        
		DOUBLE PRECISION                                      :: MaxWeight, NextRec
	    DOUBLE PRECISION, dimension(maxsurvey)                :: Timing	                ! Timing of each of the age-structured surveys in fractions of a year
        DOUBLE PRECISION, dimension(maxparm)                  :: X
		DOUBLE PRECISION, dimension(maxage)                   :: S
		DOUBLE PRECISION, dimension(maxyear)                  :: SOP, Npool             ! dynamic pool plus-gp Ns
		DOUBLE PRECISION, dimension(maxbsurv,maxyear)         :: BSurvey	            ! SSB survey data indexed as (survey, year)
	    DOUBLE PRECISION, dimension(maxparm,maxparm)          :: COV
		DOUBLE PRECISION, dimension(maxyear,maxage)           :: FPerceived, NPerceived
		DOUBLE PRECISION, dimension(maxsurvey,maxyear,maxage) :: ASurvey                ! Age structured survey data indexed as (survey, year, age)
		
     END MODULE CPUE

! RESULTS parameter estimates, fitted values from the model fit
   MODULE RESULTS
       USE DIMS
       INTEGER HiFage     ! Lowest age for calcualting arithmetic mean F, for output table  
       INTEGER LoFage     ! Highest age for calcualting arithmetic mean F, for output table  
       
	   DOUBLE PRECISION, dimension(maxyear,maxage)           :: PredCN
       DOUBLE PRECISION, dimension(maxsurvey,maxyear,maxage) :: PredAINDEX ! Age structured survey data indexed
       DOUBLE PRECISION, dimension(maxbsurv,maxyear)	     :: PredBINDEX         ! SSB survey data indexed as (survey, year)                               
       DOUBLE PRECISION A         ! Parameters of the Beverton and Holt SRR
       DOUBLE PRECISION B         ! Parameters of the Beverton and Holt SRR        
	   DOUBLE PRECISION, dimension(maxyear)				     :: PredRecruit
	END MODULE RESULTS



! The names of files produced by ICA:      
    MODULE FILES
	   character (len=7),  parameter :: ica_sen = 'ica.sen'			! Aberdeen sensitivity analysis file
       character (len=7),  parameter :: ica_sum = 'ica.sum'			! Aberdeen summary file
       character (len=7),  parameter :: ica_vie = 'ica.vie'			! input file for ICAVIEW
       character (len=7),  parameter :: ica_out = 'ica.out'			! Main results file
       character (len=9),  parameter :: icacn_prd = 'icacn.prd'		! fitted values for the catches at age
       character (len=9),  parameter :: icacn_res = 'icacn.res'		! residuals for the catches at age
       character (len=12), parameter :: icaasurv_prd = 'icaasurv.prd'	! fitted values for the aged surveys 
       character (len=12), parameter :: icaasurv_res = 'icaasurv.res'	! residuals values for the aged surveys 
       character (len=12), parameter :: icabsurv_prd = 'icabsurv.prd'	! fitted values for the SSB surveys 
       character (len=12), parameter :: icabsurv_res = 'icabsurv.res'	! residuals values for the SSB surveys 
       character (len=5),  parameter :: ica_n = 'ica.n'				! Population abundances
       character (len=5),  parameter :: ica_f = 'ica.f'				! Fishing mortality
       character (len=7),  parameter :: ica_tmp = 'ica.tmp'			! input file for ICPROJ
       character (len=11), parameter :: icadiag_out = 'icadiag.out'	! diagnostics of survey divergence
       character (len=7),  parameter :: ica_shr = 'ica.shr'			! file to write the shrunk VPA
       character (len=6),  parameter :: ica_vc = 'ica.vc'				! variance covariance matrix
       character (len=7),  parameter :: ica_log = 'ica.log'			! screen output is copied to here

       ! Files for the MC parametric bootstrap iterations                                                 
       character (len=9),  parameter :: stock_mci ='stock.mci'			! These hold the MC iterations
       character (len=12), parameter :: recruits_mci ='recruits.mci'
       character (len=9),  parameter :: meanf_mci ='meanf.mci'
       character (len=9),  parameter :: yield_mci ='yield.mci'
       character (len=6),  parameter :: ica_mc ='ica.mc' 

	   ! MCMC file, holds the sampled parameters
       character (len=9),  parameter :: stock_pbi ='stock.pbi'			! These hold the percentiles of the
       character (len=12), parameter :: recruits_pbi ='recruits.pbi'	!      distributions
       character (len=9),  parameter :: meanf_pbi ='meanf.pbi'
       character (len=9),  parameter :: yield_pbi ='yield.pbi'

	   ! Data labels (from LABELS.INC)
       character (len=40) :: MainTitle
       character (len=40), dimension (50) :: ASurvLab, BSurvLab
   END MODULE FILES

! Message definitions (from MESSAGE1.INC and MESSAGE2.INC)
MODULE data_definition
    USE DIMS
    USE STCK
    USE CONTROL
    USE STATS
	USE CPUE
    USE RESULTS
    USE FILES

end module data_definition

! *****************************************************************************


! *****************************************************************************

module kind_module

	integer, parameter :: wp = kind(1.0d0) ! kind value for double precision

end module kind_module

! *****************************************************************************


module message_definition

	use kind_module
      
	! Message definitions (from MESSAGE1.INC and MESSAGE2.INC)

        character (len=80), dimension (30,4) :: H  ! messages used by IN.for
        character (len=80), dimension (45,4) :: HK ! messages used by PARMSET.for
        character (len=6), dimension (10,4)  :: KO ! valid character options 
        character (len=2), dimension (10,4)  :: KY ! character options for action
        character (len=80), dimension (43,4) :: HL ! messages used by ICA1 and BY
        character (len=80), dimension (61,4) :: HM ! messages used by ICA2 and SHRINKF
        character (len=80), dimension (79,4) :: HP ! messages used by OUT
        character (len=77), dimension (64,4) :: HW ! messages used by ICP4                                         
        character (len=10), dimension (5,4)  :: KW ! options used by ICP4

        integer :: language

contains

subroutine Text_Strings

! Defines ICA text strings

      H(1,1) = "Enter the name of the index file"
      H(1,2) = 'Teclee el nombre del archivo °ndice'                 
      H(1,3) = 'Frapper le nom du fichier index'                     
      H(1,4) = 'Skriv navnet pÜ indeksfilen'                         
     
      H(2,1)= 'No index file name. Run abandonded'                  
      H(2,2)= 'No se encontr¢ el archivo °ndice.'                   
      H(2,3)= 'Le fichier index n''a pas ÇtÇ trouvÇ.'               
      H(2,4)= 'Indeksfilen ble ikke funnet.'                        
     
      H(3,1)= 'File not found. Try again ?'                         
      H(3,2)= 'No se encontr¢ el archivo. ? Otro intento ?'         
      H(3,3)= 'Le fichier n''a pas ÇtÇ trouvÇ. Un autre essai ?'    
      H(3,4)= 'Filen ble ikke funnet. Et forsîk til ?'              
     
      H(4,1)= 'Landings file format error.'                         
      H(4,2)= 'Error en el formato del archivo de desembarques.'    
      H(4,3)= 'Le format du fichier des captures n''est pas correcte.'                                                             
      H(4,4)= 'Formatet av fangstfilen er feil.'                    
     
      H(5,1)='Too many years in the data. The Maximum is '          
      H(5,2)='Demasiados a§os en los datos. El M ximo es'           
      H(5,3)='Trop d''annÇes dans les donnÇes. Le maximum est'      
      H(5,4)='For mange Ür i data. Maksimum er '                    
     
      H(6,1)='Too many ages in the data. The Maximum is '           
      H(6,2)='Demasiadas edades en los datos. El M ximo es '        
      H(6,3)='Trop d''Éges dans les donnÇes. Le maximum est'        
      H(6,4)='For mange aldersgrupper i fangstdata. Maksimum er '   
     
      H(7,1)='File identifier is wrong :'                           
      H(7,2)='La identificaci¢n del archivo es'                         &                                                          
       //' err¢nea :'                                               
      H(7,3)='L''identification du fichier '//                          &                                                          
       'est incorrecte :'                                           
      H(7,4)='Filen er galt identifisert :'                         
     
      H(8,1)='Error in year range of file : '                       
      H(8,2)='El rango de a§os en el archivo de es err¢neo :'       
      H(8,3)='Le choix dans les annÇes du fichier est incorrecte :' 
      H(8,4)='Valget av Ür i filen er galt :'                       
     
      H(9,1)='Error in age range of file :'                         
      H(9,2)='El rango de edades en el archivo es err¢neo :'        
      H(9,3)='Le choix dans les Éges du fichier est incorrecte.'    
      H(9,4)='Valget av alder i filen er galt.'                     
     
      H(10,1)='File must be matrix format :'                        
      H(10,2)='El archivo ha de estar en'                               &                                                          
      //' formato de matriz :'                                      
      H(10,3)='Le fichier  doit àtre prÇparÇ en format de matrice :'
      H(10,4)='Filen mÜ ha matrise format : '                       
     
      H(11,1)='File must be in scalar format :'                     
      H(11,2)='El archivo debe tener un valor escalar :'            
      H(11,3)='Le fichier doôt avoir une valeur ''scalar'' :'       
      H(11,4)='Filen mÜ ha en ''scalar'' verdi : '                  
     
      H(12,1)=' used for the year '                                 
      H(12,2)=' usados para el a§o '                                
      H(12,3)=' utilisÇs pour l''annÇe '                            
      H(12,4)=' brukes for Üret '                                   
     
      H(13,1)=' Stock weights in '                                  
      H(13,2)=' Pesos en el stock en '                              
      H(13,3)=' Poids dans le stock en '                            
      H(13,4)=' Vekter i bestanden i Üret '                         
     
      H(14,1)=' Natural mortality in '                              
      H(14,2)=' Mortalidad natural en '                             
      H(14,3)=' MortalitÇ naturelle en '                            
      H(14,4)=' Naturligt dîdelighet i Üret'                        
     
      H(15,1)=' Maturity ogive in '                                 
      H(15,2)=' Ogiva de madurez en '                               
      H(15,3)=' MaturitÇs en '                                      
      H(15,4)=' Modning i Üret '                                    
     
      H(16,1) =' read successfully.'                                
      H(16,2) =' ha sido le°do sin errores.'                        
      H(16,3) =' a ÇtÇ lu correctement.'                            
      H(16,4) =' ble lest uten feil.'                               
     
      H(17,1)='Name of the SSB index file (Enter if none)'          
      H(17,2)='Teclee el nombre del archivo de °ndice de biomasa desovante '                                                       
      H(17,3)='Frapper le nom du fichier de biomasse '              
      H(17,4)='Skriv navnet pÜ fil for indeks av gytebestand '      
     
      H(18,1)='No indices of spawning biomass to be used.'          
      H(18,2)='No hay °ndice de biomasa desovante.'                 
      H(18,3)='Aucun fichier de biomasse.'                          
      H(18,4)='Ingen indeks av gytebestand.'                        
     
      H(19,1)='Error in year range for the SSB indices '            
      H(19,2)='Error en el rango de a§os en los °ndices de'             &                                                          
      //' biomasa desovante'                                        
      H(19,3)='Erreur dans le choix dans les annÇes du fichier'          &                                                         
      //' de biomasse '                                             
      H(19,4)='Feil i Ürene i indeks av gytebestand.'               
     
      H(20,1)='Name of age-structured index file (Enter if none) :' 
      H(20,2)='Teclee el nombre del archivo de °ndices estructurado'    &                                                          
      //' por edades '                                              
      H(20,3)='Frapper le nom du fichier des indexes structurÇs'        &                                                          
      //' par ages'                                                 
      H(20,4)='Skriv navnet pÜ fil for alderstrukturerte indekser'  
     
     
      H(21,1)='No age-structured indices to be used.'               
      H(21,2)='No hay °ndices estructurados por edades.'            
      H(21,3)='Aucun indice structurÇ par Ége.'                     
      H(21,4)='Ingen alderstrukturerte indekser.'                   
     
     
      H(22,1)='Too many age-structured indices. No. allowed is: '   
      H(22,2)='Demasiados °ndices estructurados por edad.'                &                                                        
      //' El m ximo permitido es: '                                 
      H(22,3)='Trop d''indices structurÇs par ages. Le maximum est :'                                                              
      H(22,4)='For mange indekser. Maksimum er :'                   
     
      H(23,1)='ERROR: No indices read from file. Check format.'     
      H(23,2)='ERROR: No se ha le°do ning£n °ndice del archivo.'        &                                                          
      //'Revise el formato.'                                        
      H(23,3)='ERREUR: Aucun indice a ÇtÇ lu du fichier. RÇviser le'     &                                                         
      //' format'                                                   
      H(23,4)='FEIL: Ingen indeks ble lest fra filen.'              
     
     
      H(24,1)='Too many ages in the index file. Index : '           
      H(24,2)='Demasiadas edades en el archivo °ndice en :'         
      H(24,3)='Trop d''Éges dans l''indice :'                       
      H(24,4)='For mange aldersgrupper i indeksfilen for indeks :'  
     
     
      H(25,1)='Too many years in the age index file. Index : '      
      H(25,2)='Demasiados a§os en el archivo °ndice. Indice : '     
      H(25,3)='Trop d''annÇes dans le fichier. Indice : '           
      H(25,4)='For mange Ür i filen. Indeks: '                      
     
      H(26,1)='First year of index is before start of catch-'           &                                                          
      //'at-age matrix :'                                           
      H(26,2)='El °ndice empieza antes del primer a§o en las'           &                                                          
      //' capturas.'                                                
      H(26,3)='L''indice commence avant la premiäre annÇe des donnÇes de captures'                                                 
      H(26,4)='Fîrtse indeks Ür begynner fîr det fîrste Ür i fangst data.'                                                         
     
      H(27,1)='First age of index is younger than first age in '        &                                                          
      //'catch-at-age matrix :'                                     
      H(27,2)='La primera edad es inferior a la'                        &                                                          
      //' primera edad en la matriz de capturas: '                  
      H(27,3)='Le premier Ége est infÇrieure au premier Ége de'   & 
      //' la matrice des captures.'                                 
      H(27,4)='Fîrste alder er mindre enn fîrste alder i'                &                                                         
      //' fansgt matrise'                                           
     
     
      H(28,1) ='1-English, 2-Espa§ol, 3-Franáais, 4-Norsk ?'        
      H(28,2) ='1-English, 2-Espa§ol, 3-Franáais, 4-Norsk ?'        
      H(28,3) ='1-English, 2-Espa§ol, 3-Franáais, 4-Norsk ?'        
      H(28,4) ='1-English, 2-Espa§ol, 3-Franáais, 4-Norsk ?'        
     
     
     
     
      HK(1,1)='No of years for separable constraint ?'              
      HK(1,2)='N£mero de a§os para aplicar el modelo separable ?'   
      HK(1,3)='Nombre d''annÇes pour utiliser le modäle separable ?'
      HK(1,4)='Hvor mange Ür skal brukes i separabel modellen ?'    
     
      HK(2,1)='Reference age for separable constraint ?'            
      HK(2,2)='Edad de referencia para el modelo separable ?'       
      HK(2,3)='Age de reference pour le modäle separable ?'         
      HK(2,4)='Referansealder for den separabel modellen ?'         
     
      HK(3,1)='Constant selection pattern model (Y/N) ?'            
      HK(3,2)='Modelo de selecci¢n constante (S/N) ?'               
      HK(3,3)='Modäle de selection constante (O/N) ?'               
      HK(3,4)='Konstant fiskemînster- modell (J/N) ?'               
     
      KO(1,1)='yYnN'                                                
      KO(1,2)='sSnN'                                                
      KO(1,3)='oOnN'                                                
      KO(1,4)='jJnN'                                                
     
      Ky(1,1) = 'yY'                                                
      Ky(1,2) = 'sS'                                                
      Ky(1,3) = 'oO'                                                
      Ky(1,4) = 'jJ'                                                
     
      HK(4,1)='Enter last year in which selection is constant '     
      HK(4,2)='Teclee el £ltimo a§o en que el patr¢n de selecci¢n es'   &                                                          
      //' constante'                                                
      HK(4,3)='Frapper la derniäre annÇe dans laquelle la selection est constante'                                                 
      HK(4,4)='Siste Ür for konstant fiskemînster '                 
     
      HK(5,1) = 'Gradual or Abrupt change in selection (G/A) ? '    
      HK(5,2) = 'Cambio en selecci¢n Brusco o Lento (B/L) ? '       
      HK(5,3) = 'Changement de selection Abrupte ou Lent (A/L) ? '  
      HK(5,4) = 'Forandringen i fiskemînster er Plutselig eller '     &                                                            
      //'Gradvis (P/G) ?'                                           
     
      KO(2,1) = 'gGaA'                                              
      KO(2,2) = 'bBlL'                                              
      KO(2,3) = 'aAlL'                                              
      KO(2,4) = 'pPgG'                                              
     
      KY(2,1) = 'aA'                                                
      KY(2,2) = 'Bb'                                                
      KY(2,3) = 'aA'                                                
      KY(2,4) = 'pP'                                                
     
      HK(6,1)='S to be fixed on last age ?'                         
      HK(6,2)='Valor de selecci¢n fijado a la £ltima edad ? '       
      HK(6,3)='Valeur de selection a fixer pour le dernier age ?'   
      HK(6,4)='Fiskemînster verdien for den siste alderen ?'        
     
      HK(7,1)='S for last age in later selection pattern ?'         
      HK(7,2)='Valor de selecci¢n terminal para el segundo patr¢n'      &                                                          
       // ' de selecci¢n ?'                                         
      HK(7,3)='Valeur de selection terminale pour la deuxiäme periode'                                                             
      HK(7,4)='Fiskemînster verdien for den siste alderen i den '      &                                                           
      //' andre perioden'                                           
     
      HK(8,1)='First age for calculation of reference F ?'          
      HK(8,2)='Primera edad para el c lculo de la F de referencia ?'
      HK(8,3)='Premiàre age pour le calcul de la F de reference ?'  
      HK(8,4)='Fîrste alder for beregning av referanse '             &                                                             
      //' F-verdien ?'                                              
     
      HK(9,1)='Last age for calculation of reference F ?'           
      HK(9,2)='Ultima edad para el c lculo de la F de referencia ?' 
      HK(9,3)='Derniäre age pour le calcul de la F de reference ?'  
      HK(9,4)='Siste alder for beregningen av referanse '              &                                                           
      //' F-verdien ?'                                              
     
      HK(10,1)='First is older than last ! Try again. '             
      HK(10,2)='La primera es mayor que la £ltima. Intente otra vez.'                                                              
      HK(10,3)='La premiäre est plus ÉgÇe que la derniäre.'             &                                                          
      // ' Essayez de nouveau.'                                     
      HK(10,4)='Den siste alderen er mindre enn den gamle.'        &
      //' Prîv igjen.'                                              
     
      HK(11,1) ='Use default weighting (Y/N) ?'                     
      HK(11,2) ='Usar la ponderaci¢n est†ndar (S/N) ?'              
      HK(11,3) ='Utiliser les valeurs de LAMBDA recommendÇes (O/N) ?'                                                              
      HK(11,4) ='Bruker normalvekting (J/N) ?'                      
     
      HK(12,1)='Enter relative weights at age '                     
      HK(12,2)='Teclee la ponderaci¢n deseada para cada edad '      
      HK(12,3)='Frapper la valeur de LAMBDA desirÇe pour chaque age '                                                              
      HK(12,4)='Skriv verdiene for vekting i hver alder'            
     
      HK(13,1)='Weight for age'                                     
      HK(13,2)='Ponderaci¢n a la edad '                             
      HK(13,3)='LAMBDA pour l''age '                                
      HK(13,4)='Ny verdi for vekting for alderen '                  
     
      HK(14,1)='Enter relative weights by year '                    
      HK(14,2)='Teclee ponderaci¢n deseada por a§o  '               
      HK(14,3)='Frapper les valeur de LAMBDA pour chaque annÇe '    
      HK(14,4)='Skriv verdiene for vekting for Ür'                  
     
      HK(15,1)='Weight for year'                                    
      HK(15,2)='Ponderaci¢n para el a§o'                            
      HK(15,3)='LAMBDA pour l''annÇe'                               
      HK(15,4)='Ny verdi for vekting for Üret'                      
     
      HK(16,1)='Enter new weights for specified years and ages'         &                                                          
      //' if needed'                                                
      HK(16,2)='Teclee nueva ponderaci¢n para ciertos a§os'        &                                                          
      //' o edades'                                                  
      HK(16,3)='Frapper des nouvelles valeurs de LAMBDA pour'           &                                                          
      //' certaines ages ou annÇes'                                 
      HK(16,4)='Skriv nye verdier av vekting for hver Ür og alder'  
     
      HK(17,1)='Enter year, age, new weight or -1,-1,-1 to end.'    
      HK(17,2)='Teclee a§o, edad, valor nuevo o -1,-1,-1 '        &                                                          
      //'para acabar'                                               
      HK(17,3)='Frapper l''annÇe, l''Ége, LAMBDA ou -1,-1,-1 pour finir.'                                                          
      HK(17,4)='Skriv Ür, alder, vekting eller -1,-1,-1 til ende.'  
     
      HK(18,1)='Is the last age of '                                
      HK(18,2)='Es la £ltima edad de '                              
      HK(18,3)='Est-ce-que le dernier Ége de '                      
      HK(18,4)='Er det siste Üret av '                              
     
      HK(19,1)='a plus-group (Y/N) ? '                              
      HK(19,2)='una clase acumulada ? '                             
      HK(19,3)='est une classe accumulÇe (O/N) ?'                   
      HK(19,4)='en kumulativ klasse (J/N) ?'                        
     
      HK(20,1)='You must choose a catchability model for each index.'                                                              
      HK(20,2)='Vd. debe elegir un modelo de capturabilidad para cada'   &                                                         
      //' °ndice.'                                                  
      HK(20,3)='Vous devez choisir un modäle de capturabilitÇ pour '    &                                                          
      //' chaque indice d''abondance.'                              
      HK(20,4)='Du mÜ velge en fangstmodell for hver mengdeindeks ' 
     
      HK(21,1)='Models:   A  Absolute:  Index = Abundance . e'      
      HK(21,2)='Modelos:  A  Absoluto:  Indice = Abundancia . e'    
      HK(21,3)='Modäles:  A  Absolut:   Indice = Abondance . e'     
      HK(21,4)='Modeller: A  Absolutt:  Indeks = Mengde . e'        
     
      HK(22,1)='          L  Linear:    Index = Q. Abundance . e '  
      HK(22,2)='          L  Lineal:    Indice = Q. Abundancia . e' 
      HK(22,3)='          L  Lineaire:  Indice = Q. Abondance . e'  
      HK(22,4)='          L  Lineër:    Indeks = Q. Mengde . e'     
     
      HK(23,1)='          P  Power:     Index = Q. Abundance^ K .e' 
      HK(23,2)='          P  exPonente: Indice = Q. Abundancia^ K .e'                                                              
      HK(23,3)='          P  exPonent:  Indice = Q. Abondance^K . e'
      HK(23,4)='          P  eksPonent: Indeks = Q. Mengde^K . e'   
     
      HK(24,1)='   where Q and K are parameters to be estimated, and'                                                              
      HK(24,2)='   d¢nde Q y K son par metros a estimar, y'         
      HK(24,3)='   ou Q et K sont des paramätres a estimer, et'     
      HK(24,4)='   hvor Q og K er parametre Ü beregne, og'          
     
      HK(25,1)='   e is a lognormally-distributed error.'           
      HK(25,2)='   e es error con distribuci¢n lognormal.'          
      HK(25,3)='   e est une erreur avec distribution log-normale.' 
      HK(25,4)='   e er error med lognormalfordeling.'              
     
     
      HK(26,1)='Model for '                                         
      HK(26,2)='Modelo para '                                       
      HK(26,3)='Le modäle pour '                                    
      HK(26,4)='Modell for '                                        
     
      HK(27,1)=' is to be A/L/P ?'                                  
      HK(27,2)=' a ser A/L/P ?'                                     
      HK(27,3)=' doit àtre A/L/P ?'                                 
      HK(27,4)=' mÜ vëre A/L/P ?'                                   
     
      HK(28,1)='There are '                                         
      HK(28,2)='Faltan '                                            
      HK(28,3)='Il manque '                                         
      HK(28,4)='Er '                                                
     
      HK(29,1)=' missing observations for fitting the separable'        &                                                          
      //' model.'                                                   
      HK(29,2)=' observaciones para el ajuste del modelo separable.'
      HK(29,3)=' observations pour ajuster le modäle separable.'    
      HK(29,4)=' fravërende data Ü tilpasse separabel modellen.'    
     
      HK(30,1)='MODEL OVERPARAMETERISED'                            
      HK(30,2)='INSUFFICIENTES DATOS PARA EL MODELO ELEGIDO'        
      HK(30,3)='TROP DE PARAMETRES POUR LES DONNEES DISPONIBLES'    
      HK(30,4)='IKKE NOK DATA FOR MODELLEN'                         
     
      HK(31,1)='Too few data in fitting the index : '               
      HK(31,2)='En el ajuste del °ndice : '                         
      HK(31,3)='Dans l''indice d''abondance : '                     
      HK(31,4)='I mengdeindeksen : '                                
     
      HK(42,1)=' at age '                                           
      HK(42,2)=' a la edad '                                        
      HK(42,3)=' a l''age '                                         
      HK(42,4)=' for alder '                                        
     
     
      HK(32,1)='Fit a stock-recruit relationship (Y/N) ?'           
      HK(32,2)='Inclu°r una relaci¢n stock-recluta (S/N) ?'         
      HK(32,3)='Ajouter une relation stock-recrutement (O/N) ?'     
      HK(32,4)='Tilpass en bestands-rekruttering funksjon (J/N) ?'  
     
      HK(33,1)='Enter the time lag in years between spawning'           &                                                          
      //' and the stock size'                                       
      HK(33,2)='Teclee el lapso de tiempo en a§os entre el'             &                                                          
      //' desove y el reclutamiento'                                
      HK(33,3)='Frapper la periode de temps entre la ponte et le'       &                                                          
      //' recrutement'                                              
      HK(33,4)='Tidsavnittet (i Ür) mellom gyting og rekruttering'  
     
      HK(34,1)=' of fish aged '                                     
      HK(34,2)=' a la edad '                                        
      HK(34,3)=' a l''Ége '                                         
      HK(34,4)=' som er '                                           
     
      HK(35,1)=' years on 1 January. '                              
      HK(35,2)=' a§os en el d°a 1 enero. '                          
      HK(35,3)=' ans le 1 Janvier.'                                 
      HK(35,4)=' Ür gammel den 1 januar.'                           
     
      HK(36,1)='This will probably be '                             
      HK(36,2)='Esto podr°a ser '                                   
      HK(36,3)='Ceci pourrait àtre '                                
      HK(36,4)='De kan vëre '                                       
     
      HK(37,1)='unless the stock is an autumn-spawning herring'     
      HK(37,2)='salvo si el recurso es un arenque que desove en oto§o'                                                             
      HK(37,3)='sauf s''il s''agit d''un hareng qui pond en automne.'                                                              
      HK(37,4)='unntatt som er hîstgytende sild'                    
     
      HK(38,1)=' in which case it will probably be '                
      HK(38,2)=' en cual caso probablemente ser  '                  
      HK(38,3)=' Dans ce cas ce sera probablement '                 
      HK(38,4)=' I sÜ fall kan det vëre '                           
     
      HK(43,1) = ' years.'                                          
      HK(43,2) = ' a§os.'                                           
      HK(43,3) = ' annÇes.'                                         
      HK(43,4) = ' Ür. '                                            
     
     
      HK(39,1)='Enter the lag in years (rounded up) '               
      HK(39,2)='Teclee el lapso en a§os completos '                 
      HK(39,3)='Frapper la periode en annÇes entiäres '             
      HK(39,4)='Skriv tidsavnittet i hele Ür '                      
     
      HK(40,1)='File not found: '                                   
      HK(40,2)='No se encontr¢ el archivo: '                        
      HK(40,3)='Le fichier n''a pas ÇtÇ trouvÇ :'                   
      HK(40,4)='Filen ble ikke funnet :'                            
     
      HK(41,1)='Output page width in characters (e.g. 80..132) ?'   
      HK(41,2)='Ancho de la p†gina de salida en c†racteres'             &                                                          
      //' (e.g. 80..132) ?'                                         
      HK(41,3)='Largeur de la page du fichier de resultats '            &                                                          
       //' (numero de lettres, e.g. 80..132)'                       
      HK(41,4)='Utskrift sidens bredde (tegn e.g. 80..132) ?'       
     
      HL(1,1)='You are trying to estimate too many parameters.'     
      HL(1,2)='Ud. est  intentando estimar demasiado p†rametros.'   
      HL(1,3)='Vous essayez d''estimer trop de paramätres.'         
      HL(1,4)='Du forsîker Ü beregne for mange parametre.'          
     
      HL(2,1)='Only '                                               
      HL(2,2)='Solamente '                                          
      HL(2,3)='Seulement '                                          
      HL(2,4)='Bare '                                               
     
      HL(3,1)=' allowed in this implementation'                     
      HL(3,2)=' permitidos en Çsta implementaci¢n.'                 
      HL(3,3)=' permis dans cette version.'                         
      HL(3,4)=' tillatt i denne versjon.'                           
     
      HL(4,1)=' Fitting the model specified requires '              
      HL(4,2)=' El model espec°ficado requiere '                    
      HL(4,3)=' Le modäle demandÇ contient '                        
      HL(4,4)=' Modellen din har '                                  
     
      HL(5,1)=' parameters.'                                        
      HL(5,2)=' par metros.'                                        
      HL(5,3)=' paramätres.'                                        
      HL(5,4)=' parametre.'                                         
     
      HL(6,1)= 'Enter lowest feasible F '                           
      HL(6,2)= 'Teclee la F mas baja permitida '                    
      HL(6,3)= 'Frapper la limite inferieure de F '                 
      HL(6,4)= 'Skriv den minste tillatte F-verdien'                
     
      HL(7,1)= 'Enter highest feasible F '                          
      HL(7,2)= 'Teclee la F mas alta permitida '                    
      HL(7,3)= 'Frapper la limite superieure de F '                 
      HL(7,4)= 'Skriv den maksimale tillatte F-verdien'             
     
      HL(10,1)='ERROR: Lowest > Highest. Retry. '                   
      HL(10,2)='ERROR: valor mas bajo > valor mas alto.'            
      HL(10,3)='ERREUR: limite inferieure > limite superieure.'     
      HL(10,4)='FEIL: den minste > den stîrste verdien.'            
     
      HL(11,1)='Mapping the F-dimension of the SSQ surface'         
      HL(11,2)='Evaluando valores de residuos (SSQ) en la'              &                                                          
      //' dimensi¢n de F.'                                          
      HL(11,3)='Calcul de valeurs de SSQ dans la dimension F:'      
      HL(11,4)='Beregning av SSQ-verdier i F-dimensjonen.'          
     
      HL(12,1)='Lowest SSQ is for F = '                             
      HL(12,2)='El valor m°nimo de SSQ fuÇ encontrado a F='         
      HL(12,3)='La valeur minimum de SSQ trouvÇe a F ='             
      HL(12,4)='Den minste SSQ ble funnet for F  = '                
     
      HL(13,1)='Iterating for SSQ minimum in the F-dimension.'      
      HL(13,2)='Buscando m°nimo con mas iteraciones.'               
      HL(13,3)='AmÇliorant l''estimation du minimum.'               
      HL(13,4)='Bedre verdi av minimum blir sîkt med flere iterasjoner.'                                                           
     
      HL(14,1)='Failure attempting to fit stock-recruit relation.'  
      HL(14,2)='No se pudo ajustar la relaci¢n stock-recluta.'      
      HL(14,3)='L''ajustement a la relation stock-recrutement n''as pas'&                                                          
      //' ÇtÇ possible.'                                            
      HL(14,4)='Bestands-rekrutterings funksjonen kunne ikke beregnes.'                                                            
     
      HL(15,1)='Too many observations for this implementation.'     
      HL(15,2)='Demasiadas observaciones para Çsta versi¢n.'        
      HL(15,3)='Trop de donnÇes pour cette version.'                
      HL(15,4)='For mange data for denne versjonen.'                
     
      HL(16,1)='The maximum allowed is : '                          
      HL(16,2)='El m ximo permitido es : '                          
      HL(16,3)='Le maximum permis est : '                           
      HL(16,4)='Maksimumen tillatt er : '                           
     
      HL(17,1)=' and there are '                                    
      HL(17,2)=' y hay '                                            
      HL(17,3)=' et il y a '                                        
      HL(17,4)=' og er '                                            
     
      HL(18,1)=' in this data set. '                                
      HL(18,2)=' en Çste an lisis. '                                
      HL(18,3)=' dans cette analyse.'                               
      HL(18,4)=' i denne beregning.'                                
     
      HL(19,1)='Starting SSQ : '                                    
      HL(19,2)='SSQ al comienzo : '                                 
      HL(19,3)='SSQ au commencement : '                             
      HL(19,4)='SSQ i begynnelsen : '                               
     
      HL(21,1)='No of years for separable analysis :'               
      HL(21,2)='N£mero de a§os para el an†lisis separable :'        
      HL(21,3)='Nombre d''annÇes pour l''analyse sÇparable :'       
      HL(21,4)='èrene Ü bruke i den separable modellen :'           
     
      HL(22,1)='Age range in the analysis : '                       
      HL(22,2)='Rango de edades en el an†lisis : '                  
      HL(22,3)='Ages inclus dans l''analyse : '                     
      HL(22,4)='Aldersgrupper Ü inkludere i analysen :'             
     
      HL(23,1)='Year range in the analysis :'                       
      HL(23,2)='Rango de a§os en el an†lisis :'                     
      HL(23,3)='AnnÇes incluses dans l''analyse :'                  
      HL(23,4)='èrene Ü inkludere i analysen :'                     
     
      HL(24,1)='Number of indices of SSB : '                        
      HL(24,2)='N£mero de °ndices de biomasa desovante :'           
      HL(24,3)='Numero des indices de biomasse en ponte :'          
      HL(24,4)='Indekser av gytebestand :'                          
     
      HL(25,1)='Number of age-structured indices :'                 
      HL(25,2)='N£mero de °ndices estructurados por edades :'       
      HL(25,3)='Nombre d''indices d''abondance structures par Éges :'                                                              
      HL(25,4)='Alderstrukturerte indekser : '                      
     
      HL(26,1)='Stock-recruit relationship to be fitted.'           
      HL(26,2)='Una relaci¢n stock-recluta ser  inclu°da.'          
      HL(26,3)='Une relation stock-recrutement sera incluse.'       
      HL(26,4)='Bestands-rekrutterings funksjonen skal inkluderes.' 
     
      HL(27,1)='Parameters to estimate :  '                         
      HL(27,2)='Par metros a estimar : '                            
      HL(27,3)='Paramätres Ö estimer : '                            
      HL(27,4)='Parametre Ü beregne : '                             
     
      HL(28,1)='Number of observations : '                          
      HL(28,2)='N£mero de observaciones : '                         
      HL(28,3)='Numero de donnÇes : '                               
      HL(28,4)='Antall observasjoner : '                            
     
      HL(29,1)='Two selection vectors to be fitted. '               
      HL(29,2)='Dos vectores de selecci¢n en el modelo.'            
      HL(29,3)='Modäle avec deux vecteurs de sÇlection.'            
      HL(29,4)='Modellen har to fiskemînster-vektorer.'             
     
      HL(30,1)='Abrupt change in selection specified.'              
      HL(30,2)='Cambio brusco en selecci¢n elegido.'                
      HL(30,3)='Changement abrupte de sÇlection a ÇtÇ choisi.'      
      HL(30,4)='Modellen har plutselig forandring i fiskemînster.'  
     
      HL(31,1)='Gradual change in selection specified. '            
      HL(31,2)='Cambio gradual en selecci¢n elegido.'               
      HL(31,3)='Changement graduel de sÇlection a ÇtÇ choisi.'      
      HL(31,4)='Modellen har gradvis forandring i fiskemînster.'    
     
      HL(32,1)='Selection assumed constant up to and including :'   
      HL(32,2)='Selecci¢n constante hasta (e incluyendo) : '        
      HL(32,3)='SÇlection constante jusqu''Ö (inclusif) : '         
      HL(32,4)='Konstant fiskerimînster til (og inkluderer) : '     
     
      HL(33,1)='Conventional single selection vector model to'          &                                                          
      //' be fitted.'                                               
      HL(33,2)='Modelo de vector de selecci¢n £nico.'               
      HL(33,3)='Un seul vecteur de sÇlection inclus.'               
      HL(33,4)='Modellen har en fiskemînster-vektor.'               
     
      HL(34,1)='WARNING: Stock and recruit estimates treated'           &                                                          
      //' as data.'                                                 
      HL(34,3)='ATT!: Le modäle utilise les estimations de stocks'      &                                                          
      //' et de recrutement comme donnÇes.'                         
     
      HL(34,2)='ADV!: Se utilisa estimaciones de'                       &                                                          
      //'stock y reclutas c¢mo si fueran datos.'                    
      HL(34,4)='OBS!: Modellen bruker bestandene og rekrutteringene'    &                                                          
      //' som data.'                                                
     
     
     
      HL(35,1)='Estimate historical assessment uncertainty ?'       
      HL(35,2)='Calcular incertidumbre en el ajuste ?'              
      HL(35,3)='Calculer les pourcentages de probabilitÇs ?'        
      HL(35,4)='Beregne varianser i bestanden tilbake i tid? '      
     
      HL(36,1)='How many percentiles to calculate ? '               
      HL(36,2)='Cu†ntos porcentiles desea c†lcular ? '              
      HL(36,3)='Combien de pourcentages faut-il calculer ? '        
      HL(36,4)='Hvor mange ''percentiles'' skal beregnes ?'         
     
      HL(37,1)='Enter a percentile '                                
      HL(37,2)='Teclee un porcentaje'                               
      HL(37,3)='Frappez un pourcentage'                             
      HL(37,4)='Skriv en prosent '                                  
     
      HL(38,1)='How many samples to take  ?'                        
      HL(38,2)='Cu†ntas muestras ?'                                 
      HL(38,3)='Combien d''Çchantillons ?'                          
      HL(38,4)='Hvor mange iterasjoner ?'                           
     
      HL(39,1)='Enter the required length of the Markov Chain'      
      HL(39,2)='Cu†ntos pasos en la cadena Markov ?'                
      HL(39,3)='Combien d''itÇrations de la chaône Markov ?'        
      HL(39,4)='Hvor mange iterasjoner i Markov kjeden ?'           
     
      HL(40,1)='Thin the chain by taking one sample out of '        
      HL(40,2)='Tomar una muestra de la cadena de cada '            
      HL(40,3)='Utiliser une iteration de chaque '                  
      HL(40,4)='Bruker en iterasjon fra hver '                      
     
      HL(41,1)='Burn-in period '                                    
      HL(41,2)='Periodo para descartar al inicio '                  
      HL(41,3)='Combien  d''iterations initiales Ö rejetter ?'      
      HL(41,4)='Hvor mange iterasjoner skal bli avslÜtt i begynnelsen ?'                                                           
     
      HL(42,1)='Sample from Covariances or Bayes MCMC (C/B) ?'      
      HL(42,2)='Muestrear Covarianzas o Bayes MCMC (C/B) ? '        
      HL(42,3)='Echantilloner Covariances ou Bayes MCMC (C/B) ?'    
      HL(42,4)='Bruker Covarianser eller Bayes MCMC (C/B) ?'        
     
      HL(43,1)='Succesful exit from ICA '                           
      HL(43,2)='C lculo completado sin error'                       
      HL(43,3)='TerminÇ sans erreur'                                
      HL(43,4)='Normal slutt av ICA'                                
     
     
      HM(1,1)='Insufficient memory available to programme.'         
      HM(1,2)='Insuficiente memoria disponible.'                    
      HM(1,3)='La mÇmoire disponible est insuffisante.'             
      HM(1,4)='Ikke nok lager for modellen.'                        
     
      HM(2,1)='Survey weighting to be Manual (recommended)'             &                                                          
      // ' or Iterative (M/I) ?'                                    
      HM(2,2)='Ponderaci¢n de los °ndices  Manual '                     &                                                          
      //' (recomendado) o Iterativo (M/I) ?'                        
      HM(2,3)='Specification des valeurs de LAMBDA Ö Main '             &                                                          
      //' (recommendÇ) ou Iteratif (M/I) ? '                        
      HM(2,4)='Vekting mÜ vëre Manuell (anbefalt)'            &     
      //' eller Iterativ (M/I)?'                                    
     
      KO(3,1)='MmIi'                                                
      KO(3,2)='MmIi'                                                
      KO(3,3)='MmIi'                                                
      KO(3,4)='MmIi'                                                
     
      KY(3,1)='Mm'                                                  
      KY(3,2)='Mm'                                                  
      KY(3,3)='Mm'                                                  
      KY(3,4)='Mm'                                                  
     
      HM(3,1)='Enter weight for '                                   
      HM(3,2)='Teclee la ponderaci¢n para '                         
      HM(3,3)='Frapper LAMBDA pour '                                
      HM(3,4)='Skriv vekting for '                                  
     
      HM(4,1)=' at age '                                            
      HM(4,2)=' a la edad '                                         
      HM(4,3)=' Ö l''Ége '                                          
      HM(4,4)=' i alder '                                           
     
      HM(5,1)='Enter weight for stock-recruit model '               
      HM(5,2)='Teclee ponderaci¢n para el model stock-recluta '     
      HM(5,3)='Frapper LAMBDA pour le modäle stock-recrutement'     
      HM(5,4)='Skriv vekting for bestands-rekrutterings modellen'   
     
      HM(6,1)='Enter estimates of the extent to which errors'       
      HM(7,1)='in the age-structured indices are correlated'        
      HM(8,1)='across ages. This can be in the range 0 (independence)'                                                             
      HM(9,1)='to 1 (correlated errors).'                           
     
      HM(6,2)='Teclee su estimado de la correlaci¢n de los'         
      HM(7,2)='errores en los °ndices de abundancia tras las'       
      HM(8,2)='edades. Esto puede ser en el rango 0'                    &                                                          
      //' (errores independentes)'                                  
      HM(9,2)='hasta 1 (errores correlacionados)'                   
     
      HM(6,3)='Frappez votre estimation de la correlation entre'    
      HM(7,3)='les erreurs dans les indices d''abondance par Éges.' 
      HM(8,3)='Cette valeur peut àtre entre 0'                          &                                                          
      //' (erreurs indÇpendentes)'                                  
      HM(9,3)='et 1 (erreures correlationnÇes entre les ages)'      
     
      HM(6,4)='Skriv verdi for korrelasjon av feil i'               
      HM(7,4)=' alderstrukturerte mengdeindekser. Denne verdien'    
      HM(8,4)=' kan vëre mellom 0 (uavhengige feil) og 1 (feil '    
      HM(9,4)=' med korrelasjon mellom aldersgrupper)'              
     
      HM(10,1)=' Enter value for '                                  
      HM(10,2)=' Teclee valor para '                                
      HM(10,3)=' Frapper la valeur pour '                           
      HM(10,4)=' Skriv verdien for  '                               
     
      HM(11,1)='Do you want to shrink the final fishing mortality (Y/N) ?'                                                         
      HM(11,2)='Desea Ud. aplicar ''shrinkage'' a las F  '     &    
      // ' terminales (S/N) ? '                                     
      HM(11,3)='Desirez-vous appliquer ''shrinkage'' aux F '   &    
      // ' terminales (O/N) ? '                                     
      HM(11,4)='Bruker ''shrinkage'' i den siste F-verdien'    &    
      // ' (J/N) ? '                                                
     
     
      HM(12,1)='Enter the no. of years to shrink to '               
      HM(12,2)='Teclee el n£mero de a§os para el ''shrinkage'' '    
      HM(12,3)='Frapper le numero d''annÇes pour le ''shrinkage'' ' 
      HM(12,4)='Skriv antall Ür for ''shrinkage'' '                 
     
      HM(13,1)='Enter the minimum c.v. of the mean '                
      HM(13,2)='Teclee el c.v. m°nimo del promedio '                
      HM(13,3)='Frapper le coÇfficient de variation '                   &                                                          
      // ' minimum de la moyenne'                                   
      HM(13,4)='Skriv minimum C.V. av middels-F verdier'            
     
      HM(14,1)='The analysis will be repeated with recalculated'        &                                                          
      //' weights until reference F'                                
      HM(15,1)='changes by a proportion less than a specified'          &                                                          
      //' tolerance.'                                               
     
      HM(14,2)='El an†lisis ser† repetido con ponderaciones'            &                                                          
      //' recalculadas hasta que la F'                              
      HM(15,2)='de referencia cambie menos que una tolerancia '         &                                                          
      //'elegida.'                                                  
     
      HM(14,3)='L''analyse sera rÇpÇtÇe jusqu''a ce que la F de'        &                                                          
      // 'rÇfÇrence change moins qu''une'                           
      HM(15,3)='tolerance choisie.'                                 
     
      HM(14,4)='Analysen skal gjentas med ny vekting til '      &   
      //' referanse-F'                                              
      HM(15,4)=' bli endret mindre enn toleranse-verdien.'          
     
      HM(16,1)='Enter your choice for i.v. reweighting tolerance '  
      HM(16,2)='Elige tolerancia en F para terminar las iteraciones  '                                                             
      HM(16,3)='Choissisez la valeur de la tolÇrance pour terminer les itÇrations'                                                 
      HM(16,4)='Skriv den înskede toleranse-verdien for F '         
     
      HM(17,1)='Seeking solution. Please wait.'                     
      HM(17,2)='Buscando soluci¢n. Espere por favor.'               
      HM(17,3)='Attendez SVP.'                                      
      HM(17,4)='Leter etter lîsning. Vër  snill Ü vente.'           
     
      HM(18,1)='F in '                                              
      HM(18,2)='F en '                                              
      HM(18,3)='F en '                                              
      HM(18,4)='F i Ür '                                            
     
      HM(19,1)= ' at age '                                          
      HM(19,2)= ' a la edad '                                       
      HM(19,3)= ' Ö l''Ége '                                        
      HM(19,4)= ' og i alder '                                      
     
      HM(20,1)=' is '                                               
      HM(20,2)=' es '                                               
      HM(20,3)=' est '                                              
      HM(20,4)=' er '                                               
     
      HM(21,1)= ' in iteration '                                    
      HM(21,2)= ' en la iteraci¢n '                                 
      HM(21,3)= '   l''itÇration '                                  
      HM(21,4)= ' i iterasjon '                                     
     
      HM(22,1)=' Memory deallocation failure '                      
      HM(22,2)=' Error en deallocaci¢n de memoria '                 
      HM(22,3)=' Erreur en la dÇallocation de la mÇmoire '          
      HM(22,4)=' Feil i bruk av lager '                             
     
      HM(23,1)='Aged index weights '                                
      HM(23,2)='Ponderaci¢n para °ndices por edades '               
      HM(23,3)='Valeurs de LAMBDA par indices d''Éges '             
      HM(23,4)='Vekting for alderstrukturerte indekser '            
     
      HM(24,1)='Age   : '                                           
      HM(24,2)='Edad  : '                                           
      HM(24,3)='Age   : '                                           
      HM(24,4)='Alder : '                                           
     
      HM(25,1)='Wts : '                                             
      HM(25,2)='Pn  : '                                             
      HM(25,3)='Lmd : '                                             
      HM(25,4)='Lmd : '                                             
     
      HM(26,1)='Stock-recruit weight '                              
      HM(26,2)='Ponderaci¢n stock-recluta '                         
      HM(26,3)='LAMBDA pour stock-recrutement '                     
      HM(26,4)='Vekting for bestand-rekruttering '                  
     
      HM(27,1)=' E04FDF : No error reported. '                      
      HM(27,2)=' E04FDF : Sin error. '                              
      HM(27,3)=' E04FDF : Sans erreur.'                             
      HM(27,4)=' E04FDF : Uten feil.'                               
     
      HM(28,1)= ' E04FDF : Minimisation unsuccessful.'              
      HM(28,2)= ' E04FDF : No se pudo hacer el ajuste. '            
      HM(28,3)= ' E04FDF : La minimisation n''as pas ÇtÇ possible.' 
      HM(28,4)= ' E04FDF : Minimum ble ikke funnet.'                
     
      HM(29,1)=' E04YCF : No error reported. '                      
      HM(29,2)=' E04YCF : Sin error. '                              
      HM(29,3)=' E04YCF : Sans erreur.'                             
      HM(29,4)=' E04YCF : Uten feil.'                               
     
      HM(30,1)=' E04YCF : Approximate variance calculation only.'   
      HM(30,2)=' E04YCF : El c†lculo de varianzas es una'               &                                                          
      //' aproximaci¢n.'                                            
      HM(30,3)=' E04YCF : Le calcul des variances est une'              &                                                          
      //' approximation.'                                           
      HM(30,4)=' E04YCF : Beregningen av variansene er omtrentlig.' 
     
      HM(31,1)=' E04YCF : Error in variance calculation.'           
      HM(31,2)=' E04YCF : Error en el c†lculo de varianzas. '       
      HM(31,3)=' E04YCF : Erreur dans le calcul des variances.'     
      HM(31,4)=' E04YCF : Feil i beregningen av variansene.'        
     
      HM(32,1)='ANALYSIS OF VARIANCE'                               
      HM(32,2)='ANALISIS DE VARIANZA'                               
      HM(32,3)='ANALYSE DE VARIANCE'                                
      HM(32,4)='ANALYSE AV VARIANSEN'                               
     
      HM(34,1)='Unweighted Statistics '                             
      HM(34,2)='Valores sin ponderaci¢n'                            
      HM(34,3)='Valeurs brutes'                                     
      HM(34,4)='Uvektede Statistikker'                              
     
      HM(35,1)='Weighted Statistics'                                
      HM(35,2)='Valores ponderados'                                 
      HM(35,3)='Valeurs ajustÇes'                                   
      HM(35,4)='Vektede Statistikker'                               
     
      HM(36,1)='SSQ'                                                
      HM(36,2)='SSQ'                                                
      HM(36,3)='SSQ'                                                
      HM(36,4)='SSQ'                                                
     
      HM(37,1)='Data'                                               
      HM(37,2)='Datos'                                              
      HM(37,3)='DonnÇes'                                            
      HM(37,4)='Data'                                               
     
      HM(38,1)='Parameters'                                         
      HM(38,2)='Par†metros'                                         
      HM(38,3)='Paramätres'                                         
      HM(38,4)='Parametre'                                          
     
      HM(39,1)='d.f.'                                               
      HM(39,2)='g.l.'                                               
      HM(39,3)='d.l.'                                               
      HM(39,4)='g.f.'                                               
     
      HM(40,1)='Variance'                                           
      HM(40,2)='Varianza'                                           
      HM(40,3)='Variance'                                           
      HM(40,4)='Varians'                                            
     
      HM(41,1)='Total for model'                                    
      HM(41,2)='Total para el modelo'                               
      HM(41,3)='Total pour le modäle'                               
      HM(41,4)='Total modell'                                       
     
      HM(42,1)='Catches at age'                                     
      HM(42,2)='Capturas por edades'                                
      HM(42,3)='Captures par Éges'                                  
      HM(42,3)='Fangster pÜ alder'                                  
     
      HM(43,1)='SSB Indices'                                        
      HM(43,2)='Indices de biomasa desovante'                       
      HM(43,3)='Indices de biomasse'                                
      HM(43,4)='Indekser av gytebestand'                            
     
      HM(44,1)='Aged Indices'                                       
      HM(44,2)='Indices por edades'                                 
      HM(44,3)='Indices par Éges'                                   
      HM(44,4)='Aldersstrukturerte indekser '                       
     
      HM(45,1)='Stock-recruit model '                               
      HM(45,2)='Modelo stock-recluta '                              
      HM(45,3)='Modäle stock-recrutement'                           
      HM(45,4)='Bestands-rekrutterings modell'                      
     
      HM(46,1)='SSB index weights'                                  
      HM(46,2)='Indices de biomasa desovante'                       
      HM(46,3)='Indices de biomasse'                                
      HM(46,4)='Vekting av gytebestandsindeks'                      
     
      HM(47,1)='Program terminated with no errors detected.'        
      HM(47,2)='Calculo terminado sin haber detectado errores.'     
      HM(47,3)='Calcul terminÇ sans erreurs dÇtectÇes.'             
      HM(47,4)='Avslutt uten Ü finne feil.'                         
     
      HM(48,1)='Enter maximum value for any weight'                 
      HM(48,2)='Teclee el valor m†ximo de LAMBDA'                   
      HM(48,3)='Frappez la valeur maximum de LAMBDA'                
      HM(48,4)='Skriv maksimums verdier av vekting'                 
     
      HM(49,1)='Shrinking F to mean ...'                            
      HM(49,2)='Usando ''F shrinkage'' hasta el promedio ...'       
      HM(49,3)=' ''F shrinkage'' Ö la moyenne utilisÇ...'           
      HM(49,4)=' ''F shrinkage'' til middel skal brukes '           
     
      HM(50,1)='Can''t use shrinkage when there are missing values in the catches at age.'                                         
      HM(50,2)='No se puede aplicar ''shrinkage'' cuando faltan valores en las capturas'                                           
      HM(50,3)='Option inadmissible: il manque des valeurs de captures.'                                                           
      HM(50,4)='''Shrinkage'' kan ikke bli brukt hvis du har manglende fangstdata.'                                                
     
      HM(51,1)='Conventional VPA with Fishing Mortality Shrinkage'  
      HM(51,2)='APV tradicional con ''shrinkage'' en Mortalidad por Pesca '                                                        
      HM(51,3)='APV traditionel avec ''shrinkage'' en MortalitÇ par Pàche'                                                         
      HM(51,4)='VPA med ''shrinkage'' i F-verdier'                  
     
      HM(52,1)='Fs shrunk over '                                    
      HM(52,2)='''Shrinkage'' aplicado para '                       
      HM(52,3)='''Shrinkage'' utilisÇ sur '                         
      HM(52,4)='''Shrinkage blir brukt over '                       
     
      HM(53,1)='years'                                              
      HM(53,2)='a§os'                                               
      HM(53,3)='annÇes'                                             
      HM(53,4)='Ür'                                                 
     
      HM(54,1)='Minimum CV of the mean taken as '                   
      HM(54,2)='CV m°nino del promedio es '                         
      HM(54,3)='CV minimum de la moyenne est '                      
      HM(54,4)='Minimum CV av middel er '                           
     
      HM(55,1)='Shrinkage Diagnostics'                              
      HM(55,2)='Diagn¢sticos del ''shrinkage'''                     
      HM(55,3)='Diagnostiques du ''shrinkage'''                     
      HM(55,4)='Opplysninger over ''shrinkage'''                    
     
      HM(56,1)='F from model fit'                                   
      HM(56,2)='F del ajuste'                                       
      HM(56,3)='F du modäle'                                        
      HM(56,4)='F fra modellen'                                     
     
      HM(57,1)='Historic Mean F'                                    
      HM(57,2)='Prom. F Hist¢rico'                                  
      HM(57,3)='Moy. F Historique'                                  
      HM(57,4)='Historiske F-Midd'                                  
     
      HM(58,1)='Shrunk estimate'                                    
      HM(58,2)='Valor encogido'                                     
      HM(58,3)='Valeur ''shr.'''                                    
      HM(58,4)='''Shr.'' verdi'                                     
     
      HM(59,1)='Estimate  Variance'                                 
      HM(59,2)='Estimado  Varianza'                                 
      HM(59,3)='Valeur    Variance'                                 
      HM(59,4)='Verdi     Varians'                                  
     
      HM(60,1)='Wt for F from Model'                                
      HM(60,2)='Import.  del Modelo'                                
      HM(60,3)='Influence du Modäle'                                
      HM(60,4)='Betydning av Modell'                                
     
      HM(61,1)=' Shrunk VPA written to file '                       
      HM(61,2)=' APV con ''shrinkage'' escrito al archivo '         
      HM(61,3)=' APV avec ''shrinkage'' Çcrit au fichier '          
      HM(61,4)=' VPA med ''shrinkage'' ble skrevet til fil '        
     
      HP(1,1)='Detailed, Normal or Summary output (D/N/S) '         
      HP(1,2)='Salida Detallada, Normal o Sumaria (D/N/S) '         
      HP(1,3)='Resultats DÇtaillÇs, Normales ou Sommaires'              &                                                          
      //' (D/N/S)'                                                  
      HP(1,4)='Skal utformat vëre Detaljert, Normal eller Summarisk'       &                                                       
      //' (D/N/S) '                                                 
     
      KO(4,1)='DdNnSs'                                              
      KO(4,2)=KO(4,1)                                               
      KO(4,3)=KO(4,1)                                               
      KO(4,4)=KO(4,1)                                               
     
      HP(2,1)='Output Generated by ICA Version 1.4'                 
      HP(2,2)='Archivo Producido por ICA Versi¢n 1.4'               
      HP(2,3)='Resultats Produits par ICA Version 1.4'              
      HP(2,4)='Filen Skrevet av ICA Versjon 1.4'                    
     
      HP(4,1)='PARAMETERS OF THE DISTRIBUTION OF ln(CATCHES AT AGE)'
      HP(4,2)='PARAMETROS DE LA DISTRIBUCION DE ln(CAPTURAS'            &                                                          
      //' POR EDADES)'                                              
      HP(4,3)='PARAMETRES DE LA DISTRIBUTION DE ln(CAPTURES PAR'        &                                                          
      //' AGES)'                                                    
      HP(4,4)='PARAMETRE AV FORDELING AV Ln(FANGST I NUMMER'        &                                                              
      //' Pè ALDRE)'                                                
     
      HP(5,1)='Separable model fitted from'                         
      HP(5,2)='Model separable usado desde'                         
      HP(5,3)='Modäle separable utilisÇ depuis'                     
      HP(5,4)='Separabel modellen ble tilpasset fra'                
     
      HP(6,1)= ' to '                                               
      HP(6,2)= ' hasta '                                            
      HP(6,3)= ' jusqu''Ö'                                          
      HP(6,4)= ' til '                                              
     
      HP(7,1)=' Variance '                                          
      HP(7,2)=' Varianza '                                          
      HP(7,3)=' Variance '                                          
      HP(7,4)=' Varians '                                           
     
      HP(8,1)='Skewness test stat. '                                
      HP(8,2)='Skewness test stat. '                                
      HP(8,3)='Skewness test stat. '                                
      HP(8,4)='Skjevhet test stat. '                                
     
      HP(9,1)='Kurtosis test statistic '                            
      HP(9,2)='Kurtosis test statistic'                             
      HP(9,3)='Kurtosis test statistic'                             
      HP(9,4)='Kurtose  test stat.'                                 
     
      HP(10,1)='Partial chi-square '                                
      HP(10,2)='Chi-cuadrado parcial '                              
      HP(10,3)='Chi-carrÇ partiel'                                  
      HP(10,4)='Partiell Kji-kvadrat'                               
     
      HP(11,1)='Significance in fit'                                
      HP(11,2)='Significancia del ajuste'                           
      HP(11,3)='Significance du modäle'                             
      HP(11,4)='Signifikans av tilpasning'                          
     
      HP(12,1)='Degrees of freedom'                                 
      HP(12,2)='Grados de libertad'                                 
      HP(12,3)='DegrÇs de libertÇ'                                  
      HP(12,4)='Grader av frihet'                                   
     
      HP(13,1)='PARAMETERS OF DISTRIBUTIONS OF THE SSB INDICES'     
      HP(13,2)='PARAMETROS DE LAS DISTRIBUCIONES DE LOS INDICES DE'     &                                                          
      //' BIOMASA DESOVANTE'                                        
      HP(13,3)='PARAMETRES DES DISTRIBUTIONS DES INDICES DE'            &                                                          
      //' BIOMASSE'                                                 
      HP(13,4)='PARAMETRE AV FORDELINGENE AV INDEKSENE AV'          &                                                              
      //' GYTEBESTAND'                                              
     
      HP(14,1)='DISTRIBUTION STATISTICS FOR'                        
      HP(14,2)='ESTADISTICAS PARA LA DISTRIBUCION DE'               
      HP(14,3)='STATISTIQUES DE LA DISTRIBUCION DE '                
      HP(14,4)='FORDELINGSSTATISTIKK FOR '                          
     
      HP(15,1)='Index used as absolute measure of abundance'        
      HP(15,2)='Indice fuÇ usado como medida absoluta de abundancia'
      HP(15,3)='L''indice a ÇtÇ utilisÇ comme mesure absolue'          &                                                           
      //' d''abondance'                                             
      HP(15,4)='Indeks ble brukte som absolutt mÜl av mengde'       
     
      HP(16,1)='Linear catchability relationship assumed'           
      HP(16,2)='Relaci¢n lineal de capturabilidad usada'            
      HP(16,3)='Relation lineaire de capturabilitÇ utilisÇe'        
      HP(16,4)='Lineër fangstmodell ble brukt'                      
     
      HP(17,1)='Power catchability relationship assumed'            
      HP(17,2)='Relaci¢n exponencial de capturabilidad usada'       
      HP(17,3)='Relation exponentielle de capturabilitÇ utilisÇe'   
      HP(17,4)='Eksponent ble includert i fangstmodellen'           
     
      HP(18,1)='Weight in the analysis'                             
      HP(18,2)='Ponderaci¢n en el an lisis'                         
      HP(18,3)='Valeurs de LAMBDA '                                 
      HP(18,4)='Vekting i analyse'                                  
     
      HP(19,1)='PARAMETERS OF THE DISTRIBUTION'                         &                                                          
      //' OF THE AGE-STRUCTURED INDICES'                            
      HP(19,2)='PARAMETROS DE LAS DISTRIBUCIONES DE LOS INDICES'        &                                                          
      //' ESTRUCTURADOS POR EDADES'                                 
      HP(19,3)='PARAMETRES DES DISTRIBUTIONS DES INDICES '              &                                                          
      //' D''ABONDANCE STRUCTURES PAR AGES'                         
      HP(19,4)='FORDELINGSPARAMETRE TIL DE ALDERSTRUKTURERTE'     & 
      //' INDEKSENE '                                               
     
      HP(20,1)=' Age '                                              
      HP(20,2)=' Edad'                                              
      HP(20,3)=' Age'                                               
      HP(20,4)=' Alder'                                             
     
      HP(21,1)='DATA TRANSFER FILE FOR ICAVIEW'                     
      HP(21,2)='ARCHIVO PARA TRANSFERIR DATOS A ICAVIEW'            
      HP(21,3)='FICHIER POUR PASSER LES DONNEES A ICAVIEW'          
      HP(21,4)='FIL FOR DATA TIL ICAVIEW'                           
     
      HP(22,1)='LANDINGS BY YEAR'                                   
      HP(22,2)='DESEMBARQUES POR A•O'                               
      HP(22,3)='CAPTURES PAR ANNEE'                                 
      HP(22,4)='FANGSTENE VED èR'                                   
     
      HP(23,1)='FISHING MORTALITY +/- STANDARD DEVIATION'           
      HP(23,2)='MORTALIDAD POR PESCA +/- DESVIACION ESTANDAR'       
      HP(23,3)='MORTALITE PAR PECHE +/- DEVIATION STANDARD'         
      HP(23,4)='F-VERDIER +/- SD'                                   
     
      HP(24,1)='RECRUITMENT'                                        
      HP(24,2)='RECLUTAMIENTO'                                      
      HP(24,3)='RECRUTEMENT'                                        
      HP(24,4)='REKRUTTERING'                                       
     
      HP(25,1)='TOTAL AND SPAWNING BIOMASS'                         
      HP(25,2)='BIOMASA TOTAL Y DESOVANTE'                          
      HP(25,3)='BIOMASSE TOTALE ET DE PONTE'                        
      HP(25,4)='GYTE- OG TOTAL BESTAND'                             
     
      HP(26,1)='SELECTION PATTERN +/- STANDARD DEVIATION'           
      HP(26,2)='VALOR DE SELECCION +/- DESVIACION ESTANDAR'         
      HP(26,3)='VALEURS DE SELECTION +/- DEVIATION STANDARD'        
      HP(26,4)='FISKEMôNSTER +/ S.D.'                               
     
      HP(27,1)='SECOND SELECTION PATTERN +/- STANDARD DEVIATION'    
      HP(27,2)='SEGUNDO PATRON DE SELECCION +/- DESVIACION ESTANDAR'
      HP(27,3)='DEUXIEME VALEURS DE SELECTION +/- DEVIATION STANDARD'                                                              
      HP(27,4)='ANDRE FISKEMôNSTER +/ S.D.'                         
     
      HP(28,1)='NUMBERS AT AGE AND YEAR'                            
      HP(28,2)='ABUNDANCIA EN NUMEROS POR EDAD Y A•O'               
      HP(28,3)='ABONDANCE (NUMEROS DE POISSONS) PAR AGE ET ANNEE'   
      HP(28,4)='MENGDE (ANTALL) VED ALDER OG èR'                    
     
      HP(29,1)='FISHING MORTALITY AT AGE AND YEAR'                  
      HP(29,2)='MORTALIDAD POR PESCA POR EDAD Y A•O'                
      HP(29,3)='MORTALITE PAR PECHE PAR AGE ET ANNEE'               
      HP(29,4)='FISKERIDôDELIGHET VED ALDER OG èR'                  
     
      HP(30,1)='NATURAL MORTALITY BY AGE AND YEAR'                  
      HP(30,2)='MORTALIDAD NATURAL POR EDAD Y A•O'                  
      HP(30,3)='MORTALITE NATURELLE PAR AGE ET ANNEE'               
      HP(30,4)='NATURLIGT DôDELIGHET VED ALDER OG èR'               
     
      HP(31,1)='BIOMASS INDEX CATCHABILITIES'                       
      HP(31,2)='CAPTURABILIDADES PARA LOS INDICES DE BIOMASA'       
      HP(31,3)='CAPTURABILITES POUR LES INIDICES DE BIOMASSE'       
      HP(31,4)='BIOMASS INDEX CATCHABILITIES ????' ! ????           
     
      HP(32,1)='AGE-STRUCTURED INDEX CATCHABILITIES'                
      HP(32,2)='CAPTURABILIDADES PARA INDICES ESTRUCTURADOS POR EDAD'                                                              
      HP(32,3)='CAPTURABILITES POUR LES INDICES STRUCTURES PAR AGES'
      HP(32,4)='Q og K-VERDIER FOR ALDERSTRUKTURERTE INDEKSER'      
     
      HP(33,1)='Catch in Number'                                    
      HP(33,2)='Capturas en N£mero'                                 
      HP(33,3)='Captures en Nombre'                                 
      HP(33,4)='Fangst i Antall'                                    
     
      HP(34,1)='Predicted Catch in Number'                          
      HP(34,2)='Capturas en N£mero Predichos para el Modelo'        
      HP(34,3)='Captures en Nombre PrÇdits par le Modäle'           
      HP(34,4)='Predikerte Fangst i Antall'                         
     
      HP(35,1)='Weights at age in the catches (Kg)'                 
      HP(35,2)='Peso por edades en las capturas (Kg)'               
      HP(35,3)='Poids par ages dans les captures (Kg)'              
      HP(35,4)='Vekter ved alder i fangstene (Kg)'                  
     
      HP(36,1)='Weights at age in the stock (Kg)'                   
      HP(36,2)='Peso por edades en el stock (Kg)'                   
      HP(36,3)='Poids par ages dans le stock (Kg)'                  
      HP(36,4)='Vekter ved alder i bestanden (Kg)'                  
     
      HP(37,1)='Natural Mortality (per year)'                       
      HP(37,2)='Mortalidad Natural (por a§o)'                       
      HP(37,3)='MortalitÇ Naturelle (par an)'                       
      HP(37,4)='Naturlig Dîdelighet (Ürlig)'                        
     
      HP(38,1)='Proportion of fish spawning'                        
      HP(38,2)='Proporci¢n de peces desovando'                      
      HP(38,3)='Proportion de poissons en ponte'                    
      HP(38,4)='Modning i bestanden ved Ür og alder'                
     
      HP(39,1)='INDICES OF SPAWNING BIOMASS'                        
      HP(39,2)='INDICES DE BIOMASA DESOVANTE'                       
      HP(39,3)='INDICES DE BIOMASSE'                                
      HP(39,4)='INDEKSER AV GYTEBESTAND'                            
     
      HP(40,1)='AGE-STRUCTURED INDICES'                             
      HP(40,2)='INDICES DE ABUNDANCIA POR EDAD'                     
      HP(40,3)='INDICES D''ABONDANCE PAR AGES'                      
      HP(40,4)='ALDERSTRUKTURERTE INDEKSER'                         
     
      HP(41,1)='Fishing Mortality (per year)'                       
      HP(41,2)='Mortalidad por Pesca (por a§o)'                     
      HP(41,3)='MortalitÇ par pàche (par an)'                       
      HP(41,4)='Fiskeridîdelighet (Ürlig)'                          
     
      HP(42,1)='Population Abundance (1 January)'                   
      HP(42,2)='Abundancia (1 enero) '                              
      HP(42,3)='Abondance (1 Janvier)'                              
      HP(42,4)='Bestand i Antall (1 januar)'                        
     
      HP(43,1)='Predicted SSB Index Values'                         
      HP(43,2)='Valores Predichos de los Indices de Biomasa Desovante'                                                             
      HP(43,3)='Valeurs des Indices de Biomasse PrÇdits par le Modäle'                                                             
      HP(43,4)='Predikerte Gytebestand Indeksverdier'               
     
      HP(44,1)='Predicted Age-Structured Index Values'              
      HP(44,2)='Valores Predichos de los Indices de Abundancia'         &                                                          
      //' por Edades'                                               
      HP(44,3)='Valeurs des Indices d''Abondances par Ages'             &                                                          
      //' PrÇdits par le Modäle'                                    
      HP(44,4)='Predikerte Alderstrukturerte Indeksverdier '        
     
      HP(45,1)='STCK SUMMARY'                                      
      HP(45,2)='SUMARIO DEL ANALYSIS'                               
      HP(45,3)='SOMMAIRE DE L''ANALYSE'                             
      HP(45,4)='RESYMET AV ANALYSEN'                                
     
      HP(46,1)=' ≥ Year ≥  Recruits  ≥  Total  ≥ Spawning≥ Landings ≥'  &                                                          
      //' Yield ≥ Mean F ≥ SoP ≥'                                   
      HP(47,1)=' ≥      ≥   Age      ≥ Biomass ≥ Biomass ≥          ≥'  &                                                          
      //' /SSB  ≥  Ages  ≥     ≥'                                   
      HP(48,1)=' ≥      ≥  thousands ≥  tonnes ≥ tonnes  ≥ tonnes   ≥'  &                                                          
      //' ratio ≥        ≥ (%) ≥'                                   
     
      HP(46,2)=' ≥ A§o  ≥  Reclutas  ≥ Biomasa ≥ Biomasa ≥ Capturas ≥'  &                                                          
      //' Rel.  ≥ F Prom.≥ SoP ≥'                                   
      HP(47,2)=' ≥      ≥   Edad     ≥  Total  ≥ Desov.  ≥          ≥'  &                                                          
      //' Capt  ≥  Edades≥     ≥'                                   
      HP(48,2)=' ≥      ≥  miles     ≥   (t.)  ≥  (t.)   ≥   (t)    ≥'  &                                                          
      //' Biom. ≥        ≥ (%) ≥'                                   
     
      HP(46,3)=' ≥AnnÇe ≥ Recrutement≥ Biomasse≥ Biomasse≥ Captures ≥'  &                                                          
      //' Rel.  ≥F Moyenn≥ SoP ≥'                                   
      HP(47,3)=' ≥      ≥   Age      ≥  Totale ≥ Ponte   ≥          ≥'  &                                                          
      //' Capt  ≥  Ages  ≥     ≥'                                   
      HP(48,3)=' ≥      ≥  milliers  ≥   (t.)  ≥  (t.)   ≥   (t)    ≥'  &                                                          
      //' Biom. ≥        ≥ (%) ≥'                                   
     
      HP(46,4)=' ≥ èr   ≥Rekruttering≥ Total   ≥  Gyte   ≥ Fangst   ≥'  &                                                          
      //'Forhold≥F Middel≥ SoP ≥'                                   
      HP(47,4)=' ≥      ≥ Alder      ≥ Bestand ≥ Bestand ≥          ≥'  &                                                          
      //'Fangst/≥Alderer ≥     ≥'                                   
      HP(48,4)=' ≥      ≥ tusen      ≥   (t.)  ≥  (t.)   ≥   (t)    ≥'  &                                                          
      //'Bestand≥        ≥ (%) ≥'                                   
     
     
      HP(49,1)='PARAMETER ESTIMATES'                                
      HP(49,2)='ESTIMACIONES DE LOS PARAMETROS'                     
      HP(49,3)='ESTIMATION DES PARAMETRES'                          
      HP(49,4)='BEREGNETE PARAMETRE'                                
     
       HP(50,1)='≥Parm.≥      ≥ Maximum ≥    ≥        ≥'                &                                                          
      //'         ≥         ≥         ≥ Mean of ≥'                  
      HP(51,1)='≥ No. ≥      ≥ Likelh. ≥ CV ≥  Lower ≥'                 &                                                          
      //' Upper   ≥  -s.e.  ≥   +s.e. ≥ Param.  ≥'                  
      HP(52,1)='≥     ≥      ≥ Estimate≥ (%)≥ 95% CL ≥'                 &                                                          
      //' 95% CL  ≥         ≥         ≥ Distrib.≥'                  
     
      HP(50,2)='≥Parm.≥      ≥ Estim.  ≥    ≥ L°mite ≥'                 &                                                          
      //' L°mite  ≥    -    ≥     +   ≥Promedio ≥'                  
      HP(51,2)='≥ No. ≥      ≥ Probab. ≥ CV ≥  95%   ≥'                 &                                                          
      //'  95%    ≥  error  ≥   error ≥ de la   ≥'                  
      HP(52,2)='≥     ≥      ≥ Maximum ≥ (%)≥Inferior≥'                 &                                                          
      //' Superior≥ est ndar≥ est ndar≥ Distrib.≥'                  
     
     
      HP(50,3)='≥Parm.≥      ≥ Estim.  ≥    ≥ Limite ≥'                 &                                                          
      //' Limite  ≥    -    ≥    +    ≥ Moyenne ≥'                  
      HP(51,3)='≥ No. ≥      ≥ Probab. ≥ CV ≥  95%   ≥'                 &                                                          
      //'  95%    ≥ erreur  ≥ erreur  ≥ de la   ≥'                  
      HP(52,3)='≥     ≥      ≥ Maximum ≥ (%)≥ Infer. ≥'                 &                                                          
      //' Super.  ≥ standard≥ standard≥ Distrib.≥'                  
     
      HP(50,4)='≥Parm.≥      ≥Maksimum ≥    ≥  ôvre  ≥'                 &                                                          
      //'  Nedre  ≥         ≥         ≥ Middels ≥'                  
      HP(51,4)='≥ Num.≥      ≥Sannsynl.≥ CV ≥  95%   ≥'                 &                                                          
      //'  95%    ≥  -s.e.  ≥  +s.e   ≥   av    ≥'                  
      HP(52,4)='≥     ≥      ≥ Beregn. ≥ (%)≥ Grense ≥'                 &                                                          
      //' Grense  ≥         ≥         ≥ Distrib.≥'                  
     
     
      HP(53,1)='Separable Model: Selection (S) by age'              
      HP(53,2)='Modelo Separable: Selecci¢n (S) por edad'           
      HP(53,3)='Modäle Separable: Selection (S) par Ége'            
      HP(53,4)='Separabel Modell: Fiskemînster (S) ved alder'       
     
      HP(54,1)='Separable Model: Selection (S1) by age'             
      HP(54,2)='Modelo Separable: Selecci¢n (S1) por edad'          
      HP(54,3)='Modäle Separable: Selection (S1) par age'           
      HP(54,4)='Separabel Modell: Fiskemînster (S1) ved alder'      
     
      HP(55,1)='Separable Model: Selection (S2) by age from'        
      HP(55,2)='Modelo Separable: Selecci¢n (S2) por edad desde'    
      HP(55,3)='Modäle SÇparable: Selection (S2) par Ége depuis'    
      HP(55,4)='Separabel Modell: Fiskemînster (S2) ved alder fra'  
     
      HP(56,1)=' Fixed : Reference Age'                             
      HP(56,2)=' Fijado : Edad de referencia'                       
      HP(56,3)=' FixÇ : Age de rÇfÇrence'                           
      HP(56,4)=' Fastsatt : Referansealder'                         
     
      HP(57,1)=' Fixed : Last true age'                             
      HP(57,2)=' Fijado : Ultima edad en el modelo'                 
      HP(57,3)=' FixÇ : Dernier Ége dans le modäle'                 
      HP(57,4)=' Fastsatt : Siste alder i modellen'                 
     
      HP(58,1)='Separable model: Populations in year'               
      HP(58,2)='Modelo Separable : Poblaciones por a§o'             
      HP(58,3)='Modäle sÇparable : Populations par an'              
      HP(58,4)='Separabel Modell : Mengde i Ür'                     
     
      HP(59,1)='Separable model: Populations at age'                
      HP(59,2)='Modelo Separable : Poblaciones por edad'            
      HP(59,3)='Modäle sÇparable : Populations par age'             
      HP(59,4)='Separabel Modell : Mengde ved alder'                
     
      HP(60,1)='Recruitment in year'                                
      HP(60,2)='Reclutamiento por a§o'                              
      HP(60,3)='recrutement par an'                                 
      HP(60,4)='Rekruttering i Ür'                                  
     
      HP(61,1)='SSB Index catchabilities'                           
      HP(61,2)='Capturabilidades para los °ndices de'                   &                                                          
      //' biomasa desovante'                                        
      HP(61,3)='CapturabilitÇs pour les indices de biomasse'        
      HP(61,4)='Fangstmodell for bestandindeksene'                  
     
      HP(62,1)='Age-structured index catchabilities'                
      HP(62,2)='Capturabilidades para los °ndices'                      &                                                          
      //' estructurados por edades'                                 
      HP(62,3)='CapturabilitÇs pour les indices'                        &                                                          
      //' structurÇs par ages'                                      
      HP(62,4)='Verdier for fangstmodellene av '                       &                                                           
      //' alderstrukturerte indekser'                               
     
      HP(63,1)='Absolute estimator. No fitted catchability.'        
      HP(63,2)='Modelo Absoluto. No hay capturabilidad ajustada.'   
      HP(63,3)='Modäle Absolu. Aucune capturabilitÇ calculÇe.'      
      HP(63,4)='Absolutt Modell. Ingen fangstmodell.'               
     
      HP(64,1)='Linear model fitted. Slopes at age :'               
      HP(64,2)='Modelo Lineal. Valores de Q por edades :'           
      HP(64,3)='Modäle LinÇaire. Valeurs de Q par ages :'           
      HP(64,2)='Lineër Modell. Verdiene av Q ved alder :'           
     
      HP(65,1)='Power model fitted. Slopes (Q) and exponents (K)'       &                                                          
      //' at age'                                                   
      HP(65,2)='Modelo exponencial usado. Pendientes (Q) y '            &                                                          
      //' exponentes (K) por edad'                                  
      HP(65,3)='Modäle exponentiel. Valeurs de Q y K par Éges :'    
      HP(65,4)='Eksponent inkludert i Modellen. '                       &                                                          
      //' Verdiene av Q og K ved alder :'                           
     
      HP(66,1)='Parameters of the stock-recruit relationship'       
      HP(66,2)='Par metros de la relaci¢n stock-recluta'            
      HP(66,3)='Paramätres de la relation stock-recrutement'        
      HP(66,4)='Verdiene av parametre i bestands-rekrutterings '            &                                                      
      //'funksjon '                                                 
     
      HP(67,1)='RESIDUALS ABOUT THE MODEL FIT'                      
      HP(67,2)='RESIDUOS ALREDEDOR DEL MODELO AJUSTADO'             
      HP(67,3)='ERREURS AUTOUR DU MODELE'                           
      HP(67,4)='RESIDUALER FRA MODELLEN'                            
     
      HP(68,1)='SPAWNING BIOMASS INDEX RESIDUALS'                   
      HP(68,2)='RESIDUOS DE LOS INDICES DE BIOMASA DESOVANTE'       
      HP(68,3)='ERREURS DANS LES INDICES DE BIOMASSE'               
      HP(68,4)='RESIDUALER I BESTANDS INDEKSER'                     
     
      HP(69,1)='AGE-STRUCTURED INDEX RESIDUALS'                     
      HP(69,2)='RESIDUOS DE LOS INDICES ESTRUCTURADOS POR EDADES'   
      HP(69,3)='ERREURS DANS LES INDICES STRUCTURES PAR AGES'       
      HP(69,4)='RESIDUALER I ALDERSTRUKTURERTE INDEKSER'            
     
      HP(3,1)='Last age is a plus-group'                            
      HP(3,2)='La £ltima edad es una clase acumulativa'             
      HP(3,2)='Le dernier Ége est une classe accumulÇe'             
      HP(3,3)='Sist alder er en pluss gruppe'                       
     
      HP(70,1)='Number of observations '                            
      HP(70,2)='N£mero de observaciones'                            
      HP(70,3)='Nombre d''observations'                             
      HP(70,4)='Antall observasjoner '                              
     
      HP(71,1)='Weighting factors for the catches in number'        
      HP(71,2)='Factores de ponderaci¢n para las capturas en '          &                                                          
      //'n£mero '                                                   
      HP(71,3)='Facteurs LAMBDA pour les captures structurÇes par'      &                                                          
      //' Éges'                                                     
      HP(71,4)='LAMBDA-verdier for alderstrukturerte fangster'      
     
      HP(72,1)='Predicted '                                         
      HP(72,2)='Predicho '                                          
      HP(72,3)='PrÇdits par le modäle'                              
      HP(72,4)='Predikert'                                          
     
      HP(73,1)='Fitted Selection Pattern'                           
      HP(73,2)='Valores de Selecci¢n Calculados'                    
      HP(73,3)='Valeurs de SÇlection calculÇes'                     
      HP(73,4)='Fiskemînster verdier'                               
     
      HP(74,1)='Separable model : F by year'                        
      HP(74,2)='Modelo Separable : F por a§o'                       
      HP(74,3)='Modäle SÇparable : F par an'                        
      HP(74,4)='Separabel Modell : F ved Ür'                        
     
      HP(75,1)=' to '                                               
      HP(75,2)=' hasta '                                            
      HP(75,3)='jusqu''Ö'                                           
      HP(75,4)=' til '                                              
     
      HP(76,1)= 'Separable Model Residuals'                         
      HP(76,2)= 'Residuos Para el Modelo Separable'                 
      HP(76,3)= 'Erreurs autour du Modäle Separable'                
      HP(76,4)= 'Residualer fra Separabel Modell'                   
     
      HP(77,1)='Age'                                                
      HP(77,2)='Ed.'                                                
      HP(77,3)='Age  '                                              
      HP(77,4)='Ald'                                                
     
      HP(78,1)=' '                                                  
      HP(78,2)='Archivo de salida en InglÇs (S/N) ?'                
      HP(78,3)='Ecrir le fichier de rÇsultats en Anglais (O/N) ?'   
      HP(78,4)='Utskrivtsfilen pÜ Engelsk (J/N) ? '                 
     
     
      HP(79,1)=' '                                                  
      HP(79,2)='C¢digos para §, Ç etc. en IBM-DOS o en WINDOWS (D/W) ?'                                                            
      HP(79,3)='Codes pour Ç,  , etc. en IBM-DOS ou en WINDOWS (D/W) ?'                                                            
      HP(79,4)='Norske tegn Ü, ë, etc. mÜ bruke IBM-DOS eller WINDOWS (D/W) ?'                                                     
     
      KO(5,1)='dDwW'                                                
      KO(5,2)=KO(5,1)                                               
      KO(5,3)=KO(5,1)                                               
      KO(5,4)=KO(5,1)                                               
     
      KY(5,1)='dD'                                                  
      Ky(5,2)=KY(5,1)                                               
      Ky(5,3)=KY(5,1)                                               
      Ky(5,4)=KY(5,1)                                               
     
      ! Messages for projection programme                                 
     
      HW(1,1)='Enter Random-Number seed '                           
      HW(1,2)='Numero para iniciar secuencia aleatoria ?'           
      HW(1,3)='Numero pour initier la sÇquence alÇatoire ?'         
      HW(1,4)='Nummer for Ü begynne random sekvens ?'               
     
      HW(2,1)='Enter the no. of years between spawning and '            &                                                          
      // ' recruitment at age '                                     
      HW(2,2)='Teclee el n£mero de a§os entre desove y reclutamiento'   &                                                          
      // ' a la edad '                                              
      HW(2,3)='Frapper le delais (en annÇes) entre la ponte et le recrutement'                                                     
      HW(2,4)='Tidsavnittet mellom gyting og rekruttering'          
     
      HW(3,1)='The model could not be fitted to the data. Try again,'   &                                                          
      //' using another model'                                      
      HW(3,2)='No se pudo ajustar el model a los datos. Intente otra'   &                                                          
      //' vez con otra selecci¢n'                                   
      HW(3,3)='Le modäle n''a pas pñ àtre ajustÇ. Essayer un autre ?'                                                              
      HW(3,4)='Modellen kunne ikke tilpasses. Forsîk en annen ?'    
     
     
      HW(5,1)= 'Choose type of stock recruit relation : '           
      HW(5,2)= 'Elige el tipo de relaci¢n stock-recluta :'          
      HW(5,3)= 'Choisissez le tipe de relation stock-recrues :'     
      HW(5,4)= 'Velg bestands-rekrutts modell : '                   
     
     
      HW(6,1)= 'S -     Shepherd        R = a.SSB/(1+SSB/b)^c  '    
      HW(6,2)=HW(6,1)                                               
      HW(6,3)=HW(6,1)                                               
      HW(6,4)=HW(6,1)                                               
     
      HW(7,1)= 'B -  Beverton-Holt   R = a.SSB/(1+SSB/b)   '        
      HW(7,2)=HW(7,1)                                               
      HW(7,3)=HW(7,1)                                               
      HW(7,4)=HW(7,1)                                               
     
      HW(8,1)= 'R -  Ricker          R = a.SSB.exp(-b.SSB) '        
      HW(8,2)=HW(8,1)                                               
      HW(8,3)=HW(8,1)                                               
      HW(8,4)=HW(8,1)                                               
     
      HW(9,1)= 'O -  Ockham          R = GM over observed SSB range'
      HW(9,2)= 'O -  Ockham          R = Promedio geomÇtrico en el'  &                                                             
      //' rango observado de B'                                     
      HW(9,3)= 'O -  Ockham          R = Moyn. gÇomÇtrique sur les B observÇs'                                                     
      HW(9,4)= 'O -  Ockham          R = Historisk middel for historiske B'                                                        
     
      HW(10,1)= '                            then linear to origin' 
      HW(10,2)= '                            y lineal hasta R=0 a B=0'                                                             
      HW(10,3)= '                            et lineaire jusqu''Ö R=0,B=0'                                                         
      HW(10,4)= '                            og lineër til startpunktet'                                                           
     
      HW(11,1)= 'N -     None            R = Historic Geometric'        &                                                          
      //' Mean R '                                                  
     
      HW(11,2)= 'N -     Ning£n          R = Promedio geomÇtrico'       &                                                          
      // ' hist¢rico'                                               
      HW(11,3)= 'N -     Aucun           R = Moyenne gÇometrique'   
      HW(11,4)= 'N -     Ingen           R = Historisk middel'      
     
      HW(12,1)= 'Enter your choice (S/B/R/O/N) ? '                  
      HW(12,2)= 'Elige su selecci¢n (S/B/R/O/N) ? '                 
      HW(12,3)= 'Faites votre choix (S/B/R/O/N) ? '                 
      HW(12,3)= 'Velg mellom S,B,R,O eller N  '                     
     
      KW(1,1)='SsBbRrOoNn'                                          
      KW(1,2)=KW(1,1)                                               
     
     
     
     
      HW(13,1)= 'Enter first year of data for stock-recruit model'  
      HW(13,2)= 'Teclee el primer a§o para usar en la relaci¢n '        &                                                          
      // 'stock-recluta'                                            
      HW(13,3)= 'Premiäre annÇe Ö utiliser pour le modäle stock-recrutement ? '                                                    
      HW(13,4)= 'Fîrste Ür i bestands-rekrutterings modellen ? '    
     
     
      HW(14,1)= 'Enter last year of data for stock-recruit model'   
      HW(14,2)= 'Teclee el £ltimo a§o para usar en la relaci¢n '        &                                                          
      // 'stock-recluta'                                            
      HW(13,3)= 'Derniäre annÇe Ö utiliser pour le modäle stock-recrues ? '                                                        
      HW(13,4)= 'Siste Ür i bestands-rekrutterings modellen ? '     
     
     
      HW(15,1)= 'Error in year range. Try again. '                  
      HW(15,2)= 'Error en el rango de a§os. Intente otra vez.'      
      HW(15,3)= 'Erreur dans le choix d''annÇes. Essayez encore: '  
      HW(15,4)= 'Feil i valg av Ür. Enda et valg : '                
     
     
      HW(16,1)='Autocorrelated or Independent errors (I/A) '        
      HW(16,2)='Errores con Autocorrelaci¢n o Independientes (I/A)' 
      HW(16,3)='Erreurs avec Autocorrelation ou Independentes (I/A)'
      HW(16,4)='Residualer med Autokorrelasjoner eller Uavhengig (A/U)'                                                            
     
      KW(2,1)='IiAa'                                                
      KW(2,2)=KW(2,1)                                               
      KW(2,3)=KW(2,1)                                               
      KW(2,4)='AaUu'                                                
     
      HW(17,1)='Enter the number of MC simulations to run'          
      HW(17,2)='Teclee el n£mero de simulaciones requeridas'        
      HW(17,3)='Frapper le numero de simulations dÇsirÇes'          
      HW(17,4)='Hvor mange iterasjoner skal beregnes ?'             
     
      HW(18,1)='Too many parameters for this version. Recompile with'   &                                                          
      // ' larger R matrix in MCPROJ'                               
     
      HW(18,2)='Demasiado parametros. Recompile con matriz R'           &                                                          
      //' en el subprograma MCPROJ mas grande'                      
     
      HW(18,3)='ERREUR. Trop de paramätres.'                        
      HW(18,4)='FEIL. For mange parametre'                          
     
      HW(19,1)='G05EAF Error  : '                                   
      HW(19,2)='G05EAF: Error : '                                   
      HW(19,3)='G05EAF: Erreur : '                                  
      HW(19,4)='G05EAF: Feil : '                                    
     
      HW(20,1)='G05EZF Error  : '                                   
      HW(20,2)='G05EZF: Error : '                                   
      HW(20,3)='G05EZF: Erreur : '                                  
      HW(20,4)='G05EZF: Feil : '                                    
     
     
      HW(21,1)='F-multiplier reset to '                             
      HW(21,2)='F relativa a la F en el £ltimo a§o limitado a '     
      HW(21,3)='F en relation Ö la F Ö la derniäre annÇe limitÇ Ö ' 
      HW(21,4)='''F-multiplier'' reset til '                        
     
      HW(22,1)=' for fleet '                                        
      HW(22,2)=' para la flotilla'                                  
      HW(22,2)=' pour le mÇtier'                                    
      HW(22,4)=' for flÜte '                                        
     
     
      HW(23,1)=' in year '                                          
      HW(23,2)=' en el a§o '                                        
      HW(23,3)=' en l''an '                                         
      HW(23,4)=' i Üret '                                           
     
      HW(24,1)='Minimisation finished with exit code '              
      HW(24,2)='Minimisaci¢n terminada con c¢digo '                 
      HW(24,3)='Minimisation terminÇe avec code '                   
      HW(24,4)='Beregning av mimimum avsluttet med kode '           
     
      HW(25,1)='Use default percentiles (Y/N)  ? '                  
      HW(25,2)='Utilisar fractiles est ndares (S/N) ? '             
      HW(25,3)='Utiliser les fractiles standards (O/N) ? '          
      HW(25,4)='Bruker normalprosenter (J/N) ? '                    
     
      KW(3,1)='yYnN'                                                
      KW(3,2)='sSnN'                                                
      KW(3,3)='oOnN'                                                
      KW(3,4)='jJnN'                                                
     
      KW(4,1)='yY'                                                  
      KW(4,2)='sS'                                                  
      KW(4,3)='oO'                                                  
      KW(4,4)='jJ'                                                  
     
      HW(26,1)='Enter the number of percentiles required'           
      HW(26,2)='Teclee el n£mero de fractiles requeridos'           
      HW(26,3)='Frappez le numero de pourcentages dÇsirÇs'          
      HW(26,4)='Hvor mange prosenter skal beregnes ?'               
     
      HW(27,1)='Enter a percentile '                                
      HW(27,2)='Teclee un fractile, en porcentaje '                 
      HW(27,3)='Frappez un pourcentage '                            
      HW(27,4)='Skriv en prosent '                                  
     
      HW(28,1)='Enter SSB reference level (e.g. MBAL, Bpa..) [t]'   
      HW(28,2)='Teclee la biomasa de referencia (e.g. MBAL, Bpa..) [t]'                                                            
      HW(28,3)='Frappez la biomasse de reference (e.g. MBAL, Bpa..) [t]'                                                           
      HW(28,4)='Skriv referansebestand (e.g. MBAL, Bpa...) [t]'     
     
      HW(29,1)='Partial Fishing Mortality for Fleet '               
      HW(29,2)='Mortalidad por pesca parcial para la flota '        
      HW(29,3)='MortalitÇ par pàche partiale pour le mÇtier'        
      HW(29,4)='Delvis-F for flÜten '                               
     
      HW(30,1)='Natural Mortality, Maturity Ogive and Stock Weights'
      HW(30,2)='Mortalidad Natural, Madurez y Peso en el Stock'     
      HW(30,3)='MortalitÇ Naturelle, MaturitÇ y Poids dans le Stock'
      HW(30,4)='Naturligt Dîdelighet, Modning og Vekter i Bestanden'
     
      HW(31,1)='s.d. of Maturity Ogive and Stock Weights'           
      HW(31,2)='Desviaci¢n Est ndar de Madurez y Peso en el Stock'  
      HW(31,3)='DÇviation Standard de la MaturitÇ et des Poids dans le Stock'                                                      
      HW(31,4)='s.d. av Modning og Vekter i Bestanden'              
     
      HW(32,1)='     for root-arcsine and log transforms'           
      HW(32,2)='     para transformaciones logar°tmicos y ''root-arcsine'' '                                                       
      HW(32,3)='     pour transformations logaritmiques et ''root-arcsine'' '                                                      
      HW(32,4)='     til  ''root-arcsine'' og logaritmiske transformasjoner '                                                      
     
      HW(33,1)= ' Stock Size Trajectory '                           
      HW(33,2)= ' Stck en Cada A§o '                                
      HW(33,3)= ' Stock a Chaque AnnÇe'                             
      HW(33,4)= ' Bestand i èr'                                     
     
      HW(34,1)=' Fleet Catches by Weight '                          
      HW(34,2)=' Capturas en Peso por Flota'                        
      HW(34,3)=' Captures en Poids par MÇtier'                      
      HW(34,4)=' Fangst av FlÜte'                                   
     
      HW(35,1)='Catch Estimates by Fleet '                          
      HW(35,2)='Capturas por Flota'                                 
      HW(35,3)='Captures par MÇtier'                                
      HW(35,4)='Fangst av FlÜte i Antall'                           
     
      HW(36,1)='Total Catches by Number'                            
      HW(36,2)='Captura Total en N£mero'                            
      HW(36,3)='Total des Captures en Nombre'                       
      HW(36,4)='Totallfangst i Antall'                              
     
      HW(37,1)='Change any of the populations (Y/N) ?'              
      HW(37,2)='Cambiar las poblaciones estimadas (S/N) ?'          
      HW(37,3)='Changer les populations estimÇes (O/N) ?'           
      HW(37,4)='ônsker Du Ü Forandre Bestanden I Antall (J/N) ?'    
     
     
      HW(38,1)='Enter Year, Age, new Population: -1,-1,-1 to finish '                                                              
      HW(38,2)='Teclee A§o, Edad, Nueva Poblaci¢n o -1,-1,-1 para acabar'                                                          
      HW(38,3)='Frapper AnnÇe, Age, Nouvelle population ou -1,-1,-1 pr terminer'                                                   
      HW(38,4)='Skriv èr, Alder og Ny Bestand i Antall eller -1,-1,-1 til ende'                                                    
     
     
      HW(39,1)=' New parameters: index, new value (-1,-1 to finish) '                                                              
      HW(39,2)=' Parametros Nuevos: °ndice, valor (-1,-1 para acabar) '                                                            
      HW(39,3)=' Nouveaux Paramätres: indice, valeur (-1,-1 pr terminer)'                                                          
      HW(39,4)=' Nye Parametre: indeks, verdi (-1,-1 til ende)'     
     
     
      HW(40,1)='Populations in the Projection'                      
      HW(40,2)='Poblaciones en las proyecciones'                    
      HW(40,3)='Populations dans les projections'                   
      HW(40,4)='Prosjektert Bestand i Antall '                      
     
      HW(41,1)='Fishing Mortality in the Projection'                
      HW(41,2)='Mortalidad por Pesca en la Proyecci¢n'              
      HW(41,3)='MortalitÇ par pàche dans les projections'           
      HW(41,4)='Prosjekterte F-verdier'                             
     
      HW(42,1)='Fleet Catches by Number'                            
      HW(42,2)='Capturas por Flota (en N£meros)'                    
      HW(42,3)='Captures par MÇtier (en Nombre)'                    
      HW(42,4)='Fansgt av FlÜte (i Antall)'                         
     
      HW(43,1)='Enter the name of the projection file'              
      HW(43,2)='Teclee el nombre del archivo de proyecci¢n'         
      HW(43,3)='Frapper le nom du fichier de projection'            
      HW(43,4)='Skriv navnet pÜ prosjekteringsfilen'                
     
      HW(44,1)='Could not access the file. Another name ? '         
      HW(44,2)='No se encontr¢ el archivo. Otro nombre ?'           
      HW(44,3)='Le fichier n''a pas ÇtÇ trouvÇ. Un autre nom ?'     
      HW(44,4)='Prosjekteringsfil ble ikke funnet. Et forsîk til ?' 
     
      HW(45,1)='Format error in projection file while attempting to'
      HW(45,2)='Error en el formato del archivo de proyecci¢n en'   
      HW(45,3)='Erreur dans le fichier de projections dans'         
      HW(45,4)='Feil i format av prosjekteringsfilen i '            
     
      HW(46,1)=' read target multipliers.'                          
      HW(46,2)=' los multiplicadores de F. '                        
      HW(46,3)=' les multiplicateurs de F.'                         
      HW(46,4)=' verdier Ü multiplisere med F'                      
     
      HW(47,1)=' read CV of target multipliers. '                   
      HW(47,2)=' los c.v. de los multiplicadores de F.'             
      HW(47,3)=' les c.v. des multiplicateurs de F.'                
      HW(47,4)= 'c.v. av verdier  Ü multiplisere med F'             
     
      HW(48,1)='ERROR: Catch ratios must sum to 1 across fleets'    
      HW(48,2)='ERROR: Las fracciones de las capturas por flota deben sumar a 1'                                                   
      HW(48,3)='ERREUR: Les fractions des captures par mÇtiers doivent faire un total=1'                                           
      HW(48,4)='FEIL: Relativ fangst pr flÜte mÜ summere til 1'     
     
     
      HW(49,1)= 'Population parameters for the projections are set by taking a mean over a'                                        
      HW(49,2)= 'Parametros en las poblaciones est n calculados de un promedio de '                                                
      HW(49,3)= 'Les paramätres dans les populations sont calculÇes d''apräs une moyenne'                                          
      HW(49,4)= 'Parametre i bestanden er beregnet som middel av '  
     
      HW(50,1)= 'number of the last years of the data set. '        
      HW(50,2)= 'unos de los £ltimos a§os de los datos.'            
      HW(50,3)= 'des donnÇes de certaines annÇes rÇcentes dans les donnÇes.'                                                       
      HW(50,4)= 'en rekke av datoene i de senere Ür.'               
     
     
      HW(51,1)= 'Use mean natural mortality from'                   
      HW(51,2)= 'Utilisar M promedia desde '                        
      HW(51,3)= 'Utiliser la moyenne de M depuis '                  
      HW(51,4)= 'Bruker M-middels fra '                             
     
      HW(52,1)= ' back to  '                                        
      HW(52,2)= ' hasta '                                           
      HW(52,3)= 'jusqu''Ö'                                          
      HW(52,4)= ' bak til '                                         
     
     
      HW(53,1)='Use mean maturity ogive from '                      
      HW(53,2)='Utilisar el promedio de madurez desde'              
      HW(53,3)='Utiliser la moyenne de maturitÇ depuis'             
      HW(53,4)='Bruker modningsmiddel fra '                         
     
      HW(54,1)='Use mean weight at age in the stock from'           
      HW(54,2)='Utilisar los pesos promedio el stock desde'         
      HW(54,3)='Utiliser les poids moyens dans le stock depuis'     
      HW(54,4)='Bruker vekter i bestand fra '                       
     
      HW(55,1)='ERROR: Could not open covariance-matrix file  '     
      HW(55,2)='ERROR: No se encontr¢ el archivo de matriz de covarianzas'                                                         
      HW(55,3)='ERREUR: Le fichier de la matrices des covariances n''a pas ÇtÇ trouvÇ'                                             
      HW(55,4)='FEIL: Filen av covarians-matrise ble ikke funnet'   
     
      HW(56,1)='Enter the reference spawning stock size (e.g. MBAL, Bpa)'                                                          
      HW(56,2)='Teclee el tama§o del stock de referencia (e.g. MBAL, Bpa)'                                                         
      HW(56,3)='Frapper le stock de rÇfÇrence (e.g. MBAL, Bpa)'     
      HW(56,4)='Skriv referansebestand (f. eks. MBAL, Bpa)'         
     
      HW(57,1)='Enter the maximum allowable F-multiplier'           
      HW(57,2)='Teclee el m†ximo valor de F relativo permitido'     
      HW(57,3)='Frapper la valeur maximum de F (relatif) permis'    
      HW(57,4)='Skriv den stîrste tillatte ''F-multiplier'' verdien'
     
     
      HW(58,1)='ERROR: Could not find temporary file ICA.TMP'       
      HW(58,2)='ERROR: No se encontr¢ el archivo ICA.TMP'           
      HW(58,3)='ERREUR: Le fichier ICA.TMP n''as pas ÇtÇ trouvÇ'    
      HW(58,4)='FEIL: Fil <ICA.TMP> ble ikke funnet'                
     
      HW(59,1)='Use ICA or SRR (I/S) model value for recruitment in '                                                              
      HW(59,2)='Usar valor de ICA o del modelo S-R (I/S) para recluta en '                                                         
      HW(59,3)='Utiliser valeur de ICA ou du modäle S-R (I/S) pour' 
      HW(59,4)='ICA eller B-S modell (I/S) verdi mÜ bli brukte i '  
     
      HW(60,1)='IiSs'                                               
      HW(60,2)=HW(60,1)                                             
      HW(60,3)=HW(60,1)                                             
      HW(60,4)=HW(60,1)                                             
     
      HW(61,1)='Ii'                                                 
      HW(61,2)='Ii'                                                 
      HW(61,3)='Ii'                                                 
      HW(61,4)='Ii'                                                 
     
      HW(62,1)='Use ICA-derived resamples ? '                       
      HW(62,2)='Usar muestras calculadas por ICA ?'                 
      HW(62,3)='Utiliser les Çchantillons calculÇs par l''ICA ?'    
      HW(62,4)='Bruker iterasjonene beregnet med ICA ? '            
     
      HW(63,1)='ERROR. Could not find the ICA iteration output.'    
      HW(63,2)='ERROR. No se encontr¢ la salida del ICA.'           
      HW(63,3)='ERREEUR. Le fichier de l''ICA n''a pas ÇtÇ trouvÇ.' 
      HW(63,4)='FEIL. Iterasjonsfil fra ICA ble ikke funnet.'       
     
      HW(64,1)='Normal Exit from ICP'                               
      HW(64,2)='Salida normal de ICP'                               
      HW(64,3)='Programme ICP terminÇ sans erreur dÇtectÇe'         
      HW(64,4)='ICP ble avsluttet uten feil'                        

end subroutine Text_Strings

end module message_definition




subroutine Initialise_Global_Vars

! (Re)initialises global variables
    use ARRAYLEN 
	use DIMS
	USE STCK
    USE CONTROL
    USE STATS
	USE CPUE
    USE RESULTS
	USE FILES

!FLR Initialised here rather than in modules since problem with C/C++
NAges		 = maxage
NYrs		 = maxyear
MaxNSurveys	 = maxsurvey
MaxNBSurveys = maxbsurv 
NData		 = maxdata 
NParm		 = maxparm 
NIters		 = maxiters

missing = -99.0d0
firstage = 0; lastage = 0; firstyear = 0; lastyear = 0
fbyear = 0; lbyear = 0; nageix = 0; nssbix = 0;
PF = 0.0d0; PM = 0.0d0; RecalculatePopulations = .false.
MaxWeight = 0.0d0; NextRec = 0.0d0; RWOpt = 0
MainTitle = ""; UseSep = .false.; writeout = .false.
Full = .false.; NySep = 0; RefAge = 0; Nxdata = 0
Nxparm = 0; HiFage = 0; LoFage = 0; TermS = 0.0d0
TwoSel = .false.; StepSel = .false.; ChangeSel = 0
TermS2 = 0.0d0; A = 0.0d0; B = 0.0d0; lag = 0; FitSRR = .false.
SRRLAMBDA = 0.0d0; fychosen = 0; lychosen = 0; SRRVar = 0.0d0;
CVar = 0.0d0; Cchi2 = 0.0d0; CPchi = 0.0d0; CCV = 0.0d0;
Cskew = 0.0d0; CKurt = 0.0d0; NoCdata = 0; dfsep = 0;
NoSRRdata = 0

do i = 1, 50
	ASurvLab(i) = "Survey"; BSurvLab(i) = "Survey"
end do

do i = 1, maxsurvey
	fage(i) = 0; lage(i) = 0; fyear(i) = 0; lyear(i) = 0
	Timing(i) = 0.0d0; plusgp(i) = .false.; QAParm(i) = 0;
	QBParm(i) = 0; IXcor(i) = 0.0d0
end do

do i = 1, maxyear
	do j = 1, maxage
		CN(i,j) = 0.0d0; CNLAMBDA(i,j) = 0.0d0;
		NM(i,j) = 0.0d0; SW(i,j) = 0.0d0; MO(i,j) = 0.0d0;
		CW(i,j) = 0.0d0; N(i,j) = 0.0d0; F(i,j) = 0.0d0
		FPerceived(i,j) = 0.0d0; NPerceived(i,j) = 0.0d0
		PredCN(i,j) = 0.0d0; W(i,j) = 0.0d0
	end do
end do

do i = 1, maxyear
	LA(i) = 0.0d0; SOP(i) = 0.0d0; Npool(i) = 0.0d0;
	PredRecruit(i) = 0.0d0; YearWt(i) = 0.0d0
end do

do i = 1, maxsurvey
	do j = 1, maxyear
		do k = 1, maxage
			ASurvey(i,j,k) = 0.0d0
			PredAINDEX(i,j,k) = 0.0d0
		end do
	end do
end do

do i = 1, maxsurvey
		do j = 1, maxage
			ALAMBDA(i,j) = 0.0d0; Avar(i,j) = 0.0d0
			Achi2(i,j) = 0.0d0; APchi(i,j) = 0.0d0;
			ACV(i,j) = 0.0d0; ASkew(i,j) = 0.0d0
			AKurt(i,j) = 0.0d0; NoAdata(i,j) = 0
		end do
end do

do i = 1, maxbsurv
	do j = 1, maxyear
		BSurvey(i,j) = 0.0d0; PredBINDEX(i,j) = 0.0d0
	end do
end do

do i = 1, maxbsurv
	BLAMBDA(i) = 0.0d0; Bvar(i) = 0.0d0; Bchi2(i) = 0.0d0
	BPchi(i) = 0.0d0; BCV(i) = 0.0d0; BSkew(i) = 0.0d0
	BKurt(i) = 0.0d0; NoBdata(i) = 0
end do

do i = 1, maxparm
	X(i) = 0.0d0; Xbest(i) = 0.0d0; Xlow(i) = 0.0d0
	Xhigh(i) = 0.0d0
end do

do i = 1, maxage
	S(i) = 0.0d0; RelWt(i) = 0.0d0
end do

do i = 1, maxparm
	do j = 1, maxparm
		COV(i,j) = 0.0d0
	end do
end do

end subroutine Initialise_Global_Vars
