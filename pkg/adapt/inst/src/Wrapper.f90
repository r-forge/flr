subroutine VPA_DATA
      USE PARAMETERS ; USE STATISTICS, ONLY: LASTYEAR
      USE DATUM, ONLY : PDF_STOCKRECRUIT, IGNORE_RECRUIT
      IMPLICIT NONE
      
	  CHARACTER (75) FILENAME

      INTEGER :: IDUMMY,I,LINE=0
      REAL (KIND=8) :: DUMMY
      CHARACTER (LEN=1) :: CH,CH2,CH3


      OPEN(12,FILE='VPA-2BOX.LOG',STATUS='UNKNOWN')
!      CONFILE = 'C:\Projects\\FLR\Classes\FLAdapt\Data\ALB00.C01' !FILENAME
!     CONFILE = 'C:\Projects\FLR\Secondary Packages\FLAdapt\inst\etc\VPA2boxBIGEYE2004\Bet2004.C6' !FILENAME
1     OPEN(10,FILE=CONFILE,STATUS='OLD',ERR=10)
      DO I=0,6
2       READ(10,'(3A1)')  CH,CH2,CH3 ; LINE=LINE+1
        IF(CH.NE.'#'.AND.CH.NE.'!'.AND.CH.NE.'_'.AND.CH.NE.'@') THEN
            IF(CH2=='#'.OR.CH2=='!'.OR.CH2=='@'.OR.CH3=='#'.OR.CH3=='!'.OR.CH3=='@') THEN ;
              WRITE(*,'(1x,a23,a1,a4,a1,a8)') 'INPUT ERROR: Misplaced ',CH2,'and ',CH3,' symbols'
              WRITE(*,'(1x,a21,i3,a8,a50)')   '             in line ',line,'of file ',CONFILE ; STOP
            ENDIF
            BACKSPACE(10) ; READ(10,*) INFILE(I)  
          ELSE
            GOTO 2
        ENDIF
      END DO
!CLOSE(10)
!goto 7
      CALL READVAR('INTR',DUMMY,OPTION(1),LINE)
      CALL READVAR('INTR',DUMMY,MODEL_TYPE,LINE)
      CALL READVAR('INTR',DUMMY,PDF_TAG,LINE)
      IF(PDF_TAG /= 0) THEN ; BACKSPACE(10) ; READ(10,*) IDUMMY,SIGMA_TAG_MOD,FISHING_PRESSURE; ENDIF
      CALL READVAR('INTR',DUMMY,SEED,LINE)
      CALL READVAR('INTR',DUMMY,MAXITER,LINE)
      CALL READVAR('INTR',DUMMY,CHECKFLAG,LINE)
      CALL READVAR('REAL',PDEV,IDUMMY,LINE)
      CALL READVAR('INTR',DUMMY,SCALES,LINE)
      CALL READVAR('REAL',CV_OVERIDE,IDUMMY,LINE) ! over-ride index cv's to this value
      CALL READVAR('INTR',DUMMY,ADD_VAR,LINE) ! (1) additive variance (0) multiplicative variance
      CALL READVAR('INTR',DUMMY,LINK_FT,LINE) ! link selectivities in last n years
      IF(LINK_FT.GT.1) THEN ; BACKSPACE(10) ; READ(10,*) IDUMMY,SIGMA_FT,LINK_YOUNGEST,LINK_OLDEST ; ENDIF
      CALL READVAR('INTR',DUMMY,LINK_REC,LINE) ! link recruitments in last n years
      IF(LINK_REC.GT.0) THEN ; BACKSPACE(10) ; READ(10,*) IDUMMY,SIGMA_REC ; ENDIF
      CALL READVAR('INTR',DUMMY,LINK_STOCK,LINE) ! link recruitments of two stocks
      IF(LINK_STOCK.GT.0) THEN ; BACKSPACE(10) ; READ(10,*) IDUMMY,SIGMA_STOCK,RATIO_STOCK ; ENDIF
      CALL READVAR('INTR',DUMMY,PDF_STOCKRECRUIT,LINE) ! Impose a Beverton and Holt stock recruitment penalty over certain years
      IF(PDF_STOCKRECRUIT /= 0) THEN ; BACKSPACE(10) ; READ(10,*) IDUMMY,IGNORE_RECRUIT ; ENDIF
      CALL READVAR('INTR',DUMMY,OPTION(2),LINE) ! estimate F's or N's
      CALL READVAR('INTR',DUMMY,OPTION(4),LINE) ! estimate q via mle or as part of search
      CALL READVAR('INTR',DUMMY,OPTION(3),LINE) ! bootstraps
      IF(OPTION(3)>0) THEN ; BACKSPACE(10) ; READ(10,*) IDUMMY,STINE_CORR ; ENDIF
      CALL READVAR('INTR',DUMMY,N_RETRO,LINE) ! retrospective analyses

	  CALL READ_DATA  ! user defined
      CLOSE(10)

7     continue    
      
      do 8 i=0,NPAR+1
		 PARM_KEY(i) = 0
8     continue

      CALL READ_PARAMETERS
      CLOSE(11)
      CLOSE(12)
10    RETURN
END

subroutine VPA_PARAMETERS();
      USE PARAMETERS
      IMPLICIT NONE
      CHARACTER (LEN=1) :: CH,CH2,CH3
      REAL (KIND=8) :: DEFAULTS(5)
      INTEGER :: I,K,J,REPETITIONS,L,LINE=0,METHOD
      INTEGER :: ERRORVAL = 0 
	        
      !INFILE(2) = "C:\\Projects\\FLR\\Classes\\FLAdapt\\Data\\alb00.p01";
	  OPEN(11,FILE=INFILE(2),STATUS='OLD',ERR=100)
      I=1 ; K=0 ; ICALL=0   
1     READ(11,'(3A1)',END=10)  CH,CH2,CH3 ; LINE=LINE+1
!       wade through comments and read the parameter specifications
        IF(CH.EQ.'@') THEN
            GOTO 10 ! End of file
          ELSEIF(CH.NE.'#'.AND.CH.NE.'!'.AND.CH.NE.'_') THEN
            IF(CH2=='#'.OR.CH2=='!'.OR.CH2=='@'.OR.CH3=='#'.OR.CH3=='!'.OR.CH3=='@') THEN ;
				  ERRORVAL = 1
				  RETURN
!FLR              WRITE(*,'(1x,a23,a1,a4,a1,a8)') 'INPUT ERROR: Misplaced ',CH2,'and ',CH3,' symbols'
!FLR              WRITE(*,'(1x,a21,i3,a8,a50)')   '             in line ',line,'of file ',INFILE(2) ; STOP
            ENDIF
            BACKSPACE(11)
            IF(CH.EQ.'$') THEN ! Multiple parameters are being specifed with the same form
                READ(11,*) CH,REPETITIONS,DEFAULTS
              ELSE ! One lone parameter is being specified
                READ(11,*) DEFAULTS ; REPETITIONS=1
            ENDIF
            METHOD=INT(DEFAULTS(4))
            SELECT CASE (METHOD)  ! Convert new Bayesian designation to old style
                 CASE (2) ; DEFAULTS(4)=0.3
                 CASE (3) ; DEFAULTS(4)=0.1
                 CASE (4) ; DEFAULTS(4)=0.2
                 CASE DEFAULT
            END SELECT
            IF(DEFAULTS(4)>=1.0) THEN
!             identify improperly-specified boundary conditions of free parameters
              IF(DEFAULTS(1).GT.DEFAULTS(2)) THEN
				  ERRORVAL = 2
				  RETURN
!FLR                   WRITE(*,'(1X,A59,I3)')          'ERROR: Lower bound greater than best estimate of parameter ',I
!FLR                   WRITE(*,'(1x,a25,i4,a8,a50)')   '       specified on line ',line,'of file ',INFILE(2) ; STOP
                ELSEIF(DEFAULTS(3).LT.DEFAULTS(2)) THEN
				  ERRORVAL = 2
				  RETURN
!FLR                  WRITE(*,'(1X,A56,I3)')          'ERROR: Upper bound less than best estimate of parameter ',I
!FLR                  WRITE(*,'(1x,a25,i4,a8,a50)')   '       specified on line ',line,'of file ',INFILE(2) ; STOP
              ENDIF
            ENDIF
            DO 2 L=1,REPETITIONS
              DO 3 J=1,5 ; PARM_SPECS(I,J)=DEFAULTS(J) ; 3 CONTINUE
!             identify estimable parameters and set them initial to the best guess
              IF(PARM_SPECS(I,4).GT.0) THEN
                K=K+1 ; PARM_KEY(K)=I
                PARM_EST(K)=PARM_SPECS(I,2)
              ENDIF
              I=I+1
2           CONTINUE
        ENDIF
      GOTO 1
10    PARM_KEY(0)=K     ! total number of parameters to be estimated 
      PARM_KEY(NPAR+1)=I-1 ! total number of parameters in model
				  ERRORVAL = 3
				  RETURN
!FLR      WRITE(*,'(1x,a31,i4)') 'Number of estimated parameters: ',PARM_KEY(0)
!FLR      WRITE(*,'(1x,a31,i4)') 'Total number of parameters:     ',PARM_KEY(NPAR+1)
100				  ERRORVAL = 3
				  RETURN
!FLR  WRITE(*,'(1X,A26,A50)') 'ERROR: The parameter file ',INFILE(2)
!FLR  WRITE(*,'(8X,A55)') 'does not exist or is being used by another application.'  ; PAUSE ; STOP
end

subroutine VPA_RUN
	  USE PARAMETERS ; USE STATISTICS, ONLY: LASTYEAR

      
	  CALL ESTIMATE(0)
end

subroutine VPA_RUN_SCRNIO
	  USE PARAMETERS ; USE STATISTICS, ONLY: LASTYEAR

      do 12 i=0,NPAR+1
		 PARM_KEY(i) = 0
12    continue  

	  CALL ESTIMATE(1)
end

subroutine VPA_OUTPUT
	USE STATISTICS; USE DATUM; USE PARAMETERS ; USE LOGLIKELIHOODS
      IMPLICIT NONE
      INTEGER A,G,Y,OTH,I,THE_END,Y_REL,A_REL,B_REC
      REAL (KIND=8) :: SIG2,OBS,PRED,X(1000),RAN1,DEV_EFFORT(Gs,Bs),DEV_TAG(Bs),DEV_EFF,DEVIANCE,mOBS(1000),mPRED(1000), &
                       AIC,AICC,BIC,FULL,LESSCONSTANTS
      CHARACTER ch2*21
      CHARACTER (LEN=10) DUMMY(3)
      EXTERNAL OBJECTIVE, RAN1

	  infile(3) = 'C:\temp\t.R'
	  infile(4) = 'C:\temp\t.p'
	  infile(5) = 'C:\temp\t.e'

      CALL DATE_AND_TIME(DUMMY(1),DUMMY(2),DUMMY(3),DATE_VALUES)
      OTH=2 ; EFFORT_DISCREPANCY=0 ; SUM_EFFORT_DISCREPANCY=0 ; DEV_EFF=0 ; DISCREPANCY=0

!     Compute probability of the data given the data to use for computing deviance
      I=0 ; CALL OBJECTIVE(X,SIG2,I) ; DEVIANCE=LIKELIHOODS
      DO BOX=1,NBOX
       DEV_TAG(box)=LIKE_TAG(box)
       DO G=1,NGEARS(BOX)
         DEV_EFFORT(g,box)=LIKE_EFFORT(g,box) ; DEV_EFF=DEV_EFF+LIKE_EFFORT(g,box)
      END DO ; END DO

!     Compute information criteria using full likelihood expression (with constants)
      CONSTANTS='Y'; CALL OBJECTIVE(PARM_EST,PARM_EST(PARM_KEY(0)+1),PARM_KEY(0))
      FULL=-LIKELIHOODS-POSTERIORS-CONSTRAINTS+PENALTY
      AIC  = 2.0*(FULL+DBLE(PARM_KEY(0)+N_Qs))
      AICc = 2.0*FULL+2.0*DBLE(PARM_KEY(0)+N_Qs)*(1.0+DBLE(PARM_KEY(0)+N_Qs+1)/DBLE(N_DATA-PARM_KEY(0)-N_Qs-1.0))
      BIC  = 2.0*FULL+(PARM_KEY(0)+N_Qs)*LOG(DBLE(N_DATA))

!     Drop constants in likelihood expressions for other statistics
      CONSTANTS='N'; CALL OBJECTIVE(PARM_EST,PARM_EST(PARM_KEY(0)+1),PARM_KEY(0))
      LESSCONSTANTS=-LIKELIHOODS-POSTERIORS-CONSTRAINTS+PENALTY
      DEVIANCE=-2.0*(LIKELIHOODS-DEVIANCE)
      CALL WRITE_PARAMETERS
!FLR      WRITE(*,*) 'WRITING TO FILE ',INFILE(3)
      DO BOX=1,NBOX
       DO G=1,NGEARS(BOX)
        IF(PDF_EFFORT(G,box)>0) THEN
          DO Y=1,LASTYEAR
            IF((PDF_EFFORT(g,box)>1 .AND. EFFORT_DATA(g,box,y)>-9) .OR. (PDF_EFFORT(g,box)==1 .AND. EFFORT_DATA(g,box,y)>0)) THEN
               CALL GETVARIANCE(EFFORT_DATA(g,box,y),EFFORT(g,box,y),SIGMA_EFFORT(g,box,y),SCALE_VARIANCE(g,box), &
                                PDF_EFFORT(g,box),SIG2)
               CALL GETDISCREPANCY(EFFORT_DATA(g,box,y),EFFORT(g,box,y),SIG2,PDF_EFFORT(g,box),EFFORT_DISCREPANCY(g,box,y))
               SUM_EFFORT_DISCREPANCY(g,box)=SUM_EFFORT_DISCREPANCY(g,box)+EFFORT_DISCREPANCY(g,box,y)
               DISCREPANCY=DISCREPANCY+EFFORT_DISCREPANCY(g,box,y)
            ENDIF
          END DO
        ENDIF
      END DO ; END DO
      OPEN(7,file=infile(3),status='unknown')
      OPEN(17,file=infile(5),status='unknown')
      WRITE(7,'(16X,A40)') '****************************************'
      WRITE(7,'(16X,A40)') '                VPA-2BOX                '
      WRITE(7,'(16X,A40)') 'SUMMARY STATISTICS AND DIAGNOSTIC OUTPUT'
      WRITE(7,'(16X,A40)') '****************************************'
      WRITE(7,*)
      WRITE(7,'(A50)') INFILE(0)     ! Title of run
      A=DATE_VALUES(6)/10 ; G=DATE_VALUES(6)-A*10 ; Y=DATE_VALUES(1) ;
      WRITE(7,'(I2,A1,I1,I1,A2,I2,A1,A9,A1,I4)') DATE_VALUES(5),':',A,G,', ',DATE_VALUES(3),' ',MONTH_NAME(DATE_VALUES(2)),' ',Y
      WRITE(7,*)
      WRITE(7,*) '======================================================================='
      OBS=0 ; PRED=0
      DO 20  BOX=1,NBOX
       DEV_TAG(box)=-2.0*(LIKE_TAG(box)-DEV_TAG(box))
       DO 20  G=1,NGEARS(BOX)
        OBS=OBS+LIKE_Q(g,box) ; PRED=PRED+LIKE_EFFORT(g,box) ; DEV_EFFORT(g,box)=-2.0*(LIKE_EFFORT(g,box)-DEV_EFFORT(g,box))
20    CONTINUE
!      IF((PARM_KEY(0)+N_Qs)/N_DATA>0.25) THEN
!        WRITE(*,'(/)')
!        WRITE(*,*) 'WARNING: The number of parameters (',PARM_KEY(0)+N_Qs,')'
!        WRITE(*,*) '         exceeds 25% of the number of data points (',N_DATA,')'
!        wRITE(*,*) '         Model performance is likely questionable and bootstrap'
!        wRITE(*,*) '         estimates of variance will probably be biased low'
!        WRITE(*,'(/)')
!        WRITE(7,'(/)')
!        WRITE(7,*) 'WARNING: The number of parameters (',PARM_KEY(0)+N_Qs,')'
!        WRITE(7,*) '         exceeds 25% of the number of data points (',N_DATA,')'
!        wRITE(7,*) '         Model performance is likely questionable and bootstrap'
!        wRITE(7,*) '         estimates of variance will probably be biased low'
!        WRITE(7,'(/)')
!      ENDIF
      CALL WRITE_CHAR(7,26,'Total objective function =',SPACE(1)) ; CALL WRITE_REAL(7,12,LESSCONSTANTS,ENDL)
      CALL WRITE_CHAR(7,26,'      (with constants)   =',SPACE(1)) ; CALL WRITE_REAL(7,12,FULL,ENDL)
      CALL WRITE_CHAR(7,26,'Number of parameters (P) =',SPACE(1)) ; CALL WRITE_INT(7,12,PARM_KEY(0)+N_Qs,ENDL)
      CALL WRITE_CHAR(7,26,'Number of data points (D)=',SPACE(1)) ; CALL WRITE_INT(7,12,N_DATA,ENDL)
      CALL WRITE_CHAR(7,26,'AIC : 2*objective+2P     =',SPACE(1)) ; CALL WRITE_REAL(7,12,AIC,ENDL)
      CALL WRITE_CHAR(7,26,'AICc: 2*objective+2P(...)=',SPACE(1)) ; CALL WRITE_REAL(7,12,AICc,ENDL)
      CALL WRITE_CHAR(7,26,'BIC : 2*objective+Plog(D)=',SPACE(1)) ; CALL WRITE_REAL(7,12,BIC,ENDL)
      CALL WRITE_CHAR(7,26,'Chi-square discrepancy   =',SPACE(1)) ; CALL WRITE_REAL(7,12,DISCREPANCY,ENDL)
      WRITE(7,*)
      CALL WRITE_CHAR(7,26,'Loglikelihoods (deviance)=',SPACE(1)) ; CALL WRITE_REAL(7,12,LIKELIHOODS,SPACE(1))
           CALL WRITE_CHAR(7,1,'(',SPACE(0)) ; CALL WRITE_REAL(7,12,DEVIANCE,SPACE(0)) ; CALL WRITE_CHAR(7,1,')',ENDL) ;
      CALL WRITE_CHAR(7,26,'   effort data           =',SPACE(1)) ; CALL WRITE_REAL(7,12,pred,SPACE(1)) ;
           CALL WRITE_CHAR(7,1,'(',SPACE(0)) ; CALL WRITE_REAL(7,12,-2.0*(PRED-DEV_EFF),SPACE(0)) ; CALL WRITE_CHAR(7,1,')',ENDL) ;
      IF(PDF_TAG>0) THEN
        CALL WRITE_CHAR(7,26,'   tagging data          =',SPACE(1)) ; CALL WRITE_REAL(7,12,LIKE_TAG(1)+LIKE_TAG(2),SPACE(1))
           CALL WRITE_CHAR(7,1,'(',SPACE(0)); CALL WRITE_REAL(7,12,DEV_TAG(1)+DEV_TAG(2),SPACE(0)); CALL WRITE_CHAR(7,1,')',ENDL);
      ENDIF
      WRITE(7,*)
      CALL WRITE_CHAR(7,26,'Log-posteriors           =',SPACE(1)) ; CALL WRITE_REAL(7,12,POSTERIORS         ,ENDL)
      CALL WRITE_CHAR(7,26,'   catchability          =',SPACE(1)) ; CALL WRITE_REAL(7,12,obs                ,ENDL)
      CALL WRITE_CHAR(7,26,'   f-ratio               =',SPACE(1)) ; CALL WRITE_REAL(7,12,LIKE_F(1)+LIKE_F(2),ENDL)
      CALL WRITE_CHAR(7,26,'   natural mortality     =',SPACE(1)) ; CALL WRITE_REAL(7,12,LIKE_M(1)+LIKE_M(2),ENDL)
      CALL WRITE_CHAR(7,26,'   mixing coeff.         =',SPACE(1)) ; CALL WRITE_REAL(7,12,LIKE_T(1)+LIKE_T(2),ENDL)
      IF(PDF_TAG>0) THEN
       CALL WRITE_CHAR(7,26,'   initial tag survival  =',SPACE(1)); CALL WRITE_REAL(7,12,LIKE_TAG_SURV(1)+LIKE_TAG_SURV(2)    ,ENDL)
       CALL WRITE_CHAR(7,26,'   tag shedding rate     =',SPACE(1)); CALL WRITE_REAL(7,12,LIKE_TAG_LOSS(1)+LIKE_TAG_LOSS(2)    ,ENDL)
       CALL WRITE_CHAR(7,26,'   tag reporting rate    =',SPACE(1)); CALL WRITE_REAL(7,12,LIKE_TAG_REPORT(1)+LIKE_TAG_REPORT(2),ENDL)
       CALL WRITE_CHAR(7,26,'   tag nomixing factor   =',SPACE(1)); CALL WRITE_REAL(7,12,LIKE_TAG_nomix(1)+LIKE_TAG_nomix(2)  ,ENDL)
      ENDIF
      WRITE(7,*)
      CALL WRITE_CHAR(7,26,'Constraints              =',SPACE(1));CALL WRITE_REAL(7,12,CONSTRAINTS                            ,ENDL)
      CALL WRITE_CHAR(7,26,'   terminal F            =',SPACE(1));CALL WRITE_REAL(7,12,LIKE_terminal(1)+LIKE_terminal(2)      ,ENDL)
      CALL WRITE_CHAR(7,26,'   stock-rec./sex ratio  =',SPACE(1));CALL WRITE_REAL(7,12,LIKE_recruitment(1)+LIKE_recruitment(2),ENDL)
      WRITE(7,*)
      CALL WRITE_CHAR(7,26,'Out of bounds penalty    =',SPACE(1)) ; CALL WRITE_REAL(7,12,-PENALTY,ENDL)
      WRITE(7,*) '======================================================================='
      Write(7,'(/)')

      DO 30 BOX=1,NBOX
        IF(NBOX>1) THEN
          IF(BOX==2) THEN ; OTH=1 ; ELSE ; OTH=2 ; ENDIF
          WRITE(7,'(50I1)') (BOX,I=1,50) ; WRITE(7,'(A20)') TITLE_FISHERY(box) ; WRITE(7,'(50I1)') (BOX,I=1,50) ; WRITE(7,'(/)')
        ENDIF
!       get stock abundance at the beginning of the year after last 
        DO 35 A=FIRSTAGE,LASTAGE
          IF(MODEL_TYPE==1) THEN ! diffusion and no-mixing models
              N_AREA(box,a+1,lastyear+1)=(N_AREA(box,a,lastyear)*(1-T(box,a,lastyear))+N_AREA(oth,a,lastyear)*T(oth,a,lastyear))* &
                                  EXP(-F(box,a,lastyear)-M(box,a,lastyear))
              IF(A==LASTAGE) N_AREA(box,a,lastyear+1)=N_AREA(box,a,lastyear+1)+N_AREA(box,a+1,lastyear+1)
            ELSE ! overlap model
              N_STOCK(box,a+1,lastyear+1)=N_STOCK(box,a,lastyear)*((1-T(box,a,lastyear))*EXP(-F(box,a,lastyear)-M(box,a,lastyear))+&
                                                     T(box,a,lastyear)*EXP(-F(oth,a,lastyear)-M(oth,a,lastyear)))
              IF(A==LASTAGE) N_STOCK(box,a,lastyear+1)=N_STOCK(box,a,lastyear+1)+N_STOCK(box,a+1,lastyear+1)
          ENDIF
35      CONTINUE
!       get stock-specific (model 1) or area-specific (model 2) abundances
        IF(NBOX>1) THEN    
          IF(MODEL_TYPE==1) THEN ! get stock specific abundance
              CALL GET_STOCK
            ELSE ! get area specific abundance
            DO Y=FIRSTYEAR,LASTYEAR
              DO A=FIRSTAGE,LASTAGE ; N_AREA(box,a,y)=N_STOCK(box,a,y)*(1-T(box,a,y))+N_STOCK(oth,a,y)*T(oth,a,y) ; END DO
            END DO
          ENDIF
        ENDIF
        IF(NBOX>1) THEN
            write(7,'(A8,I1,A28,A50)') 'TABLE 1.',box,' FISHING MORTALITY RATE FOR ',TITLE_FISHERY(box)
          ELSE
            write(7,'(A36,A50)') 'TABLE 1. FISHING MORTALITY RATE FOR ',TITLE_FISHERY(box)
        ENDIF
        write(7,311) ('=======',A=FIRSTAGE,LASTAGE+1)
        write(17,*) 'F'
        write(7,301) (A,A=FIRSTAGE,LASTAGE)
        write(7,311) ('-------',A=FIRSTAGE,LASTAGE+1)
        DO Y=FIRSTYEAR,LASTYEAR ; write(7,300) Y+DISPLAYYEAR-1,(F(box,a,y),A=FIRSTAGE,LASTAGE) ; END DO
        DO Y=1,99
          IF(Y<=LASTYEAR) THEN ; write(17,300) Y+DISPLAYYEAR-1,(F(box,a,y),A=FIRSTAGE,LASTAGE) ; ELSE ; WRITE(17,*) ; ENDIF
        END DO
        write(7,311) ('=======',A=FIRSTAGE,LASTAGE+1)
        WRite(7,'(/)')
        IF(NBOX>1) THEN
            write(7,'(A8,I1,A54,A50)') 'TABLE 2.',box,' ABUNDANCE AT THE BEGINNING OF THE YEAR [BY AREA] FOR ',TITLE_FISHERY(box)
          ELSE
            write(7,'(A62,A50)') 'TABLE 2. ABUNDANCE AT THE BEGINNING OF THE YEAR [BY AREA] FOR ',TITLE_FISHERY(box)
        ENDIF
        write(7,211) ('=============',A=FIRSTAGE,LASTAGE+1)
        write(17,*) 'N by area'
        write(7,*)        
        write(7,201) (A,A=FIRSTAGE,LASTAGE)
        write(7,211) ('-------------',A=FIRSTAGE,LASTAGE+1)
        DO Y=FIRSTYEAR,LASTYEAR ; write(7,200) Y+DISPLAYYEAR-1,(N_AREA(box,a,y),A=FIRSTAGE,LASTAGE) ; END DO
          write(7,202) LASTYEAR+DISPLAYYEAR,(N_AREA(box,a,y),A=FIRSTAGE+1,LASTAGE)
        DO Y=1,99
          IF(Y<=LASTYEAR) THEN ; write(17,200) Y+DISPLAYYEAR-1,(N_AREA(box,a,y),A=FIRSTAGE,LASTAGE)
            ELSE ; WRITE(17,*) ; ENDIF
        END DO
        write(7,211) ('=============',A=FIRSTAGE,LASTAGE+1)
        WRite(7,'(/)')
        IF(NBOX==1 .OR. (LINK_STOCK < 1 .AND. MODEL_TYPE < 2)) GOTO 55
        write(17,*) 'N by stock'
        DO Y=1,99
          IF(Y<=LASTYEAR) THEN ; write(17,200) Y+DISPLAYYEAR-1,(N_STOCK(box,a,y),A=FIRSTAGE,LASTAGE) ; ELSE ; WRITE(17,*) ; ENDIF
        END DO
        IF(NBOX>1) THEN
            write(7,'(A9,I1,A55,A50)') 'TABLE 2a.',box,' ABUNDANCE AT THE BEGINNING OF THE YEAR [BY STOCK] FOR ',TITLE_FISHERY(box)
          ELSE
            write(7,'(A64,A50)') 'TABLE 2a. ABUNDANCE AT THE BEGINNING OF THE YEAR [BY STOCK] FOR ',TITLE_FISHERY(box)
        ENDIF
        write(7,211) ('=============',A=FIRSTAGE,LASTAGE+1)
        write(7,201) (A,A=FIRSTAGE,LASTAGE)
        write(7,211) ('-------------',A=FIRSTAGE,LASTAGE+1)
        DO Y=FIRSTYEAR,LASTYEAR ; write(7,200) Y+DISPLAYYEAR-1,(N_STOCK(box,a,y),A=FIRSTAGE,LASTAGE) ; END DO
           write(7,202) LASTYEAR+DISPLAYYEAR,(N_STOCK(box,a,y),A=FIRSTAGE+1,LASTAGE)
        IF(MODEL_TYPE==1 .AND. PLUSGROUP>1) WRITE(7,*) '     Note: The last age in this table does not represent a plus-group.'
        write(7,211) ('=============',A=FIRSTAGE,LASTAGE+1)
        WRite(7,'(/)')
55      IF(NBOX>1) THEN
            write(7,'(A8,I1,A10,A50)') 'TABLE 3.',box,' CATCH OF ',TITLE_FISHERY(box)
          ELSE
            write(7,'(A18,A50)') 'TABLE 3. CATCH OF ',TITLE_FISHERY(box)
        ENDIF
        write(7,211) ('=============',A=FIRSTAGE,LASTAGE+1)
        write(17,*) 'CATCH'
        write(7,201) (A,A=FIRSTAGE,LASTAGE)
        write(7,211) ('-------------',A=FIRSTAGE,LASTAGE+1)
        DO Y=FIRSTYEAR,LASTYEAR ; write(7,200) Y+DISPLAYYEAR-1,(CATCH_DATA(box,a,y),A=FIRSTAGE,LASTAGE) ; END DO
        write(7,211) ('=============',A=FIRSTAGE,LASTAGE+1)
        DO Y=1,99
          IF(Y<=LASTYEAR) THEN ; write(17,200) Y+DISPLAYYEAR-1,(CATCH_DATA(box,a,y),A=FIRSTAGE,LASTAGE) ; ELSE ; WRITE(17,*) ; ENDIF
        END DO
        write(17,*) 'WEIGHT FOR SSB'
        DO Y=1,99
          IF(Y<=LASTYEAR) THEN ; write(17,200) Y+DISPLAYYEAR-1,(WEIGHT_SSB(box,a,y),A=FIRSTAGE,LASTAGE) ; ELSE ; WRITE(17,*) ; ENDIF
        END DO
        WRite(7,'(/)')
        IF(NBOX>1) THEN
            write(7,'(A8,I1,A45,A50)') 'TABLE 4.',box,' SPAWNING STOCK FECUNDITY AND RECRUITMENT OF ',TITLE_FISHERY(box)
          ELSE
            write(7,'(A53,A50)') 'TABLE 4. SPAWNING STOCK FECUNDITY AND RECRUITMENT OF ',TITLE_FISHERY(box)
        ENDIF
        WRITE(7,*)     '============================================================='
        IF(PDF_STOCKRECRUIT/=0) THEN
            write(7,'(1x,a4,2x,3(a12,1X))')  '    ','  measure of','    recruits','   recruits'
            write(7,'(1x,a4,2x,3(a12,1X))')  'year','   fecundity','    from VPA','  S/R curve'
            WRITE(7,*) '---------------------------------------------'
            DO 70 Y=FIRSTYEAR,LASTYEAR
              IF(Y.GT.FIRSTAGE) THEN ; PRED=STOCK_RECRUIT(box,1)*SSB(box,Y-firstage)/(STOCK_RECRUIT(box,2)+SSB(box,Y-firstage))
                ELSE ; PRED=99999999999999999.0 ; ENDIF
              IF(MODEL_TYPE==1) THEN ; WRITE(7,200) Y+DISPLAYYEAR-1,SSB(BOX,Y),N_AREA(BOX,FIRSTAGE,Y),PRED
                ELSE ; WRITE(7,200) Y+DISPLAYYEAR-1,SSB(BOX,Y),N_STOCK(BOX,FIRSTAGE,Y),PRED ; ENDIF
70          CONTINUE
            write(7,*) '    std. dev. of random component',STOCK_RECRUIT(box,5)
            write(7,*) '    std. dev. of recruitment     ',STOCK_RECRUIT(box,5)/sqrt(1-STOCK_RECRUIT(box,4)**2)
          ELSE  
            write(7,'(1x,a4,2x,3(a12,1X))')  '    ','    spawning','    recruits'
            write(7,'(1x,a4,2x,3(a12,1X))')  'year','     biomass','    from VPA'
            WRITE(7,*) '--------------------------------'
            DO 71 Y=FIRSTYEAR,LASTYEAR
              IF(MODEL_TYPE==1) THEN ; WRITE(7,200) Y+DISPLAYYEAR-1,SSB(BOX,Y),N_AREA(BOX,FIRSTAGE,Y)
                ELSE ; WRITE(7,200) Y+DISPLAYYEAR-1,SSB(BOX,Y),N_STOCK(BOX,FIRSTAGE,Y) ; ENDIF
71          CONTINUE           
        ENDIF
        WRITE(7,*)     '======================================================='
        WRite(7,'(/)')
        
        IF(NBOX>1) THEN
            write(7,'(A8,I1,A24,A50)') 'TABLE 5.',box,' FITS TO INDEX DATA ',TITLE_FISHERY(box)
          ELSE
            write(7,'(A32,A50)') 'TABLE 5. FITS TO INDEX DATA FOR ',TITLE_FISHERY(box)
        ENDIF
        WRITE(7,*) '======================================================================='
        DO 80 g=1,NGEARS(box)
          WRITE(7,*)
          WRITE(7,'(4X,A20)') '--------------------'
          IF(NBOX>1) THEN
              IF(G<10) THEN ; WRITE(7,'(4X,A2,I1,A1,I1,A1,A50)') '5.',BOX,'.',G,' ',TITLE_EFFORT(g,box)
                ELSE ; WRITE(7,'(4X,A2,I1,A1,I2,A1,A50)') '5.',BOX,'.',G,' ',TITLE_EFFORT(g,box) ; ENDIF
            ELSE
              IF(G<10) THEN ; WRITE(7,'(4X,A2,I1,A1,A50)') '5.',G,' ',TITLE_EFFORT(g,box)
                ELSE ; WRITE(7,'(4X,A2,I2,A1,A50)') '5.',G,' ',TITLE_EFFORT(g,box) ; ENDIF
          ENDIF
          WRITE(7,'(4X,A20)') '--------------------'
          WRITE(7,'(4X,A20)') PDFNAME(PDF_EFFORT(g,box))
          IF(PDF_EFFORT(G,box).LE.0) GOTO 80
          SELECT CASE (BIO_EFFORT(g,box))
              CASE (2) ;     CH2='biomass               '
              CASE (3) ;     CH2='MAX(F) - F            '
              CASE (4) ;     CH2='MAX(Z) - Z            '
              CASE (5) ;     CH2='fishing mortality rate'
              CASE (6) ;     CH2='total mortality rate  '
              CASE DEFAULT ; CH2='numbers               '
          END SELECT
          IF(SEASON_EFFORT(g,box)<0) THEN
              WRITE(7,'(4X,A8,A21)') 'average ',CH2
            ELSE
              WRITE(7,'(4X,A5,1X,I2,1X,A7)') 'month',SEASON_EFFORT(g,box),CH2
          ENDIF
          write(7,'(4x,a5,i2,1x,a1,1x,i2)') 'Ages ',age_EFFORT(1,g,box),'-',age_EFFORT(2,g,box)
          write(7,'(4x,a21,F12.2)') 'log-likelihood     = ',LIKE_EFFORT(g,box)
          write(7,'(4x,a21,F12.2)') 'deviance           = ',DEV_EFFORT(g,box)
          write(7,'(4x,a21,F12.2)') 'Chi-sq. discrepancy= ',SUM_EFFORT_DISCREPANCY(g,box)
          write(7,*)
          IF (PDF_EFFORT(g,box)==1) then
             write(7,401) '    ','           ','           ',' Residuals ',' Standard  ','     Q     ','Untransfrmd','Untransfrmd',&
                                 'Chi-square '
             write(7,401) 'Year','  Observed ',' Predicted ',' (Obs-pred)',' Deviation ',' Catchabil.','  Observed ',' Predicted ',&
                                 'Discrepancy'
             write(7,401) '----','-----------','-----------','-----------','-----------','-----------','-----------','-----------',&
                                 '-----------'
           ELSE
             write(7,401) '    ','           ','           ',' Residuals ',' Standard  ','     Q     ','Chi-square '
             write(7,401) 'Year','  Observed ',' Predicted ',' (Obs-pred)',' Deviation ',' Catchabil.','Discrepancy'
             write(7,401) '----','-----------','-----------','-----------','-----------','-----------','-----------'
          ENDIF
          DO Y=1,LASTYEAR
            IF((PDF_EFFORT(g,box)>1 .AND. EFFORT_DATA(g,box,y)>-9) .OR. (PDF_EFFORT(g,box)==1 .AND. EFFORT_DATA(g,box,y)>0)) THEN
               CALL GETVARIANCE(EFFORT_DATA(g,box,y),EFFORT(g,box,y),SIGMA_EFFORT(g,box,y),SCALE_VARIANCE(g,box), &
                                PDF_EFFORT(g,box),SIG2)
               IF(PDF_EFFORT(g,box)==1) THEN
                 OBS=DLOG(EFFORT_DATA(g,box,y))
                 IF(EFFORT(g,box,y)<=0) THEN ; PRED=-999999999999999999.0 ; ELSE ; PRED=DLOG(EFFORT(g,box,y)) ; ENDIF
                 WRITE(7,400) y+DISPLAYYEAR-1,OBS,PRED,OBS-PRED,dSQRT(SIG2),Q_EFFORT(g,box,y)*INDEX_MEAN(g,box), &
                              EFFORT_DATA(g,box,y)*INDEX_MEAN(g,box),EFFORT(g,box,y)*INDEX_MEAN(g,box),EFFORT_DISCREPANCY(g,box,y)
                ELSE
                 OBS=EFFORT_DATA(g,box,y) ; PRED=EFFORT(g,box,y)
                 WRITE(7,400) y+DISPLAYYEAR-1,OBS,PRED,OBS-PRED,dSQRT(SIG2),Q_EFFORT(g,box,y)*INDEX_MEAN(g,box), &
                              EFFORT_DISCREPANCY(g,box,y)
               ENDIF
            ENDIF
          END DO
          WRITE(7,*)
          write(7,'(4x,a34)') 'Selectivities by age              '
          write(7,501) 'Year',(a,a=AGE_EFFORT(1,g,box),AGE_EFFORT(2,g,box))
          write(7,499) '----',('-----',a=AGE_EFFORT(1,g,box),AGE_EFFORT(2,g,box))
          DO 82 YEAR=FIRSTYEAR,LASTYEAR
           IF((PDF_EFFORT(g,box)>1 .AND. EFFORT_DATA(g,box,year)>-9) .OR. (PDF_EFFORT(g,box)==1 .AND. EFFORT_DATA(g,box,year)>0)) &
             write(7,500) YEAR+DISPLAYYEAR-1,(SEL_EFFORT(g,box,a,year),a=AGE_EFFORT(1,g,box),AGE_EFFORT(2,g,box))
82        CONTINUE
          write(7,*)
80      CONTINUE        
        WRITE(7,*) '======================================================================='

        write(17,*) 'Indices (paired columns with observed and predicted values)'
        WRITE(17,251) '# ',(G,G,G=1,NGEARS(box))
        DO Y=1,98
          IF(Y<=LASTYEAR) THEN
            DO G=1,NGEARS(box)
              IF(EFFORT_DATA(g,box,y)<0.OR.(PDF_EFFORT(g,box)==1.and.EFFORT_DATA(g,box,y)<=0.0)) THEN
                  mOBS(g)=-9.0; mPRED(g)=-9.0
                ELSE
                  IF(PDF_EFFORT(g,box)==1) THEN
                      mOBS(g)=DLOG(EFFORT_DATA(g,box,y)) ; mPRED(g)=DLOG(EFFORT(g,box,y))
                    ELSE
                      mOBS(g)=EFFORT_DATA(g,box,y) ; mPRED(g)=EFFORT(g,box,y)
                  ENDIF
              ENDIF
            END DO
            write(17,250) Y+DISPLAYYEAR-1,(mOBS(g),mPRED(g),g=1,ngears(box))
           ELSE ; WRITE(17,*) ; ENDIF
        END DO

      
        IF(PDF_TAG>=1) THEN
          WRITE(7,'(/)')
          IF(NBOX>1) THEN
              write(7,'(A8,I1,A40,A50)') 'TABLE 6.',box,' FITS TO TAGS RELEASED IN HOME RANGE OF ',TITLE_FISHERY(box)
            ELSE
              write(7,'(A48,A50)') 'TABLE 6. FITS TO TAGS RELEASED IN HOME RANGE OF ',TITLE_FISHERY(box)
          ENDIF
          WRITE(7,'(4X,A20)') PDFNAME(PDF_TAG)
          WRITE(7,'(4x,a17,F12.2)') 'log-likelihood = ',LIKE_TAG(box)
          WRITE(7,'(4x,a17,F12.2)') 'deviance       = ',DEV_TAG(box)
          WRITE(7,*)
          WRITE(7,'(4x,A10,2x,a9,14x,a36)') ' Release  ','Recapture','Recaptures by year following release'
          WRITE(7,'(4x,A4,1x,A4,5x,a4,6x,a4,4x,50(2x,i4,2x))') 'Year',' Age','Area','Type',(y,y=firstyear,lastyear)
          WRITE(7,*) '======================================================================='
          DO B_REC=1,NBOX
            DO A_REL=FIRSTAGE,LASTAGE
              DO Y_REL=FIRSTYEAR,LASTYEAR
                  IF(NEWTAGS(box,a_rel,y_rel)>0) THEN
                    WRITE(7,'(4x,i4,1x,i4,3x,i4,8x,a4,4x,50F8.2)') y_rel+DISPLAYYEAR-1,a_rel,b_rec,'obsd', &
                                                             (Rdata(box,b_rec,a_rel,y_rel,y),y=y_rel,lastyear)
                    WRITE(7,'(28x,a4,4x,50F8.2)') 'pred',(R(box,b_rec,a_rel,y_rel,y),y=y_rel,lastyear)
                  ENDIF
              END DO ! Y_REL
            END DO ! A_REL
          END DO ! B_REC
          WRITE(7,*) '======================================================================='
        ENDIF
        WRITE(7,'(/)')

30    CONTINUE
      write(7,*) 'TOTAL NUMBER OF FUNCTION EVALUATIONS = ',ICALL

300   format(1x,i4,2x,50(f6.3,1X)) ; 301   format(5x,50(I6,1X)) ; 311   format(50(A7))
200   format(1x,i4,2x,50(f12.0,1X)) ; 201   format(3x,50(i12,1x)) ; 202   format(1x,i4,15x,50(f12.0,1X))
211   format(50(A13))
250   format(1x,i4,2x,1000(E10.3,1X))
251   format(7x,a2,1000(I6,5X))
400   format(4x,i4,4(2x,f11.3),2X,E11.3,3(2X,F11.3))
401   format(4x,a4,10(2x,a11))
499   format(4x,a4,3x,50(a6,1x)) ; 500   format(4x,i4,3x,50(f6.3,1x)) ; 501   format(4x,a4,3x,50(i4,3x))

1000  FORMAT(A27,F12.2) ; 1001 FORMAT(A27,D12.5)
      
      CLOSE(7) ; CLOSE(17)
END

subroutine VPA_INIT
      USE STATISTICS ; USE DATUM
      USE PARAMETERS, ONLY : INFILE,OPTION,SCALEs,MODEL_TYPE,PDF_TAG,NBOX,CV_OVERIDE,LINK_OLDEST,LINK_YOUNGEST,ICALL,FIRSTCALL
      IMPLICIT NONE

!     initialize all data (negative values mean no data)
      CATCH_DATA = 1.0 ; EFFORT_DATA = -9.0 ; SEL_EFFORT = 1.0 ; PSEL = -1.0 ; INDEX_MEAN=0 ; PDF_EFFORT=0 ; EFFORT_DATA_STORE=-1.0
      SIGMA_Q=0 ; SIGMA_F=0 ; SIGMA_M=0 ; SIGMA_T=0 ; SIGMA_TERMINAL=0 ; WEIGHT_CATCH=0 ; WEIGHT_SSB=0
      ETA_Q=0   ; ETA_F=0   ; ETA_M=0   ; ETA_T=0  ; ETA_TERMINAL=0
      SIGMA_TAG_REPORT=0 ; SIGMA_TAG_SURV=0 ; SIGMA_TAG_LOSS=0 ; ETA_TAG_REPORT=0 ; ETA_TAG_SURV=0 ; ETA_TAG_LOSS=0
      SIGMA_TAG_NOMIX2=0 ; SIGMA_TAG_NOMIX=0 ; ETA_TAG_NOMIX=0 ; ETA_TAG_NOMIX2=0 ; N_Qs=0


	  ETA_SR=0

	  ICALL      = 0
      FIRSTCALL  = 0
      OPTION(1)  = 1 ! One area
      MODEL_TYPE = 1 ! Single Stock
	  NBOX       = 1
end

subroutine VPA_INIT_
      USE STATISTICS ; USE DATUM
      USE PARAMETERS, ONLY : INFILE,OPTION,SCALEs,MODEL_TYPE,PDF_TAG,NBOX,CV_OVERIDE,LINK_OLDEST,LINK_YOUNGEST,ICALL,FIRSTCALL
      IMPLICIT NONE

!     initialize all data (negative values mean no data)
      CATCH_DATA = 1.0 
      EFFORT_DATA = -9.0 
      SEL_EFFORT = 1.0 
      PSEL = -1.0 
!      INDEX_MEAN=0 
!      PDF_EFFORT=0 
!  	   EFFORT_DATA_STORE=-1.0
!      SIGMA_Q=0 ; SIGMA_F=0 ; SIGMA_M=0 ; SIGMA_T=0 ; SIGMA_TERMINAL=0 ; WEIGHT_CATCH=0 ; WEIGHT_SSB=0
!      ETA_Q=0   ; ETA_F=0   ; ETA_M=0   ; ETA_T=0  ; ETA_TERMINAL=0
!      SIGMA_TAG_REPORT=0 ; SIGMA_TAG_SURV=0 ; SIGMA_TAG_LOSS=0 ; ETA_TAG_REPORT=0 ; ETA_TAG_SURV=0 ; ETA_TAG_LOSS=0
!      SIGMA_TAG_NOMIX2=0 ; SIGMA_TAG_NOMIX=0 ; ETA_TAG_NOMIX=0 ; ETA_TAG_NOMIX2=0 ; N_Qs=0

	  ETA_SR=0

	  ICALL      = 0
      FIRSTCALL  = 0
      OPTION(1)  = 1 ! One area
      MODEL_TYPE = 1 ! Single Stock
	  NBOX       = 1
end


!_________________________________________________________________      
      SUBROUTINE VPA_PARAMS(PARAM, NPARAM)  
! reads the parameters to be used in the objective function
!
!    Table of variables
!    -------------------
!    PARM_SPECS(I,J=1-5)    specifications for I'th parameter of model (1: lower bound,
!                           2: best estimate, 3: upper bound, 4: estimation indicator, 5: log-scale std. error)
!             PARM_SPECS(i,4) = 0      set to fixed value
!                               3 (.1) random deviation from previous parameter
!                               4 (.2) random deviation from STAR_VARIABLE
!                               2 (.3) random deviation from prior "best" estimate
!                               1      independent parameter without Bayesian prior imposed
!                              -0.1    set equal to value of previous estimated parameter (0.3 or 1 designation)
!                              -n      set equal to value of parameter number n
!    PARM_EST(K)            estimate of k'th estimable parameter
!    PARM_KEY(K)            index of parameter corresponding to k'th estimable parameter
!_________________________________________________________________      
	  USE PARAMETERS
      IMPLICIT NONE
      
	real*8  PARAM(6,2002)
	integer NParam

	  REAL (KIND=8) :: DEFAULTS(5)
      INTEGER :: I,K,J,REPETITIONS,L,LINE=0,METHOD, iParam
      I=1 ; K=0 ; ICALL=0   
    
	  Do iParam = 1, NPARAM     
		   DEFAULTS(1) = PARAM(1, iParam)
		   DEFAULTS(2) = PARAM(2, iParam)
		   DEFAULTS(3) = PARAM(3, iParam)
		   DEFAULTS(4) = PARAM(4, iParam)
		   DEFAULTS(5) = PARAM(5, iParam)
		   REPETITIONS = PARAM(6, iParam)
		   	
		   METHOD=INT(DEFAULTS(4))
           SELECT CASE (METHOD)  ! Convert new Bayesian designation to old style
                 CASE (2) ; DEFAULTS(4)=0.3
                 CASE (3) ; DEFAULTS(4)=0.1
                 CASE (4) ; DEFAULTS(4)=0.2
                 CASE DEFAULT
            END SELECT

            DO 2 L=1,REPETITIONS
              DO 3 J=1,5 ; PARM_SPECS(I,J)=DEFAULTS(J) ; 3 CONTINUE
!             identify estimable parameters and set them initial to the best guess
              IF(PARM_SPECS(I,4).GT.0) THEN
                K=K+1 ; PARM_KEY(K)=I
                PARM_EST(K)=PARM_SPECS(I,2)
              ENDIF
              I=I+1
2           CONTINUE
      end do 


10    PARM_KEY(0)=K     ! total number of parameters to be estimated 
      PARM_KEY(NPAR+1)=I-1 ! total number of parameters in model
!FLR      WRITE(*,'(1x,a31,i4)') 'Number of estimated parameters: ',PARM_KEY(0)
!FLR      WRITE(*,'(1x,a31,i4)') 'Total number of parameters:     ',PARM_KEY(NPAR+1)
      RETURN 
100   WRITE(*,'(1X,A26,A50)') 'ERROR: The parameter file ',INFILE(2)
      WRITE(*,'(8X,A55)') 'does not exist or is being used by another application.'  ; PAUSE ; STOP

      END

      SUBROUTINE VPA_SCALECPUE  
! reads the data to be used in the objective function
!-----------------------------------------------------------------
      USE STATISTICS ; USE DATUM
      USE PARAMETERS, ONLY : INFILE,OPTION,SCALEs,MODEL_TYPE,PDF_TAG,NBOX,CV_OVERIDE,LINK_OLDEST,LINK_YOUNGEST
      IMPLICIT NONE
      INTEGER :: G,Y,A,COUNT
    
!       scale indices by their mean
        IF(SCALEs.GT.0) THEN ! Scale indices by their mean (geometric in case of lognormal model)
          DO G=1,NGEARS(box)
            IF(PDF_EFFORT(g,box)==12) exit
			INDEX_MEAN(g,box) = 0.0
            COUNT=0
            DO 83 Y=1,LASTYEAR
              IF(PDF_EFFORT(g,box)>1 .and. EFFORT_DATA(g,box,y)>=0) THEN
                  INDEX_MEAN(g,box)=INDEX_MEAN(g,box)+EFFORT_DATA(g,box,y) ; COUNT=COUNT+1
                ELSEIF(PDF_EFFORT(g,box)==1 .and. EFFORT_DATA(g,box,y)>0) THEN
                  INDEX_MEAN(g,box)=INDEX_MEAN(g,box)+dLOG(EFFORT_DATA(g,box,y)) ; COUNT=COUNT+1
              ENDIF
83          CONTINUE
            INDEX_MEAN(g,box)=INDEX_MEAN(g,box)/COUNT
            IF(PDF_EFFORT(g,box)==1) INDEX_MEAN(g,box)=DEXP(INDEX_MEAN(g,box))
            DO 84 Y=1,LASTYEAR
              IF(EFFORT_DATA(g,box,y)>0 .OR. (PDF_EFFORT(g,box)>1 .AND. EFFORT_DATA(g,box,y)>=0)) THEN
                EFFORT_DATA(g,box,y)=EFFORT_DATA(g,box,y)/INDEX_MEAN(g,box)  
                EFFORT_DATA_STORE(g,box,y)=EFFORT_DATA(g,box,y)/INDEX_MEAN(g,box)
              ENDIF
84          CONTINUE
          END DO
         ELSE ! Do not scale by the mean
          INDEX_MEAN=1
        ENDIF

	END

subroutine VPA_CHECK(Params,NParam)

real*8  Params(6,2002)
integer NParam

open(unit=111,file='c:\temp\param.txt')
write (111,'(6(a12,a1))') 'lower',',','best',',','upper',',','pdf',',','prior',',','reps'
Do i = 1,NParam
write (111,'(6(f12.4,a1))') (Params(j,i),',',j=1,6)
enddo
close(111)

return
END

subroutine VPA_CHECK_CPUE
   USE PARAMETERS; USE STATISTICS; USE DATUM

	open(unit=111,file='c:\temp\a.txt')
	
	write (111,'(3(a12,a1))') 'Year',',','Index',',','CPUE'

	Do j = 1,NGEARS(1)
		Do i = FIRSTYEAR,LASTYEAR
			write (111,'(2(i8,a1),f8.4,a1)') i,',',j,',',EFFORT_DATA(j,1,i),','
		enddo
	enddo
	
	close(111)

return
END


subroutine VPA_CHECK_SEL(k)
   USE PARAMETERS; USE STATISTICS; USE DATUM
   
   IMPLICIT NONE
   integer k

   integer i,j,a

    if(k.eq.1) then
		open(unit=111,file='c:\temp\a1.csv')
	else 
      	open(unit=111,file='c:\temp\a2.csv')
	endif
	write (111,'(4(a12,a1))') 'Year',',','Age',',','Index',',','CPUE',','

	Do j = 1,NGEARS(1)
		Do i = FIRSTYEAR,LASTYEAR
		    Do a=AGE_EFFORT(1,j,1),AGE_EFFORT(2,j,1)
				write (111,'(3(i12,a1),f16.4,a1)') i,',',a,',',j,',',PSEL(j,1,a,i),','
			enddo
		enddo
	enddo
	
	close(111)

return
END

subroutine VPA_PARAMETERS_OUTPUT();
      USE PARAMETERS, ONLY : INFILE
      INFILE(4) = "c:\temp\p1.txt";
      CALL WRITE_PARAMETERS
end
subroutine VPA_PARAMETERS_OUTPUT2();
      USE PARAMETERS, ONLY : INFILE
      INFILE(4) = "c:\temp\p2.txt";
      CALL WRITE_PARAMETERS
end

!------------------------------------------------------------------------------
integer function VPA_WRITE_PARAMETERS(PARAM_VALS)
!------------------------------------------------------------------------------
      USE STATISTICS ; USE PARAMETERS, ONLY : INFILE,NBOX,OPTION,PDF_TAG
      IMPLICIT NONE
	  REAL*8 PARAM_VALS(11,200)
      INTEGER I,K,A,Y,g
      K=0 ; I=1

      DO BOX=1,NBOX

       ! terminal age structure of population'
       DO A=FIRSTAGE,LASTAGE-1 ; CALL VPA_WRITEPAR(I,K,1,PARAM_VALS) ; END DO
       
       ! terminal-age f-ratios'
       DO Y=FIRSTYEAR,LASTYEAR ; CALL VPA_WRITEPAR(I,K,2,PARAM_VALS) ; END DO

       ! natural mortality'
       DO A=FIRSTAGE,LASTAGE ; CALL VPA_WRITEPAR(I,K,3,PARAM_VALS) ; END DO

       ! transfer coefficients'
       DO A=FIRSTAGE,LASTAGE ; CALL VPA_WRITEPAR(I,K,4,PARAM_VALS) ; END DO

       ! Stock recruitment relationship
       DO A=1,5 ; CALL VPA_WRITEPAR(I,K,5,PARAM_VALS) ; END DO
      
       ! Variance scaling parameters'
       DO G=1,NGEARS(box) ; CALL VPA_WRITEPAR(I,K,6,PARAM_VALS) ; END DO

       ! Catchability coefficients
       IF(OPTION(4)>0) THEN
           DO G=1,NGEARS(box)
             DO y=FIRSTYEAR,LASTYEAR ; CALL VPA_WRITEPAR(I,K,7,PARAM_VALS) ; END DO
100        END DO
       ENDIF

       IF(PDF_TAG>0) THEN

        ! fraction surviving the intial tagging process'
        DO A=FIRSTAGE,LASTAGE ; CALL VPA_WRITEPAR(I,K,8,PARAM_VALS) ; END DO

        ! tag shedding rate'
        DO A=FIRSTAGE,LASTAGE ; CALL VPA_WRITEPAR(I,K,9,PARAM_VALS) ; END DO

        ! tag reporting rate'
        DO Y=FIRSTYEAR,LASTYEAR ; CALL VPA_WRITEPAR(I,K,10,PARAM_VALS) ; END DO

        ! tag nonmixing factor for first year'
        DO Y=FIRSTYEAR,LASTYEAR
          DO A=FIRSTAGE,LASTAGE ; I = -I ; CALL VPA_WRITEPAR(I,K,11,PARAM_VALS) ; END DO ! negative I tells program to write year and age
        END DO

        ! tag nonmixing factor for second year'
        DO Y=FIRSTYEAR,LASTYEAR
          DO A=FIRSTAGE,LASTAGE ; I = -I ; CALL VPA_WRITEPAR(I,K,12,PARAM_VALS) ; END DO ! negative I tells program to write year and age
        END DO

       ENDIF

      END DO ! BOX

      ! Number of parameters = ',i-1
	  VPA_WRITE_PARAMETERS = I
	END


SUBROUTINE VPA_WRITEPAR(I,K,PTYPE,PARAM_VALS)
!----------------------------------------------------------------------------        
! Writes the values of the parameters to a file
!
! Table of variables
!     PARM_SPECS(i,4) = 0      set to fixed value
!                       1      independent parameter without Bayesian prior imposed
!                       2 (.3) random deviation from prior "best" estimate
!                       3 (.1) random deviation from previous parameter
!                       4 (.2) random deviation from reference parameter
!                      -0.1    set equal to value of previous estimated parameter (0.3 or 1 designation)
!                      -n      set equal to value of parameter number n
!
!----------------------------------------------------------------------------
      USE PARAMETERS ; USE STATISTICS, ONLY : FIRSTYEAR, LASTYEAR, DISPLAYYEAR, FIRSTAGE, LASTAGE
      IMPLICIT NONE
	  real*8 PARAM_VALS(11,200)

      INTEGER :: I,K,J,L,WRITE_YRAGE=0,A,Y,PTYPE
      REAL (KIND=8) :: VARIABLE,CV,VARIANCE,METHOD
      CHARACTER (LEN=5) :: BOUND

      IF(I>=0) THEN
          WRITE_YRAGE=0
        ELSE
          IF(WRITE_YRAGE==0) THEN ; Y=FIRSTYEAR+DISPLAYYEAR-1 ; A=FIRSTAGE ; WRITE_YRAGE=1 ; ENDIF
          I=ABS(I)
      ENDIF
      CV=-1
      SELECT CASE (INT(PARM_SPECS(I,4)*10))
          CASE (3)     ; METHOD=2
          CASE (1)     ; METHOD=3
          CASE (2)     ; METHOD=4
          CASE DEFAULT ; METHOD=PARM_SPECS(I,4)
      END SELECT
      IF(PARM_SPECS(I,4)==0) THEN ! parameter set to fixed value
          VARIABLE=PARM_SPECS(I,2) 
        ELSEIF(PARM_SPECS(I,4)>0) THEN 
          K=K+1 ; VARIABLE=PARM_EST(K)
          VARIANCE=PARM_VAR(K)
          IF(VARIANCE>0) CV=100*ABS(SQRT(VARIANCE)/VARIABLE)
        ELSEIF(PARM_SPECS(I,4).LT.0 .AND. PARM_SPECS(I,4).GT.-1) THEN ! Equate to most recently estimated parameter value
          VARIABLE=PARM_EST(K)
        ELSEIF(PARM_SPECS(I,4).LE.-1) THEN ! Equate to value of parameter n
          J=INT(ABS(parm_specs(i,4)))
          IF(PARM_SPECS(J,4).EQ.0) THEN; VARIABLE=PARM_SPECS(J,2) 
            ELSEIF(PARM_SPECS(J,4)>0) THEN;
              DO L=1,PARM_KEY(0); IF(PARM_KEY(L).EQ.J) EXIT ; END DO ; VARIABLE=PARM_EST(L)
          ENDIF
      ENDIF
      IF((VARIABLE<=1.01*PARM_SPECS(I,1) .OR. VARIABLE>=0.99*PARM_SPECS(I,3)) .AND. PARM_SPECS(I,4)>=1) THEN 
        BOUND='BOUND' ; ELSE ; BOUND=' '
      ENDIF
      IF(WRITE_YRAGE==0) THEN
          IF(CV<=0) THEN 
			 PARAM_VALS(1,I)  = PARM_SPECS(I,1)
			 PARAM_VALS(2,I)  = VARIABLE
			 PARAM_VALS(3,I)  = PARM_SPECS(I,3)
			 PARAM_VALS(4,I)  = METHOD
			 PARAM_VALS(5,I)  = PARM_SPECS(I,5)
			 PARAM_VALS(6,I)  = I
        	 PARAM_VALS(11,I) = PTYPE
          ELSE 
			 PARAM_VALS(1,I)  = PARM_SPECS(I,1)
			 PARAM_VALS(2,I)  = VARIABLE
			 PARAM_VALS(3,I)  = PARM_SPECS(I,3)
			 PARAM_VALS(4,I)  = METHOD
			 PARAM_VALS(5,I)  = PARM_SPECS(I,5)
			 PARAM_VALS(6,I)  = I
			 PARAM_VALS(7,I)  = k
	  		 PARAM_VALS(8,I)  = CV
	     	 PARAM_VALS(11,I) = PTYPE
       		 !BOUND
          ENDIF
        ELSE
          IF(CV<=0) THEN 
			  PARAM_VALS(1,I)  = PARM_SPECS(I,1)
			  PARAM_VALS(2,I)  = VARIABLE
			  PARAM_VALS(3,I)  = PARM_SPECS(I,3)
			  PARAM_VALS(4,I)  = METHOD
			  PARAM_VALS(5,I)  = PARM_SPECS(I,5)
			  PARAM_VALS(6,I)  = I
			  PARAM_VALS(7,I)  = Y
			  PARAM_VALS(8,I)  = A
	     	  PARAM_VALS(11,I) = PTYPE
       	      !BOUND
            ELSE 
			  PARAM_VALS(1,I)  = PARM_SPECS(I,1)
			  PARAM_VALS(2,I)  = VARIABLE
			  PARAM_VALS(3,I)  = PARM_SPECS(I,3)
			  PARAM_VALS(4,I)  = METHOD
			  PARAM_VALS(5,I)  = PARM_SPECS(I,5)
			  PARAM_VALS(6,I)  = I
			  PARAM_VALS(7,I)  = Y
			  PARAM_VALS(8,I)  = A
			  PARAM_VALS(9,I)  = k
			  PARAM_VALS(10,I) = CV
			  PARAM_VALS(11,I) = PTYPE
			  !BOUND
          ENDIF
          IF(A<LASTAGE) THEN ; A=A+1 ; ELSE ; A=FIRSTAGE ; Y=Y+1 ; ENDIF
      ENDIF
      I=I+1
	RETURN 
END