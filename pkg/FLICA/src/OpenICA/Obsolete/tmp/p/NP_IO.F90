!                                                                                                                                   
!    I/O Routines for version 3 of ICPROJ                                                                                           
!                                                                                                                                   
!                                                                                                                                   
! ////////////////////////////////////////////////////////////////////                                                              
                                                                                                                                    
      Subroutine  ReadProjFile ! Read Projection File                                                                               
                                                                                                                                    
! ///////////////////////////////////////////////////////////////////                                                               
      implicit none                                                                                                                 
      Include 'INDAT.INC'                                                                                                           
      Include 'PROJC3.INC'                                                                                                          
      Include 'MESSAGE1.INC'

      character*77 Text(3)
      integer year, iyear, age, iage, ioerr, ifleet
      character*132 line                                                                                                            
      character*40 filename                                                                                                         
      logical debug
      debug = .true.

! -------------------- EXECUTABLE CODE -----------------------------                                                                
                                                                                                                                    
!      write(*,*)                                                                                                                    
      filename = 'x'
122   continue                                                                                                                      
      ioerr = 1                                                                                                                     

      do while ((filename .ne. ' ') .and. (ioerr .ne. 0))                                                                           
                                                                                                                                    
        if (filename .eq. 'x') then
          Call Screen_in_s(HW(43,Language),filename)
        else
          Call Screen_in_s(HW(44,Language),filename)
          if (filename .eq. ' ') then                                                                                               
            stop                                                                                                                    
          endif                                                                                                                     
        endif                                                                                                                       
        open(12, file= filename, iostat=ioerr, status = 'old',          &                                                           
          recl=6000)                                                                                                                
        if (ioerr .ne. 0) goto 122                                                                                                  
        read(12,120) line
        if (debug) write (*,*) line
        read(12,120) line                                                                                                           
        read(12,*) NFleet, YtPro                                                                                                    
        if (debug) write (*,*) line
        YtPro=YtPro+1 ! because the NPro(1) = N(lastyear)
        read(12,120) line                                                                                                           
        if (debug) write (*,*) line     
        read(12,120) line                                                                                                           
        if (debug) write (*,*) line
        do iage = 1, lastage-firstage+1
          read(12,*) age, (CRatio(ifleet,iage), ifleet =1,Nfleet)   !------- catch ratio by fleet and age in last year of analysis  
        enddo                                                                                                                       
                                                                                                                                    
        read(12,120) line                                                                                                           
        if (debug) write (*,*) line
        do iage = 1, lastage-firstage+1
          read(12,*) age, (Retention(ifleet,iage), ifleet =1,Nfleet)   !------- catch ratio by fleet and age in last year of analysi
        enddo                                                                                                                       
                                                                                                                                    
        read(12,120) line                                                                                                           
        if (debug) write (*,*) line
        do iyear = 2, YtPro
          read(12,*) year, (ConsCat(ifleet, iyear),ifleet=1,Nfleet) !------ F multiplier by fleet and year                          
          if (year .ne. lastyear+iyear-1) then                                                                                      
           write(*,121)'Error in years specified for projection. These' &    
       //' should start at ',lastyear+1
121        format(' ',A63,1X,I4)                                                                                                    
          endif                                                                                                                     
                                                                                                                                    
         enddo                                                                                                                      
                                                                                                                                    
!        do ifleet = 1, Nfleet                                                                                                      
!          Conscat(ifleet, YtPro+1) = Conscat(ifleet, YtPro)                                                                        
!        enddo                                                                                                                      
                                                                                                                                    
        read(12,120) line                                                                                                           
        if (debug) write (*,*) line
        do iage = 1, lastage-firstage+1
          read(12,*) age, (CWPro(ifleet, iage), ifleet=1, Nfleet)    !------ catch weight in projections                            
        enddo                                                                                                                       
        read(12,120) line                                                                                                           
        do iage = 1, lastage-firstage+1                                                                                             
          read(12,*) age, (DiscWt(ifleet, iage), ifleet=1, Nfleet)    !------ catch weight in projections                           
        enddo                                                                                                                       
                                                                                                                                    
!       Read in TargetFM and CVTarg                                                                                                 
        read(12,120) line                                                                                                           
        if (debug) write (*,*) line
        read(12,*) ftyear
        read(12,120) line                                                                                                           
        if (debug) write (*,*) line
        do iyear = ftyear-lastyear+1, YtPro
          read(12,*) year,(TargetFM(ifleet,iyear), ifleet =1,Nfleet)                                                                
          if (year .ne. lastyear+iyear-1) then
            Text(1) = HW(45,Language)
            Text(2) =HW(46,Language)
            Call Screen_out_a(Text,3,2)
            stop                                                                                                                    
          endif                                                                                                                     
        enddo                                                                                                                       
        read(12,120) line                                                                                                           
        if (debug) write (*,*) line
        do iyear = ftyear-lastyear+1, YtPro
          read(12,*) year,(CVTarg(ifleet,iyear), ifleet =1,Nfleet)                                                                  
          if (year .ne. lastyear+iyear-1) then
            Text(1)= HW(45, language)
            Text(2)=HW(47, Language)
            Call Screen_out_a(Text,3,2)
            stop                                                                                                                    
          endif                                                                                                                     
        enddo                                                                                                                       
      close(12)                                                                                                                     
      enddo                                                                                                                         
                                                                                                                                    
                                                                                                                                    
100   format(' ',A38, ' --> ')  ! \                                                                                                 
110   format(A40)                                                                                                                   
120   format(A132)                                                                                                                  
130   format(A13,1X,I4,1X,A15,1X,A30)                                                                                               
140   format(A29,' --> ') ! \                                                                                                       
                                                                                                                                    
      return                                                                                                                        
      end                                                                                                                           
                                                                                                                                    
                                                                                                                                    
! /////////////////////////////////////////////////////////////////////////                                                         
                                                                                                                                    
      Subroutine ChkCatRatio ! Check catch ratios                                                                                   
                                                                                                                                    
! /////////////////////////////////////////////////////////////////////////                                                         
!                                                                                                                                   
!      The ratios of catches in number by fleet at each age should sum to 1 at                                                      
!          each age, but it is easy to make an error here. If such an error is                                                      
!          made then fish will either be 'invented' or lost from the projection.                                                    
!       The routine makes sure that the ratios do sum to exactly 1. A 5% tolerance                                                  
!          is allowed as an error in typing the file, which is then rounded so                                                      
!          that the ratio sums to one exactly. If the sum of ratios is further than 5%                                              
!          from 1, it is assumed that a significant error has been made in the file and                                             
!          the programme stops with a warning message.                                                                              
!                                                                                                                                   
!                                                                                                                                   
      implicit none                                                                                                                 
      Include 'INDAT.INC'                                                                                                           
      Include 'PROJC3.INC'
      include 'MESSAGE1.INC'
      integer iage, ifleet                                                                                                          
      character*77 Text(3)   
      double precision RatioSum                                                                                                     
                                                                                                                                    
      do iage = 1,lastage-firstage+1                                                                                                
        RatioSum = 0.0                                                                                                              
        do ifleet = 1, Nfleet                                                                                                       
          RatioSum = RatioSum + CRatio(ifleet, iage)                                                                                
        enddo  ! fleets                                                                                                             
        If ((RatioSum .lt. 0.95) .or. (RatioSum .gt. 1.05)) then ! 5% rounding error allowed

          Text(1)=HW(48,Language)
          Call screen_out_a(Text,3,1)
          stop                                                                                                                      
        endif                                                                                                                       
                                                                                                                                    
        ! if it's within 5% of 1, the just round it off                                                                             
                                                                                                                                    
        do ifleet = 1, NFleet                                                                                                       
          CRatio(ifleet, iage) = CRatio(ifleet, iage) /RatioSum ! Ensures catch ratios sum exactly to 1                             
        enddo  ! fleets                                                                                                             
      enddo ! ages                                                                                                                  
                                                                                                                                    
      return                                                                                                                        
      end                                                                                                                           
                                                                                                                                    
                                                                                                                                    
! ////////////////////////////////////////////////////////////////////                                                              
                                                                                                                                    
      Subroutine SetProjVars                                                                                                        
                                                                                                                                    
! ///////////////////////////////////////////////////////////////////                                                               
                                                                                                                                    
!                                                                                                                                   
!    Stock weights, maturity ogives and natural mortality must be                                                                   
!    specified for the projections. The most usual way to set these is by                                                           
!    using a mean over historic data. As these have already been                                                                    
!    entered and are accessible in the INDAT common block, it seems                                                                 
!    appropriate to calculate these from the available data rather than                                                             
!    calculate them externally and re-enter them by hand.                                                                           
!                                                                                                                                   
!    This programme provides an option for calculating these three parameters                                                       
!    from a user-specified range of the historic data.                                                                              
!                                                                                                                                   
!                                                                                                                                   
!                                                                                                                                   
      implicit none                                                                                                                 
      include 'INDAT.INC'                                                                                                           
      include 'PROJC3.INC'                                                                                                          
      include 'MESSAGE1.INC'
      character*77 text(3)
      character*5 ytext
      integer iyear,iage, datyear                                                                                                   
      double precision sum, sumsq, value, SQSAFE, SAFELOG                                                                           
                                                                                                                                    
      Text(1)=HW(49,Language)
      Text(2)=HW(50,Language)
      CAll Screen_out_a(Text,3,2)

                                                                                                                                    
                                                                                                                                    
!        NATURAL MORTALITY                                                                                                          
                                                                                                                                    

      write(ytext,'(I4)') lastyear
      call concat(Text(1),HW(51,1),ytext)
      call concat(Text(1),Text(1),HW(52,1))
      datyear=lastyear-3
      call screen_in_i(Text(1),datyear,lastyear,firstyear,language)
                                                                                                                                    
                                                                                                                                   
      do iage = 1, lastage-firstage+1                                                                                               
        NMPro(iage) = 0.0                                                                                                           
      enddo                                                                                                                         
      do iage = 1, lastage-firstage+1                                                                                               
       do iyear = 1, lastyear-DatYear+1                                                                                             
         NMpro(iage)=NMPro(iage)+(NM(iyear,iage))/                      &                                                           
              dble(lastyear-DatYear+1)                                                                                              
       enddo                                                                                                                        
      enddo                                                                                                                         
                                                                                                                                    
!     MATURITY OGIVE                                                                                                                


      write(ytext,'(I4)') lastyear
      call concat(Text(1),HW(53,1),ytext)
      call concat(Text(1),Text(1),HW(52,1))
      datyear=lastyear-3
      call screen_in_i(Text(1),datyear,lastyear,firstyear,language)
                                                                                                                                    
      do iage = 1, lastage-firstage+1                                                                                               
        MOPro(iage) = 0.0d0                                                                                                         
        VarMat(iage) = 0.0d0                                                                                                        
        Sum = 0d0                                                                                                                   
        SumSQ = 0d0                                                                                                                 
        do iyear = DatYear-firstyear+1, lastyear-firstyear+1                                                                        
          value = DASIN( SQSAFE(dble(MO(iyear,iage))))                                                                              
          MOpro(iage)=MOPro(iage)+value                                                                                             
          Sum = Sum + value                                                                                                         
          if (DatYear .ne. lastyear) then                                                                                           
            SumSQ = SumSQ + value*value                                                                                             
          endif                                                                                                                     
                                                                                                                                    
       enddo   ! years                                                                                                              
       if (DatYear .ne. lastyear) then                                                                                              
         VarMat(iage) = SumSQ - (Sum*Sum)/dble(Lastyear-DatYear+1)                                                                  
         VarMat(iage) = SQSAFE(VarMat(iage)/dble(Lastyear-DatYear))                                                                 
       else                                                                                                                         
         VarMat(iage) = 0d0                                                                                                         
       endif                                                                                                                        
       MOPro(iage) = Sum/dble(Lastyear-Datyear+1)                                                                                   
      enddo    ! ages                                                                                                               
                                                                                                                                    
!     WEIGHT AT AGE IN THE STOCK                                                                                                    


      write(ytext,'(I4)') lastyear
      call concat(Text(1),HW(54,1),ytext)
      call concat(Text(1),Text(1),HW(52,1))

      datyear=lastyear-3
      call screen_in_i(Text(1),datyear,lastyear,firstyear,language)

                                                                                                                                    
      do iage = 1, lastage-firstage+1                                                                                               
        SWPro(iage) = 0d0                                                                                                           
        VarSW(iage) = 0d0                                                                                                           
       do iyear = datyear-firstyear+1, lastyear-firstyear+1                                                                         
         SWpro(iage)=SWPro(iage)+(safelog(dble(SW(iyear,iage))))                                                                    
         if (DatYear .ne. lastyear) then                                                                                            
          VarSW(iage)=VarSW(iage)+                                      &                                                           
             dble(safelog(dble(SW(iyear,iage)))*                        &                                                           
               safelog(dble(SW(iyear,iage))))                                                                                       
         else                                                                                                                       
           VarSW(iage) = 0d0                                                                                                        
         endif                                                                                                                      
       enddo   ! years                                                                                                              
       if (DatYear .ne. lastyear) then                                                                                              
         VarSW(iage)=VarSW(iage)- (SWPro(iage)*SWPro(iage))             &                                                           
            /dble(lastyear-Datyear+1)                                                                                               
         VarSW(iage)=sqsafe(VarSW(iage)/dble(lastyear-Datyear))                                                                     
       else                                                                                                                         
         VarSW(iage) = 0d0                                                                                                          
       endif                                                                                                                        
       SWPro(iage)=(SWPro(iage)/dble(lastyear-Datyear+1))                                                                           
      enddo    ! ages                                                                                                               
                                                                                                                                    
                                                                                                                                    
100   format(' ',A33,1X,I4,A9,' --> ') ! \                                                                                          
110   format(' ',A30,1X,I4,A9,' --> ') ! \                                                                                          
120   format(' ',A41,1X,I4,A9,' --> ') ! \                                                                                          
                                                                                                                                    
      return                                                                                                                        
      end                                                                                                                           
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
! /////////////////////////////////////////////////////////////////////                                                             
                                                                                                                                    
      Subroutine ReadVCV(VCV)                                                                                                       
                                                                                                                                    
! /////////////////////////////////////////////////////////////////////                                                             
                                                                                                                                    
!                                                                                                                                   
!      Reads the variance-covariance matrix from file ICA.VC to the                                                                 
!      matrix VCV.                                                                                                                  
!                                                                                                                                   
!                                                                                                                                   
      implicit none                                                                                                                 
      integer ioerr, i, j                                                                                                           
      INCLUDE 'INDAT.INC'                                                                                                           
      INCLUDE 'SEPMODEL.INC'
      include 'MESSAGE1.INC'
      character*77 text(1)
      double precision VCV(maxparm,maxparm)
      character*80 line                                                                                                             
      integer dummy                                                                                                                 
                                                                                                                                    
! --------------------- Executable Code -------------------------------                                                             
                                                                                                                                    
      Open(12, file = ICA_VC, status = 'OLD', iostat = ioerr,         &                                                           
       recl=10000 )
!      write(*,*) Nxparm
      read (12,10) line                                                                                                             
      read(12,*) (dummy , j = 1,Nxparm)                                                                                             
!      write(*,*) ' two lines ok...'   
                                                                                                                                    
10    format (A80)                                                                                                                  
      do i = 1, Nxparm                                                                                                              
        read(12, *) dummy, (VCV(i, j),  j = 1,Nxparm)
!        write(*,*) 'VCV: Parameters ',i
      enddo                                                                                                                         
      close (12)                                                                                                                    
      if (ioerr .ne. 0) then
        Text(1)=HW(55,Language)
        call Screen_out_a(Text,1,1)
        stop                                                                                                                        
      endif                                                                                                                         
                                                                                                                                    
      return                                                                                                                        
      end                                                                                                                           
                                                                                                                                    
                                                                                                                                    
! //////////////////////////////////////////////////////////////////////                                                            
                                                                                                                                    
      Subroutine ReadMBAL(MBAL)                                                                                                     
                                                                                                                                    
! //////////////////////////////////////////////////////////////////////                                                            
      implicit none                                                                                                                 
      double precision MBAL
      include 'MESSAGE1.INC'
                                                                                                                                    
!------------------------- EXECUTABLE CODE ------------------------------                                                           


      MBAL = 800000d0
      Call Screen_in_r(HW(56,1),MBAL,1d8,0d0,Language)
                                                                                                                                    
      return                                                                                                                        
      end                                                                                                                           
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                              
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
! ////////////////////////////////////////////////////////////////////                                                              
                                                                                                                                    
      Subroutine WriteNewSRR                                                                                                        
                                                                                                                                    
! ///////////////////////////////////////////////////////////////////                                                               
      implicit none                                                                                                                 
      include 'INDAT.INC'                                                                                                           
      include 'SEPMODEL.INC'                                                                                                        
      include 'SRR.INC'                                                                                                             
      include 'PROJC3.INC'                                                                                                          
                                                                                                                                    
      integer i, nosrrdata                                                                                                          
      double precision StockS(maxyear), Recruit(maxyear)                                                                            
                                                                                                                                    
      Double precision SRRFUNCT                                                                                                     
                                                                                                                                    
      NoSrrData = lastyear-firstyear+1                                                                                              
                                                                                                                                    
      Call GetSRR( StockS, Recruit, NoSRRdata)                                                                                      
                                                                                                                                    
      open(1, file = 'icapro.srr', status='unknown')                                                                                
      write(1,*) 'STOCK - RECRUIT RELATION PARAMETERS'                                                                              
                                                                                                                                    
      do i =1,8                                                                                                                     
        if (dabs(SRRPARM(i)) .gt. 1d-9) then                                                                                        
          write(1,100) i, SRRPARM(i)                                                                                                
        else                                                                                                                        
          write(1,100) i, 0d0                                                                                                       
        endif                                                                                                                       
      enddo                                                                                                                         
                                                                                                                                    
100   format(' ',I2,3X,E23.12)                                                                                                      
                                                                                                                                    
      write(1,*) 'Stock and Observed and Expected Recruitments by Year'                                                             
                                                                                                                                    
      do i = 1,NoSRRdata                                                                                                            
        write(1,200) i+firstyear-1,StockS(i),Recruit(i),                &                                                           
           SRRFUNCT(SRRPARM,StockS(i))                                                                                              
      enddo                                                                                                                         
                                                                                                                                    
200   format(' ',I4,2X,3(E23.12))                                                                                                   
                                                                                                                                    
      close(1)                                                                                                                      
                                                                                                                                    
      return                                                                                                                        
                                                                                                                                    
      end   ! of WriteNewSRR                                                                                                        
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                            
! ////////////////////////////////////////////////////////////////////                                                              
!                                                                                                                                   
      Subroutine Hello                                                                                                              
!                                                                                                                                   
! ////////////////////////////////////////////////////////////////////                                                              
                                                                                                                                    
      Character*77 text(20)

      integer i                                                                                                                     
                                                                                                                                    
                                                                                                                                    
      do i=1,5                                                                                                                      
        Text(i) = ' '
      enddo                                                                                                                         
                                                                                                                                    
      Text(6)= '                          Medium-Term Projections'                                                                
      Text(7)= '                          -----------------------'                                                                
      Text(8)=' '
      Text(9)=  '                                   ICP  ' 
      Text(10)=' '
      Text(11)='                               K.R. Patterson  '
      Text(12)='                         SOAEFD Marine Laboratory'                                                                 
      Text(13)= '                                  Aberdeen '                                                                      
      do i=14,16
        Text(i)=' '
      enddo                                                                                                                         
      Text(17)= '                     Written  December 1997 for ICA v1.4 w '  
      Text(18)= '                                Revision March 1999 '
      do i=19,20
        Text(i)= ' '
      enddo                                                                                                                         
                                                                                                                                    
      Call Screen_out_a(Text,20,20) 
      
      return                                                                                                                        
                                                                                                                                    
      end                                                                                                                           
                                                                                                                                    
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                          
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
! ///////////////////////////////////////////////////////////////////////                                                           
                                                                                                                                    
      Subroutine ReadMaxFMult                                                                                                       
                                                                                                                                    
! ///////////////////////////////////////////////////////////////////////                                                           
                                                                                                                                    
                                                                                                                                    
      include 'INDAT.INC'                                                                                                           
      include 'PROJC3.INC'                                                                                                          
      include 'MESSAGE1.INC'

                                                                                                                                    
      MaxFMult = 10.0d0                                                                                                            

      Call Screen_in_r(HW(57,Language),MaxFMult,20d0,1d0,Language)

                                                                                                                                    
      return                                                                                                                        
      end                                                                                                                           
                                                                                                                                    
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                           
                                                                                                                                    
                                                                                                                                    
! //////////////////////////////////////////////////////////////////////                                                            
                                                                                                                                    
        Double precision Function SafeLog(X)                                                                                        
                                                                                                                                    
! ///////////////////////////////////////////////////////////////////////                                                           
                                                                                                                                    
        double precision X                                                                                                          
                                                                                                                                    
        if (X .le. 0) then                                                                                                          
          SafeLog = dlog(1d-8)                                                                                                      
        else                                                                                                                        
          SafeLog = dlog(X)                                                                                                         
        endif                                                                                                                       
                                                                                                                                    
        return                                                                                                                      
        end                                                                                                                         
