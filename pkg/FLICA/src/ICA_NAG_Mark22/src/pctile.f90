! //////////////////////////////////////////////////////////////////////                                                            
                                                                                                                                    
       Subroutine GetPercentiles(NoPercentiles, Percentile, psize)
                                                                                                                                    
! //////////////////////////////////////////////////////////////////////                                                            
!                                                                                                                                   
!  Routine sets up number of percentiles and the percentiles to                                                                     
!     be plotted                                                                                                                    
!                                                                                                                                   
!                                                                                                                                   
      implicit none                                                                                                                 
      integer NoPercentiles, i                                                                                                      
      integer psize
      double precision percentile(psize)
      character*1 response                                                                                                          
      integer tscan                                                                                                                 
      
      include 'MESSAGE1.INC'

      response=KW(3,language)
      Call Screen_in_a(HW(25,language),response,KW(3,language),         &  
       language) 
                                                                                                                                    
      if (Tscan(response, KW(4,language)) .ne. 0) then ! use default values                                                         
        NoPercentiles = 5    
        Percentile(1) =5d0                                                                                                          
        Percentile(2) =25d0                                                                                                         
        Percentile(3) = 50d0                                                                                                        
        Percentile(4) =75d0                                                                                                         
        Percentile(5) = 95d0                                                                                                        
      else                                                                                                                          
        NoPercentiles=5
        Call Screen_in_i(HW(26,language),NoPercentiles,10,              & 
           3,language)  
        do i = 1, NoPercentiles    
          Percentile(i) = 1d0 
          Call Screen_in_r(HW(27,language),Percentile(i),99d0,1d0,      & 
             language)                                  
        enddo                                                                                                                       
      endif                                                                                                                         
!                                                                                                                                   
!    sort, in case where not entered in ascending order                                                                             
!                                                                                                                                   
                                                                                                                                    
      Call Shell(NoPercentiles,Percentile)                                                                                          
                                                                                                                                    
                                                                                                                                    
      return                                                                                                                        
                                                                                                                                    
      end                                                                                                                           
                                                                                                                                    
                                                                                                                                    

! //////////////////////////////////////////////////////////////////////

      Subroutine CreatePBYfilesFP(NFleets, NP, P, SP)

!c///////////////////////////////////////////////////////////////////////

      character*30 filename1,filename2,filename3,filename4, filename5, Title1, Title2, Title3
!c
!c    NP - no of percentiles
!c     P - percentiles
!c    SP - size of percentile vector
!c
      integer ifleet, Nfleets, SP, NP
      double precision P(SP)

!       version for projections: looks for *.mc files, 
!        looks for fleet-disaggregated catches and discards


      Call ReadAndSortStock('stock.mc','stock.pby','Stock',.true., NP, P, SP, nfleets)
      Call ReadAndSortStock('recruits.mc','recruits.pby','Recruits',.false., NP, P, SP,nfleets)
      Call ReadAndSortStock('MeanF.mc','MeanF.pby','MeanF',.false., NP, P, SP,nfleets)
      Call ReadAndSortStock('Yield.mc','Yield.pby','Yield',.false., NP, P, SP,nfleets)

      do ifleet= 1, Nfleets
        filename1='Fmult'//char(48+ifleet)//'.mc'
        filename2='Fmult'//char(48+ifleet)//'.pby'
        Title1 = 'F-multiplier for fleet '//char(48+ifleet)
        filename3='Catch'//char(48+ifleet)//'.mc'
        filename4='Catch'//char(48+ifleet)//'.pby'
        filename5='Discard'//char(48+ifleet)//'.pby'
        Title2='Catches by Fleet '//char(48+ifleet)
        Title3='Discards by Fleet '//char(48+ifleet)
        Call ReadAndSortStock(filename1,filename2,Title1, .false., NP,P,SP,nfleets)
        Call ReadAndSortCatch(filename3,filename4,Title2, filename5, NP, P, SP)
      enddo

      return
      end

!c ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++





                                                                                                                                    
! //////////////////////////////////////////////////////////////////////                                                            
                                                                                                                                    
      Subroutine CreatePBYfiles(NP, P, SP)                                                                                 
                                                                                                                                    
!///////////////////////////////////////////////////////////////////////                                                            
                                                                                                                                    
      include "INDAT.INC"
      
!                                                                                                                                   
!    NP - no of percentiles                                                                                                         
!     P - percentiles                                                                                                               
!    SP - size of percentile vector                                                                                                 
!                                                                                                                                   
!     THIS ROUTINE FOR USE BY ICA - READS *.PBI FILES, DOESN't LOOK FOR 
!       FLEET-DISAGREGATED DATAFILES
!  
      integer  SP, NP                                                                                               
      double precision P(SP)                                                                                                        
                                                                                                                                    
                                                                                                                                    
      Call ReadAndSortStock(stock_mci,stock_pbi,'Stock',             &                                                           
                                          .true., NP, P, SP,0)  
      Call ReadAndSortStock(recruits_mci,recruits_pbi,               &                                                           
                              'Recruits',.false., NP, P, SP,0)  
      Call ReadAndSortStock(MeanF_mci,MeanF_pbi,                     &                                                           
                                 'MeanF',.false., NP, P, SP,0)  
      Call ReadAndSortStock(Yield_mci,Yield_pbi,                     &                                                           
                                 'Yield',.false., NP, P, SP,0)  
                                                                                                                                    
                                                                                                                                    
      return                                                                                                                        
      end                                                                                                                           
                                                                                                                                    
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                              
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
! ///////////////////////////////////////////////////////////////////////                                                           
                                                                                                                                    
      SUBROUTINE SHELL(N, ARR)                                                                                                      
                                                                                                                                    
! ////////////////////////////////////////////////////////////////////////                                                          
!                                                                                                                                   
!  From page 229 of Numerical Recipes                                                                                               
!                                                                                                                                   
!                                                                                                                                   
      implicit none                                                                                                                 
      integer M,N, NN,K,L,I,J, LogNB2                                                                                               
      Double precision ALN2I, Tiny                                                                                                  
      Double precision Arr(N), T                                                                                                    
                                                                                                                                    
                                                                                                                                    
      Parameter (ALN2I = 1./0.69314718, TINY=1d-5)                                                                                  
                                                                                                                                    
      LognB2=IDINT(Dlog(Dble(N))*ALN2I + Tiny)                                                                                      
      M=N                                                                                                                           
      Do NN=1, LognB2                                                                                                               
        M =M /2                                                                                                                     
        K=N-M                                                                                                                       
        Do j=1,k                                                                                                                    
          i=j                                                                                                                       
3         continue                                                                                                                  
          l=i+m                                                                                                                     
          if (ARR(L) .lt. Arr(i)) then                                                                                              
            T=Arr(i)                                                                                                                
            Arr(i)=Arr(l)                                                                                                           
            Arr(l)=t                                                                                                                
            i=i-m                                                                                                                   
            if (i .ge. 1) goto 3                                                                                                    
          endif                                                                                                                     
        enddo                                                                                                                       
      enddo                                                                                                                         
      return                                                                                                                        
      end                                                                                                                           
                                                                                                                                    
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                              
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
! //////////////////////////////////////////////////////////////////////                                                            
                                                                                                                                    
      Subroutine ReadAndSortStock( Infile, Outfile, Title, Calc_RISK,   &                                                           
         Nopercentiles, Percentile, PSIze,Nfleet)   
                                                                                                                                    
! //////////////////////////////////////////////////////////////////////                                                            
!                                                                                                                                   
!     Reads an .MC file, sorts iterations and creates .PBY file                                                                     
!                                                                                                                                   
       implicit none                                                                                                                
       include 'INDAT.INC'                                                                                                          

       character*(*) Infile, OutFile, Title                                                                                         
       logical Calc_RISK                                                                                                            
       integer NoPercentiles, PSize, YtPro, nfleet
       Double Precision Percentile(PSize), MBAL                                                                                     
                                                                                                                                    
       integer StartYear,  NoFleets, IterationsRun                                                                                  
       integer itn, iyr, j, year                                                                                                    
                                                                                                                                    
       double precision Data(Maxyear, maxiters)   
       double precision Mean(Maxyear), MBALProb(Maxyear) 
       double precision Vector(maxiters)    
       double precision fractile(Maxyear,NoPercentiles)  
                                                                                                                                    
                                                                                                                                    
       Open(11, File=Infile, Status='old',recl=6000)                                                                                
       Open(12, File=Outfile, Status='unknown',recl=6000)                                                                           
                                                                                                                                    
                                                                                                                                    
       read(11, *) IterationsRun,NoFleets, YtPro, MBAL, StartYear                                                                   

       do itn=1,IterationsRun                                                                                                       
                                                                                                                                    
         read(11,*) (Data(iyr,itn), iyr=1,YtPro)                                                                                    
                                                                                                                                    
       enddo                                                                                                                        
       close(11)                                                                                                                    
                                                                                                                                    
!                                                                                                                                   
!      do the sorting, year by year                                                                                                 
!                                                                                                                                   
       do iyr = 1, YtPro                                                                                                            
         do itn=1,IterationsRun                                                                                                     
           Vector(itn) = Data(iyr,itn)                                                                                              
         enddo                                                                                                                      
         Call SHELL(IterationsRun, Vector)                                                                                          
                                                                                                                                    
         if (Calc_Risk) then ! find the proportion above & below MBAL by year                                                       
           itn= 1                                                                                                                   
           do while ((Vector(itn).lt. MBAL).and.                        &                                                           
                                         (itn.lt.IterationsRun))                                                                    
             itn=itn+1                                                                                                              
             MBALPROB(iyr) = float(itn)/float(iterationsRun)                                                                        
           enddo                                                                                                                    
         endif                                                                                                                      
                                                                                                                                    
                                                                                                                                    
         do j =1, NoPercentiles                                                                                                     
           Fractile(iyr,j) =                                            &                                                           
            Vector(1 + INT(DBLE(IterationsRun)*Percentile(j)/100d0))                                                                
         enddo                                                                                                                      
       enddo                                                                                                                        
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
       ! the expectation of distribution                                                                                            
                                                                                                                                    
                                                                                                                                    
       do iyr= 1, YtPro                                                                                                             
         Mean(iyr) = 0d0                                                                                                            
         do itn = 1, IterationsRun                                                                                                  
           Mean(iyr) = Mean(iyr)+Data(iyr,itn)                                                                                      
         enddo                                                                                                                      
         Mean(iyr) = Mean(iyr)/Dble(IterationsRun)                                                                                  
       enddo                                                                                                                        
                                                                                                                                    
       write(12,'(A)') Title                                                                                                        
       write(12, '(A)') 'Percentile'                                                                                                
       write(12,'(1X,I6,5X,I6,5X,I6,5X,I6)') startyear, startyear+YtPro-1,    &    
        nopercentiles, nfleet   
       write(12, 100) (year, year=startyear, startyear+YtPro-1)                                                                     
100    format(16X,120(5X,I5,5X))                                                                                                    
       do j =1,NoPercentiles                                                                                                        
         write(12,200) Int(Percentile(j)),(Fractile(iyr,j),             &                                                           
            iyr=1,YtPro)                                                                                                          
       enddo                                                                                                                        
200    format(' ', 3X,I3,9X,120(1X,E14.6))                                                                                          
       write(12,300) (Mean(iyr), iyr=1,YtPro)                                                                                     
300    format(' Mean           ',120(1X,E14.6))                                                                                     
       if (Calc_Risk) then                                                                                                          
         write(12,'(A)') 'P (SSB < MBAL) '
         write(12,*) MBAL
         do year = startyear, startyear+YtPro-1                                                                                     
           write(12,400) year, MBalProb( year-startyear+1)                                                                          
         enddo                                                                                                                      
       endif                                                                                                                        
400    format(5X,I5,5X, E15.6)                                                                                                      
                                                                                                                                    
                                                                                                                                    
       close(11)                                                                                                                    
                                                                                                                                    
                                                                                                                                    
       return                                                                                                                       
       end ! of ReadAndSortStock                                                                                                    
                                                                                                                                    
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                          
                                                                                                                                    
                                                                                                                                    
! //////////////////////////////////////////////////////////////////////                                                            
                                                                                                                                    
      Subroutine ReadAndSortCatch( Infile, Outfile1,Title,Outfile2,     &                                                           
         Nopercentiles, Percentile, PSIze)                                                                                          
                                                                                                                                    
! //////////////////////////////////////////////////////////////////////                                                            
!                                                                                                                                   
!     Reads an .MC file, sorts iterations and creates .PBY file                                                                     
!      for catch and discards                                                                                                       
!                                                                                                                                   
       implicit none                                                                                                                
       include 'INDAT.INC'                                                                                                          

       character*(*) Infile, OutFile1, Outfile2, Title                                                                              
       integer NoPercentiles, PSize, YtPro
       Double Precision Percentile(PSize), MBAL                                                                                     
                                                                                                                                    
       integer StartYear,  NoFleets, IterationsRun                                                                                  
       integer itn, iyr, j, year                                                                                                    
                                                                                                                                    
       double precision Catch(MaxYear, maxiters)     
       double precision discarded(MaxYear, maxiters)            
       double precision MeanCatch(MaxYear), MeanDiscard(MaxYear)

       double precision Vector(maxiters)    
       double precision FractileCatch(MaxYear,NoPercentiles)    
       double precision FractileDiscard(MaxYear,NoPercentiles)  
                                                                                                                                    
       Open(11, File=Infile, Status='old',recl=6000)                                                                                
       Open(12, File=Outfile1, Status='unknown',recl=6000)                                                                          
       Open(13, File=Outfile2, Status='unknown',recl=6000)                                                                          
                                                                                                                                    
       read(11, *) IterationsRun,NoFleets, YtPro, MBAL, StartYear                                                                   
                                                                                                                                    
       do itn=1,IterationsRun                                                                                                       
         read(11,*) (Catch(iyr,itn), iyr=1,YtPro)                                                                                   
         read(11,*) (discarded(iyr,itn), iyr=1,YtPro)                                                                               
       enddo                                                                                                                        
       close(11)                                                                                                                    
                                                                                                                                    
!                                                                                                                                   
!      do the sorting, year by year                                                                                                 
!                                                                                                                                   
       do iyr = 1, YtPro                                                                                                            
         do itn=1,IterationsRun                                                                                                     
           Vector(itn) = Catch(iyr,itn)                                                                                             
         enddo                                                                                                                      
                                                                                                                                    
                                                                                                                                    
         Call SHELL(IterationsRun, Vector)                                                                                          
         do j =1, NoPercentiles                                                                                                     
           FractileCatch(iyr,j) =                                       &                                                           
            Vector(1 + INT(DBLE(IterationsRun)*Percentile(j)/100d0))                                                                
         enddo                                                                                                                      
       enddo                                                                                                                        
                                                                                                                                    
       do iyr = 1, YtPro                                                                                                            
         do itn=1,IterationsRun                                                                                                     
           Vector(itn) = discarded(iyr,itn)                                                                                         
         enddo                                                                                                                      
         Call SHELL(IterationsRun, Vector)                                                                                          
         do j =1, NoPercentiles                                                                                                     
           FractileDiscard(iyr,j) =                                     &                                                           
            Vector(1 + INT(DBLE(IterationsRun)*Percentile(j)/100d0))                                                                
         enddo                                                                                                                      
       enddo                                                                                                                        
                                                                                                                                    
                                                                                                                                    
       ! the expectation of distribution                                                                                            
                                                                                                                                    
                                                                                                                                    
       do iyr= 1, YtPro                                                                                                             
         MeanCatch(iyr) = 0d0                                                                                                       
         do itn = 1, IterationsRun                                                                                                  
           MeanCatch(iyr) = MeanCatch(iyr)+Catch(iyr,itn)                                                                           
         enddo                                                                                                                      
         MeanCatch(iyr) = MeanCatch(iyr)/Dble(IterationsRun)                                                                        
       enddo                                                                                                                        
                                                                                                                                    
       do iyr= 1, YtPro                                                                                                             
         MeanDiscard(iyr) = 0d0                                                                                                     
         do itn = 1, IterationsRun                                                                                                  
           MeanDiscard(iyr) = MeanDiscard(iyr)+Discarded(iyr,itn)                                                                   
         enddo                                                                                                                      
         MeanDiscard(iyr) = MeanDiscard(iyr)/Dble(IterationsRun)                                                                    
       enddo                                                                                                                        
                                                                                                                                    
       write(12,'(A)') Title                                                                                                        
       write(12, '(A)') 'Percentile'                                                                                                
       write(12,'(1X,I6,5X,I6,5X,I6)') startyear, startyear+YtPro-1,    &                                                           
        nopercentiles                                                                                                               
       write(12, 100) (year, year=startyear, startyear+YtPro-1)                                                                     
100    format(15X,120(5X,I5,5X))                                                                                                    
       do j =1,NoPercentiles                                                                                                        
         write(12,200) int(Percentile(j)),(FractileCatch(iyr,j),        &                                                           
          iyr=1,YtPro)                                                                                                            
       enddo                                                                                                                        
200    format(' ',3X,I3,9X,120(1X,E14.6))                                                                                           
       write(12,300) (MeanCatch(iyr), iyr=1,YtPro)                                                                                
300    format(' Mean           ',120(1X,E14.6))                                                                                     
                                                                                                                                    
       close(12)                                                                                                                    
                                                                                                                                    
       write(13,'(A)') Title                                                                                                        
       write(13, '(A)') 'Percentile'                                                                                                
       write(13,'(1X,I6,5X,I6,5X,I6)') startyear, startyear+YtPro-1,    &                                                           
        nopercentiles                                                                                                               
       write(13, 100) (year, year=startyear, startyear+YtPro-1)                                                                     
       do j =1,NoPercentiles                                                                                                        
         write(13,200) int(Percentile(j)),(FractileDiscard(iyr,j),      &                                                           
          iyr=1,YtPro)                                                                                                            
       enddo                                                                                                                        
       write(13,300) (MeanDiscard(iyr), iyr=1,YtPro)                                                                              
                                                                                                                                    
       close(13)                                                                                                                    
                                                                                                                                    
                                                                                                                                    
       return                                                                                                                       
       end ! of ReadAndSortStock        
                                                                                                                                    
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                          
                                                                                                                                    
                                                                                                                                    
