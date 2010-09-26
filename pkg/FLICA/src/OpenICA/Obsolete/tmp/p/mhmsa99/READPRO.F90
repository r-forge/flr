                                                                                                                                    
                                                                                                                                    
! ///////////////////////////////////////////////////////////////////////                                                           
!                                                                                                                                   
      Subroutine ReadPro  ! Same as Reader for ICA with no title                                                                    
                                                                                                                                    
! //////////////////////////////////////////////////////////////////////                                                            
                                                                                                                                    
                                                                                                                                    
!      Reads in data from files in the Lowestoft format, except that (1) it                                                         
!      assumes all data are in matrix format save for PF and PM                                                                     
!      and (2) the natural mortality, maturity ogive and stock weight                                                               
!      files may extend one year past the last year of catch-at-age data.                                                           
!      If they do not, these values are assumed equal to the values                                                                 
!      given for the last year and the user is warned to this effect.                                                               
!                                                                                                                                   
!      This routine does not read the survey data.                                                                                  
!                                                                                                                                   
                                                                                                                                    
      Include "Indat.inc"                                                                                                           
                                                                                                                                    
                                                                                                                                    
!     ____________L O C A L ____V A R I A B L E S_____________________                                                              
!                                                                                                                                   
      character *40 indexname, cnname, laname, cwname, swname, moname,  &                                                           
      nmname, pfname, pmname, title                                                                                                 
                                                                                                                                    
!                                                                                                                                   
!      Indexname : name of the file holding the list of names of the                                                                
!                  catch and biological sampling data files, being:                                                                 
!      CNName    : name of the file holding catch numbers at age and year                                                           
!      LAName    :                  "       landings by year                                                                        
!      CWName    :                  "       weight at age and year in the catches                                                   
!      SWName    :                  "       weight at age and year in the stock                                                     
!      MOName    :                  "       maturity at age and year                                                                
!      NMName    :                  "       natural mortality at age and year                                                       
!      PFName    :                  "       proportion of fishing mortality before spawning                                         
!      PMName    :                  "       proportion of natural mortality before spawning                                         
!      Title     : a variable used for reading and ignoring non-numeric data such as titles                                         
!                                                                                                                                   
                                                                                                                                    
      integer year, age, dummy1, dummy2, ftype, ioerr, fyy, lyy                                                                     
                                                                                                                                    
!     ioerr is an I/O error check integer. It is zero if a file is opened OK,                                                       
!     other values indicate some form of error.                                                                                     
!     here, only IOERR=6416 is interpreted as 'FILE MISSING'. The values of                                                         
!     IOERR are specific to Microsoft FORTRAN.                                                                                      
!                                                                                                                                   
                                                                                                                                    
      integer inf  ! input file number                                                                                              
                                                                                                                                    
      inf = 15                                                                                                                      
                                                                                                                                    
      do dummy1 =1,12                                                                                                               
	write(*,*)                                                                                                                         
      enddo                                                                                                                         
                                                                                                                                    
                                                                                                                                    
!----------- Read the name of the file which lists the names of datafiles                                                           
                                                                                                                                    
      ioerr = 1                                                                                                                     
      do while (ioerr .ne. 0)                                                                                                       
	write(*,907) 'Enter the name of the index file '                                                                                   
	read(*,9000) indexname                                                                                                             
	if (indexname .eq. ' ')  then                                                                                                      
	  write(*,*) 'No index file name. Run abandoned.'                                                                                  
	  stop                                                                                                                             
	endif                                                                                                                              
907     format(' ',A43,'--> ') ! \                                                                                                  
	write(*,*)                                                                                                                         
	open(inf, file = indexname, iostat=ioerr, status='old')                                                                            
	if (ioerr .eq. 6416)                                                   &                                                           
           write(*,*)'File not found. Try again, or <return> to exit.'                                                              
      enddo                                                                                                                         
                                                                                                                                    
! ------------ Read the datafile names in sequence                                                                                  
                                                                                                                                    
      read(inf,9000) title                                                                                                          
      read(inf,9000) dummy1                                                                                                         
      read(inf,9000) laname                                                                                                         
      read(inf,9000) cnname                                                                                                         
      read(inf,9000) cwname                                                                                                         
      read(inf,9000) swname                                                                                                         
      read(inf,9000) nmname                                                                                                         
      read(inf,9000) moname                                                                                                         
      read(inf,9000) pfname                                                                                                         
      read(inf,9000) pmname                                                                                                         
      close(inf)                                                                                                                    
                                                                                                                                    
! --------------- Open and read the landings file                                                                                   
                                                                                                                                    
      open(inf, file = laname, status = 'old', iostat=ioerr)                                                                        
                                                                                                                                    
      call iocheck(ioerr, laname)                                                                                                   
                                                                                                                                    
      read(inf,9000) title                                                                                                          
      read(inf,*) dummy1, dummy2                                                                                                    
      read(inf,*) firstyear, lastyear                                                                                               
      read(inf,*) firstage, lastage                                                                                                 
      read(inf,*) ftype                                                                                                             
      if (ftype .ne. 5)  write(*,*) 'LANDINGS FILE FORMAT ERROR'                                                                    
      if (dummy1 .ne. 1) write(*,*) 'LANDINGS FILE FORMAT ERROR'                                                                    
      if (lastyear-firstyear+1 .gt. maxyear) then                                                                                   
         write(*,*) 'Too many years in the catch-at-age data. Maximum'          &   
      //'is ',maxyear   
	 stop                                                                                                                              
      endif                                                                                                                         
      if (lastage-firstage+1 .gt. maxage) then                                                                                      
         write(*,*) 'Too many ages in the catch-at-age data. Maximum'   &                                                           
      //' is ',maxage 
	 stop                                                                                                                              
      endif                                                                                                                         
                                                                                                                                    
      do  year = firstyear, lastyear                                                                                                
	read(inf, *) la(year-firstyear+1)                                                                                                  
      enddo                                                                                                                         
                                                                                                                                    
      close(inf)                                                                                                                    
                                                                                                                                    
! ------------- Open and read the catch numbers at age and year                                                                     
                                                                                                                                    
      write(*,*) 'Reading ',cnname                                                                                                  
                                                                                                                                    
      open(inf, file = cnname, status = 'old', iostat=ioerr)                                                                        
      call iocheck(ioerr, cnname)                                                                                                   
      read(inf,9000) title                                                                                                          
      read(inf,*) dummy1, dummy2                                                                                                    
      if(dummy2 .ne. 2)  write(*,*) 'CATCH NUMBERS FILE ID IS WRONG'                                                                
      read(inf,*) dummy1, dummy2                                                                                                    
      if ((dummy1 .ne. firstyear).or. (dummy2 .ne. lastyear))           &                                                           
             write(*,*) 'CATCH NUMBERS FILE FORMAT ERROR: YEAR RANGE'                                                               
      read(inf,*) dummy1, dummy2                                                                                                    
      if ((dummy1 .ne. firstage) .or. (dummy2 .ne. lastage))            &                                                           
             write(*,*) 'CATCH NUMBERS FILE FORMAT ERROR: AGE RANGE'                                                                
      read(inf,*) ftype                                                                                                             
      if (ftype .ne. 1)                                                 &                                                           
           write(*,*) 'CATCH NUMBERS FILE FORMAT ERROR: MUST BE MATRIX'                                                             
                                                                                                                                    
      do year = firstyear, lastyear                                                                                                 
       read(inf, *) (cn(year-firstyear+1, age-firstage+1), age =        &                                                           
        firstage,lastage)                                                                                                           
      enddo                                                                                                                         
                                                                                                                                    
      close(inf)                                                                                                                    
                                                                                                                                    
!  --------------- The catch weight file is not used, but code to                                                                   
!  ---------------       access it is left here.                                                                                    
                                                                                                                                    
!      open(inf, file = cwname, status = 'old')                                                                                     
!      read(inf,9000) title                                                                                                         
!      read(inf,*) dummy1, dummy2                                                                                                   
!      if (dummy2 .ne. 3)    write(*,*) 'CATCH WEIGHT FILE ID IS WRONG'                                                             
!      read(inf,*) dummy1, dummy2                                                                                                   
!      if ((dummy1 .ne. firstyear) .or. (dummy2 .ne. lastyear))                                                                     
!     *       write(*,*) 'CATCH WEIGHT FILE FORMAT ERROR: YEAR RANGE'                                                               
!      read(inf,*) dummy1, dummy2                                                                                                   
!      if ((dummy1 .ne. firstage) .or. (dummy2 .ne. lastage))                                                                       
!     *       write(*,*) 'CATCH WEIGHT FILE FORMAT ERROR: AGE RANGE'                                                                
!      read(inf,*) ftype                                                                                                            
!      if (ftype .ne. 1)                                                                                                            
!     *     write(*,*) 'CATCH WEIGHT FILE FORMAT ERROR: MUST BE MATRIX'                                                             
!                                                                                                                                   
!      do 300 year = firstyear, lastyear, 1                                                                                         
! 300   read(inf, *) (cw(year-firstyear+1, age-firstage+1), age = firstage                                                          
!     *,lastage)                                                                                                                    
!      close(inf)                                                                                                                   
!                                                                                                                                   
!      write(*,*) cwname,' read OK'                                                                                                 
!                                                                                                                                   
                                                                                                                                    
!-------------- Open and read the stock weight file                                                                                 
                                                                                                                                    
                                                                                                                                    
      open(inf, file = swname, status = 'old', iostat=ioerr)                                                                        
      call iocheck(ioerr, swname)                                                                                                   
                                                                                                                                    
      write(*,*) 'Reading  ', swname                                                                                                
      read(inf,9000) title                                                                                                          
      read(inf,*) dummy1, dummy2                                                                                                    
      if (dummy2 .ne. 4)                                                &                                                           
             write(*,*) 'STOCK WEIGHT FILE ID IS WRONG'                                                                             
      read(inf,*) fyy, lyy                                                                                                          
      if ((fyy .ne. firstyear).or. (lyy .ne. lastyear) ) then                                                                       
	if (lyy .ne. lastyear+1 ) then                                                                                                     
	  write(*,*) 'STOCK WEIGHT FILE FORMAT ERROR: YEAR RANGE'                                                                          
	endif                                                                                                                              
      else                                                                                                                          
	write(*,1010) 'Stock weights in ',lastyear+1,' assumed = ',            &                                                           
        'stock weights in ',lastyear                                                                                                
      endif ! year range difference                                                                                                 
                                                                                                                                    
1010  format(' ',A17,1X,I4,1X,A11,A17,1X, I4)                                                                                       
                                                                                                                                    
      read(inf,*) dummy1, dummy2                                                                                                    
      if ((dummy1 .ne. firstage) .or. (dummy2 .ne. lastage))            &                                                           
             write(*,*) 'STOCK WEIGHT FILE FORMAT ERROR: AGE RANGE'                                                                 
      read(inf,*) ftype                                                                                                             
      if (ftype .ne. 1)                                                 &                                                           
           write(*,*) 'STOCK WEIGHT FILE FORMAT ERROR: MUST BE MATRIX'                                                              
                                                                                                                                    
      do  year = firstyear, lyy                                                                                                     
      read(inf, *) (sw(year-firstyear+1, age-firstage+1), age =         &                                                           
       firstage,lastage)                                                                                                            
      enddo                                                                                                                         
      close(inf)                                                                                                                    
                                                                                                                                    
      if (lyy .eq. lastyear) then ! copy across from lastyear to lastyear+1                                                         
	do age = firstage,lastage                                                                                                          
	  SW(lastyear-firstyear+1+1,age-firstage+1)=                           &                                                           
            SW(lastyear-firstyear+1,age-firstage+1)                                                                                 
	enddo                                                                                                                              
      endif                                                                                                                         
                                                                                                                                    
                                                                                                                                    
! -------------- Open and read the natural mortality file                                                                           
                                                                                                                                    
      open(inf, file = nmname, status = 'old')                                                                                      
      call iocheck(ioerr, nmname)                                                                                                   
      write(*,*) 'Reading ',nmname                                                                                                  
                                                                                                                                    
      read(inf,9000) title                                                                                                          
      read(inf,*) dummy1, dummy2                                                                                                    
      if (dummy2 .ne. 5) write(*,*) 'NATURAL MORTALITY FILE ID IS WRONG'                                                            
      read(inf,*) fyy, lyy                                                                                                          
                                                                                                                                    
      if ((fyy .ne. firstyear).or. (lyy .ne. lastyear) ) then                                                                       
	if (lyy .ne. lastyear+1 ) then                                                                                                     
	  write(*,*) 'NATURAL MORTALITY FILE FORMAT ERROR: YEAR RANGE'                                                                     
	endif                                                                                                                              
      else                                                                                                                          
	write(*,1020) 'M in ',lastyear+1,' assumed = ',                        &                                                           
                      'M  in ',lastyear                                                                                             
      endif ! year range difference                                                                                                 
1020  format(' ',A5, 1X, I4, 1X, A11, A5, 1X, I4)                                                                                   
                                                                                                                                    
      read(inf,*) dummy1, dummy2                                                                                                    
      if ((dummy1 .ne. firstage) .or. (dummy2 .ne. lastage))            &                                                           
           write(*,*) 'NATURAL MORTALITY FILE FORMAT ERROR: AGE RANGE'                                                              
      read(inf,*) ftype                                                                                                             
      if (ftype .ne. 1)                                                 &                                                           
          write(*,*) 'NATURAL MORTALITY FILE FORMAT ERROR: MUST BE MATRIX'
                                                                                                                                    
      do  year = fyy, lyy                                                                                                           
      read(inf,*) (NM(year-firstyear+1, age-firstage+1),age=firstage    &                                                           
      ,lastage)                                                                                                                     
      enddo                                                                                                                         
                                                                                                                                    
      if (lyy .eq. lastyear) then ! copy across                                                                                     
	 do age = firstage,lastage                                                                                                         
	  NM(lastyear-firstyear+1+1,age-firstage+1)=                           &                                                           
            NM(lastyear-firstyear+1,age-firstage+1)                                                                                 
	 enddo                                                                                                                             
      endif                                                                                                                         
                                                                                                                                    
      close(inf)                                                                                                                    
                                                                                                                                    
                                                                                                                                    
!----------------- Open and read the maturity ogive file                                                                            
                                                                                                                                    
      open(inf, file = moname, status = 'old',iostat=ioerr)                                                                         
      call iocheck(ioerr, moname)                                                                                                   
      write(*,*) 'Reading ',Moname                                                                                                  
                                                                                                                                    
      read(inf,9000) title                                                                                                          
      read(inf,*) dummy1, dummy2                                                                                                    
      if (dummy2 .ne. 6) write(*,*) 'MATURITY OGIVE FILE ID IS WRONG'                                                               
      read(inf,*) fyy, lyy                                                                                                          
                                                                                                                                    
      if ((fyy .ne. firstyear).or. (lyy .ne. lastyear) ) then                                                                       
	if (lyy .ne. lastyear+1 ) then                                                                                                     
	  write(*,*) 'MATURITY OGIVE FILE FORMAT ERROR: YEAR RANGE'                                                                        
	endif                                                                                                                              
      else                                                                                                                          
	write(*,1030) 'Ogive in ',lastyear+1,' assumed = ',                    &                                                           
                      'ogive in ',lastyear                                                                                          
      endif ! year range difference                                                                                                 
1030  format(' ',A9, 1X, I4, 1X, A11, A9, 1X, I4)                                                                                   
                                                                                                                                    
                                                                                                                                    
      read(inf,*) dummy1, dummy2                                                                                                    
      if ((dummy1 .ne. firstage) .or. (dummy2 .ne. lastage))            &                                                           
             write(*,*) 'MATURITY OGIVE FILE FORMAT ERROR: AGE RANGE'                                                               
      read(inf,*) ftype                                                                                                             
      if (ftype .ne. 1)                                                 &                                                           
           write(*,*)'MATURITY OGIVE FILE FORMAT ERROR: MUST BE MATRIX'                                                             
      do  year = fyy, lyy                                                                                                           
      read(inf, *) (MO(year-firstyear+1, age-firstage+1), age = firstage&                                                           
      ,lastage)                                                                                                                     
      enddo                                                                                                                         
                                                                                                                                    
                                                                                                                                    
      if (lyy .eq. lastyear) then                                                                                                   
	do age = firstage,lastage                                                                                                          
	  MO(lastyear-firstyear+1+1,age-firstage+1)=                           &                                                           
            MO(lastyear-firstyear+1,age-firstage+1)                                                                                 
	enddo                                                                                                                              
      endif                                                                                                                         
                                                                                                                                    
      close(inf)                                                                                                                    
                                                                                                                                    
                                                                                                                                    
! ----------------- Open and read the proportion of F before spawning                                                               
                                                                                                                                    
      open(inf, file = pfname, status = 'old',iostat=ioerr)                                                                         
      call iocheck(ioerr, pfname)                                                                                                   
      write(*,*) 'Reading ',Pfname                                                                                                  
                                                                                                                                    
      read(inf,9000) title                                                                                                          
      read(inf,*) dummy1, dummy2                                                                                                    
      if (dummy2 .ne. 7)    write(*,*) 'PF FILE ID IS WRONG'                                                                        
      read(inf,*) dummy1, dummy2                                                                                                    
      if ((dummy1 .ne. firstyear).or.  (dummy2 .ne. lastyear))          &                                                           
             write(*,*) 'PF FILE FORMAT ERROR: YEAR RANGE'                                                                          
      read(inf,*) dummy1, dummy2                                                                                                    
      if ((dummy1 .ne. firstage) .or. (dummy2 .ne. lastage))            &                                                           
             write(*,*) 'PF FILE FORMAT ERROR: AGE RANGE'                                                                           
      read(inf,*) ftype                                                                                                             
      if (ftype .ne. 3)                                                 &                                                           
           write(*,*) 'PF FILE FORMAT ERROR: MUST BE SCALAR '                                                                       
      read(inf,*) PF                                                                                                                
                                                                                                                                    
      close(inf)                                                                                                                    
                                                                                                                                    
                                                                                                                                    
! ----------------- Open and read the proprtion of M before spawning                                                                
                                                                                                                                    
                                                                                                                                    
      open(inf, file = pmname, status = 'old', iostat=ioerr)                                                                        
      call iocheck(ioerr, pmname)                                                                                                   
      write (*,*) 'Reading  ',pmname                                                                                                
                                                                                                                                    
      read(inf,9000) title                                                                                                          
      read(inf,*) dummy1, dummy2                                                                                                    
      if (dummy2 .ne. 8)      write(*,*) 'PM FILE ID IS WRONG'                                                                      
      read(inf,*) dummy1, dummy2                                                                                                    
      if ((dummy1 .ne. firstyear).or. (dummy2 .ne. lastyear))           &                                                           
             write(*,*) 'PM FILE FORMAT ERROR: YEAR RANGE'                                                                          
      read(inf,*) dummy1, dummy2                                                                                                    
      if ((dummy1 .ne. firstage) .or. (dummy2 .ne. lastage))            &                                                           
             write(*,*) 'PM FILE FORMAT ERROR: AGE RANGE'                                                                           
      read(inf,*) ftype                                                                                                             
      if (ftype .ne. 3)                                                 &                                                           
           write(*,*) 'PM FILE FORMAT ERROR: MUST BE SCALAR '                                                                       
      read(inf,*) PM                                                                                                                
                                                                                                                                    
      close(inf)                                                                                                                    
                                                                                                                                    
      write(*,*)                                                                                                                    
                                                                                                                                    
9000  format (A40)                                                                                                                  
9010  format (I2)                                                                                                                   
9020  format (A8, A40)                                                                                                              
                                                                                                                                    
      return                                                                                                                        
      end                                                                                                                           
