! //////////////////////////////////////////////////////////////////////                                                            
!    UNIT SCR_IO : SCREEN INPUT ROUTINES                                                                                            
!         Three routines to read an integer, a real or a character                                                                  
!         string from the screen                                                                                                    
!         THIS VERSION NOT FOR MS-FORTRAN and FORTRAN dialects                                                                      
!         that support the line-feed inhibit character  \                                                                           
!                                                                                                                                   
! //////////////////////////////////////////////////////////////////////                                                            
																    
!      Program Test                                                                                                                 
																    
!      character*60 textin, Valid                                                                                                   
!      integer output,max,min                                                                                                       
																    
!      max=100                                                                                                                      
!      min = -100                                                                                                                   
																    
!      textin = 'Choose --> '                                                                                                       
!      VAlid = 'aAlLpP'                                                                                                             
!      Call SCREEN_IN_I(Textin, Output, Max, Min)                                                                                   
!      write(*,*) output                                                                                                            
																    
!      end                                                                                                                          
																    
																    
! //////////////////////////////////////////////////////////////////////                                                            
!                                                                                                                                   
      Subroutine SCREEN_IN_I(Text, Output, Max, Min,Language)                                                                       
!                                                                                                                                   
! //////////////////////////////////////////////////////////////////////                                                            
!     Reads an integer from screen                                                                                                  
!                                                                                                                                   
																    
      character*(*) Text                                                                                                            
      character*19 flength                                                                                                          
      integer output, Max,Min, length, L1, L2                                                                                       
      character*31 MinErr(4), Maxerr(4)                                                                                             
      integer language                                                                                                              
																    
      if (Language .gt. 4) Language = 1                                                                                             
      if (Language .le. 0 ) Language =1                                                                                             
																    
      MinErr(1)='ERROR: Minimum allowed is : '                                                                                      
      MinErr(2)='ERROR: El m°nimo permitido es :'                                                                                   
      MinErr(3)='ERREUR: Le minimum permis est :'                                                                                   
      MinErr(4)='FEIL: Minimum tillatelig er :'                                                                                     
																    
      MaxErr(1)='ERROR: Maximum allowed is : '                                                                                      
      MaxErr(2)='ERROR: El m†ximo permitido es :'                                                                                   
      MaxErr(3)='ERREUR: Le maximum permis est :'                                                                                   
      MaxErr(4)='FEIL: Maximum tillatelig er :'                                                                                     
																    
      length = LEN(Text)                                                                                                            
!                                                                                                                                   
!     count the number of trailing blanks                                                                                           
!                                                                                                                                   
																    
      do while (Text(length:length) .eq. ' ')                                                                                       
	length = length-1                                                                                                           
      enddo                                                                                                                         
																    
      length=length+1                                                                                                               
																    
!      if (length .ge. 75) then                                                                                                     
!        write(*,*) 'Error in routine SCREEN_IN_I: '                                                                                
!        write(*,*) 'Text string : ',text                                                                                           
!        write(*,*) 'is too long. It should be shorter than 75 chars.'                                                              
!        stop                                                                                                                       
!      endif                                                                                                                        
																    
      if (Max .le. Min) then                                                                                                        
	write(*,*) 'Error in routine SCREEN_IN_I: '                                                                                 
	write(*,*) 'Inconsistent MAX and MIN :',Max, Min                                                                            
	stop                                                                                                                        
      endif                                                                                                                         
																    
      l1 = length/10       ! the tens                                                                                               
      l2 = length-l1*10    ! the units                                                                                              
																    
																    
      if (l1 .ne. 0) then                                                                                                           
	L1=L1+48                                                                                                                    
	l2=l2+48                                                                                                                    
																    
	flength = '('' '',A'//char(L1)//char(L2)//','' --> '')'                                                                     
      else                                                                                                                          
	l2=l2+48                                                                                                                    
	flength = '('' '',A'//char(L2)//','' --> '')'                                                                               
      endif                                                                                                                         
																    
																    
      output = max*2                                                                                                                
																    
      do while ((output .gt. max) .or. (output .lt. min))                                                                           
																    
	write (*,flength) text                                                                                                      
	read(*,*) output                                                                                                            
																    
	if (output .gt. max) then                                                                                                   
																    
																    
	  write(*,*) Maxerr(language),max                                                                                           
	else if (output .lt. min) then                                                                                              
	  write(*,*) Minerr(language),min                                                                                           
	endif                                                                                                                       
																    
      enddo                                                                                                                         
																    
      return                                                                                                                        
      end ! of routine SCREEN_IN_I                                                                                                  
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                        
																    
																    
																    
! //////////////////////////////////////////////////////////////////////                                                            
!                                                                                                                                   
      Subroutine SCREEN_IN_R(Text, Output, Max, Min, Language)                                                                      
!                                                                                                                                   
! //////////////////////////////////////////////////////////////////////                                                            
!     Reads a double-precision real  from screen                                                                                    
      character*(*) Text                                                                                                            
      character*19 flength                                                                                                          
      integer  length, L1, L2                                                                                                       
																    
      double precision  output, Max,Min                                                                                             
																    
																    
      character*31 MinErr(4), Maxerr(4)                                                                                             
      integer language                                                                                                              
																    
      if (Language .gt. 4) Language = 1                                                                                             
      if (Language .le. 0 ) Language =1                                                                                             
																    
      MinErr(1)='ERROR: Minimum allowed is : '                                                                                      
      MinErr(2)='ERROR: El m°nimo permitido es :'                                                                                   
      MinErr(3)='ERREUR: Le minimum permis est :'                                                                                   
      MinErr(4)='FEIL: Minimum tillatelig er : '                                                                                    
																    
      MaxErr(1)='ERROR: Maximum allowed is : '                                                                                      
      MaxErr(2)='ERROR: El m†ximo permitido es :'                                                                                   
      MaxErr(3)='ERREUR: Le maximum permis est :'                                                                                   
      MaxErr(4)='FEIL: Maximum tillatelig er : '                                                                                    
																    
      length = LEN(Text)                                                                                                            
!                                                                                                                                   
!     count the number of trailing blanks                                                                                           
!                                                                                                                                   
																    
      do while (Text(length:length) .eq. ' ')                                                                                       
	length = length-1                                                                                                           
      enddo                                                                                                                         
																    
      length=length+1                                                                                                               
																    
!      if (length .ge. 75) then                                                                                                     
!        write(*,*) 'Error in routine SCREEN_IN_I: '                                                                                
!        write(*,*) 'Text string : ',text                                                                                           
!        write(*,*) 'is too long. It should be shorter than 75 chars.'                                                              
!        stop                                                                                                                       
!      endif                                                                                                                        
																    
      if (Max .le. Min) then                                                                                                        
	write(*,*) 'Error in routine SCREEN_IN_R: '                                                                                 
	write(*,*) 'Inconsistent MAX and MIN :',Max, Min                                                                            
	stop                                                                                                                        
      endif                                                                                                                         
																    
																    
      l1 = length/10       ! the tens                                                                                               
      l2 = length-l1*10    ! the units                                                                                              
																    
																    
      if (l1 .ne. 0) then                                                                                                           
	L1=L1+48                                                                                                                    
	l2=l2+48                                                                                                                    
																    
	flength = '('' '',A'//char(L1)//char(L2)//','' --> '')'                                                                     
      else                                                                                                                          
	l2=l2+48                                                                                                                    
	flength = '('' '',A'//char(L2)//','' --> '')'                                                                               
      endif                                                                                                                         
																    
      output = max*2d0                                                                                                              
																    
      do while ((output .gt. max) .or. (output .lt. min))                                                                           
																    
	write (*,flength) text                                                                                                      
	read(*,*) output                                                                                                            
																    
	if (output .gt. max) then                                                                                                   
	  write(*,*) MAXERR(Language),max                                                                                           
	  write(*,*)                                                                                                                
	else if (output .lt. min) then                                                                                              
	  write(*,*) MinErr(Language),min                                                                                           
	  write(*,*)                                                                                                                
	endif                                                                                                                       
																    
      enddo                                                                                                                         
																    
      return                                                                                                                        
      end ! of routine SCREEN_IN_I                                                                                                  
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                        
																    
																    
																    
																    
! //////////////////////////////////////////////////////////////////////                                                            
!                                                                                                                                   
      Subroutine SCREEN_IN_A(Text, Output, Valid, Language)                                                                         
!                                                                                                                                   
! //////////////////////////////////////////////////////////////////////                                                            
!                                                                                                                                   
!     Accepts a non-blank character variable from screen                                                                            
!      Output string is 'Output'                                                                                                    
!      it must be contained within the string 'valid'                                                                               
!      which holds a list of valid choices                                                                                          
!                                                                                                                                   
																    
      character*(*) Text                                                                                                            
      character*(*) Valid                                                                                                           
      character*19 flength                                                                                                          
      character*(*) output                                                                                                          
      integer  length, L1, L2,  tscan                                                                                               
      character*31 Err(4)                                                                                                           
      integer language                                                                                                              
																    
      if (Language .gt. 4) Language = 1                                                                                             
      if (Language .le. 0 ) Language =1                                                                                             
																    
      Err(1)='ERROR: Choose between : '//valid                                                                                      
      Err(2)='ERROR: Elige : '//valid                                                                                               
      Err(3)='ERREUR: Choissez : '//valid                                                                                           
      Err(4)='FEIL: Valg : '//valid                                                                                                 
																    
																    
      length = LEN(Text)                                                                                                            
!                                                                                                                                   
!     count the number of trailing blanks                                                                                           
!                                                                                                                                   
																    
      do while (Text(length:length) .eq. ' ')                                                                                       
	length = length-1                                                                                                           
      enddo                                                                                                                         
																    
      length=length+1                                                                                                               
																    
!      if (length .ge. 75) then                                                                                                     
!        write(*,*) 'Error in routine SCREEN_IN_I: '                                                                                
!        write(*,*) 'Text string : ',text                                                                                           
!        write(*,*) 'is too long. It should be shorter than 75 chars.'                                                              
!        stop                                                                                                                       
!      endif                                                                                                                        
																    
      l1 = length/10       ! the tens                                                                                               
      l2 = length-l1*10    ! the units                                                                                              
																    
																    
      if (l1 .ne. 0) then                                                                                                           
	L1=L1+48                                                                                                                    
	l2=l2+48                                                                                                                    
																    
	flength = '('' '',A'//char(L1)//char(L2)//','' --> '')'                                                                     
      else                                                                                                                          
	l2=l2+48                                                                                                                    
	flength = '('' '',A'//char(L2)//','' --> '')'                                                                               
      endif                                                                                                                         
																    
      output = ' '                                                                                                                  
																    
      do while ((tscan(output, Valid) .eq. 0).or.(output.eq.' '))                                                                   
																    
	write (*,flength) text                                                                                                      
	read(*,'(A)') output                                                                                                        
																    
	if ( tscan(output, Valid) .eq. 0) then                                                                                      
	  write(*,*)                                                                                                                
	  write(*,*) Err(Language)                                                                                                  
	  write(*,*)                                                                                                                
	endif                                                                                                                       
																    
      enddo                                                                                                                         
																    
      return                                                                                                                        
      end ! of routine SCREEN_IN_I                                                                                                  
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                        
																    
																    
																    
																    
! ////////////////////////////////////////////////////////////////////////////////                                                  
																    
       Integer Function Tscan( str1, str2 )                                                                                         
																    
! ////////////////////////////////////////////////////////////////////////////////                                                  
																    
!     Returns the position of the first string character that matches a                                                             
!     character in str2, returns zero if there's no match                                                                           
!                                                                                                                                   
      character* (*) str1, str2                                                                                                     
      integer p,j, out, l1, l2                                                                                                      
      l1 = len(str1)                                                                                                                
      l2 = len(str2)                                                                                                                
      out =0                                                                                                                        
      do p=1, l2                                                                                                                    
	do j= 1, l1                                                                                                                 
	  if (str1(j:j) .eq. str2(p:p)) then                                                                                        
	    out =p                                                                                                                  
	    goto 100                                                                                                                
	  endif                                                                                                                     
	enddo                                                                                                                       
      enddo                                                                                                                         
100   continue                                                                                                                      
      tscan = out                                                                                                                   
																    
      return                                                                                                                        
																    
      end ! of function tscan                                                                                                       
																    
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                            
																    
																    
!///////////////////////////////////////////////////////////////////////                                                            
																    
      Subroutine Screen_Out_A(Text, size, lines)                                                                                    
																    
! /////////////////////////////////////////////////////////////////////                                                             
!     writes text strings to screen                                                                                                 
																    
      integer size, lines, n                                                                                                        
      character*(*) Text(size)                                                                                                      
																    
      do n=1, lines                                                                                                                 
	write(*,'(A)') Text(n)                                                                                                      
      enddo                                                                                                                         
      return                                                                                                                        
      end                                                                                                                           
																    
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                         
																    
! ////////////////////////////////////////////////////////////////////////                                                          
																    
      SUBROUTINE SCREEN_IN_IR2                                           &   
       (TEXT, I1,R, I1Max,I1Min,RMax,RMin,Language)                                                                  
!                                                                                                                                   
! /////////////////////////////////////////////////////////////////////////                                                         
!                                                                                                                                   
!     Reads one integers and one real, preceded by a request text                                                                   
!                                                                                                                                   
      implicit none                                                                                                                 
      character*(*) Text                                                                                                            
      character*19 flength                                                                                                          
      integer length, I1, I1max, I1Min, l1,l2                                                                     
      double precision R, Rmax, RMin                                                                                                
      logical error                                                                                                                 
      character*31 MinErr(4), Maxerr(4)                                                                                             
      integer language                                                                                                              
																    
      if (Language .gt. 4) Language = 1                                                                                             
      if (Language .le. 0 ) Language =1                                                                                             
																    
      MinErr(1)='ERROR: Minimum allowed is : '                                                                                      
      MinErr(2)='ERROR: El m°nimo permitido es : '                                                                                  
      MinErr(3)='ERREUR: Le minimum permis est : '                                                                                  
      MinErr(4)='FEIL: Minimum tillatelig er : '                                                                                    
																    
      MaxErr(1)='ERROR: Maximum allowed is : '                                                                                      
      MaxErr(2)='ERROR: El m†ximo permitido es : '                                                                                  
      MaxErr(3)='ERREUR: Le maximum permis est : '                                                                                  
      MaxErr(4)='FEIL: Maximum tillatelig er : '                                                                                    
																    
																    
      write(*,*)                                                                                                                    
      length = LEN(Text)                                                                                                            
!                                                                                                                                   
!     count the number of trailing blanks                                                                                           
!                                                                                                                                   
																    
      do while (Text(length:length) .eq. ' ')                                                                                       
	length = length-1                                                                                                           
      enddo                                                                                                                         
																    
      length=length+1                                                                                                               
																    
																    
      l1 = length/10       ! the tens                                                                                               
      l2 = length-l1*10    ! the units                                                                                              
																    
																    
      if (l1 .ne. 0) then                                                                                                           
	L1=L1+48                                                                                                                    
	l2=l2+48                                                                                                                    
																    
	flength = '('' '',A'//char(L1)//char(L2)//','' --> '')'                                                                     
      else                                                                                                                          
	l2=l2+48                                                                                                                    
	flength = '('' '',A'//char(L2)//','' --> '')'                                                                               
      endif                                                                                                                         
																    
																    
																    
      error=.true.                                                                                                                  
      do while (error)                                                                                                              
																    
	write (*,flength) text                                                                                                      
	read(*,*) I1,R
	if (i1 .eq. -1) goto 1011
	error=.false.                                                                                                               
	if (I1 .gt. I1max) then                                                                                                     
	  write(*,*) Maxerr(Language), I1max,' <  ',I1                                                                              
	  error=.true.                                                                                                              
	else if  (I1 .lt. I1min) then                                                                                               
	  write(*,*) Minerr(language),I1min, ' > ', I1                                                                              
	  error=.true.                                                                                                              
	else if  (R .gt. Rmax) then                                                                                                 
	  write(*,*) Maxerr(language),Rmax,' < ',R                                                                                  
	  error=.true.                                                                                                              
	else if (R .lt. Rmin) then                                                                                                  
	  write(*,*) Minerr(language),Rmin, ' > ', R                                                                                
	  error=.true.                                                                                                              
	endif                                                                                                                       
																    
      enddo

1011  continue

      return                                                                                                                        
      end ! of routine SCREEN_IN_IR                                                                                                 
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                        
																    
																    
! ////////////////////////////////////////////////////////////////////////                                                          
																    
      SUBROUTINE SCREEN_IN_IR                                           &                                                           
       (TEXT, I1,I2,R, I1Max,I1Min,I2Max,I2Min,RMax,RMin,Language)                                                                  
!                                                                                                                                   
! /////////////////////////////////////////////////////////////////////////                                                         
!                                                                                                                                   
!     Reads two integers and one real, preceded by a request text                                                                   
!                                                                                                                                   
      implicit none                                                                                                                 
      character*(*) Text                                                                                                            
      character*19 flength                                                                                                          
      integer length, I1, I1max, I1Min, I2, I2max, I2Min, l1,l2                                                                     
      double precision R, Rmax, RMin                                                                                                
      logical error                                                                                                                 
      character*31 MinErr(4), Maxerr(4)                                                                                             
      integer language                                                                                                              
																    
      if (Language .gt. 4) Language = 1                                                                                             
      if (Language .le. 0 ) Language =1                                                                                             
																    
      MinErr(1)='ERROR: Minimum allowed is : '                                                                                      
      MinErr(2)='ERROR: El m°nimo permitido es : '                                                                                  
      MinErr(3)='ERREUR: Le minimum permis est : '                                                                                  
      MinErr(4)='FEIL: Minimum tillatelig er : '                                                                                    
																    
      MaxErr(1)='ERROR: Maximum allowed is : '                                                                                      
      MaxErr(2)='ERROR: El m†ximo permitido es : '                                                                                  
      MaxErr(3)='ERREUR: Le maximum permis est : '                                                                                  
      MaxErr(4)='FEIL: Maximum tillatelig er : '                                                                                    
																    
																    
      write(*,*)                                                                                                                    
      length = LEN(Text)                                                                                                            
!                                                                                                                                   
!     count the number of trailing blanks                                                                                           
!                                                                                                                                   
																    
      do while (Text(length:length) .eq. ' ')                                                                                       
	length = length-1                                                                                                           
      enddo                                                                                                                         
																    
      length=length+1                                                                                                               
																    
																    
      l1 = length/10       ! the tens                                                                                               
      l2 = length-l1*10    ! the units                                                                                              
																    
																    
      if (l1 .ne. 0) then                                                                                                           
	L1=L1+48                                                                                                                    
	l2=l2+48                                                                                                                    
																    
	flength = '('' '',A'//char(L1)//char(L2)//','' --> '')'                                                                     
      else                                                                                                                          
	l2=l2+48                                                                                                                    
	flength = '('' '',A'//char(L2)//','' --> '')'                                                                               
      endif                                                                                                                         
																    
																    
																    
      error=.true.                                                                                                                  
      do while (error)                                                                                                              
																    
	write (*,flength) text                                                                                                      
	read(*,*) I1,I2,R
	if (i1 .eq. -1) goto 1011
	error=.false.                                                                                                               
	if (I1 .gt. I1max) then                                                                                                     
	  write(*,*) Maxerr(Language), I1max,' <  ',I1                                                                              
	  error=.true.                                                                                                              
	else if  (I1 .lt. I1min) then                                                                                               
	  write(*,*) Minerr(language),I1min, ' > ', I1                                                                              
	  error=.true.                                                                                                              
	else if (I2 .gt. I2max) then                                                                                                
	  write(*,*) Maxerr(language),I2max,'  < ',I2                                                                               
	  error=.true.                                                                                                              
	else if (I2 .lt. I2min) then                                                                                                
	  write(*,*) MinErr(language),I2min, ' > ', I2                                                                              
	  error=.true.                                                                                                              
	else if  (R .gt. Rmax) then                                                                                                 
	  write(*,*) Maxerr(language),Rmax,' < ',R                                                                                  
	  error=.true.                                                                                                              
	else if (R .lt. Rmin) then                                                                                                  
	  write(*,*) Minerr(language),Rmin, ' > ', R                                                                                
	  error=.true.                                                                                                              
	endif                                                                                                                       
																    
      enddo

1011  continue

      return                                                                                                                        
      end ! of routine SCREEN_IN_IR                                                                                                 
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                        
																    
																    
																    
! //////////////////////////////////////////////////////////////                                                                    
																    
      Subroutine IntToChar(INTX,STRING,LENGTH)                                                                                      
																    
! //////////////////////////////////////////////////////////////                                                                    
																    
	  integer intx, length , k, j                                                                                               
	  character*(*) string                                                                                                      
	  character*1 ak(100), blank                                                                                                
	  integer tscan                                                                                                             
																    
	  j=intx                                                                                                                    
	  blank= ' '                                                                                                                
																    
	  if (intx .lt. 0)then                                                                                                      
	    write(*,*) 'Error in IntTOChar: -ve argument : ',intx                                                                   
	    stop                                                                                                                    
	  endif                                                                                                                     
																    
	  do k =1,length                                                                                                            
	    ak(k) = blank                                                                                                           
	  enddo                                                                                                                     
																    
	  if  (intx .eq. 0) then                                                                                                    
	    ak(1) = '0'                                                                                                             
	  else                                                                                                                      
	    do k= INT(Dlog10(Dble(j)))+1,1,-1                                                                                       
	      if ( j .ge. 10**(k-1)) then                                                                                           
		ak(k) = char(48+j/10**(k-1))                                                                                        
		j = j- 10**(k-1)*(j/10**(k-1))                                                                                      
	      else                                                                                                                  
		ak(k) = '0'                                                                                                         
	      endif                                                                                                                 
	    enddo                                                                                                                   
	  endif                                                                                                                     
																    
!         concatenate the AK strings without blanks                                                                                 
																    
	  string = '     '                                                                                                          
	  do k=length, 1, -1                                                                                                        
	    if (ak(k) .ne. ' ') then                                                                                                
	      if (String(1:1) .eq. ' ') then                                                                                        
		String(1:1) = ak(k)                                                                                                 
	      else                                                                                                                  
		STRING= STRING(1:TSCAN(' ',STRING)-1)//ak(k)                                                                        
	      endif                                                                                                                 
	    endif                                                                                                                   
!            write(*,*) k, ak(k)                                                                                                    
!            write(*,*) 'Substring>',string(1: TSCAN(' ',STRING)),'<'                                                               
!            write(*,*) 'string>',string,'<'                                                                                        
!            write(*,*) 'Posn Blank =',Tscan(' ',string)                                                                            
	  enddo                                                                                                                     
																    
																    
																    
																    
																    
	  return                                                                                                                    
	  end                                                                                                                       
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                               
																    
! ////////////////////////////////////////////////////////////////////                                                              
																    
      Subroutine Screen_in_s(TextIn, TextOut)                                                                                       
																    
! ///////////////////////////////////////////////////////////////////                                                               
!                                                                                                                                   
!    writes Textin to the screen, reads TextOut from user                                                                           
!                                                                                                                                   
      implicit none                                                                                                                 
      character*19  flength                                                                                                         
      character*(*) TextIn, TextOut                                                                                                 
      integer length, l1,l2                                                                                                         
																    
      write(*,*)                                                                                                                    
      length = LEN(TextIn)                                                                                                          
!                                                                                                                                   
!     count the number of trailing blanks                                                                                           
!                                                                                                                                   
																    
      do while (TextIn(length:length) .eq. ' ')                                                                                     
	length = length-1                                                                                                           
      enddo                                                                                                                         
																    
      length=length+1                                                                                                               
																    
!      if (length .ge. 75) then                                                                                                     
!        write(*,*) 'Error in routine SCREEN_IN_A: '                                                                                
!        write(*,*) 'Text string : ',textin                                                                                         
!        write(*,*) 'is too long. It should be shorter than 75 chars.'                                                              
!        stop                                                                                                                       
!      endif                                                                                                                        
																    
      l1 = length/10       ! the tens                                                                                               
      l2 = length-l1*10    ! the units                                                                                              
																    
																    
      if (l1 .ne. 0) then                                                                                                           
	L1=L1+48                                                                                                                    
	l2=l2+48                                                                                                                    
																    
	flength = '('' '',A'//char(L1)//char(L2)//','' --> '')'                                                                     
      else                                                                                                                          
	l2=l2+48                                                                                                                    
	flength = '('' '',A'//char(L2)//','' --> '')'                                                                               
      endif                                                                                                                         
																    
      write (*,flength) textin                                                                                                      
      read(*, '(A)') TextOut                                                                                                        
																    
      return                                                                                                                        
      end ! of routine Screen_in_a                                                                                                  
																    
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                           
																    
! ///////////////////////////////////////////////////////////////////////                                                           
																    
      Subroutine ConCat(Str1, Str2, Str3)                                                                                           
																    
! ////////////////////////////////////////////////////////////////////////                                                          
!                                                                                                                                   
!  trims trailing blanks from string2, adds on string3, retruns string1                                                             
!                                                                                                                                   
!                                                                                                                                   
      character*(*) Str1                                                                                                            
      character*(*) Str2                                                                                                            
      character*(*) Str3                                                                                                            
      integer length                                                                                                                

																    
																    
      length = LEN(STR2)                                                                                                            

      str2(length:length) = ' '

      do while (STR2(length:length) .eq. ' ')                                                                                       
	length = length-1                                                                                                           
      enddo                                                                                                                         
																    
      length=length+1                                                                                                               
																    
      STR1=STR2(1:length)//STR3                                                                                                     
																    
      return                                                                                                                        
      end                                                                                                                           

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
																    
																    
!//////////////////////////////////////////////////////////////////////////                                                         
																    
      Subroutine WinConvert( Text, j,k)                                                                                             
																    
!///////////////////////////////////////////////////////////////////////////                                                        
!                                                                                                                                   
!   converts a matrix of <text> from IBM extended-ASCII to                                                                          
!    MS-Windows extended character codes.                                                                                           
!    Only                                                                                                                           
!                                                                                                                                   
!                                                                                                                                   
       integer j, k                                                                                                                 
       character*(*) text(j,k)                                                                                                      
       integer l, ichange, i1,i2,i3                                                                                                 
       character*18 WIN, IBM                                                                                                        
       character*500 line                                                                                                           
																    
       IBM='†ÖÉÇäà¢ìï§á£°Üëîèôí'                                                                                                    
       WIN='·‡‚ÈËÍÛÙÚÒÁ˙ÌÂÊ¯≈ÿ∆'                                                                                                    
																    
       l=len(Text(1,1))                                                                                                             
       if (l .gt. 500) then                                                                                                         
	 write(*,*) 'ERROR in (SCR_IO) WinConvert: line too lonng'                                                                  
	 stop                                                                                                                       
       endif                                                                                                                        
																    
       do i1=1,j                                                                                                                    
	 do i2=1,k                                                                                                                  
	   LINE = text(i1,i2)                                                                                                       
	   do i3=1,l                                                                                                                
	     do ichange=1,18                                                                                                        
	       if (Line(i3:i3) .eq. IBM(ichange:ichange))               &                                                           
		  Line(i3:i3)= WIN(ichange:ichange)                                                                                 
	     enddo                                                                                                                  
	   enddo                                                                                                                    
	   text(i1,i2)=line                                                                                                         
	 enddo                                                                                                                      
       enddo                                                                                                                        
																    
       return                                                                                                                       
       end                                                                                                                          
																    
																    
! ////////////////////////////////////////////////////////////////////                                                              
																    
      Subroutine Iocheck(ioerr, filename)                                                                                           
																    
! ///////////////////////////////////////////////////////////////////                                                               
!                                                                                                                                   
!    Simply dectects a missing file error code and warns the user.                                                                  
!                                                                                                                                   
!                                                                                                                                   
!                                                                                                                                   
      include 'message1.inc'                                                                                                        
      integer ioerr                                                                                                                 
      character*40 filename                                                                                                         
      character*80 text(1)                                                                                                          
																    
! ----------- Code 6416 is specific to Microsoft FORTRAN                                                                            
																    
																    
!      if (ioerr .eq. 6416) then                                                                                                    
!        write(*,*) 'File not found: ',filename                                                                                     
!        stop                                                                                                                       
!      endif                                                                                                                        
																    
      if (ioerr .gt. 0) then                                                                                                        
	write(*,*) 'Ioerr : ',ioerr                                                                                                        
	Call Concat(Text(1),HK(40,Language),filename)                                                                               
	Call Screen_out_a(Text,1,1)                                                                                                        
      endif                                                                                                                         
      return                                                                                                                        
      end                                                                                                                           
																    
																    
																    
! //////////////////////////////////////////////////////////////////

      Subroutine SOutputWindow(Title, InfileName)

! ///////////////////////////////////////////////////////////////////
!
!  For DOS, this does nothing.
!
      Character*(*) Title, InfileName

!
 
      return
      end



!////////////////////////////////////////////////////////////////////////

      Subroutine WinCodes

!//////////////////////////////////////////////////////////////////////////
      include 'message1.inc'

      CAll WinConvert(HK,45,4)                                                                                                     
      Call WinConvert(H,30,4)                                                                                                      
      Call WinConvert(KO,10,4)                                                                                                     
      Call WinConvert(KY,10,4)                                                                                                     
      CAll WinConvert(HL,38,4)                                                                                                     
      CAll WinConvert(HM,61,4)                                                                                                     
      Call Winconvert(HP,79,4)                                                                                                     
      Call WinConvert(HW,30,4)
      CAll WinConvert(KW,5,4)
      return
      end



!////////////////////////////////////////////////////////////////////////

      Subroutine DOSCodes

!//////////////////////////////////////////////////////////////////////////
      include 'message1.inc'

      CAll DOSConvert(HK,45,4)                                                                                                     
      Call DOSConvert(H,30,4)                                                                                                      
      Call DOSConvert(KO,10,4)                                                                                                     
      Call DOSConvert(KY,10,4)                                                                                                     
      CAll DOSConvert(HL,38,4)                                                                                                     
      CAll DOSConvert(HM,61,4)                                                                                                     
      Call DOSconvert(HP,79,4)                                                                                                     
      Call DOSConvert(HW,30,4)
      CAll DOSConvert(KW,5,4)
      return
      end




!c////////////////////////////////////////////////////////////////////////// 

      Subroutine DOSConvert( Text, j,k)

!c///////////////////////////////////////////////////////////////////////////
!c
!c   converts a matrix of <text> from IBM extended-ASCII to 
!c    MS-Windows extended character codes.
!c    Only some codes altered, for spanish, french and norwegian text
!c
!c                                        
       implicit none
       integer j, k
       character*(*) text(j,k)
       integer l, ichange, i1,i2,i3
       character*18 FWIN, IBM
       character*500 line

       Parameter (IBM='†ÖÉÇäà¢ìï§á£°Üëîèôí')
       Parameter (FWIN='·‡‚ÈËÍÛÙÚÒÁ˙ÌÂÊ¯≈ÿ∆')

       l=len(Text(1,1))



       if (l .gt. 500) then
	 write(*,*) 'ERROR in (SCR_IO) DOSConvert: line too lonng'
	 stop
       endif

       do i1=1,j
	 do i2=1,k
	   LINE = text(i1,i2)

	   do i3=1,l
	     do ichange=1,18
	       if (Line(i3:i3) .eq. FWIN(ichange:ichange)) then
		  Line(i3:i3)= IBM(ichange:ichange)      

	       endif
	     enddo
	   enddo
	   text(i1,i2)=line
	 enddo
       enddo

       return
       end

