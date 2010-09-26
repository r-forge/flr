
!c //////////////////////////////////////////////////////////////////////
!c    UNIT SCR_IO : SCREEN INPUT ROUTINES
!c         Three routines to read an integer, a real or a character
!c         string from the screen
!c         THIS VERSION FOR WINDOWS I/O USING SALFORD CLEARWIN
!c      
!c //////////////////////////////////////////////////////////////////////


!      Program Test
!      use MSWIn
!      implicit none


!      character*60 textin, Valid, Text(5)
!      integer output,max,min, Language
!      character*1 dummy

!      double precision R, Rmax, RMin
!      integer I1, I1min,I1Max,I2, I2Min, I2Max, ctl,fch,i

!      common  /control/ ctl

!      max=100
!      min = -100
!      language=1
!      output=-200
!      fch=6
!
!      textin = 'Choose --> '
!      VAlid = 'aAlLpP'
!
!      call SOutputWindow(i)
!      write(*,*) 'Test 1'
!      
!      Output=-200
!      Call SCREEN_IN_I(Textin, Output, Max, Min, Language)
!      write(*,*) 'Test 2'
!
!      Text(1)= '--------'
!      write(Text(2),*) Output
!      Text(3)= '--------'
!      call Screen_out_A(Text, 5, 3)
!
!
!      CAll SCREEN_IN_A(Textin, Dummy, Valid, Language)
!      write(*,*) dummy
!
!      I1=1995
!      I1Max=2050
!      I1Min=1900
!      I2=0
!      I2Min=0
!      I2Max=10
!      R=1d0
!      RMin=1d-3
!      Rmax=10d0
!
!
!      CAll SCREEN_IN_IR(                                     &
!       TEXTin,I1,I2,R,I1Max,I1Min,I2Max,I2Min,RMax,RMin,Language)
!      write(*,*) I1, I2, R
!
!      end
!
!
!c //////////////////////////////////////////////////////////////////////
!c
      Subroutine SCREEN_IN_I(Text, Output, Max, Min,Language)
!c
!c //////////////////////////////////////////////////////////////////////
!c     Reads an integer from screen      
!c
      implicit none
      character*(*) Text

      integer output, Max,Min, length  

      integer language
      character*15 Progname
      integer g


      integer logfile

      common /log/ logfile, Progname, g


      Call Screen_in_iv(Text, Output, Max, Min, Language)


      length=len(text)
      do while (text(length:length) .eq. ' ')
	length=length-1
      enddo

      write(*,*) Text(1:length),'-->',Output
      write(logfile,*) Text(1:length),'-->',Output


      end

!
!c //////////////////////////////////////////////////////////////////////
!c
      Subroutine SCREEN_IN_R(Text, Output, Max, Min,Language)
!c
!c //////////////////////////////////////////////////////////////////////
!c     Reads a real*8 from screen      
!c
      implicit none
      character*(*) Text
      character*19 flength
      double precision output, Max,Min
      integer  length
      character*31 MinErr(4), Maxerr(4)
      integer language, ctl
      character*15 Progname
      integer g

      integer logfile

      common /log/ logfile, Progname, g



      Call Screen_in_v(Text, Output, Max, Min, Language)

!      Output=Routput

      length=len(text)
      do while (text(length:length) .eq. ' ')
	length=length-1
      enddo

      write(*,*) Text(1:length),'-->',Output
      write(logfile,*) Text(1:length),'-->',Output

      return
      end


!c //////////////////////////////////////////////////////////////////////
!c
      Subroutine SCREEN_IN_IV(Text, value, Max, Min, Language)
!c
!c //////////////////////////////////////////////////////////////////////
!c     Reads an INTEGER  from WINDOW

      Use MSWIN32
      Use CLRWIN

      implicit none
      character*(*) Text
      character*200 Textx 
      character*19 flength
      integer  length, L1, L2,ctl, end_input
      external end_input
      character*15 Progname
      integer g


      integer  value, Max,Min

      logical begin, error2
      character *200 Etext

      character*45 MinErr(4), Maxerr(4), Vtext
      integer language, ans, logfile

      common /log/ logfile, Progname, g

      IF (mIN .GT. mAX) then
	write(*,*) Text
	write(*,*) 'Error in Screen_in_I: Max, Min : ',Max, Min
      endif

      do l1=1,len(Textx)
	Textx(l1:l1)=' '
      enddo

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
!c
!c     count the number of trailing blanks
!c
      
      do while (Text(length:length) .eq. ' ')
	length = length-1
      enddo
!c
!c     Make up the text argument for WINIO call
!c

!      length=length+1
      Textx= Text(1:length)//"%rd%ta"//char(38)

!c  The windows text box for input
!c
!c    THIS IS WINDOWS I/O
!c
!c


      begin = .true.
      error2= .false.

      do while (error2 .or. begin)

      begin=.false.
      ANS=winio@('%sp& ', 70,470)
      ANS=WINIO@('%ww[]&')
      ANS=WINIO@('%ca[ '//Progname(1:g)//' Input Box I]&')
      if (Error2) ANS=WINIO@('%si#'//Etext)

      ANS=WINIO@(Textx(1:length+8), Value)
      ans=WINIO@('%ta%`^bt[&Ok]&','EXIT')
      ans=WINIO@('%cc',end_input)

      error2=.false.
      if (Value .gt. Max) then
	error2=.true.
	write(Vtext,*) Max
	Call ConCat(Etext,Maxerr(Language),VText)
	Call ConCat(Etext,Etext, Char(10)//'&')
      endif

      if (Value .lt. Min) then
	error2=.true.
	write(Vtext,*) Min
	Call ConCat(Etext,Minerr(Language),VText)
	Call ConCat(Etext,Etext, Char(10)//'&')
      endif

      enddo





      if (Max .le. Min) then
	write(*,*) 'Error in routine SCREEN_IN_R: '
	write(*,*) 'Inconsistent MAX and MIN :',Max, Min
	stop
      endif




      return
      end ! of routine SCREEN_IN_R

      integer function End_Input()
       End_Input=0
      return
      end

!c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


!c //////////////////////////////////////////////////////////////////////
!c
      Subroutine SCREEN_IN_V(Text, value, Max, Min, Language)
!c
!c //////////////////////////////////////////////////////////////////////
!c     Reads a double-precision real  from WINDOW

      Use MSWIN32
      Use CLRWIN

      implicit none
      character*(*) Text
      character*200 Textx 
      character*19 flength
      integer  length, L1, L2,ctl, end_input
      external end_input
      character*15 Progname
      integer g


      double precision  value, Max,Min

      logical begin, error2
      character *200 Etext

      character*45 MinErr(4), Maxerr(4), Vtext
      integer language, ans, logfile

      common /log/ logfile, Progname, g

      IF (mIN .GT. mAX) then
	write(*,*) Text
	write(*,*) 'Error in Screen_in_R: Max, Min : ',Max, Min
      endif

      do l1=1,len(Textx)
	Textx(l1:l1)=' '
      enddo

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
!c
!c     count the number of trailing blanks
!c
      
      do while (Text(length:length) .eq. ' ')
	length = length-1
      enddo
!c
!c     Make up the text argument for WINIO call
!c

!      length=length+1
      Textx= Text(1:length)//"%rf%ta"//char(38)

!c  The windows text box for input
!c
!c    THIS IS WINDOWS I/O
!c
!c


      begin = .true.
      error2= .false.

      do while (error2 .or. begin)

      begin=.false.
      ANS=winio@('%sp& ', 70,470)
      ANS=WINIO@('%ww[]&')
      ANS=WINIO@('%ca[ '//Progname(1:g)//' Input Box R]&')
      if (Error2) ANS=WINIO@('%si#'//Etext)

      ANS=WINIO@(Textx(1:length+8), Value)
      ans=WINIO@('%ta%`^bt[&Ok]&','EXIT')
      ans=WINIO@('%cc',end_input)

      error2=.false.
      if (Value .gt. Max) then
	error2=.true.
	write(Vtext,*) Max
	Call ConCat(Etext,Maxerr(Language),VText)
	Call ConCat(Etext,Etext, Char(10)//'&')
      endif

      if (Value .lt. Min) then
	error2=.true.
	write(Vtext,*) Min
	Call ConCat(Etext,Minerr(Language),VText)
	Call ConCat(Etext,Etext, Char(10)//'&')
      endif

      enddo





      if (Max .le. Min) then
	write(*,*) 'Error in routine SCREEN_IN_R: '
	write(*,*) 'Inconsistent MAX and MIN :',Max, Min
	stop
      endif




      return
      end ! of routine SCREEN_IN_R


!c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++




!c //////////////////////////////////////////////////////////////////////
!c
      Subroutine SCREEN_IN_A(Text, Dummy, Valid, Language)
!c
!c //////////////////////////////////////////////////////////////////////
!c  
!c     Accepts a non-blank character variable from screen
!c      Output string is 'Output' 
!c      it must be contained within the string 'valid'
!c      which holds a list of valid choices
!c      
      Use MSWIN32
      Use CLRWin

      character*(*) Text
      character*(*) Valid
      character*19 flength
      character*200 text2
      character*15 Progname
      character*(*) dummy
      integer  length, tscan, ans, elength
      character*100 Err(4), etext
      integer language, g
      logical error2, first

      integer logfile
      
      common /log/ logfile, Progname, g

      if (Language .gt. 4) Language = 1
      if (Language .le. 0 ) Language =1 

      Err(1)='ERROR: Choose between  '//valid//char(10)//'%ta&'
      Err(2)='ERROR: Elige  '//valid//char(10)//'%ta&' 
      Err(3)='ERREUR: Choissez  '//valid//char(10)//'%ta&'
      Err(4)='FEIL: Valg mellom '//valid//char(10)//'%ta&'

      Etext=Err(language)
      elength=len(etext)
      do while (Etext(elength:elength) .eq. ' ')
	elength=elength-1
      enddo

      
      length = LEN(Text)
!c
!c     count the number of trailing blanks
!c
      
      do while (Text(length:length) .eq. ' ')
	length = length-1
      enddo

!      length=length+1

      Text2=Text(1:length)//' %rs%ta&'

      error2=.true.
      first =.true.

      do while (error2)
	ANS=WINIO@('%sp&',70,470)
	ANS=WINIO@('%ww[]&')
	ANS=WINIO@('%ca[ ' //Progname(1:g)//' Input Box A]&')
	if (error2.and. .not. first )  ANS=WINIO@('%si#'//EText(1:elength))
	ans=WINIO@(Text2(1:length+8), dummy)
	ans=WINIO@('%ta%`^bt[O&k]','EXIT') 
	IF (tscan(dummy, Valid) .ne. 0) error2=.false.
	first= .false.
      enddo

      write(*,*) text(1:length),'-->',dummy

      write(logfile,*) text(1:length),'-->',dummy

      return

      end ! of routine SCREEN_IN_I

!c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      



!C ////////////////////////////////////////////////////////////////////////////////

       Integer Function Tscan( str1, str2 )
       
!c ////////////////////////////////////////////////////////////////////////////////       
										  
!c     Returns the position of the first string character that matches a 
!c     character in str2, returns zero if there's no match
!c            
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
			
!c ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                                                       


!c///////////////////////////////////////////////////////////////////////

      Subroutine Screen_out_A(Text, isize, lines)

!c /////////////////////////////////////////////////////////////////////
!c     writes text strings to screen      

      Use MSWIN32
      Use ClrWin

      integer isize, lines, n, fch,ctl,i, g
      character*(*) Text(isize)
      character*15 Progname

      integer logfile

      common /log/ logfile, progname, g

      do n=1, lines
	write(*,'(A)') Text(n)
	write(logfile,'(A)') Text(n)

!         i=WinIO@('%aw'//Text(n)//Char(10)//,ctl)
      enddo
!      i=WINIO@('%aw ',ctl)
      return
      end

!c +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!c ////////////////////////////////////////////////////////////////////////

      SUBROUTINE SCREEN_IN_IR2(                                     &
       TEXT,I1,R,I1Max,I1Min,RMax,RMin,Language)

!c /////////////////////////////////////////////////////////////////////////
!c
!c     Reads one integer and one real, preceded by a request text
!c
      Use MSWIN32
      Use CLRWIN

      implicit none




      character*(*) Text
      character*19 flength
      character*77 Text2
      character*90 Etext
      character*50 Maxerr(4), Minerr(4)
      integer length, I1, I1max, I1Min, l1,l2
      double precision R, Rmax, RMin

      integer terminate, ans,g
      external terminate
      integer language
      character*15 progname
      logical error2, begin
      character*77 Vtext

      integer logfile
      common /log/ logfile, progname,g


      if (Language .gt. 4) Language = 1
      if (Language .le. 0 ) Language =1 

      MinErr(1)=': ERROR: Minimum allowed is : '
      MinErr(2)=': ERROR: El m°nimo permitido es :'
      MinErr(3)=': ERREUR: Le minimum permis est :'
      MinErr(4)=': FEIL: Minimum tillatelig er : '

      MaxErr(1)=': ERROR: Maximum allowed is : '
      MaxErr(2)=': ERROR: El m†ximo permitido es :'
      MaxErr(3)=': ERREUR: Le maximum permis est :'
      MaxErr(4)=': FEIL: Maximum tillatelig er : '



      length = LEN(Text)
!c
!c     count the number of trailing blanks
!c
      
      do while (Text(length:length) .eq. ' ')
	length = length-1
      enddo

!      length=length+1

      error2=.false.
      begin = .true.

      do while (begin .or. error2)
	begin=.false.

	TExt2=Text(1:length)//char(10)//'%ta%rd%ta%rf%ta&'

	ANS=WINIO@('%sp&',70,460)
	ANS=WINIO@('%ca[ '//ProgName(1:g)//' Input Box IR2]&')
	if (Error2) ANS=WINIO@('%si#'//Etext)
	ans=WINIO@(Text2(1:length+23),I1,R)
	ans=WINIO@('%ta%`^bt[&Next]&','EXIT')
	ANS=WINIO@('%ta%^bt[&Finish]',Terminate)

	if (ANS .eq. 2) then
	  I1=-1
	  R=-1d0
	endif


	error2=.false.


	if ((R .gt. RMax) .and. (R .ne. -1d0)) then
	  error2=.true.
	  
	  write(Vtext,'(E20.11)') R
	  Call ConCat(Etext,Vtext,Minerr(Language))
	  write(Vtext,'(E20.11)') RMax
	  Call ConCat(Etext,Etext,Vtext)
	  Call ConCat(Etext,Etext, Char(10)//'&')
	endif

	if ((R .lt. RMin).and.(R .ne.-1d0)) then
	  error2=.true.
	  
	  write(Vtext,'(E20.11)') R
	  Call ConCat(Etext,Vtext,Minerr(Language))
	  write(Vtext,'(E20.11)') RMin
	  Call ConCat(Etext,Etext,Vtext)
	  Call ConCat(Etext,Etext, Char(10)//'&')
	endif





	if ((I1 .gt. I1Max) .and. (I1 .ne. -1)) then
	  error2=.true.
	  write(Vtext,*) I1
	  Call ConCat(Etext,Vtext,Maxerr(Language))
	  write(Vtext,*) I1Max
	  Call ConCat(Etext,Etext,Vtext)
	  Call ConCat(Etext,Etext, Char(10)//'&')
	endif

	if ((I1 .lt. I1Min).and.(I1 .ne.-1)) then
	  error2=.true.
	  write(Vtext,*) I1
	  Call ConCat(Etext,Vtext,Minerr(Language))
	  write(Vtext,*) I1Min
	  Call ConCat(Etext,Etext,Vtext)
	  Call ConCat(Etext,Etext, Char(10)//'&')
	endif

      enddo ! reading/error checking




      write(*,*) Text(1:length),' ',I1,R
      write(logfile,*) Text(1:length),' ',I1,R


      return

      end ! of routine SCREEN_IN_IR2




!c ////////////////////////////////////////////////////////////////////////

      SUBROUTINE SCREEN_IN_IR(                                     &
       TEXT,I1,I2,R,I1Max,I1Min,I2Max,I2Min,RMax,RMin,Language)

!c /////////////////////////////////////////////////////////////////////////
!c
!c     Reads two integers and one real, preceded by a request text
!c
      Use MSWIN32
      Use CLRWIN

      implicit none




      character*(*) Text
      character*19 flength
      character*77 Text2
      character*90 Etext
      character*50 Maxerr(4), Minerr(4)
      integer length, I1, I1max, I1Min, I2, I2max, I2Min, l1,l2
      double precision R, Rmax, RMin

      integer terminate, ans,g
      external terminate
      integer language
      character*15 progname
      logical error2, begin
      character*77 Vtext

      integer logfile
      common /log/ logfile, progname,g


      if (Language .gt. 4) Language = 1
      if (Language .le. 0 ) Language =1 

      MinErr(1)=': ERROR: Minimum allowed is : '
      MinErr(2)=': ERROR: El m°nimo permitido es :'
      MinErr(3)=': ERREUR: Le minimum permis est :'
      MinErr(4)=': FEIL: Minimum tillatelig er : '

      MaxErr(1)=': ERROR: Maximum allowed is : '
      MaxErr(2)=': ERROR: El m†ximo permitido es :'
      MaxErr(3)=': ERREUR: Le maximum permis est :'
      MaxErr(4)=': FEIL: Maximum tillatelig er : '



      length = LEN(Text)
!c
!c     count the number of trailing blanks
!c
      
      do while (Text(length:length) .eq. ' ')
	length = length-1
      enddo

!      length=length+1

      error2=.false.
      begin = .true.

      do while (begin .or. error2)
	begin=.false.

	TExt2=Text(1:length)//char(10)//'%ta%rd%ta%rd%ta%rf%ta&'

	ANS=WINIO@('%sp&',70,460)
	ANS=WINIO@('%ca[ '//ProgName(1:g)//' Input Box IR]&')
	if (Error2) ANS=WINIO@('%si#'//Etext)
	ans=WINIO@(Text2(1:length+23),I1,I2,R)
	ans=WINIO@('%ta%`^bt[&Next]&','EXIT')
	ANS=WINIO@('%ta%^bt[&Finish]',Terminate)

	if (ANS .eq. 2) then
	  I1=-1
	  I2=-1
	  R=-1d0
	endif


	error2=.false.


	if ((R .gt. RMax) .and. (R .ne. -1d0)) then
	  error2=.true.
	  
	  write(Vtext,'(E20.11)') R
	  Call ConCat(Etext,Vtext,Minerr(Language))
	  write(Vtext,'(E20.11)') RMax
	  Call ConCat(Etext,Etext,Vtext)
	  Call ConCat(Etext,Etext, Char(10)//'&')
	endif

	if ((R .lt. RMin).and.(R .ne.-1d0)) then
	  error2=.true.
	  
	  write(Vtext,'(E20.11)') R
	  Call ConCat(Etext,Vtext,Minerr(Language))
	  write(Vtext,'(E20.11)') RMin
	  Call ConCat(Etext,Etext,Vtext)
	  Call ConCat(Etext,Etext, Char(10)//'&')
	endif



	if ((I2 .gt. I2Max) .and. (I2 .ne. -1)) then
	  error2=.true.

	  write(Vtext,*) I2
	  Call ConCat(Etext,Vtext,Maxerr(Language))
	  write(Vtext,*) I2Max
	  Call ConCat(Etext,Etext,Vtext)
	  Call ConCat(Etext,Etext, Char(10)//'&')
	endif

	if ((I2 .lt. I2Min).and.(I2 .ne.-1)) then
	  error2=.true.
	  write(Vtext,*) I2
	  Call ConCat(Etext,Vtext,Minerr(Language))
	  write(Vtext,*) I2Min
	  Call ConCat(Etext,Etext,Vtext)
	  Call ConCat(Etext,Etext, Char(10)//'&')
	endif



	if ((I1 .gt. I1Max) .and. (I1 .ne. -1)) then
	  error2=.true.
	  write(Vtext,*) I1
	  Call ConCat(Etext,Vtext,Maxerr(Language))
	  write(Vtext,*) I1Max
	  Call ConCat(Etext,Etext,Vtext)
	  Call ConCat(Etext,Etext, Char(10)//'&')
	endif

	if ((I1 .lt. I1Min).and.(I1 .ne.-1)) then
	  error2=.true.
	  write(Vtext,*) I1
	  Call ConCat(Etext,Vtext,Minerr(Language))
	  write(Vtext,*) I1Min
	  Call ConCat(Etext,Etext,Vtext)
	  Call ConCat(Etext,Etext, Char(10)//'&')
	endif

      enddo ! reading/error checking




      write(*,*) Text(1:length),' ',I1,I2,R
      write(logfile,*) Text(1:length),' ',I1,I2,R


      return

      end ! of routine SCREEN_IN_IR

      Integer function Terminate()
	Terminate = 0
      end


!c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++




!c //////////////////////////////////////////////////////////////

      Subroutine IntToChar(INTX,STRING,LENGTH)

!c //////////////////////////////////////////////////////////////

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

!c         concatenate the AK strings without blanks
	  
	  string = '     '
	  do k=length, 1, -1
	    if (ak(k) .ne. ' ') then 
	      if (String(1:1) .eq. ' ') then
		String(1:1) = ak(k)
	      else
		STRING= STRING(1:TSCAN(' ',STRING)-1)//ak(k)
	      endif
	    endif
!c            write(*,*) k, ak(k)
!c            write(*,*) 'Substring>',string(1: TSCAN(' ',STRING)),'<'
!c            write(*,*) 'string>',string,'<'
!c            write(*,*) 'Posn Blank =',Tscan(' ',string)
	  enddo

	  
	  


	  return
	  end
!c +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!c ////////////////////////////////////////////////////////////////////

      Subroutine Screen_in_s(TextIn, TextO)

!c ///////////////////////////////////////////////////////////////////
!c
!c    writes Textin to the screen, reads TextOut from user
!c
      Use MSWIN32
      Use CLRWIN

      implicit none
      character*19  flength
      character*(*) TextIn, TextO
      character*140 Textx
      integer length, l1,l2, ans, g
      integer logfile
      character*15 Progname

      common /log/ logfile, progname, g


      length = LEN(TextIn)

      if (length .ge. 140) then
	write(*,*) 'Error in routine SCREEN_IN_S: '
	write(*,*) 'Text string : ',textin
	write(*,*) 'is too long. It should be shorter than 140 chars.'
	stop
      endif

!c
!c     count the number of trailing blanks
!c



      do while (TextIn(length:length) .eq. ' ')
	length = length-1
      enddo

!      length=length+1

      Textx=TextIn(1:Length)//char(10)//'%rs&'

      ANS=winio@('%sp& ', 70,470)
      ANS=WINIO@('%ww[]&')
      ANS=WINIO@('%ca[ '//Progname(1:g)//' Input Box S]&')


      ANS=WINIO@(Textx(1:Length+5),  TextO)
      ans=WINIO@('%ta%`^bt[&Ok]','EXIT')

      write(*,*) Textx(1:length),' -->',TextO
      write(logfile,*) Textx(1:length),' -->',TextO



	return
	end
!c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!c ///////////////////////////////////////////////////////////////////////

      Subroutine ConCat(Str1, Str2, Str3)

!c ////////////////////////////////////////////////////////////////////////
!c
!c  trims trailing blanks from string2, adds on string3, retruns string1
!c
!c
      character*(*) Str1
      character*(*) Str2
      character*(*) Str3
      integer length
      character*1 blank


      length = LEN(STR2)

      str2(length:length) = ' '

      do while (STR2(length:length) .eq. ' ')
	length = length-1
      enddo

      length=length+1

      STR1=STR2(1:length)//STR3

      return
      end
!c ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


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


!c////////////////////////////////////////////////////////////////////////// 

      Subroutine WINConvert( Text, j,k)

!c///////////////////////////////////////////////////////////////////////////
!c
!c   converts a matrix of <text> from IBM extended-ASCII to 
!c    MS-Windows extended character codes.
!c    Only some coes altered, for spanish, french and norwegian text
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
	 write(*,*) 'ERROR in (SCR_IO) WinConvert: line too lonng'
	 stop
       endif

       do i1=1,j
	 do i2=1,k
	   LINE = text(i1,i2)

	   do i3=1,l
	     do ichange=1,18
	       if (Line(i3:i3) .eq. IBM(ichange:ichange)) then
		  Line(i3:i3)= FWIN(ichange:ichange)      

	       endif
	     enddo
	   enddo
	   text(i1,i2)=line
	 enddo
       enddo

       return
       end


!c ////////////////////////////////////////////////////////////////////

      Subroutine Iocheck(ioerr, filename)

!c ///////////////////////////////////////////////////////////////////
!c
!c    Simply dectects a missing file error code and warns the user.
!c
!c
!c
      include 'message1.inc'
      integer ioerr, ctl
      character*40 filename
      character*80 text(1)

!c ----------- Code 6416 is specific to Microsoft FORTRAN


!c      if (ioerr .eq. 6416) then
!c        write(*,*) 'File not found: ',filename
!c        stop
!c      endif

      if (ioerr .gt. 0) then
	write(*,*) 'Ioerr : ',ioerr
	Call Concat(Text(1),HK(40,Language),filename)
	Call Screen_out_a(Text,1,1)
      endif
      return
      end



!c ////////////////////////////////////////////////////////////////

      Subroutine SOutputWindow(Title,LOGFILENAME)

!c ////////////////////////////////////////////////////////////////


     Use MSWIN32
     Use CLRWIN

     implicit none
     integer i, ctl, ctl2, color, red, green, blue
     save ctl
     character*(*) Title, LogFileName

     character*15 ProgName 

     include 'indat.inc'
     include 'message1.inc'


     integer logfile, x, y, g

     common /log/ logfile, Progname, g

!
!    Open a file for run log
!
     logfile = 99
     open(logfile, file=LOGFILENAME, status='Unknown')

     Progname=Title
     g=len(ProgName)
     do while(progname(g:g) .eq. ' ')
       g=g-1
     enddo

!
!    Code to convert the extended ascii chars in messgages from DOS IBM codes to MS Windows codes                                                                         
!                                                                                                                                   
																    
      CAll WinCodes


!
!   Set up the output window using ClearWin calls
!


      ctl=-1

      color=rgb@(60,30,30)
      i= winio@('%ww%mn[&File[E&xit]]&','STOP')
      i = winio@('%bg&',color)

      x=750
      y =500 

!      i = winio@('%sp&', 20,10)
      i = winio@('%sz&',x,y )  
      i= winio@('%ca['//ProgName(1:g)//']&')

      i=winio@('%fr%lw',x,y, ctl)    ! 700, 470
      i= winio@('%aw%ca['//Progname(1:g)//' Output]&',ctl)

      i= winio@('%aw%ww%mn[&File[E&xit],&Edit[&Copy,Cu&t,&Paste]]&',ctl,'EXIT','COPY','CUT','PASTE')
      i= winio@('%aw%78.20cw[hscroll,vscroll]',ctl,0)

!      call setMaxLines(i,200)




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
      CAll WinConvert(HL,43,4)                                                                                                     
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
      CAll DOSConvert(HL,43,4)                                                                                                     
      CAll DOSConvert(HM,61,4)                                                                                                     
      Call DOSconvert(HP,79,4)                                                                                                     
      Call DOSConvert(HW,30,4)
      CAll DOSConvert(KW,5,4)
      return
      end


