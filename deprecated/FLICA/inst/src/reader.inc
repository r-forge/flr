module reader_module

! Principal I/O routines for ICA

	use kind_module
	use data_definition
	use message_definition
	use screen_io_module

contains   
  
! **********************************************************************

!FLR modified so that file name passed as argument to function
Subroutine Reader(indexname)

! (LA, CN, CW, SW, NM, MO,PF, PM, firstyear, &
! lastyear, firstage, lastage, maxyear, maxage)
  
	! Reads in data from files in the Lowestoft format, except that (1) it   
	! assumes all data are in matrix format save for PF and PM  
	! and (2) the natural mortality, maturity ogive and stock weight 
	! files may extend one year past the last year of catch-at-age data.
	! If they do not, these values are assumed equal to the values 
	! given for the last year and the user is warned to this effect. 
	! This routine does not read the survey data. 

	! 6/11/00: input stream redirected from keyboard to local
	! emas.in file

	! 8/11/00: text output commented out to hasten bootstrapped runs
	
! Input variables

! integer :: maxyear, maxage, firstyear, lastyear, firstage, lastage
! real (wp), dimension (maxyear) :: LA
! real (wp), dimension (maxyear, maxage) :: CN, CW, SW, NM, MO
! real (wp) :: PF, PM

	! Local variables

	character (len=60) :: indexname, cnname, laname, cwname, swname, moname, &
		nmname, pfname, pmname, title   
	character (len = 76), dimension (30) :: Text(30)
  
	integer :: year, dummy1, dummy2, ftype, ioerr
	character (len = 5) :: ytext
	integer :: inf ! input file number  
  
	! Indexname : name of the file holding the list of names of the 
	!   catch and biological sampling data files, being: 
	! CNName : name of the file holding catch numbers at age and year
	! LAName :   " landings by year  
	! CWName :   " weight at age and year in the catches  
	! SWName :   " weight at age and year in the stock  
	! MOName :   " maturity at age and year 
	! NMName :   " natural mortality at age and year  
	! PFName :   " proportion of fishing mortality before spawning 
	! PMName :   " proportion of natural mortality before spawning 
	! Title : a variable used for reading and ignoring non-numeric data such as titles 
  
	! ioerr is an I/O error check integer. It is zero if a file is opened OK,  
	! other values indicate some form of error. 
	! here, only IOERR=6416 is interpreted as 'FILE MISSING'. The values of   
	! IOERR are specific to Microsoft FORTRAN. 
  
	inf = 15   
	language = 1  
  
	! Display title 
  
	! Read the name of the file which lists the names of datafiles
  
	ioerr = 1   
!FLR	indexname= ' '
	do while (ioerr .ne. 0)
!FLR		read (12, *) indexname !************************************************
 		! call SCREEN_IN_S(H(1,language), indexname)  

!FLR added to turn off cosole IO for DLL
		if (DLLFlag .eq. .false.) then 
			print *, "NAME OF INDEX FILE = ", indexname
		endif

		if (indexname .eq. '*') then 
			Text(1) = H(28,language) 
			call Screen_in_i(text(1),Language,4,1,Language) 
		endif 
  		if (indexname .eq. ' ') then 
			Text(1)=H(2,language) 
			Call Screen_Out_A(Text,30,1) 
			stop 
		endif 
		if (indexname .ne. '*') then 
			open(inf, file = indexname, iostat=ioerr, status='old')  
			if (ioerr .gt. 0) then 
				Text(1)=H(3,Language) 
	 			call Screen_Out_A(Text,30,1)   
			endif 
		endif 
	 end do 
  
	! Read the datafile names in sequence 
  
	read(inf,9000) title 
	! print *, title
	read(inf,9000) ytext 
	! print *, ytext
	read(inf,9000) laname 
	! print *, laname
	read(inf,9000) cnname 
	! print *, cnname
	read(inf,9000) cwname 
	! print *, cwname
	read(inf,9000) swname 
	! print *, swname
	read(inf,9000) nmname 
	! print *, nmname
	read(inf,9000) moname 
	! print *, moname
	read(inf,9000) pfname 
	! print *, pfname
	read(inf,9000) pmname 
	! print *, pmname
	close(inf)  
  
	! Open and read the landings file 
  
	open(inf, file = laname, status = 'old', iostat=ioerr)  
  	call iocheck(ioerr, laname)
  
	read(inf,9000) title 
	read(inf,*) dummy1, dummy2
	read(inf,*) firstyear, lastyear  
	read(inf,*) firstage, lastage   
	read(inf,*) ftype  
	if ((ftype .ne. 5) .or. (dummy1 .ne. 1)) then
		Text(1)= H(4,language)  
		call Screen_Out_A(Text,30,1) 
	endif 

	if (lastyear-firstyear+2 .gt. maxyear) then 
		Call IntToChar(maxyear-1,ytext,4)   
		Call CONCAT( Text(1),H(5,Language),ytext)  
		call Screen_Out_A(Text,30,1) 
	endif 
	if (lastage-firstage+1 .gt. maxage) then 
		Call IntToChar(maxage,ytext,4) 
		Call CONCAT( Text(1),H(6,Language),ytext)  
		call Screen_Out_A(Text,30,1) 
		stop 
	endif 
  
	do year = firstyear, lastyear  
		read(inf, *) la(year-firstyear+1)   
	end do 
  
	close(inf)  
  
	! Open and read the age-structured files   
 
	call ReadAgeMat(maxyear,maxage,firstage,lastage,firstyear, &
		lastyear, cnname, CN, 2) 
	call ReadAgeMat(maxyear,maxage,firstage,lastage,firstyear, &
		 lastyear, cwname, CW, 3) 
	call ReadAgeMat(maxyear,maxage,firstage,lastage,firstyear, &
		lastyear, swname, SW, 4) 
	call ReadAgeMat(maxyear,maxage,firstage,lastage,firstyear, &
		lastyear, nmname, NM, 5) 
	call ReadAgeMat(maxyear,maxage,firstage,lastage,firstyear, &
		lastyear, moname, MO, 6) 
    
	! Open and read the proportion of F before spawning 
  
	open(inf, file = pfname, status = 'old',iostat=ioerr)  
	call iocheck(ioerr, pfname)

	read(inf,9000) title 
	read(inf,*) dummy1, dummy2
	if (dummy2 .ne. 7) then 
		Text(1)= 'PF FILE ID IS WRONG' 
		call Screen_Out_A(Text, 30,1) 
	end if
  
	read(inf,*) dummy1, dummy2
	if ((DLLFlag .eq. .false.) .and. ((dummy1 .ne. firstyear).or. (dummy2 .ne. lastyear)))  & 
		write(*,*) 'PF FILE FORMAT ERROR: YEAR RANGE'  
	read(inf,*) dummy1, dummy2
	if ((DLLFlag .eq. .false.) .and. ((dummy1 .ne. firstage) .or. (dummy2 .ne. lastage))) &
		write(*,*) 'PF FILE FORMAT ERROR: AGE RANGE'  
	read(inf,*) ftype  
	if ((DLLFlag .eq. .false.) .and. ftype .ne. 3)  &
		write(*,*) 'PF FILE FORMAT ERROR: MUST BE SCALAR '  
	read(inf,*) PF  
  
	close(inf)  
  
	! Open and read the proprtion of M before spawning 

	open(inf, file = pmname, status = 'old', iostat=ioerr)  
	call iocheck(ioerr, pmname)
  
	read(inf,9000) title 
	read(inf,*) dummy1, dummy2
	if (dummy2 .ne. 8) write(*,*) 'PM FILE ID IS WRONG'  
	read(inf,*) dummy1, dummy2
	if ((DLLFlag .eq. .false.) .and. ((dummy1 .ne. firstyear).or. (dummy2 .ne. lastyear))) &
		write(*,*) 'PM FILE FORMAT ERROR: YEAR RANGE'  
	read(inf,*) dummy1, dummy2
	if ((DLLFlag .eq. .false.) .and. ((dummy1 .ne. firstage) .or. (dummy2 .ne. lastage)))  &
		write(*,*) 'PM FILE FORMAT ERROR: AGE RANGE'  
	read(inf,*) ftype  
	if ((DLLFlag .eq. .false.) .and. ftype .ne. 3)  &
		write(*,*) 'PM FILE FORMAT ERROR: MUST BE SCALAR '  
	read(inf,*) PM  

	close(inf)  
    
	9000 format (A)  
	9010 format (I2)  
	9020 format (A8, A40)  
  
end subroutine Reader
    
! ************************************************************************
  
!FLR modified so that file name passed as argument to function
subroutine ReadSBIx(fileName)

! (BIndex, nssbix, maxBsurv, maxyear, fbyear, lbyear, &
! firstyear, lastyear, Missing) 

	! Reads in any SSB Indices from the 'RCT' format file  

! Input variables

! integer :: nssbix, maxyear, fbyear, lbyear, firstyear, lastyear
! real (wp), dimension (maxBsurv,maxyear) :: BIndex
! real (wp) :: Missing
  
	! Local variables
  
	character (len = 60) :: FileName
	character (len = 200) :: String, Title
	integer :: inf, nyear, nvpa, i, year, index, j, k
	real (wp) :: VPA 
	logical :: BetweenQuotes 
	integer :: ioerr, iyear 
	character (len = 1) :: dummy  
	character (len = 40), dimension (maxbsurv+3) :: Label
	character (len = 78), dimension (3) :: Text

	! FileName : Name of the file holding indices of SSB  
	! inf : input file channel number 
	! nvpa : the column number in which the VPA estimates of 
	!   stock size are held. This information is used 
	!   by the MAFF RCT3 programme but not here.  
	! VPA : the VPA estimates of stock size
  
	inf = 15   
	ioerr = 1   
	nssbix = 1  
!FLR	fileName= ' '
	do while (ioerr .ne. 0) 
!FLR		read (12, *) Filename !************************************************
		! call Screen_in_s(H(17,Language), Filename)
  
		if (FileName .EQ. ' ') THEN ! its a blank 
			Text(1)=H(18,Language) 
			call Screen_Out_A(Text,3,1) 
			nssbix = 0
			ioerr = 0
		endif 
  
		if (nssbix .gt. 0) then 
			open(inf, file=filename, status='old',iostat=ioerr)
			call iocheck(ioerr,filename) 
	 		if (ioerr .eq. 0) then  
				read(inf, 9000) title 
				Text(1)=filename  
				Text(2)=title  
				read(inf, *) nssbix, nyear, nvpa   
	
				!  Error checking on year-range  

				if (nyear .gt. lastyear-firstyear+1+1) then
					goto 97  
				endif ! max number of years exceeded  
				read(inf, 9000) title 
	  
				! Read in the names for each survey across the line ....  
  	
				i = 0 
				j = 1 
				String = '  ' 
				BetweenQuotes = .False. 
  
				do while ((j .le. len_trim(Title) ).and. (i .le. nssbix+2))  
					dummy = Title(j:j) 
					if ((dummy .eq. '''') .or. (dummy .eq. """")) then  
						BetweenQuotes = (.not. BetweenQuotes)  
						if (BetweenQuotes) Then 
							if (i .gt. 0) Label(i) = String ! Put name into label  
							i = i+1 ! Moves to next label  
							j = j+1 ! Move on to next character  
							k = 1 ! flag start of this label  
						endif 
					endif
					if (BetweenQuotes .and. (i .le. nssbix+2)) then   
						k = k+1 
						dummy = Title(j:j)  
						String = String(1:k)//dummy 
					endif
					j=j+1 ! move to next character  
		 		end do ! labels/ characters
  
				Label(i) = String  
  	
				do i = 3, nssbix+2  
					BSurvLab(i-2) = Label(i) 
				end do 
  
				do i = 1,nyear  
					read(inf, *) year, VPA, (BSurvey(index,i), index=1,nssbix)  
					if (i .eq. 1) fbyear = year
					lbyear = year  
					if ((year.gt.lastyear+1).or.(year .lt. firstyear))then 
						goto 97  
					endif  
				end do ! reading years 
		
			end if ! no i/o error  

		end if ! an SSB index to be read  

	end do ! reading/checking SSb index file 
  
	! Convert to logs, insert missing value code 
  
	do index = 1, nssbix 
		do iyear = 1, lbyear-fbyear+1 
			if (BSurvey(index,iyear) .le. 0d0) then  
				BSurvey(index,iyear) = missing   
			else 
				BSurvey(index,iyear) = dlog(BSurvey(index,iyear))
			endif 
		end do 
	enddo 
  
	9000 format (A200)  
	return
  
	97 continue   
	Text(1) = H(19,Language) 
	call Screen_Out_A(Text,3,1)
	stop 
  
end subroutine ReadSBIx

! *************************************************************************

!FLR modified so that file name passed as argument to function  
subroutine ReadAgix(FileName)

! (AIndex,Maxsurvey,maxbsurv,maxyear,maxage,nageix,fyear,lyear, &
! fage, lage, Timing, firstage, lastage, firstyear, lastyear, missing) 
  
	! Reads an age-structured index file  

! Import variables

! integer :: firstage, lastage, firstyear, lastyear
! integer :: maxsurvey, maxbsurv, maxyear,maxage,nageix
! integer, dimension (maxsurvey) :: fyear, lyear, fage, lage
! real (wp), dimension (maxsurvey,maxyear,maxage) :: AIndex
! real (wp), dimension (maxsurvey) :: Timing
! real (wp) :: missing

	! Local variables  

	character (len = 60) :: Filename, title   
	integer :: index, age, inf, year, iyear, iage, ioerr   
	real (wp) :: dummy1, dummy2, dummy3 
	real (wp), dimension (maxsurvey,maxyear) :: effort
	character (len = 78), dimension (3) :: Text
	character (len = 5) :: ytext  

	! FileName : Name of the file holding the age-disaggregated tuning data   
	! Title : variable used to read a line of text
	! effort : In the MAFF format the first column holds the effort data for  
	!   fleet disaggregated CPUE tuning. For consistency the format  
	!   has been maintained here and so all the index data on each line are divided
	!   through by the first value on each line, which is the 'effort' value. 
  
	inf =15
	ioerr =1   
!FLR	FileName = ' '
	do while (ioerr .ne. 0)
!FLR		read (12, *) Filename !************************************************
		! write (*,*) "Filename = ", filename
		! call Screen_in_s(H(20,Language),filename)  
		if (FileName .EQ. ' ') then 
			nageix = 0
			Text(1)= H(21,Language) 
			call Screen_Out_A(Text,3,1) 
			ioerr = 0
			goto 9090 ! exit the routine 
		endif 

		open(inf, file = Filename, status = 'old', iostat=ioerr)  
		call iocheck(ioerr, filename) 
 
	end do ! reading/checking file name  
  
	read(inf,9000) title

	Text(1)=Filename  
	Text(2)=Title  
  
	read(inf, *) nageix
	nageix = nageix-100 
	if (nageix .gt. MaxSurvey) then  
		Call IntToChar(maxsurvey, ytext, 5)  
		Call Concat(Text(3),H(22,Language),ytext)  
		Call Screen_Out_A(Text,3,3) 
		stop 
	endif 
	if (nageix .lt. 1) then 
		Text(3)=H(23,Language) 
		call Screen_Out_A(Text,3,3) 
		stop 
		nageix = 0
	endif 
  
	do index = 1,nageix 
		read(inf, 9000) ASurvLab(index)  
		read(inf, *) fyear(index), lyear(index)
		read(inf, *) dummy1, dummy2, timing(index), dummy3   
		timing(index) = (timing(index)+dummy3)/2 
		read(inf, *) fage(index), lage(index)

		if (lage(index) .gt. lastage) then  
			call Concat(Text(3),H(24,language), ASurvlab(index))  
			call Screen_Out_A(Text,3,3)   
			stop 
		endif 
		if (lyear(index) .gt. lastyear+1) then 
			call Concat(Text(3),H(25,language), ASurvLab(index))  
			call Screen_Out_A(Text,3,3)   
			stop 
		endif 
		if (fyear(index) .lt. firstyear) then  
			call Concat(Text(3),H(26,language), ASurvLab(index))  
			call Screen_Out_A(Text,3,3)   
			stop 
		endif 
  
  
		 do year = fyear(index),lyear(index)  
			read(inf,*) (ASurvey(index,(year-fyear(index)+1),&
				(age-fage(index)+1) ),&
 				age = fage(index),lage(index)+1 )
		 end do 
	end do 
  
	! Data at first age in nageix is the effort value: extract, shift down
	! and divide the index through by the effort value
  
	do index = 1, nageix 
		do year = fyear(index), lyear(index)  
			iyear = year-fyear(index)+1 
			effort(index,iyear) = ASurvey(index, iyear, 1) 
			do age= fage(index), lage(index)  
				iage = age-fage(index)+1
				ASurvey(index,iyear, iage)= ASurvey(index,iyear,iage+1)/  &
 					effort(index,iyear)   
			end do ! ages  
	 	end do ! years  
	end do ! indices 
  
	! Convert to logs  
  
	do index = 1, nageix 
		do year = fyear(index), lyear(index)  
			iyear = year-fyear(index)+1 
			do age= fage(index), lage(index)  
				iage = age-fage(index)+1
				If (ASurvey(index,iyear,iage) .le. 0d0) then 
					ASurvey(index,iyear, iage)= missing
				else
					ASurvey(index,iyear, iage)= &
						dlog(ASurvey(index,iyear, iage))
				endif
			end do ! ages  
		end do ! years  
	end do ! indices 

	close(inf)  
  
	9000 format (A40)  
	9090 continue   
	 return

end subroutine ReadAgix
  
! ///////////////////////////////////////////////////////////////////// 
  
 Subroutine ReadAgeMat(maxyear,maxage,firstage,&
  lastage, firstyear, lastyear, filename, Matrix,filetype) 
  
! ///////////////////////////////////////////////////////////////////// 

 implicit none  
 integer maxyear, maxage, firstage, lastage 
 integer firstyear, lastyear, filetype  
 character*(*) filename 
 character* 132 title 
 character*80 Text(2) 
 character*5 ytext, ytext2 
 double precision Matrix(maxyear,maxage) 
 integer ioerr, dummy1, dummy2, lyy, inf, datatype,year,age 
  
 inf=15
  
 open(inf, file = filename, status = 'old', iostat=ioerr)  
  
 call iocheck(ioerr, filename)   
 read(inf,9000) title 
  
 read(inf,*) dummy1, dummy2
 if(dummy2 .ne. filetype) then  
	Text(1) = H(7,language) 
	Text(2) = Title  
	Call Screen_Out_A(Text, 2,2) 
	write(*,*) dummy2, filetype 
	stop 
 endif 
  
 read(inf,*) dummy1, lyy 
  
 if ((dummy1 .ne. firstyear).or.  &
 ((lyy .ne. lastyear) .and. (lyy.ne.lastyear+1))) then  
	Call Concat(Text(1), H(8,Language), filename) 
	Text(2) = Title  
	Call Screen_Out_A(Text, 2,2) 
	Stop 
 endif 
  
 read(inf,*) dummy1, dummy2
 if ((dummy1 .ne. firstage) .or. (dummy2 .ne. lastage)) then 
	Call Concat(Text(1), H(9,Language), filename) 
	Text(2) = Title  
	Call Screen_Out_A(Text, 2,2) 
	stop 
 endif 
  
 read(inf,*) datatype 
 if (datatype .ne. 1) then 
	Call Concat(Text(1), H(10,Language), filename) 
	Text(1) = filename  
	Call Screen_Out_A(Text, 2,2) 
	stop 
 endif 
  
  
 do year = firstyear, lyy 
     read(inf, *) (matrix(year-firstyear+1, age-firstage+1), age = firstage,lastage)
 enddo 

! Error trap to avoid negative catch numbers - BUT these should
! be allowed to be used!

!do year = firstyear, lyy
!	write (*,*) (matrix(year - firstyear + 1, age - firstage + 1), age = firstage, lastage)
!	do age = firstage, lastage
!		if (matrix(year-firstyear+1, age-firstage+1) .le. 0) then
!			continue			
!			matrix(year-firstyear+1, age-firstage+1) = -99.00
!			write (*,*) "Hit error trap!"
!		end if
!	end do
!end do
  
 if ((lyy .ne. lastyear+1) .and. (  &
  (filetype .eq. 4) .or. &
  (filetype .eq. 5) .or. &
  (filetype .eq. 6))) then
  
	Call IntToChar(lastyear+1,ytext,5)   
	Call IntToChar(lastyear,ytext2,5)   
  
	Call ConCat(Text(1),H(13+filetype-4,Language),ytext)
	Call ConCat(Text(1),Text(1),H(12,Language)) 
	Call Concat(Text(1),Text(1),ytext2)  
	Call Screen_Out_A(text,30,1) 
	do age = firstage,lastage 
		matrix(lastyear-firstyear+1+1,age-firstage+1)= &
			matrix(lastyear-firstyear+1,age-firstage+1)   
	enddo 
 endif 
  
  
 Call Concat(Text(1), filename,H(16,Language)) 
 Text(1) = filename 

 Call Screen_Out_A(Text, 2,1)   
  
  
 close(inf)  
 return
9000 format (A)  
 end subroutine ReadAgeMat
  
  
  
  
  
! ////////////////////////////////////////////////////////////////////////   
  
 Subroutine ReadBlock 
  
! ////////////////////////////////////////////////////////////////////////   
  
 ! Reads the values from the 'ICA.TMP' temporary file  
 ! into the three common blocks, ready to start the  
 ! minimisation.  
 ! 
 ! The corresponding WRITEBLK programme is in a separate unit, 
 ! named 'Writeblk' 
 ! 
  
! ------------------------LOCAL VARIABLES-------------------------- 
 integer age, iage, fch, ioerr, index, idummy, year,iyear,i 
 double precision rdummy
 character*77 Text(1)
! ------------------------------------------------------------------
  
! --------------------------EXECUTABLE CODE-------------------------
  
 fch = 14   
  
 Open (UNIT=fch,STATUS='OLD',IOSTAT=ioerr,FILE='ICA.TMP')  
  
 if (ioerr .ne. 0) then 
 Text(1)=HW(58,Language)
 Call Screen_Out_A(Text,1,1)
 stop
 endif 
  
 read(fch,7000)firstage,lastage,firstyear,lastyear,nageix,nssbix &
 , PF, PM, NxParm  
 if (nxParm .gt. maxparm) then   
	write(*,*)'Too many parameters. Recompile with maxparm >',  &
  nxparm  
	stop 
 else if ( (lastage-firstage+1).gt. maxage) then
	write(*,*) 'Too many ages. Recompile with maxage > ',   &
  (lastage-firstage+1) 
	stop 
 else if (( lastyear-firstyear+1) .gt. maxyear) then 
	write(*,*) 'Too many years. Recompile with maxyear > ',  &
 (lastyear-firstyear+1) 
	stop 
 else if (nageix .gt. maxsurvey) then  
	write(*,*) 'Too many aged surveys. Recompile with maxsurvey >'  &
 ,nageix   
	stop 
 else if (nssbix .gt. maxbsurv) then  
 write(*,*) 'Too many SSB indices. Recompile with maxbsurv >'  & 
 ,nssbix   
	 stop 
 endif 
  
7000 format(6(I4,1X),2(E25.16,1X),I4)  
  
! INIDICES   
  
 if (nssbix .gt. 0) then 
	read(fch,7010) fbyear, lbyear 
7010 format(2(I4,1X))  
	do year = fbyear,lbyear 
	 read(fch,7020) (BSurvey(index,year-fbyear+1), &
  index =1,nssbix) 
	enddo ! years of the ssb indices
 endif ! any ssb indices 
7020 format(8(E25.16,1X)) 
 do index = 1,nageix 
	read(fch,7030) fage(index),lage(index),fyear(index),&
  lyear(index), Timing(index)  
 enddo 
7030 format(4(I4,1X),1X,E25.16)
  
 do index = 1,nageix 
	do year = fyear(index),lyear(index)  
	 iyear = year-fyear(index)+1 
	 read(fch, 7040) (ASurvey(index,iyear,age-fage(index)+1),  &
  age=fage(index),lage(index))  
	enddo 
 enddo 
7040 format(15(E25.16,1X)) 
  
! AGE-STRUCTURED DATA 
  
 do year = firstyear,lastyear+1  
	iyear = year-firstyear+1 
	do age = firstage, lastage 
	 iage = age-firstage+1 
	 read(fch,7045) CN(iyear,iage),SW(iyear,iage),MO(iyear,iage),  &
  NM(iyear,iage), N(iyear,iage), F(iyear,iage)  
	enddo 
 enddo 
7045 format (6(E25.16,1X)) 
  
! LANDINGS   
  
 do year = firstyear, lastyear   
	read(fch, 7050) LA(year-firstyear+1)  
 enddo 
7050 format (E25.16)  
  
! THE CONTROL BLOCK FOR THE MODEL FITTING 
  
 read(fch, 7060) UseSep, Writeout, Full, TwoSel
7060 format(4L3)  
  
 read(fch, 7070) NySep, RefAge, NxData, NxParm, TermS,TermS2 
7070 format(4(I4,1X),2(E25.16,1X))   
  
 do i = 1,Nxparm  
	read(fch, 7080) Xbest(i), Xlow(i), Xhigh(i) 
 enddo 
7080 format(3(E25.16),1X) 
  
 do index = 1,nssbix 
	read(fch,7090) Blambda(index) 
 enddo 
  
 do index = 1, nageix 
	read(fch, 7090) (Alambda(index,iage),iage=1, &
  lage(index)-fage(index)+1)   
 enddo 
7090 format(13(E25.16,1X)) 
  
 do age = firstage, lastage
	iage = age-firstage+1  
	read(fch,7100) RelWt(iage) 
 enddo 
7100 format(E25.16)  
  
 do age = firstage, lastage
	iage = age-firstage+1  
	do year = firstyear, lastyear 
	 iyear = year-firstyear+1 
	 read(fch, 7110) W(iyear, iage)
	enddo 
 enddo 
7110 format(E25.16)  
  
 do i = 1,nageix  
	if (i .le. nssbix) then 
	 read(fch, 7120) rdummy, rdummy, Plusgp(i), QAparm(i),  &
  QBParm(i)  
	else 
	 read(fch, 7120) rdummy, rdummy, Plusgp(i), QAParm(i),  &
  idummy  
	endif 
 enddo 
7120 format(2(E25.16),L3,2(I2,1X))   
  
! the Stock-Recruit parameters   
  
 read(fch,*) lag  
  
! if (FitSRR) then  
! read(fch, *) SRRLambda, SRRVar  
! read(fch, *) fryear, lag, a, b  
! do i = 1,20  
!  read(fch, *) OldStock(i), OldRecruit(i) 
! enddo   
! endif
  
! Weights by year on the catches at age  
  
 do iyear=1,lastyear-firstyear+1  
	read(fch,*) YearWt(iyear) 
 enddo 
  
! First and last age for calculating reference F
  
 read(fch,*) LoFage, HiFage
  
!8100 format (L3)  
!8110 format (I4,1X,I2,1X,2E25.16)   
!8120 format (2E25.16)  
  
  
  
  
 close(fch)  
  
 return
 end  subroutine ReadBlock 

end module reader_module  
  
 
