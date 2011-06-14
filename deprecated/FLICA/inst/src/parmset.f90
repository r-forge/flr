module parmset_module

use kind_module
use data_definition
use message_definition
use screen_io_module

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!  
! UNIT PARMSET.FOR 
!  
! contains most of the code for screen dialogue, 
! and setting up the parameter list, 
! the weights on the catches at age 
! and also three functions to do with the VPA 
! calculations 
!  
!  
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
! Updated 5 November 1997 to make all screen-inputs and outputs using 
! routines in the unit 'scr_io.for', so that this can be changed 
! for each implmentation with different screen interfaces. 
! Code to check that there are enough degrees of freedom to fit the 
! specified survey models. 

! Changed Dec 1998 to work as subroutine call without using 'INDAT'
! common block, and add in choice of weights for
! SRR age-surveys and SSB surveys
!
! Modified L Kell so that filename is passed as an argument

contains

! ******************************************************************************

Subroutine ParmSet(ControlFile)

!(maxSep, maxyear, maxage, maxsurvey, maxbsurv,Missing, &
!NySep, RefAge, TermS, TermS2, LoFage, HiFage, &
!CN, Aindex,Bindex, CNLambda, &
!nssbix,nageix,QAparm,QBParm,FitSrr,TwoSel,StepSel,ChangeSel,plusgp, &
!nxparm, nxdata, &
!firstage, lastage, firstyear, lastyear,fyear,lyear,fage,lage,fbyear,lbyear) 

! Starting-up housekeeping for various parameters 
! 
! Reads run-time options from the screen: 
! Number of years for separable constraint 
! Reference age 
! Terminal selection 
! First and last ages for arithmetic mean F in output table 
! Weighting options for the catch-at-age matrix 
! Choice of catchability model for each index 
! whether to fit a stock-recruit relationship 
! 
! 
! 
! 
 implicit none 

! Variables passed as subroutine parameters

! integer maxsep
! integer maxage,maxyear,maxsurvey, maxBsurv
! integer firstyear,lastyear, firstage, lastage, fbyear,lbyear
! logical TwoSel, FitSRR, StepSel, plusgp(maxsurvey)
! integer fyear(MaxSurvey),lyear(maxsurvey), fage(maxsurvey), lage(maxsurvey)

! double precision CNlambda(maxyear,maxage), missing
! double precision CN(maxyear,maxage)
! double precision Aindex(maxsurvey,maxyear,maxage), Bindex(maxBsurv,maxyear)


! integer NySep, RefAge, LoFage, HiFage, nssbix,nageix, QAParm(maxsurvey), &
! QBparm(MaxBsurv), ChangeSel, nxparm, nxdata, lag

! double precision TermS, TermS2
 
! Local variables 

	integer :: age, year, index, iage, iyear, j ! local looping vars 
	real (wp) :: sum, count 
	character (len = 75), dimension (10) :: Text(10)
    character*(*) ControlFile
	character (len = 5) :: Ytext 
	character (len = 1) :: dummy 
	real (wp) :: rdummy, MaxLambda, MinLambda 
	real (wp), dimension (maxsurvey) :: ascale
	real (wp), dimension (maxbsurv) :: bscale
	real (wp), dimension (maxage) :: RelWt
	real (wp), dimension (maxyear) :: YearWt

 
! Executable code 

	! Open input control file
	open (unit = 12, file=ControlFile)

 
	MaxLambda = 10.0d0 
	MinLambda = 0.0001d0

!FLR
	language = 1

! ENTER NUMBER OF YEARS FOR SELECTION PATTERN FIT

	!NySep = 6
	!call Screen_in_i(HK(1,language),NySep,MaxSep,3,Language) 
	read (12, *) NySep !************************************************
	!write (*, *) HK(1, Language), NySep

	!Refage = firstage+3
	!call Screen_in_i(HK(2,Language), Refage, lastage-3, firstage+1 & 
	!	,Language) 
	read (12, *) Refage !************************************************
	!write (*, *) HK(2, Language), Refage

	!dummy = KO(1,Language)
	!call Screen_in_a(HK(3,Language),dummy,KO(1,language),Language) 
	read (12, *) dummy !************************************************
	!write (*, *) HK(3, Language), dummy

! CHOOSE ONE OR TWO SELECTION PATTERNS

 TwoSel = .false. 
 StepSel = .false. 
 if (scan(Dummy, KY(1,language)) .eq. 0) then 

	TwoSel = .True.
	read (12, *) ChangeSel !************************************************
	!write (*, *) HK(4,Language), ChangeSel

	!ChangeSel= lastyear-3
	!Call Screen_in_i(HK(4,Language),ChangeSel,lastyear-1, & 
	!	lastyear-NySep,Language)

	read (12, *) dummy !************************************************
	!write (*, *) HK(5,Language), dummy

	!dummy=' '
	!call Screen_in_a(HK(5,Language),dummy,KO(2,Language),Language) 
	if (scan(dummy, KY(2,Language)) .eq. 0) then 
		StepSel = .False. 
	else 
		StepSel = .True. 
	endif 

 endif ! two selection patterns

! TERMINAL SELECTION VALUES

	!TermS=1d0
	!call Screen_in_R(HK(6,Language), TermS, 1.7d0, 0.01d0,Language) 

	read (12, *) TermS !************************************************
	!write (*, *) HK(6,Language), TermS

!TermS2=1d0 
if (TwoSel) then 
	read (12, *) TermS2 !************************************************
	!write (*, *) HK(7, Language), TermS2
	!call Screen_in_R(HK(7,Language),TermS2,1.7d0,0.01d0,Language) 
endif 
 
! First and last ages for calculation of mean F 
 
LoFage = -1 
do while ((LoFage .gt. HiFage) .or. (LoFage .lt. firstage) & 
	.or. (HiFage .gt. lastage)) 

	LoFage=firstage
	HiFage=lastage
	
	read (12, *) LoFage !************************************************
	!write (*, *) HK(8, Language), LoFage
	!call Screen_in_i(HK(8,Language),LoFage,lastage,firstage, & 
	!	Language) 

	read (12, *) HiFage !************************************************
	!write (*, *) HK(9, Language), HiFage
	!call Screen_in_i(HK(9,Language), HiFage, lastage, firstage, & 
	!	Language) 

	if (LoFage .gt. HiFage) then 
		Text(1)=HK(10,Language) 
		call Screen_out_a(Text,10,1) 
	endif 
 enddo ! reading and checking first and last ages 
 
read (12, *) dummy !************************************************
!call Screen_in_a(HK(11,Language),dummy,KO(1,Language),Language) 
 
 IF (scan(dummy, KY(1,Language)) .eq. 0) then
	Text(1)= HK(12,Language) 
	Call SCREEN_OUT_A( Text,1,1) 
	 
	do age = firstage, lastage 
		if (age .le. 9) then 
			Call ConCat(Text(1), HK(13,Language),char(48+age)) 
		else 
			Call ConCat(Text(1), HK(13,Language), & 
				char(48+(age/10))//char(48+age-10*(age/10)) ) 
		endif 

		read (12, *) RelWt(age-firstage+1) !************************************************
		!RelWt(age-firstage+1)=1d0
		!CAll Screen_in_r(Text(1),RelWt(age-firstage+1), &
		!	MaxLambda,MinLambda,Language) 
	enddo 
	Text(1)= HK(14,Language) 
	Call SCREEN_OUT_A(Text,1,1) 
	do year = lastyear-NySep+1, lastyear 
		Call IntToChar(year,ytext,5) 
		Call Concat(Text(1),HK(15,Language), ytext) 

		read (12, *) rdummy !************************************************
		!rdummy=1d0
		!CAll Screen_in_r(Text(1),rdummy,MaxLambda, &
		!	MinLambda,Language)

		YearWt(year-firstyear+1)=rdummy
	enddo ! years 
 

	open (unit=123,status="unknown",file="c:\temp\icaCNWTdebug.txt")                  
    write(123,*) "type"," value"," year"," age"
    do year = lastyear-NySep+1, lastyear 
		iyear = year-firstyear+1 
		do age = firstage, lastage 
			iage = age-firstage+1 
			CNLambda(iyear,iage)=dsqrt(RelWt(iage)*YearWt(iyear)) 
	    write(123,*) "catch",CNLambda(iyear,iage),iyear,iage
  	   enddo 
	enddo 
	close(123)

    
	Text(1) =HK(16,Language) 
	Call SCREEN_OUT_A( Text, 10,1 ) 
	year = 2 
	Text(1)=HK(17,Language) 
	year=firstyear
	age=firstage
	do while (year .ne. -1)
		rdummy=1d0
		read (12, *) year, age, rdummy	! ***********************************
		!Call SCREEN_IN_IR(Text(1), year, age, rdummy, lastyear, & 
		!	lastyear-NySep,lastage,firstage,MaxLambda,MinLambda,Language) 
		if ((year .ne. -1) .and.(age .ne. -1) .and. (rdummy .ne. -1d0)) & 
			CNlambda(year-firstyear+1,age-firstage+1)=dsqrt(rdummy)
		if (year .ne. -1) then
			if (age .ne. lastage) then
				age=age+1
			else
				age=firstage
				year=year+1
			endif
		endif
	 enddo 
 
 ELSE ! Set all weights = 1 
 
	 do age = firstage, lastage 
	 RelWt(age-firstage+1) = 1d0 
	 enddo 
 
 do year = lastyear-NySep+1, lastyear 
	 YearWt(year-firstyear+1) = 1d0 
	 enddo 


do year = lastyear-NySep+1, lastyear 
	iyear = year-firstyear+1 
	do age = firstage, lastage 
		iage = age-firstage+1
		CNLambda(iyear,iage)=(RelWt(iage)*YearWt(iyear))**0.5d0 
	 enddo 
enddo

ENDIF 
 
! WHETHER OR NOT THE PLUS-GROUP IS INCUDED IN SURVEY DATA
 
 do index = 1, nageix 
	Call ConCat(Text(1),HK(18,Language),ASurvlab(index))
	Call ConCat(Text(1),Text(1),HK(19,Language))

	read (12, *) dummy !************************************
	!write (*, *) Text(1), dummy
	!dummy= KO(1,language)
	!Call Screen_in_a(Text(1),dummy,KO(1,Language),Language) 
 
	if (scan(dummy,KY(1,Language)) .ne. 0 ) then 
	 plusgp(index) = .true. 
	else 
	 plusgp(index) = .false. 
	endif 
 enddo ! Indices 
 
	 
 Text(1) = HK(20,Language) 
 Text(2) = ' ' 
 Text(3) = HK(21,Language) 
 Text(4) = HK(22,Language) 
 Text(5) = HK(23,Language) 
 Text(6) = ' ' 
 Text(7) = HK(24, Language) 
 Text(8) = HK(25, Language) 
 Text(9) = ' ' 
 call SCREEN_OUT_A(Text,10,9) 
 
goto 33
 
 
 do index = 1,nssbix 
	Call Concat(Text(1),HK(26,Language), BsurvLab(index)) 
	CAll Concat(Text(1),Text(1),HK(27,Language)) 
	
	read (12, *) dummy !***************************************
	!write (*,*) Text(1), dummy
	!dummy='L'
	!Call SCREEN_IN_A(Text(1),dummy,'AaLlPp',Language)

	if ((dummy .eq. 'A') .or. (dummy .eq. 'a')) QBparm(index)=0
	if ((dummy .eq. 'L') .or. (dummy .eq. 'l')) QBparm(index)=1 
	if ((dummy .eq. 'P') .or. (dummy .eq. 'p')) QBparm(index)=2 
 enddo 
 
 do index = 1,nageix 
	Call Concat(Text(1),HK(26,Language), AsurvLab(index)) 
	CAll Concat(Text(1),Text(1),HK(27,Language)) 

	read (12, *) dummy !***************************************
	!write (*,*) Text(1), dummy
	!dummy='L'
	!Call SCREEN_IN_A(Text(1),dummy,'AaLlPp',Language)

	if ((dummy .eq. 'A') .or. (dummy .eq. 'a')) QAparm(index)=0 
	if ((dummy .eq. 'L') .or. (dummy .eq. 'l')) QAparm(index)=1 
	if ((dummy .eq. 'P') .or. (dummy .eq. 'p')) QAparm(index)=2 
 enddo 
 
goto 33
 
! count the catch-at-age observations 
	nxdata = 0 
	 
 do year = lastyear-NySep+1, lastyear 
	do age = firstage, lastage-1 
 ! if (CN(year-firstyear+1, age-firstage+1) .ne. Missing) then 
if (CN(year-firstyear+1, age-firstage+1) > 0.0d0) then 
	 nxdata = nxdata+1 
	 endif 
	enddo 
 enddo 
 
 if (nxdata .ne. NySep*(lastage-firstage)) then
 
	j = Nysep*(lastage-firstage)-nxdata 
 
 ! j is number of missing observations in separable model 
 
 write(Ytext, '(I5)') j 
 
 CAll ConCat(Text(1), HK(28,Language), Ytext)
 CAll ConCat(Text(1), Text(1), HK(29,Language) ) 
 Call SCREEN_OUT_A(Text, 10,2) 
 endif 
 
  

	NxParm = 2*NySep-1 + (lastage-firstage) + (lastage-firstage-2) 
	 
	if (TwoSel) NxParm = NxParm+ (lastage-firstage-2) 
 
 
	do index = 1, nssbix 
	 sum = 0d0 
	 count = 0d0 
	 Nxparm = Nxparm+1 
	 DO year = fbyear, lbyear 
	 if (BSurvey(index, year-fbyear+1) .ge. 0.0 ) then 
	 sum = sum+dble(BSurvey(index, year-fbyear+1))
	 count = count+1d0 
	 endif 
	 enddo 
! 
! CHECK ENOUGH DEGREES OF FREEDOM 
! 
 if (count .lt. QBParm(index)) then 
	 Text(1) = HK(30,Language) 
	 Call Concat(Text(2),HK(31,Language),BSurvlab(index)) 
	 Call Screen_Out_A(Text,10,2) 
	 Stop 
	 endif 
 
 
	 Bscale(index) = sum/count 
 
	 nxdata = nxdata + idint(count) 
 
	enddo ! indices 
 
 ! Next do the age-structured indices 
 
	do index = 1, nageix 
	 do age = fage(index), lage(index) 
	 NxParm = Nxparm+1 
	 sum = 0d0 
	 count = 0d0 
	 DO year = fyear(index), lyear(index) 
	 iage = age-fage(index)+1 
	 if (ASurvey(index, year-fyear(index)+1,iage) .ne. missing) & 
 then 
		 sum =sum+dble(ASurvey(index, year-fyear(index)+1,iage)) 
		 count = count+1d0 
	 endif 
	 enddo ! years 
 
	 nxdata = nxdata + idint(count) 
	 Ascale(index) = sum/count 
 if (count .le. QAParm(index)) then 
	 Text(1) = HK(30,Language) 
	 Call Concat(Text(2),HK(31,Language),ASurvlab(index)) 
	 if (age .gt. 9) then 
		Call concat(Text(2),HK(42,Language), & 
 char(48+age/10)//char(48+age-10*(age/10))) 
	 else 
		Call concat(Text(2),HK(42,Language), & 
 char(48+age)//'.') 
	 endif 
	 Call Screen_Out_A(Text,10,2) 
	 Stop 
	 endif 
 
	 enddo ! ages 
	enddo ! indices 

  
! Stock-recruit relationship 
 
read (12, *) dummy !***************************************
!write (*,*) HK(32,Language), dummy
!dummy = KY(1,Language)
!Call SCREEN_IN_A(HK(32,Language),dummy, KO(1,Language),Language)
 
If (scan(dummy, KY(1,Language)) .ne. 0) then 
	FitSRR = .true. 
else 
	FitSrr = .false. 
endif 
 
lag = 0 
If (FitSRR) then 
	Text(1) = HK(33,Language) 
	Call Concat(Text(2),HK(34,Language),char(48+firstage)) 
	CAll Concat(Text(2),Text(2),HK(35,Language)) 
	Call Concat(Text(3),HK(36,Language),char(48+firstage)) 
	CAll Concat(Text(3),Text(3),HK(37,Language)) 
	Call Concat(Text(4),HK(38,Language),char(48+firstage+1)) 
	Call Concat(Text(4),Text(4),HK(43,Language)) 
	Call SCREEN_OUT_A(Text,10,4) 
	
	read (12, *) lag
	!write (*,*) HK(39, Language), lag
	!call SCREEN_IN_I(HK(39,Language),lag,5,-5,Language)
 else
 lag=-5
 endif 
 
call WeightingOptions

33 continue
close(unit = 12)

 return 
 end subroutine parmset 
 
 
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
 
 
! ///////////////////////////////////////////////////////////////////////

 Subroutine WeightingOptions

!(Alambda, Blambda, SRRLambda, IxCor, &
!fage, lage, maxweight, RWOpt,FitSRR,nageix, nssbix, & 
!maxsurvey, maxbsurvey, maxage)

! ///////////////////////////////////////////////////////////////////////

use CONTROL

 implicit none

! integer maxsurvey, maxBsurvey, maxage
! integer fage(maxsurvey), lage(maxsurvey), RWOpt, nageix, nssbix

! double precision IxCOr(maxsurvey), SRRLambda, Alambda(maxsurvey,maxage), &
! Blambda(maxBsurvey)
! double precision maxweight

! logical FitSRR


! Local Variables
 
 character*1 dummy
 integer index, age, ik
 character*80 Text(10)
 character*4 ytext


 RWOpt = 0 ! reweighting option set to null value
 language =1
 
read (12, *) dummy !***************************************
!write (*,*) HM(2,Language), dummy
!dummy=KO(3,Language) 
!Call Screen_in_a(HM(2,Language),dummy,KO(3,Language),Language) 

if (scan(dummy, KY(3,Language)) .eq. 0) then 
	RWOpt = 1 
	Maxweight=1d0
	read (12, *) Maxweight
	!write (*,*) HM(48,Language), dummy
	!Call Screen_in_r(HM(48,Language),MaxWeight,10d0,0.5d0, &
	!	Language) 
else 
	RWOpt = 2 
endif 
 
 
 if (RWOpt .eq. 1) then !-------------------------- set starting weights all = 1 
 do index = 1, nssbix ! ------------------------- but will be changed iteratively 
 Blambda(index) = 1d0 
 enddo 
 do index = 1, Nageix 
 do age= fage(index), lage(index) 
 Alambda(index, age-fage(index)+1) = 1d0 
 enddo 
 IxCor(index) = 0.5d0 
 enddo 
 if (FitSRR) then 
 SRRLambda = 1d0 
 endif 
 endif ! Reweighting option = 1 
 
 
 if (RWOpt .eq. 2) then ! ------------------------ Weights to be read from screen and fixed 
 
 
 
 do index = 1, nssbix 
 Call Concat(Text(1),HM(3,Language),BsurvLab(index))

read (12, *) Blambda(index) !***************************************
!write (*,*) Text(1), Blambda(index)
!Blambda(index)=1d0
!CAll Screen_in_r(Text(1),Blambda(index),10d0,1d-8,Language) 

 enddo 
 do index =1, Nageix 
 do age=fage(index),lage(index) 
 Call Concat(Text(1),HM(3,Language),AsurvLab(index)) 
 Call Concat(Text(1),Text(1),HM(4,Language)) 
 Call IntToChar(age,ytext(1:4),4) 
 Call Concat(Text(1),Text(1),ytext(1:4)) 

read (12, *) Alambda(index,age-fage(index)+1) !***************************************
!write (*,*) Text(1), Alambda(index,age-fage(index)+1)
!Alambda(index,age-fage(index)+1)=1d0
!CAll Screen_in_r(Text(1),Alambda(index,age-fage(index)+1) &
!	,10d0,1d-8,Language) 

 enddo ! ages 
 enddo ! indices 
 
 if (FitSRR) then ! --------------------------- this is not strictly correct, never mind
	read (12, *) SRRlambda !***************************************
	!write (*,*) HM(5,Language), SRRlambda
	!CAll Screen_in_r(HM(5,Language),SRRlambda,10d0,1d-8,Language) 
 endif 
 
 if (nageix .gt. 0) then 
 do ik =1,4 
 Text(ik)=HM(ik+5,Language) 
 enddo 
 
 Call Screen_out_a(Text,5,4) 
 
 
do index = 1,nageix 
	Call ConCat(Text(1),HM(10,Language),ASurvlab(index))

	read (12, *) IxCOr(index) !***************************************
	!write (*,*) Text(1), IxCOr(index)
	!IxCOr(index)=1d0
	!Call Screen_in_r(Text(1),IxCor(index),1d0,0d0,Language) 

	IxCor(index) = 1. - IxCor(index) 
enddo ! indices 
endif ! any age-structured indices 
 
 endif ! RWOpt = 2 , weights to be fixed by hand. 
 
 return
 end subroutine WeightingOptions

 
 
 
 
 
 
 
 
! //////////////////////////////////////////////////////////////////// 
 
 Subroutine GetPageWidth(pw, min,max) 
 
! ///////////////////////////////////////////////////////////////////// 
 
 integer pw, min, max 
 
!FLR read (12, *) pw
pw=165
!write (*,*) HK(41,Language), pw
!CAll SCREEN_IN_I(HK(41,Language),pw, max,min,Language) 
 
 return 
 
 end subroutine GetPageWidth 
 
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
 


! ///////////////////////////////////////////////////////////////////////

 Subroutine tWeightingOptions


! ///////////////////////////////////////////////////////////////////////

use CONTROL

 implicit none


! Local Variables
 
 character*1 dummy
 integer index, age, ik
 character*80 Text(10)
 character*4 ytext


 RWOpt = 0 ! reweighting option set to null value
 language =1
 
read (12, *) dummy !***************************************

if (scan(dummy, KY(3,Language)) .eq. 0) then 
	RWOpt = 1 
	Maxweight=1d0
	read (12, *) Maxweight
else 
	RWOpt = 2 
endif 
 
 
 if (RWOpt .eq. 1) then !-------------------------- set starting weights all = 1 
 do index = 1, nssbix ! ------------------------- but will be changed iteratively 
 Blambda(index) = 1d0 
 enddo 
 do index = 1, Nageix 
 do age= fage(index), lage(index) 
 Alambda(index, age-fage(index)+1) = 1d0 
 enddo 
 IxCor(index) = 0.5d0 
 enddo 
 if (FitSRR) then 
 SRRLambda = 1d0 
 endif 
 endif ! Reweighting option = 1 
 
 
 if (RWOpt .eq. 2) then ! ------------------------ Weights to be read from screen and fixed 
  
 do index = 1, nssbix 
 Call Concat(Text(1),HM(3,Language),BsurvLab(index))

read (12, *) Blambda(index) !***************************************
 enddo 

return; close(12)


 do index =1, Nageix 
 do age=fage(index),lage(index) 
 Call Concat(Text(1),HM(3,Language),AsurvLab(index)) 
 Call Concat(Text(1),Text(1),HM(4,Language)) 
 Call IntToChar(age,ytext(1:4),4) 
 Call Concat(Text(1),Text(1),ytext(1:4)) 

read (12, *) Alambda(index,age-fage(index)+1) !***************************************

 enddo ! ages 
 enddo ! indices 


 
 if (FitSRR) then ! --------------------------- this is not strictly correct, never mind
	read (12, *) SRRlambda !***************************************
 endif 
 
 if (nageix .gt. 0) then 
 do ik =1,4 
 Text(ik)=HM(ik+5,Language) 
 enddo 
 
 Call Screen_out_a(Text,5,4) 
 
 
do index = 1,nageix 
	Call ConCat(Text(1),HM(10,Language),ASurvlab(index))

	read (12, *) IxCOr(index) !***************************************

	IxCor(index) = 1. - IxCor(index) 
enddo ! indices 
endif ! any age-structured indices 
 

 endif ! RWOpt = 2 , weights to be fixed by hand. 
 
 return
 end subroutine tWeightingOptions


end module parmset_module
