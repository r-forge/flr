include "definitions.inc"
include "io_dos.inc"
include "reader.inc"
include "parmset.inc"
include "statistics.inc"
include "wtab.inc"
include "output.inc"
include "srr_.inc"
include "cvpa1.inc"
include "convpa.inc"
include "sepvpa.inc"
include "ica2.inc"
include "ica.inc"

subroutine ICAWRAPPER

	use kind_module
	use data_definition
	use message_definition
 	use wtab_module
	use output_module
	use reader_module
	use parmset_module
	use srr_module
	use convpa_module
	use sepvpa_module
	use cvpa1_module
	use ica_module
	use ica2_module
    use statistics_module
    use ARRAYLEN

		call Initialise_Global_Vars
 		call Text_Strings

		call Reader(  "D:\FLR\Packages\Tests\Validation\FLICA\ple7a\13\index.dat") 
		call ReadAgIx("D:\FLR\Packages\Tests\Validation\FLICA\ple7a\13\fleets.dat")
		call ReadSBIx("D:\FLR\Packages\Tests\Validation\FLICA\ple7a\13\ssbidx.dat")
		
		nxdata=1
		nxparm=1

		!control file same as emas but with the first 3 lines (file names) deleted
		!ParmSet modified to incorporate call to WeightingOptions
		call ParmSet("D:\FLR\Packages\Tests\Validation\FLICA\ple7a\13\control.dat")

		call AS1 (LA, CN, NM, SW, CW, MO, PF, PM, Asurvey, BSurvey, Nssbix, Nageix, &
 			maxage, maxyear, maxsurvey, maxparm, maxsep, maxBsurv, QAParm, &
			QBParm, HiFage, LoFage, Refage, X, COV, FPerceived, NPerceived, &
			S, firstyear, lastyear, firstage, lastage, fage, lage, fyear, &
			lyear, fbyear, lbyear,nextRec, FitSRR, IXCor, CNLambda, SRRlambda, &
			Alambda, Blambda, nxparm,nxdata, TermS, TermS2, ChangeSel, StepSel, &
			TwoSel, NySep, RWOpt, Maxweight, missing, Timing, plusgp)
	    
		call TableOut(1)

end subroutine 


subroutine ICAWRAPPER3

	use kind_module
	use data_definition
	use message_definition
 	use wtab_module
	use output_module
	use reader_module
	use parmset_module
	use srr_module
	use convpa_module
	use sepvpa_module
	use cvpa1_module
	use ica_module
	use ica2_module
    use statistics_module
    use ARRAYLEN

		call Initialise_Global_Vars
 		call Text_Strings

		call Reader(  "D:\FLR\Packages\Tests\Validation\FLICA\ple7a\13\index.dat") 
		call ReadAgIx("D:\FLR\Packages\Tests\Validation\FLICA\ple7a\13\fleets.dat")
		call ReadSBIx("D:\FLR\Packages\Tests\Validation\FLICA\ple7a\13\ssbidx.dat")
		
		nxdata=1
		nxparm=1

		!control file same as emas but with the first 3 lines (file names) deleted
		!ParmSet modified to incorporate call to WeightingOptions
		call ParmSet("D:\FLR\Packages\FLICA\inst\data\NSher\control.dat")

		call AS1 (LA, CN, NM, SW, CW, MO, PF, PM, Asurvey, BSurvey, Nssbix, Nageix, &
 			maxage, maxyear, maxsurvey, maxparm, maxsep, maxBsurv, QAParm, &
			QBParm, HiFage, LoFage, Refage, X, COV, FPerceived, NPerceived, &
			S, firstyear, lastyear, firstage, lastage, fage, lage, fyear, &
			lyear, fbyear, lbyear,nextRec, FitSRR, IXCor, CNLambda, SRRlambda, &
			Alambda, Blambda, nxparm,nxdata, TermS, TermS2, ChangeSel, StepSel, &
			TwoSel, NySep, RWOpt, Maxweight, missing, Timing, plusgp)
	    
		call TableOut(1)

end subroutine 


subroutine ICA_INIT

	use parmset_module

		call Initialise_Global_Vars
 		call Text_Strings
end subroutine 


subroutine ICA_STOCK_INPUT

	use reader_module

		call Reader("D:\FLR\Packages\Tests\Validation\FLICA\ple7a\13\index.dat") 
		
end subroutine ICA_STOCK_INPUT


subroutine ICA_CPUE_INPUT

	use reader_module

		call ReadAgIx("D:\FLR\Packages\Tests\Validation\FLICA\ple7a\13\fleets.dat")
		call ReadSBIx("D:\FLR\Packages\Tests\Validation\FLICA\ple7a\13\ssbidx.dat")
		
end subroutine ICA_CPUE_INPUT

subroutine ICA_CONTROL_INPUT

	use parmset_module

		nxdata=1
		nxparm=1

		!control file same as emas but with the first 3 lines (file names) deleted
		!ParmSet modified to incorporate call to WeightingOptions
		call ParmSet("D:\FLR\Packages\Tests\Validation\FLICA\ple7a\13\control.dat")

end subroutine ICA_CONTROL_INPUT


subroutine ICA_RUN


	use ica_module
	use ica2_module


! 		call Initialise_Global_Vars
!  		call Text_Strings

! 		call Reader(  "D:\FLR\Packages\Tests\Validation\FLICA\ple7a\13\index.dat") 
! 		call ReadAgIx("D:\FLR\Packages\Tests\Validation\FLICA\ple7a\13\fleets.dat")
! 		call ReadSBIx("D:\FLR\Packages\Tests\Validation\FLICA\ple7a\13\ssbidx.dat")
		
! 		nxdata=1
! 		nxparm=1

! 	nxdata=1
!	nxparm=1
! 	MaxLambda = 10.0d0 
!  	MinLambda = 0.0001d0

!	do index = 1, nageix 
!		plusgp(index) = xplusgp(index) 
!	enddo 

		!control file same as emas but with the first 3 lines (file names) deleted
		!ParmSet modified to incorporate call to WeightingOptions
! 		call ParmSet("D:\FLR\Packages\Tests\Validation\FLICA\ple7a\13\control.dat")

	do index = 1, nageix 
        if (xplusgp(index) .gt. 0) then 
			plusgp(index) = .True. 
		else
		    plusgp(index) = .False. 
		endif
	enddo 

		call AS1 (LA, CN, NM, SW, CW, MO, PF, PM, Asurvey, BSurvey, Nssbix, Nageix, &
 			maxage, maxyear, maxsurvey, maxparm, maxsep, maxBsurv, QAParm, &
			QBParm, HiFage, LoFage, Refage, X, COV, FPerceived, NPerceived, &
			S, firstyear, lastyear, firstage, lastage, fage, lage, fyear, &
			lyear, fbyear, lbyear,nextRec, FitSRR, IXCor, CNLambda, SRRlambda, &
			Alambda, Blambda, nxparm,nxdata, TermS, TermS2, ChangeSel, StepSel, &
			TwoSel, NySep, RWOpt, Maxweight, missing, Timing, plusgp)
	    
!		call TableOut(1)
	
!	call Initialise_Global_Vars
! 	call Text_Strings

!	call Reader(  "D:\FLR\Packages\Tests\Validation\FLICA\ple7a\13\index.dat") 
!	call ReadAgIx("D:\FLR\Packages\Tests\Validation\FLICA\ple7a\13\fleets.dat")
!	call ReadSBIx("D:\FLR\Packages\Tests\Validation\FLICA\ple7a\13\ssbidx.dat")
		

!    call ParmSet("D:\FLR\Packages\Tests\Validation\FLICA\ple7a\13\Emas.in2")
	
!	nxdata=1
!	nxparm=1
!	MaxLambda = 10.0d0 
!	MinLambda = 0.0001d0

!	LoFage=firstage
!	HiFage=lastage

!		call AS1 (LA, CN, NM, SW, CW, MO, PF, PM, Asurvey, BSurvey, Nssbix, Nageix, &
! 			maxage, maxyear, maxsurvey, maxparm, maxsep, maxBsurv, QAParm, &
!			QBParm, HiFage, LoFage, Refage, X, COV, FPerceived, NPerceived, &
!			S, firstyear, lastyear, firstage, lastage, fage, lage, fyear, &
!			lyear, fbyear, lbyear,nextRec, FitSRR, IXCor, CNLambda, SRRlambda, &
!			Alambda, Blambda, nxparm,nxdata, TermS, TermS2, ChangeSel, StepSel, &
!			TwoSel, NySep, RWOpt, Maxweight, missing, Timing, plusgp)

end subroutine ICA_RUN

subroutine ICA_OUTPUT

    use CONTROL
	use output_module
	    
	call TableOut(1) 

end subroutine ICA_OUTPUT

subroutine ICAWRAPPER2
	call ICA_INIT
	call ICA_STOCK_INPUT
	call ICA_CPUE_INPUT
	call ICA_CONTROL_INPUT
	call ICA_RUN
	call ICA_OUTPUT
end subroutine ICAWRAPPER2


logical function PARMINIT

use parmset_module
! Local variables 

	integer :: age, year, index, iage, iyear, j ! local looping vars 
	real (wp) :: sum, count 
	character (len = 75), dimension (10) :: Text(10)
	character (len = 5) :: Ytext 
	character (len = 1) :: dummy 
	real (wp) :: rdummy, MaxLambda, MinLambda 
	real (wp), dimension (maxsurvey) :: ascale
	real (wp), dimension (maxbsurv) :: bscale

 
! Executable code 

ParmInit = .FALSE.
 
! WHETHER OR NOT THE PLUS-GROUP IS INCUDED IN SURVEY DATA
 
! count the catch-at-age observations 
	nxdata = 0 
	 
 do year = lastyear-NySep+1, lastyear 
	do age = firstage, lastage-1 
	   if (CN(year-firstyear+1, age-firstage+1) > 0.0d0) then 
	      nxdata = nxdata+1 
	   endif 
	enddo 
 enddo 
 
 NxParm = 2*NySep-1 + (lastage-firstage) + (lastage-firstage-2) 
	 
 if (TwoSel) NxParm = NxParm+ (lastage-firstage-2) 
  
   do index = 1, nssbix 
      sum    = 0d0 
      count  = 0d0 
	  Nxparm = Nxparm+1 
	     DO year = fbyear, lbyear 
	        if (BSurvey(index, year-fbyear+1) .ge. 0.0 ) then 
	           sum = sum+dble(BSurvey(index, year-fbyear+1))
	           count = count+1d0 
	        endif 
	     enddo 

     ! CHECK ENOUGH DEGREES OF FREEDOM 
     if (count .lt. QBParm(index)) then 
        return
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
	           if (ASurvey(index, year-fyear(index)+1,iage) .ne. missing) then 
		          sum =sum+dble(ASurvey(index, year-fyear(index)+1,iage)) 
                  count = count+1d0 
	           endif 
	        enddo ! years 
 
	        nxdata = nxdata + idint(count) 
	        Ascale(index) = sum/count 
         if (count .le. QAParm(index)) then 
			return 
		 endif 
	  enddo ! ages 
   enddo ! indices 
 
ParmInit = .TRUE.

return 
end function PARMINIT 


