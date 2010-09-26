


      Program Convert ! from f77 to f90 format


      !Takes initial c comment and changes to !
     !  if there's a non-blank character in position 6, append
     !  an ampersand to the previous line

       character*200 infilen, outfilen, line1, line2
       integer in, out

       in =1
       out =2
       write(*,*) 'File to read ?'
       read(*,'(A)') infilen
       write(*,*) 'File to create ?'
       read(*,'(A)') outfilen

       open(in, file=infilen, status='old')
       open(out, file=outfilen, status='unknown')

       read(in, '(A)') line1
100    read(in, '(A)', END=200) line2
       
! process line 1
       if ((line1(1:1) .eq. 'c') .or. (line1(1:1) .eq.'C')) line1(1:1)='!'

       if ((line2(6:6) .EQ. '*') .and. (line2(1:1) .ne. 'c') .and.(line2(1:1).ne.'C') .and.(line2(1:1).ne.'!') ) then
        write(*,*)  ' 1 ',line2(1:1), ' 6 ',line2(6:6)
        line1(73:73) = '&'
        line2(6:6) = ' '
       endif
       write(out,'(A132)') line1
       line1=line2
       goto 100

200    close(in)
       close(out)

       end






