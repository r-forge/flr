
            Program trim

            character*132 line

            open (1, file='xin.f90', status='old')
            open(2,file='xout.f90', status='unknown')

10          read(1,'(A131)',end=100) line
            write(2,'(A131)') line
            goto 10

100         stop
            end    
