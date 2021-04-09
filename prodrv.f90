
program prodrv
    use param
    use prolate_swf
    
    implicit none
    
    integer   i, im, j, mmin, minc, mnum, m, l, lnum, ioprad, iopang, &
              iopnorm, ioparg, narg, kind, kindd, kindq
    real(knd) c, x1, arg1, darg, api

    
    real(knd), dimension(:), allocatable ::    arg, r1c, r1dc, r2c, r2dc
    integer, dimension(:), allocatable ::      ir1e, ir1de, ir2e, ir2de, naccr
    real(knd), dimension (:,:), allocatable :: s1c, s1dc
    integer, dimension(:,:), allocatable ::    is1e, is1de, naccs
    
    kindd =  8
    kindq = 16

!   open input and output files
    open(1, file='profcn.dat')
    open(20 ,file='fort.20')
    open(30, file='fort.30')
    
!
!   read input data
    read(1,*) mmin, minc, mnum, lnum
    read(1,*) ioprad, iopang, iopnorm
    read(1,*) c, x1
    if(iopang /= 0) read(1,*) ioparg, arg1, darg, narg

    api = acos(-1.0e0_knd) / 180.0e0_knd

    allocate (arg(narg), r1c(lnum), r1dc(lnum), r2c(lnum), r2dc(lnum))
    allocate (ir1e(lnum), ir1de(lnum), ir2e(lnum), ir2de(lnum), naccr(lnum))
    allocate (s1c(lnum, narg), s1dc(lnum, narg))
    allocate (is1e(lnum, narg), is1de(lnum, narg), naccs(lnum, narg))
        
    if (iopang /= 0) then
        do j = 1, narg  
            arg(j) = arg1 + (j - 1) * darg
            if(ioparg == 0) arg(j) = cos(api * arg(j))        
        end do
    end if

    do im = 1, mnum
        m = mmin + (im - 1) * minc

        call profcn(c, m, lnum, ioprad, x1, iopang, iopnorm, narg, arg, &
                    r1c, ir1e, r1dc, ir1de, r2c, ir2e, r2dc, ir2de, naccr, &
                    s1c, is1e, s1dc, is1de, naccs)
        
        if (ioprad /= 0) then

            if(knd == kindd) write(20, 10) x1 + 1.00e0_knd, c, m
            if(knd == kindq) write(20, 20) x1 + 1.00e0_knd, c, m
10          format(1x,e23.14,e23.14,i5)
20          format(1x,e39.30,e39.30,i5)
            
            do i = 1, lnum
                l = m + i - 1
                if(ioprad == 2) write(20,690) l, r1c(i), ir1e(i), r1dc(i), ir1de(i), r2c(i), ir2e(i), r2dc(i), ir2de(i), naccr(i)
                if(ioprad == 1) write(20,710) l, r1c(i), ir1e(i), r1dc(i), ir1de(i)
690             format(1x,i6,2x,4(f17.14,1x,i6,2x),i2, ' ')
710             format(1x,i6,2x,2(f17.14,1x,i6,2x))
            end do
            
        end if
        
        if (iopang /= 0) then
            
            if(knd == kindd) write(30, 30) c, m
            if(knd == kindq) write(30, 40) c, m
30          format(1x,e23.14,i5)
40          format(1x,e39.30,i5)
            
            do i = 1, lnum
                l = m + i - 1

                write(30, 50) l
50              format(1x,i6)

                do j = 1, narg

                    if(ioparg == 0) then
                        if(iopang == 1) write(30, 60) arg(j), s1c(i, j), is1e(i, j), naccs(i, j)
                        if(iopang == 2) write(30, 70) arg(j), s1c(i, j), is1e(i, j), s1dc(i, j), is1de(i, j), naccs(i, j)
                    end if
                    if(ioparg == 1) then
                        if(iopang == 1) write(30, 60) arg(j), s1c(i, j), is1e(i, j), naccs(i, j)
                        if(iopang == 2) write(30, 70) arg(j), s1c(i, j), is1e(i, j), s1dc(i, j), is1de(i, j), naccs(i, j)
                    end if    
60                  format(1x,f19.14,2x,f17.14,2x,i5,2x,', ',i2)
70                  format(1x,f19.14,2x,f17.14,2x,i5,2x,f17.14,2x,i5,2x,i2)
                end do
            end do
        end if
    end do
    
    deallocate (is1e, is1de, naccs)
    deallocate (s1c, s1dc)
    deallocate (ir1e, ir1de, ir2e, ir2de, naccr)
    deallocate (arg, r1c, r1dc, r2c, r2dc)
    close(1)
    close(30)
    close(20)

end program prodrv
                         