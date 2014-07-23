program radet

    use kernel_prep
    use reverse
	use data, only : VECTOR
    use cla
	
	implicit none
	
	interface 
	    subroutine spline_cubic_set(n, t, y, ibcbeg, ybcbeg, ibcend, ybcend, ypp)
		    integer(4), intent(in) :: n, ibcbeg, ibcend
			real(8), intent(in) :: t(n), y(n), ybcbeg, ybcend
			real(8), intent(out) :: ypp(n)
		end subroutine spline_cubic_set
		
		subroutine spline_cubic_val(n, t, y, ypp, tval, yval, ypval, yppval)
		    integer(4), intent(in) :: n
			real(8), intent(in) :: t(n), y(n), ypp(n), tval
			real(8), intent(out) :: yval, ypval, yppval
		end subroutine spline_cubic_val
	end interface
	
	real(8), dimension(:), allocatable :: F, W, EF, EW
	real(8), dimension(:), allocatable :: XD, YD
    logical       :: rflag, oflag
	character(len = 256) :: kername, savname
	integer   :: typ, error, Nd, i
	character :: C
	
!---------------------------------------------------------------------------
!-------command line options------------------------------------------------
	
	call cla_init
	call cla_register(key = '-r', description = 'radiation spectrum reconstruction', &
	     kkind = cla_flag, default = 'f')
	call cla_register(key = '-o', description = 'old kernel use rather new kernel generation', &
         kkind = cla_flag, default = 'f')
	call cla_register(key = '-i', description = 'input file', &
	     kkind = cla_char, default = 'kernel.ker')
    call cla_register(key = '-s', description = 'save file', &
         kkind = cla_char, default = ' ')
		 
	call cla_validate
	
	call cla_get('-r', rflag)
	call cla_get('-o', oflag)
	if (oflag) then
	    typ = OLD_KERNEL
	else
	    typ = NEW_KERNEL
	end if
	
	if (.NOT. cla_key_present('-i')) then
	    print *, 'Warning: -i parameter is absent. Default: kernel.ker'
	end if
	call cla_get('-i', kername)
!---------------------------------------------------------------------------
	call kernel_init(typ, kername, error)
	if (error /= 0) then
	    print *, 'File error. Termination'
	    stop 'Program terminated'
	end if
		
	allocate(F(N1), W(NP), EF(N1), EW(NP))
	
	read(*, *) Nd
	allocate(XD(Nd), YD(Nd))
	if (.NOT. rflag) then
	    do 
	        read(*, *) C
	        if (C == 'E') then
	            exit
	        end if
	        do i = 1, Nd
	            read(*, *) XD(i), YD(i)
	        end do
	        
	        call interpl(XD, YD, E1, F)
	        call direct_task(F, W)
	        
			write(*, *) 'O'
	        write(*, *) NP
	        do i = 1, NP
	            write(*, *) EP(i), W(i)
	        end do
	    end do
	else
        do 
            read(*, *) C
            if (C == 'E') then
                exit
            end if
            do i = 1, Nd
                read(*, *) XD(i), YD(i)
            end do

            call interpl(XD, YD, EP, W)
            call reverse_task(W, EW, F, EF)

			write(*, *) 'O'
            write(*, *) N1
            do i = 1, N1
                write(*, *) E1(i), F(i)
            end do
        end do	
	end if

    if (cla_key_present('-s')) then
        call cla_get('-s', savname)
        call kernel_free(savname)
    else
        call kernel_free()
    end if
    
    deallocate(XD, YD, F, W, EF, EW)

contains

    subroutine interpl(XD, YD, XI, YI)
        real(8), dimension(:), intent(in)  :: XD, YD, XI
        real(8), dimension(:), intent(out) :: YI
		
		integer :: ND, NI
		real(8), dimension(size(XD)) :: YDPP
		real(8) :: YIP, YIPP
		integer :: i
		
		ND = size(XD)
		NI = size(XI)
		call spline_cubic_set(ND, XD, YD, 2, 0.0d+00, 2, 0.0d+00, YDPP)
		do i = 1, NI
		    call spline_cubic_val(ND, XD, YD, YDPP, XI(i), YI(i), YIP, YIPP)
			if (XI(i) < XD(1) .OR. XI(i) > XD(ND) .OR. YI(i) < 0.0) then
			    YI(i) = 0.0
			end if
		end do
		
    end subroutine interpl

end program radet
