program radet

    use kernel_prep
    use reverse
	use data, only : VECTOR
    use cla
	use splines
	
	implicit none
	
	real(8), dimension(:), allocatable :: F, W, EF, EW
	real(8), dimension(:), allocatable :: XD, YD, ED
	real(8) :: d
    logical       :: rflag, oflag
	character(len = 256) :: kername, savname
	integer   :: typ, error, Nd, i, NA
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
	call cla_register(key = '-n', description = 'number of averaging points for error estimation', &
	     kkind = cla_int, default = 5)
		 
	call cla_validate
	
	call cla_get('-r', rflag)
	call cla_get('-o', oflag)
	call cla_get('-n', NA)
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
	allocate(XD(Nd), YD(Nd), ED(Nd))
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
				if (C == 'V') then
				    read(*, *) ED(i)
				end if
            end do
			
			if (C /= 'V') then
			    call stat_error_estim(YD, ED, NA)
			end if

            call interpl(XD, YD, EP, W, ED, EW)

            call reverse_task(W, EW, F, EF)

			write(*, *) 'O'
            write(*, *) N1
            do i = 1, N1
                write(*, *) E1(i), F(i), EF(i)
            end do
        end do	
	end if

    if (cla_key_present('-s')) then
        call cla_get('-s', savname)
        call kernel_free(savname)
    else
        call kernel_free()
    end if
    
    deallocate(XD, YD, ED, F, W, EF, EW)

contains

    subroutine interpl(XD, YD, XI, YI, ED, EI)
        real(8), dimension(:), intent(in)  :: XD, YD, XI
        real(8), dimension(:), intent(out) :: YI
		real(8), dimension(:), intent(in),  optional :: ED
		real(8), dimension(:), intent(out), optional :: EI
		
		integer :: ND, NI
		real(8), dimension(size(XD)) :: YDPP, EDPP
		real(8) :: YIP, YIPP, EIP, EIPP
		integer :: i
		
		ND = size(XD)
		NI = size(XI)
		call spline_cubic_set(ND, XD, YD, 2, 0.0d+00, 2, 0.0d+00, YDPP)

		if (present(ED)) then
		    call spline_cubic_set(ND, XD, ED, 2, 0.0d+00, 2, 0.0d+00, EDPP)
		end if

		do i = 1, NI
		    call spline_cubic_val(ND, XD, YD, YDPP, XI(i), YI(i), YIP, YIPP)
			if (present(EI)) then
			    call spline_cubic_val(ND, XD, ED, EDPP, XI(i), EI(i), EIP, EIPP)
			end if
			if (XI(i) < XD(1) .OR. XI(i) > XD(ND) .OR. YI(i) < 0.0) then
			    YI(i) = 0.0
			end if
		end do
		
    end subroutine interpl
	
	subroutine stat_error_estim(YD, ED, NA)
	    real(8), dimension(:), intent(inout)  :: YD
		real(8), dimension(:), intent(out) :: ED
		integer, intent(in) :: NA
		
		integer :: Nd, i, j, n
		integer, dimension(NA) :: c
		real(8) :: ave, ave2, sig
		
		Nd = size(YD)
		do j = 1, NA
		    c(j) = -NA / 2 + j - 1
		end do
		do i = 1, Nd
		    n = 0
			ave = 0.0d+00
			ave2 = 0.0d+00
			do j = 1, NA
			    if (i + c(j) >= 1 .OR. i + c(j) <= Nd) then
				    n = n + 1
					ave = ave + YD(i + c(j))
					ave2 = ave2 + YD(i + c(j)) ** 2
				end if
			end do
			sig = sqrt((ave2 - ave ** 2 / n) / (n - 1))
			if (ave > 0.0 .AND. sig < 0.5 * ave) then
			    ED(i) = sig / ave
			else
			    ED(i) = 0.0d+00
				YD(i) = 0.0d+00
			end if
		end do		
		
	end subroutine stat_eror_estim

end program radet
