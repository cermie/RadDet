program radet

    use kernel_prep
    use reverse
	use data, only : VECTOR
    use cla
	
	implicit none
	
	type (VECTOR) :: F, W
	real(8), dimension(:), allocatable :: XD, YD
    logical       :: rflag, oflag
	character(len = 256) :: kername, savname
	integer   :: typ, error, Nd, i
	character :: C
	
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
	call kernel_init(typ, kername, error)
	if (error /= 0) then
	    print *, 'File error. Termination'
	    stop 'Program terminated'
	end if
		
	F % N = E1 % N
	W % N = EP % N
	allocate(F % V(F % N), W % V(W % N))
	
	if (.NOT. rflag) then
	    read(*, *) Nd
	    allocate(XD(Nd), YD(Nd))
	    do 
	        read(*, *) C
	        if (C == 'E') then
	            exit
	        end if
	        do i = 1, Nd
	            read(*, *) XD(i), YD(i)
	        end do
	        
	        call interpl(XD, YD, E1 % V, F % V)
	        call direct_task(F, W)
	        
	        write(*, *) EP % N
	        do i = 1, EP % N
	            write(*, *) EP % V(i), W % V(i)
	        end do
	    end do
	else
        read(*, *) Nd
        allocate(XD(Nd), YD(Nd))
        do 
            read(*, *) C
            if (C == 'E') then
                exit
            end if
            do i = 1, Nd
                read(*, *) XD(i), YD(i)
            end do

            call interpl(XD, YD, EP % V, W % V)
            call reverse_task(W, F)

            write(*, *) E1 % N
            do i = 1, E1 % N
                write(*, *) E1 % V(i), F % V(i)
            end do
        end do	
	end if

    if (cla_key_present('-s')) then
        call cla_get('-s', savname)
        call kernel_free(savname)
    else
        call kernel_free()
    end if
    
    deallocate(XD, YD, F % V, W % V)

contains

    subroutine interpl(XD, YD, XI, YI)
        real(8), dimension(:), intent(in)  :: XD, YD, XI
        real(8), dimension(:), intent(out) :: YI
    end subroutine interpl

end program radet
