program radet

!    use kernel_prep
!	use crossections, only : VECTOR
    use cla
	
	implicit none
	
!	type (VECTOR) :: F, W
    logical :: kflag
	character(len = 80) :: csname
	
	call cla_init
	call cla_register(key = '-k', description = 'download already generated kernel', &
	     kkind = cla_flag, default = 'f')
	call cla_register(key = '--cs', description = 'crossection data file', &
	     kkind = cla_char, default = 'carbon.cs')
	call cla_register(key = '--kernel', description = 'kernel data file', &
	     kkind = cla_char, default = 'kernel.ker')
		 
	call cla_validate
	
	call cla_get('-k', kflag)
	if (.NOT. kflag) then
	    call cla_get('--cs', csname)
		print *, csname
	else
	    call cla_get('--kernel', kername)
		print *, kername
	end if

contains


end program radet
