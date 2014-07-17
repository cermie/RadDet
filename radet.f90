program radet

!    use kernel_prep
!	use crossections, only : VECTOR
	
	implicit none
	
!	type (VECTOR) :: F, W
	integer :: i, j
	
	call get_arguments()

contains

	subroutine get_arguments()
	    integer :: N, i = 1, LENGTH, STATS
		character(len = 32) :: AVAL
		
		N = command_argument_count()
		do while (i <= N)
		    call get_command_argument(i, AVAL, LENGTH, STATS)
			if (STATS == -1) then
			    print *, "Command Line argument ", i, " is too long. Exit"
				call EXIT()
            end if	
			if (AVAL(1:1) == "-") then
			    print *, i, "key = ", len(AVAL(2:))
			end if
            i = i + 1			
		end do
	end subroutine get_arguments

end program radet
