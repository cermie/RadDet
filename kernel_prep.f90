module kernel_prep
! This module allows to calculate new kernel function or 
! download it from an existing file.

    use crossections
	
	implicit none
	
	type (TABLE)  :: KERN
	type (VECTOR) :: E1, EP
	
    contains
	
	    function f(mu, E)
		    real(8), intent(in) :: mu, E
			real(8) :: f
			
			integer l
			
		end function f

end module kernel prep
