module polynomials

    use crossections, only : VECTOR

    implicit none
	
	private
	
	type (VECTOR) :: cleg(0 : 1)
	integer :: max_order_leg = -1, cur_leg_cofs = 0

contains
 
    function legendre(n, x) result(p)
        integer, intent(in) :: n
		real(8), intent(in) :: x
		real(8) :: p
		
		integer :: i
		
		if (n .LE. max_order_leg) then
		    
		else
		
		end if
    end function legendre    

end module polynomials
