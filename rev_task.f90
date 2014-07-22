module reverse

    use data
    
    implicit none

contains

    subroutine direct_task(F, W)
        real(8), dimension(:), intent(in)  :: F
        real(8), dimension(:), intent(out) :: W
        
        integer :: i, j
        real(8) :: s
        do i = 1, NP
            s = 0.0
            do j = 1, N1
                s = s + KERN(i, j) * F(j)
            end do
            W(i) = s
        end do
    end subroutine direct_task
    
    subroutine reverse_task(W, EW, F, EF)
        real(8), dimension(:), intent(in)  :: W, EW
        real(8), dimension(:), intent(out) :: F, EF
        
		real(8), dimension(:) :: wgt(NP)
				
		call init_weights(W, EW, wgt)
		
		
    end subroutine reverse_task
	
	subroutine init_weights(W, EW, wgt)
	    real(8), dimension(:), intent(in)  :: W, EW
		real(8), dimension(:), intent(out) :: wgt
		
		integer :: i
		
		do i = 1, NP
		    wgt(i) = 1.0d+00
		end do
	end subroutine init_weights
	
	subroutine descent_gradient_min(W, F, a, wgt, max_iter, prec)
	    real(8), dimension(:), intent(in) :: W, wgt
		real(8), dimension(:), intent(inout) :: F
		real(8), intent(in) :: a, prec
		integer, intent(in) :: max_iter
		
		integer :: itr = 0
		real(8) :: G(N1), L(NP), ch, zn, gamma1, cp = 1.d+99
		
		do while (itr <= max_iter .AND. cp >= prec)
		    
		    itr = itr + 1
		end do
	end subroutine descent_gradient_min

end module reverse