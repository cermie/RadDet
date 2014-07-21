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
		
	end subroutine init_weights
	
	subroutine stat_error_estim(W, EW)
	    real(8), dimension(:), intent(in)  :: W
		real(8), dimension(:), intent(out) :: EW
		
		
	end subroutine stat_eror_estim

end module reverse