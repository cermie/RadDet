module reverse

    use kernek_prep
    
    implicit none

contains

    subroutine direct_task(F, W)
        type (VECTOR), intent(in)  :: F
        type (VECTOR), intent(out) :: W
        
        integer :: i, j
        real(8) :: s
        do i = 1, EP % N
            s = 0.0
            do j = 1, E1 % N
                s = s + KERN % V(i, j) * F % V(j)
            end do
            W % V(i) = s
        end do
    end subroutine direct_task
    
    subroutine reverse_task(W, F)
        type (VECTOR), intent(in)  :: W
        type (VECTOR), intent(out) :: F
        
    end subroutine reverse_task

end module reverse