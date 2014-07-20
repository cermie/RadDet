module data
    type VECTOR
        integer :: N   ! Length of V
        real(8), dimension(:), allocatable :: V
    end type
    
    type TABLE
        integer :: NX, NY ! V dimensions
        real(8), dimension(:, :), allocatable :: V
    end type
end module data