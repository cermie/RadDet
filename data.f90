module data
!   P A R A M E T E R S
!
!   KERN - kernel itself. This is matrix of Np x N1 dimensions. 
!   E1 (len = N1) - mesh of radiation spectrum energy. 
!   EP(len = Np) - mesh of response function (deposited) energy.
!   NAT - the number of atoms in the detector
!   dE1, dEP - corresponding mesh steps.
!
!   U S A G E
!
!   W = KERN * F
!
!   where F(E1) - radiation spectrum, W(EP) - response function

	implicit none

    type VECTOR
        integer :: N   ! Length of V
        real(8), dimension(:), allocatable :: V
    end type
    
    type TABLE
        integer :: NX, NY ! V dimensions
        real(8), dimension(:, :), allocatable :: V
    end type
	
    real(8), dimension(:),    allocatable :: E1, EP
	real(8), dimension(:, :), allocatable :: KERN
	real(8) :: dE1, dEP, NAT
	integer :: N1, NP
	
end module data