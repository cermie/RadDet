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
	
contains

	! search the max index in array X so X(i)<=V
	! return 0 if V<X(1) or V>=X(N), where N - length of X
	function search(X, V)
	    real(8), dimension(:), intent(in) :: X
		real(8), intent(in) :: V
		integer(4) :: search
		
		integer(4) :: imin, imax, imid, N
		N = size(X)
		if (V < X(1) .OR. V >= X(N)) then
		    search = 0
		else
		    imin = 1
			imax = N
			do while (imax - imin > 1)
			    imid = (imin + imax) / 2
				if (X(imid) <= V) then
				    imin = imid
    			else
				    imax = imid
				end if
			end do
			search = imin
		end if
		
	end function search
	
end module data