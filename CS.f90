module crossections

    implicit none 
	
    private
	public  :: cs_init, cs_destroy, getcs, getac
	
	type VECTOR
	    integer :: N
	    real(8), dimension(:), allocatable :: V
	end type
	
	type TABLE
	    integer :: NX, NY
	    real(8), dimension(:, :), allocatable :: V
	end type
	
    type (VECTOR), dimension(:), allocatable :: CS, E, EA
	real(8),       dimension(:), allocatable :: QI, QM, M1, M2
	type (TABLE),  dimension(:), allocatable :: A
	
    contains
	
	    subroutine cs_init(filename)
		    character(len = *), intent(in) :: filename
			
			integer(4) :: i, j, Nreact
			
		end subroutine cs_init
		
		subroutine cs_destroy()
		
		    integer :: N, i
		    N = size(E)
		    do i = 1, N
		        deallocate(E(i) % V, CS(i) % V, EA(i) % V, A(i) % V)
		    end do
		    deallocate(E, CS, QI, QM, M1, M2, A, EA)
		    
		end subroutine
	
	    function getcs(cskind, energy)
		    integer(4), intent(in) :: cskind
			real(8),    intent(in) :: energy
			real(8) :: getcs
			
			integer :: i
			
			i = search(E(cskind) % V, energy)
			if (i .EQ. 0) then
			    getcs = 0.0
			else
			    getcs = CS(cskind) % V(i) + (CS(cskind) % V(i + 1) - &
				        CS(cskind) % V(i)) * (energy - E(cskind) % V(i)) / &
				        (E(cskind) % V(i + 1) - E(cskind) % V(i))
			end if
			
		end function getcs
		
		function getac(cskind, energy)
		    integer(4), intent(in) :: cskind
		    real(8),    intent(in) :: energy
		    type (VECTOR) :: getac
		    
		    integer :: i, j
		    real(8) :: fc
		    
		    i = search(EA(cskind) % V, energy)
		    
		    if (i .EQ. 0) then
		        getac % N = 1
		    else
		        getac % N = A(cskind) % NY + 1
		    end if
		    
		    allocate(getac % V(getac % N))
		    getac % V(1) = 1.0
		    fc = (energy - EA(cskind) % V(i)) / (EA(cskind) % V(i + 1) - EA(cskind) % V(i))
		    do j = 2, getac % N
		        getac % V(j) = A(cskind) % V(i, j - 1) +  &
		              (A(cskind) % V(i + 1, j - 1) - A(cskind) % V(i, j - 1)) * fc 
		    end do
		    
		end function getac
		
		function search(X, V)
		    real(8), dimension(:), intent(in) :: X
			real(8), intent(in) :: V
			integer(4) :: search
			
			integer(4) :: imin, imax, imid, N
			N = size(X)
			if (V .LE. X(1) .OR. V .GE. X(N)) then
			    search = 0
			else
			    imin = 1
				imax = N
				do while (imax - imin .GT. 1)
				    imid = (imin + imax) / 2
					if (X(imid) .LE. V) then
					    imin = imid
					else
					    imax = imid
					end if
				end do
				search = imin
			end if
			
		end function search
end module
