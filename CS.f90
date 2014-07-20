module crossections
    ! This module downloads crossection data and operate it.
	! 
	! F U N C T I O N S
	!
	! getcs(cskind, energy) - returns crossection of reaction with index cskind
	!                         at energy
    ! getac(cskind, energy) - returns type(VECTOR) containing array of angular 
	!                         distribution coefficients of cskind reaction at
	!                         energy
	! S U B R O U T I N E S
	! 
	! cs_init(filename, error) - initializes crossection data. filename - a name of textfile
	!                     which contains descriprion of files with crossection data
	!                     error - error code: 0 - no error
	! cs_destroy() - deallocates all dynamic crossection data. 
    use data
    
    implicit none 
	
    private
	public  :: cs_init, cs_destroy, getcs, getac, NReact, QI, QM, M1, M2
	
	integer :: NReact ! reactions number
    type (VECTOR), dimension(:), allocatable :: CS, E, EA     ! E - array of reaction energies. EA - angular distribution energies
	                                                          ! CS - crossections. Example:
															  ! r - the number of reaction under consideration. Then
															  ! E(r) % V - array of energies for reaction r, where crossections defined.
															  ! CS(r) % V - array of corresponding crossections. 
															  ! reaction r: E(r) % V(i) -> CS(r) % V(i), i = 1,...,E(r)%N
	real(8),       dimension(:), allocatable :: QI, QM, M1, M2 ! reaction energies and product masses
	type (TABLE),  dimension(:), allocatable :: A ! A(r) - array of coefficients of reaction r angular distribution function
	                                              ! A(r) % V(i, j) - coefficient of number j at energy EA(r) % V(i)
	
contains
	
    ! filename file format (this is testfile):
	! first line:
	! NR - Total number of reactions (integer)
	! followed by NR lines of format
	! M1 M2 QI QM 
	! CS_FILENAME 
	! ANG_FILENAME
	! where M1, M2 - product masses in u (atomic mass unit)
	!       QI, QM - reaction energy
	!       CS_FILENAME - name of file with crossection data (80 symbols max)
	!       CS_ANG_FILENAME - name of file with angular disttribution data (80 symbols max)
    subroutine cs_init(filename, error)
	    character(len = *), intent(in) :: filename
		integer, intent(out) :: error ! error code. 0 - if no error
		
		integer(4) :: i, NR, err2
		character(len = 80) :: cs_file, ang_file
		
		error = 0			
		open(11, file = filename, form = 'FORMATTED', action = 'READ')
		read(11, *) NR
		allocate(E(NR), EA(NR), CS(NR), QI(NR), QM(NR), M1(NR), M2(NR), A(NR))
		do i = 1, NR
		    read(11, *) M1(i), M2(i), QI(i), QM(i)
			read(11, *) cs_file
			call read_cs(cs_file, i, err2)
			read(11, *) ang_file
			call read_ang(ang_file, i, err2)
		end do
		close(11)
		NReact = NR
	end subroutine cs_init
	
	subroutine read_cs(filename, react, error)  ! read crossections from binary file for reaction react
	    character(len = *), intent(in) :: filename
		integer, intent(in)  :: react
		integer, intent(out) :: error
		
		integer :: N, i
		
		error = 0
		open(12, file = filename, form = 'UNFORMATTED', action = 'READ')
		read(12) N
		E(react) % N = N
		CS(react) % N = N
		allocate(E(react) % V(N), CS(react) % V(N))
		read(12) E(react) % V
           read(12) CS(react) % V
		close(12)
	end subroutine read_cs
	
	subroutine read_ang(filename, react, error) ! read angular distribution coefficients from binary file for reaction react
	    character(len = *), intent(in) :: filename
		integer, intent(in)  :: react
		integer, intent(out) :: error
		
		integer :: NX, NY, i
		
		error = 0
		open(12, file = filename, form = 'UNFORMATTED', action = 'READ')
		read(12) NX, NY
		EA(react) % N = NX
		A(react) % NX = NX
		A(react) % NY = NY
		allocate(EA(react) % V(NX), A(react) % V(NX, NY))
		read(12) EA(react) % V
		read(12) A(react) % V
		close(12)
	end subroutine read_ang
	
	subroutine cs_destroy()
	
	    integer :: N, i
		
		if (.NOT. allocated(E)) then
		    return
		end if
	    N = size(E)
	    do i = 1, N
	        deallocate(E(i) % V, CS(i) % V, EA(i) % V, A(i) % V)
	    end do
	    deallocate(E, CS, QI, QM, M1, M2, A, EA)
	    
	end subroutine
	    
    function getcs(cskind, energy)  ! linear interpolation used here
	    integer(4), intent(in) :: cskind   ! If energy lies outside avaible crossection data range 0.0 value is returned
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
	
	function getac(cskind, energy) ! linear interpolation used here. 
	    integer(4), intent(in) :: cskind  ! If energy lies outside avaible crossection data range then
	    real(8),    intent(in) :: energy  ! getac structure returned: getac%N=1, getac%V(1)=1.0 - uniform distribution is assumed
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
		
	! search the max index in array X so X(i)<=V
	! return 0 if V<X(1) or V>=X(N), where N - length of X
	function search(X, V)
	    real(8), dimension(:), intent(in) :: X
		real(8), intent(in) :: V
		integer(4) :: search
		
		integer(4) :: imin, imax, imid, N
		N = size(X)
		if (V .LT. X(1) .OR. V .GE. X(N)) then
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
