module kernel_prep
! This module allows to calculate new kernel function or 
! download it from an existing file.
!
! S U B R O U T I N E S
!
!   kernel_init(typ, filename) - initialize kernel. typ - kind of possible ways
!              to initialize kernel. Possible values: integer NEW_KERNEL = 0 or OLD_KERNEL = 1.
!              In the first case a new kernel is generated using crossection data from file filename_cs.
!              File filename contains initializing information like Np, N1, E1max, E1min, Epmax,
!              Epmin, NAT then. In the last case already generated kernel, stored in file folename_ker
!              is used. In this case filename_cs parameter is not required.
!
!   kernel_free(filename_ker) - free allocated memory when KERN is not required any more. Optional
!              parameter filename_kernel - file name where generated kernel should be saved for
!              further uses.
!
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
!
    use crossections
    use data
	
	implicit none
	
	private
	public :: KERN, E1, EP, dE1, dEP, NEW_KERNEL, OLD_KERNEL, kernel_init, kernel_free
	
	type (TABLE)  :: KERN
	type (VECTOR) :: E1, EP
	
	integer, parameter :: NEW_KERNEL = 0, OLD_KERNEL = 1
	real(8) :: dE1, dEP, NAT
	
contains
	
    subroutine kernel_init(typ, filename, error)
	    character(len = *), intent(in) :: filename
		integer, intent(in)  :: typ
		integer, intent(out) :: error
		
		character(len = 256) :: filename_cs
    	integer :: N1, Np, i, j
		real(8) :: E1min, E1max, Epmin, Epmax
		open(11, file = filename, form = 'UNFORMATTED', action = 'READ', iostat = error)
		if (typ .EQ. OLD_KERNEL) then
			read(11) N1, Np, NAT
			E1 % N = N1
			EP % N = Np
			KERN % NX = Np
			KERN % NY = N1
			allocate(E1 % V(N1), EP % V(Np), KERN % V(Np, N1))
			read(11) E1 % V
			read(11) EP % V
			read(11) KERN % V
		else if (typ .EQ. NEW_KERNEL) then
			read(11) N1, Np, E1min, E1max, Epmin, Epmax, NAT
			read(11) filename_cs
			call cs_init(filename_cs, error)
			E1 % N = N1;    Ep % N = Np
    		KERN % NX = Np;	KERN % NY = N1
            allocate(E1 % V(N1), EP % V(Np), KERN % V(Np, N1))
			dE1 = (E1max - E1min) / N1
			dEP = (Epmax - Epmin) / Np
			do i = 1, N1
			    E1 % V(i) = E1min + dE1 * (i - 0.5)
			end do
			do i = 1, Np
			    EP % V(i) = Epmin + dEp * (i - 0.5)
				do j = 1, N1
				    KERN % V(i, j) = NAT * K(EP % V(i), E1 % V(j)) * dE1
					end do
			end do
		end if
		close(11)
	end subroutine kernel_init
		
	subroutine kernel_free(filename_ker)
	    character(len = *), optional, intent(in) :: filename_ker
		
		if (present(filename_ker)) then
		    open(11, file = filename_ker, form = 'UNFORMATTED', action = 'WRITE')
			write(11) E1 % N, EP % N, NAT
            write(11) E1 % V
            write(11) EP % V
            write(11) KERN % V
            close(11)				
	    end if
		deallocate(E1 % V, EP % V, KERN % V)
        call cs_destroy()		
	end subroutine kernel_free
	
	function K(Ep, E1)
	    real(8), intent(in) :: Ep, E1
	    real(8) :: K
			
		integer :: i
			
		do i = 1, NReact
		    K = K + getcs(i, E1) * f(i, mu(i, E1, Ep), E1) * dmu_dep(i, E1, Ep)
		end do
	end function K
	
	function f(r, mu, E) ! anhular probability function for reaction r.
	    real(8), intent(in) :: mu, E ! mu - cos of scattering angle, E - incident energy
		integer, intent(in) :: r
		real(8) :: f
		
		integer l
		type (VECTOR) :: a 
		
		a = getac(r, E)		
		do l = 1, a % N
		    f = f + (2 * l - 1) / 2 * a%V(l) * legendre(l - 1, mu)
		end do
		
	end function f
	
	function mu(r, E1, Ep) ! cosine of scattering angle as function of incedent and deposited energies.
	    integer, intent(in) :: r
		real(8), intent(in) :: E1, Ep
		real(8) :: mu
		
		real(8) :: znam, s2, s3
		znam = sqrt(M2(r) * E1 * (M2(r) * E1 - QI(r) * (M1(r) + M2(r))))
		s2 = (Ep - QM(r) + QI(r)) / (2 * M1(r)) * (M1(r) + M2(r)) ** 2
		s3 = QI(r) * (M1(r) + M2(r)) / 2
		mu = (M2(r) * E1 - s2 - s3) / znam
	end function mu
		
	function dmu_dep(r, E1, Ep) ! d mu / d EP
	    integer, intent(in) :: r
		real(8), intent(in) :: E1, Ep
		real(8) :: dmu_dep
		
		real(8) :: znam, s2
		znam = sqrt(M2(r) * E1 * (M2(r) * E1 - QI(r) * (M1(r) + M2(r))))
		s2 = (M1(r) + M2(r)) ** 2 / (2 * M1(r))
		dmu_dep = s2 / znam		
	end function dmu_dep
		
	recursive function legendre(n, x) result(p) ! legendre polynomials. Recursive calculation of
	    integer, intent(in) :: n      ! legendre polynomials is used. This method is easy, and
		real(8), intent(in) :: x      ! shouldn't affect considerably on efficiency, because
		real(8) :: p                  ! in this problem n (order) is not very big
		
		if (n .EQ. 0) then
		    p = 1
		else if (n .EQ. 1) then
		    p = x
		else if (n .GT. 1) then
		    p = ((2 * n - 1) * x * legendre(n - 1, x) - &
			    (n - 1) * legendre(n - 2, x)) / n
		else
		    p = 1
		end if
	end function legendre

end module kernel_prep
