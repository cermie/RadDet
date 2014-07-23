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
!
    use crossections
    use data
	
	implicit none
	
	private
	public :: NEW_KERNEL, OLD_KERNEL, kernel_init, kernel_free
		
	integer, parameter :: NEW_KERNEL = 0, OLD_KERNEL = 1
	
contains
	
    subroutine kernel_init(typ, filename, error)
	    character(len = *), intent(in) :: filename
		integer, intent(in)  :: typ
		integer, intent(out) :: error
		
		character(len = 256) :: filename_cs
    	integer :: i, j
		real(8) :: E1min, E1max, Epmin, Epmax
		open(11, file = filename, form = 'UNFORMATTED', action = 'READ', iostat = error)
		if (typ .EQ. OLD_KERNEL) then
			read(11) N1, NP, NAT
			allocate(E1(N1), EP(NP), KERN(NP, N1))
			read(11) E1
			read(11) EP
			read(11) KERN
			dE1 = E1(2) - E1(1)
			dEP = EP(2) - EP(1)
		else if (typ .EQ. NEW_KERNEL) then
			read(11) N1, NP, E1min, E1max, Epmin, Epmax, NAT
			read(11) filename_cs
			call cs_init(filename_cs, error)
			allocate(E1(N1), EP(NP), KERN(NP, N1))
			dE1 = (E1max - E1min) / N1
			dEP = (Epmax - Epmin) / NP
			do i = 1, N1
			    E1(i) = E1min + dE1 * (i - 0.5)
			end do
			do i = 1, NP
			    EP(i) = Epmin + dEP * (i - 0.5)
				do j = 1, N1
				    KERN(i, j) = NAT * K(EP(i), E1(j)) * dE1
					end do
			end do
		end if
		close(11)
	end subroutine kernel_init
		
	subroutine kernel_free(filename_ker)
	    character(len = *), optional, intent(in) :: filename_ker
		
		if (present(filename_ker)) then
		    open(11, file = filename_ker, form = 'UNFORMATTED', action = 'WRITE')
			write(11) N1, NP, NAT
            write(11) E1
            write(11) EP
            write(11) KERN
            close(11)				
	    end if
		deallocate(E1, EP, KERN)
        call cs_free()		
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
