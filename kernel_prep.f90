module kernel_prep
! This module allows to calculate new kernel function or 
! download it from an existing file.

    use crossections
	
	implicit none
	
	private
	public :: KERN, E1, EP, dE1, dEP, kernel_init, kernel_free
	
	type (TABLE)  :: KERN
	type (VECTOR) :: E1, EP
	
	integer, parameter :: NEW_KERNEL = 0, OLD_KERNEL = 1
	real(8) :: dE1, dEP, NAT
	
    contains
	
	    subroutine kernel_init(typ, filename_ker, filename_cs)
		    character(len = *), intent(in) :: filename_ker
			character(len = *), optional, intent(in) :: filename_cs
			integer, intent(in) :: typ
			
			integer :: N1, Np, error, i, j
			real(8) :: E1min, E1max, Epmin, Epmax
			
			if (typ .EQ. OLD_KERNEL) then
			    open(11, file = filename_ker, form = 'UNFORMATTED', action = 'READ')
				read(11) N1, Np, NAT
				E1 % N = N1
				EP % N = Np
				KERN % NX = Np
				KERN % NY = N1
				allocate(E1 % V(N1), EP % V(Np), KERN % V(Np, N1))
				read(11) E1
				read(11) EP
				read(11) KERN
				close(11)
			else if (typ .EQ. NEW_KERNEL .AND. present(filename_cs)) then
			    call cs_init(filename_cs, error)
				open(11, file = filename_ker, form = 'UNFORMATTED', action = 'READ')
				read(11) N1, Np, E1min, E1max, Epmin, Epmax, NAT
				close(11)
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
			
			integer :: i
			
			do i = 1, NReact
			    K = K + getcs(i, E1) * f(i, mu(i, E1, Ep), E1) * dmu_dep(i, E1, Ep)
			end do
		end function K
	
	    function f(r, mu, E)
		    real(8), intent(in) :: mu, E
			integer, intent(in) :: r
			real(8) :: f
			
			integer l
			type (VECTOR) :: a = getac(r, E)
			
			do l = 1, a % N
			    f = f + (2 * l - 1) / 2 * a%V(l) * legendre(l - 1, mu)
			end do
			
		end function f
		
		function mu(r, E1, Ep)
		    integer, intent(in) :: r
			real(8), intent(in) :: E1, Ep
			
			real(8) :: znamm, s2, s3
			znam = sqrt(M2(r) * E1 * (M2(r) * E1 - QI(r) * (M1(r) + M2(r))))
			s2 = (Ep(r) - QM(r) + QI(r)) / (2 * M1(r)) * (M1(r) + M2(r)) ** 2
			s3 = QI(r) * (M1(r) + M2(r)) / 2
			mu = (M2(r) * E1 - s2 - s3) / znam
		end function mu
		
		function dmu_dep(r, E1, Ep)
		    integer, intent(in) :: r
			real(8), intent(in) :: E1, Ep
			
			real(8) :: znamm, s2
			znam = sqrt(M2(r) * E1 * (M2(r) * E1 - QI(r) * (M1(r) + M2(r))))
			s2 = (M1(r) + M2(r)) ** 2 / (2 * M1(r))
			dmu_dep = s2 / znam		
		end function dmu_dep
		
		recursive function legendre(n, x) result(p)
		    integer, intent(in) :: n
			real(8), intent(in) :: x
			real(8) :: p
			
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

end module kernel prep
