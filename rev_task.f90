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
		
		integer :: i
		real(8) :: pw[NP]
		
		do i = 1, NP
		    pw(i) = 1 / W(i)
		    wgt(i) = 1.0d+00
		end do
	end subroutine init_weights
	
	subroutine descent_gradient_min(W, F, a, wgt, max_iter, prec)
	    real(8), dimension(:), intent(in) :: W, wgt
		real(8), dimension(:), intent(inout) :: F
		real(8), intent(in) :: a, prec
		integer, intent(in) :: max_iter
		
		integer :: itr = 0, i, j
		real(8) :: G(N1), L(NP), ch, zn, gamma1, cp = 1.d+99, c1(NP), c2(NP)
		
		do while (itr <= max_iter .AND. cp >= prec)
		    cp = 0.0d+00
		    do i = 1, NP
			    c1(i) = -W(i)
				do j = 1, N1
				    c1(i) = c1(i) + KERN(i, j) * F(j)
				end do
				cp = cp + c1(i) ** 2
			end do
			cp = sqrt(cp)
			do i = 1, N1
			    G(i) = 2 * a * dE1 * F(i)
				do j = 1, NP
				    G(i) = G(i) + 2 * dEP * wgt(i) * KERN(j, i) * c1(j)
				end do
			end do
			G(1) = G(1) + 2 * a / dE1 * (F(1) - F(2))
			do i = 2, N1 - 1
			    G(i) = G(i) + 2 * a / dE1 * (2 * F(i) - F(i - 1) - F(i + 1))
			end do
			G(N1) = G(N1) + 2 * a / dE1 * (F(N1) - F(N1 - 1))
			do i = 1, NP
			    c2(i) = 0.0d+00
				do j = 1, N1
				    c2(i) = c2(i) + KERN(i, j) * G(j)
				end do
			end do
			ch = 0.0d+00
			zn = 0.0d+00
			do i = 1, NP
			    ch = ch + dEP * c2(i) * wgt(i) * c1(i)
				zn = zn + dEP * wgt(i) * c2(i) ** 2
			end do
			do i = 1, N1
			    ch = ch + a * dE1 * F(i) * G(i)
				zn = zn + a * dE1 * G(i) ** 2
			end do
			do i = 1, N1 - 1
			    ch = ch + a / dE1 * (F(i + 1) - F(i)) * (G(i + 1) - G(i))
				zn = zn + a / dE1 * (G(i + 1) - G(i)) ** 2
			end do
			gamma1 = ch / zn
			do i = 1, N1
			    F(i) = F(i) - gamma1 * G(i)
				if (F(i) < 0) then
				    F(i) = 0
				end if
			end do
		    itr = itr + 1
		end do
	end subroutine descent_gradient_min

end module reverse