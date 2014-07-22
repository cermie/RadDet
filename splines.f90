module splines

    interface 
	    subroutine spline_cubic_set(n, t, y, ibcbeg, ybcbeg, ibcend, ybcend, ypp)
		    integer(4), intent(in) :: n, ibcbeg, ibcend
			real(8), intent(in) :: t(n), y(n), ybcbeg, ybcend
			real(8), intent(out) :: ypp(n)
		end subroutine spline_cubic_set
	end interface

    interface	
		subroutine spline_cubic_val(n, t, y, ypp, tval, yval, ypval, yppval)
		    integer(4), intent(in) :: n
			real(8), intent(in) :: t(n), y(n), ypp(n), tval
			real(8), intent(out) :: yval, ypval, yppval
		end subroutine spline_cubic_val
	end interface

end module splines