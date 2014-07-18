
module kinds
  implicit none
  
  integer(kind=4), public, parameter :: int_kind = 4
  integer(kind=4), public, parameter :: real_kind = 4
  integer(kind=4), public, parameter :: ptr_kind = 8
  
  integer(kind=4), public, parameter :: STRLEN = 80
  integer(kind=4), public, parameter :: XSTRLEN = 256
  
end module kinds
