!
! To compile:
!
!	ifort kinds.f90 cla.f90 cla_test.f90 -o cla_test
!
  program cla_test

    use kinds
    use cla

    implicit none
    character(len=STRLEN)  :: key
    character(len=XSTRLEN) :: description
    integer(kind=int_kind) :: kkind
    character(len=STRLEN)  :: default

    integer(kind=int_kind) :: i
    integer(kind=8)        :: j, jello
    real(kind=real_kind)   :: x
    real(kind=4)           :: y4
    real(kind=8)           :: y8
    character(len=STRLEN)  :: a
    character(len=XSTRLEN) :: b
    logical                :: l, ll, flag

    call cla_init

    key = '-i'
    description = 'an integer parameter'
    kkind       = cla_int
    default     = '0'
    call cla_register(key,description,kkind,default)

    ! Might be nice to use named positional parameters to make an optional long form,
    ! instead of making and explicit separate parameter:
    call cla_register('-j',     'another int',                 cla_int    ,'72')
    call cla_register('--jello','yet another int',             cla_int    ,'72')
    call cla_register('-x',     'real',                        cla_float  ,'3.14159')
    call cla_register('-y',     'a real for testing r4, r8',   cla_float  ,'3.14159')
    call cla_register('-a',     'str>',                        cla_char   ,'Ed Zaron')
    call cla_register('-b',     'a long or extended str',      cla_xchar  ,'Ed Zaron :: This could be up to XSTRLEN in length. See kinds.f90.')
    call cla_register('-f',     'a flag',                      cla_flag,'f')
    call cla_register('-l',     'bool',                        cla_logical,'t')

    ! These can be called in any order:
!    call cla_help
!    call cla_show
    call cla_validate

    ! Test to see if the --jello key is present:
    ll = cla_key_present('--jello')
    if (ll) then
       print *,' --jello is present'
    else
       print *,' --jello is not present'
    endif

    ! Test to see if the -f flag is present:
    ll = cla_key_present('-f')
    if (ll) then
       print *,' -f is present'
    else
       print *,' -f is not present'
    endif
    
    ! Get values:
    call cla_get('-i',i)
    call cla_get('-j',j)
    call cla_get('--jello',jello)
    call cla_get('-x',x)
    call cla_get('-y',y4)
    call cla_get('-y',y8)
    call cla_get('-a',a)
!    call cla_get_xchar('-b',b) ! Looks like extended character strings not implemented.
    call cla_get('-l',l)

    print *,'After looking at your command line arguments, here is what I have:'
    print *,' i     = ',i
    print *,' j     = ',j
    print *,' jello = ',jello
    print *,' x     = ',x
    print *,' y (r4)= ',y4
    print *,' y (r8)= ',y8
    print *,' a     = ',trim(a)
    print *,' l     = ',l

  end program cla_test

