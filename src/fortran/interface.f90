module xoptfoil_interface

  implicit none

  contains

!=============================================================================80
!
! Initializes data for optimization
!
!=============================================================================80
subroutine initialize(errval, errmsg)

  use vardef
  use airfoil_operations, only : get_seed_airfoil

  integer, intent(out) :: errval
  character(80), intent(out) :: errmsg

  type(airfoil_type) :: buffer_foil

!FIXME: These should go in vardef
  character(80) :: seed_airfoil, airfoil_file
  character(4) :: naca_digits

  double precision :: xoffset, zoffset, foilscale

  errval = 0
  errmsg = ''
  
  ! Load seed airfoil into memory, including transformations and smoothing

  call get_seed_airfoil(seed_airfoil, airfoil_file, naca_digits, buffer_foil,  &
                        xoffset, zoffset, foilscale, errval, errmsg)

  if (errval /= 0) return

end subroutine initialize

end module xoptfoil_interface
