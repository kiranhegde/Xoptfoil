module xoptfoil_interface

  implicit none

  contains

!=============================================================================80
!
! Reads inputs from fortran namelist file
!
!=============================================================================80
subroutine read_namelist_inputs(cinput_file, cerrval, cerrmsg) bind(c)

  use iso_c_binding
  use vardef
  use input_output, only : read_inputs

  character(kind=C_CHAR, len=1), dimension(80), intent(in) :: cinput_file
  integer(C_INT), intent(out) :: cerrval
  character(kind=C_CHAR, len=1), dimension(80), intent(out) :: cerrmsg

  integer :: errval, i
  character(80) :: input_file, errmsg

  errval = 0
  errmsg = ''

  ! Convert C char array to Fortran char array
  
  do i = 1, 80
    input_file(i:i) = cinput_file(i)
  end do

  ! Read Fortran namelist inputs

  call read_inputs(input_file, errval, errmsg)

  ! Convert to C outputs

  cerrval = errval
  do i = 1, 80
    cerrmsg(i) = errmsg(i:i)
  end do

end subroutine read_namelist_inputs

!=============================================================================80
!
! Initializes data for optimization
!
!=============================================================================80
subroutine initialize(errval, errmsg)

  use vardef
  use airfoil_operations, only : get_seed_airfoil
  use xfoil_driver,       only : airfoil_type

  integer, intent(out) :: errval
  character(80), intent(out) :: errmsg

  type(airfoil_type) :: buffer_foil

  double precision :: xoffset, zoffset, foilscale

  errval = 0
  errmsg = ''
  
  ! Load seed airfoil into memory, including transformations and smoothing

  call get_seed_airfoil(seed_airfoil, airfoil_file, naca_digits, buffer_foil,  &
                        xoffset, zoffset, foilscale, errval, errmsg)

  if (errval /= 0) return

end subroutine initialize

end module xoptfoil_interface
