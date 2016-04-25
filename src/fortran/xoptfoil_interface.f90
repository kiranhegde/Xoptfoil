module xoptfoil_interface

  implicit none

  contains

!=============================================================================80
!
! Converts fortran errval/errmsg output to C (only for use within this module)
!
!=============================================================================80
subroutine convert_to_c(errval, errmsg, msglen, cerrval, cerrmsg)

  use iso_c_binding, only : C_INT, C_CHAR

  integer, intent(in) :: errval, msglen
  character(len=msglen), intent(in) :: errmsg
  integer(kind=C_INT), intent(out) :: cerrval
  character(kind=C_CHAR, len=1), dimension(msglen), intent(out) :: cerrmsg

  integer :: i

  cerrval = errval
  do i = 1, msglen
    cerrmsg(i) = errmsg(i:i)
  end do

end subroutine convert_to_c

!=============================================================================80
!
! Reads inputs from fortran namelist file
!
!=============================================================================80
subroutine read_namelist_inputs(cinput_file, cerrval, cerrmsg) bind(c)

  use iso_c_binding, only : C_INT, C_CHAR
  use vardef
  use input_output,  only : read_inputs

  character(kind=C_CHAR, len=1), dimension(80), intent(in) :: cinput_file
  integer(kind=C_INT), intent(out) :: cerrval
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

  call convert_to_c(errval, errmsg, 80, cerrval, cerrmsg)

end subroutine read_namelist_inputs

!=============================================================================80
!
! Initializes data for optimization
!
!=============================================================================80
subroutine initialize(cerrval, cerrmsg) bind(c)

  use iso_c_binding,      only : C_INT, C_CHAR
  use vardef,             only : seed_airfoil, airfoil_file, naca_digits
  use airfoil_operations, only : get_seed_airfoil
  use xfoil_driver,       only : airfoil_type

  integer(kind=C_INT), intent(out) :: cerrval
  character(kind=C_CHAR, len=1), dimension(80), intent(out) :: cerrmsg

  type(airfoil_type) :: buffer_foil
  integer :: errval
  character(80) :: errmsg
  double precision :: xoffset, zoffset, foilscale

  errval = 0
  errmsg = ''
  
! Load seed airfoil into memory, including transformations and smoothing

  call get_seed_airfoil(seed_airfoil, airfoil_file, naca_digits, buffer_foil,  &
                        xoffset, zoffset, foilscale, errval, errmsg)

! Return if there was an error

  if (errval /= 0) then
    call convert_to_c(errval, errmsg, 80, cerrval, cerrmsg)
    return
  end if

! Convert to C outputs

  call convert_to_c(errval, errmsg, 80, cerrval, cerrmsg)
 
end subroutine initialize

end module xoptfoil_interface
