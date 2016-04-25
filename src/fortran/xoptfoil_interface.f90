module xoptfoil_interface

  implicit none

  contains

!=============================================================================80
!
! Reads inputs from fortran namelist file
!
!=============================================================================80
subroutine read_namelist_inputs(len_input_file, input_file)                    &
                                            bind(c, name='read_namelist_inputs')

  use iso_c_binding
  use vardef
  use input_output, only : read_inputs

  integer(C_INT) :: len_input_file
  character(1), intent(in) :: input_file(len_input_file)
!  integer, intent(out) :: errval
!  character(80), intent(out) :: errmsg

  character(len_input_file) :: finput_file
integer :: errval
character(80) :: errmsg
integer i

  do i = 1, len_input_file
    finput_file(i:i) = input_file(i)(1:1)
  end do

  errval = 0
  errmsg = ''

  call read_inputs(finput_file, errval, errmsg)

end subroutine read_namelist_inputs

!=============================================================================80
!
! Initializes data for optimization
!
!=============================================================================80
!subroutine initialize(errval, errmsg)
!
!  use vardef
!  use airfoil_operations, only : get_seed_airfoil
!  use xfoil_driver,       only : airfoil_type
!
!  integer, intent(out) :: errval
!  character(80), intent(out) :: errmsg
!
!  type(airfoil_type) :: buffer_foil
!
!  double precision :: xoffset, zoffset, foilscale
!
!  errval = 0
!  errmsg = ''
!  
!  ! Load seed airfoil into memory, including transformations and smoothing
!
!  call get_seed_airfoil(seed_airfoil, airfoil_file, naca_digits, buffer_foil,  &
!                        xoffset, zoffset, foilscale, errval, errmsg)
!
!  if (errval /= 0) return
!
!end subroutine initialize

end module xoptfoil_interface
