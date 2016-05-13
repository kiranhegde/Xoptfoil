!  This file is part of XOPTFOIL.

!  XOPTFOIL is free software: you can redistribute it and/or modify
!  it under the terms of the GNU General Public License as published by
!  the Free Software Foundation, either version 3 of the License, or
!  (at your option) any later version.

!  XOPTFOIL is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU General Public License for more details.

!  You should have received a copy of the GNU General Public License
!  along with XOPTFOIL.  If not, see <http://www.gnu.org/licenses/>.

!  Copyright (C) 2014 -- 2016 Daniel Prosser

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
! Reads seed airfoil, allocates memory, checks seed
!
!=============================================================================80
subroutine initialize(cerrval, cerrmsg) bind(c)

  use iso_c_binding,      only : C_INT, C_CHAR
  use settings,           only : seed_airfoil, airfoil_file, naca_digits
  use airfoil_operations, only : get_seed_airfoil, get_split_points,           &
                                 split_airfoil, deallocate_airfoil
  use xfoil_driver,       only : airfoil_type
  use airfoil_evaluation, only : xseedt, zseedt, xseedb, zseedb, symmetrical,  &
                                 allocate_airfoil_data
  use input_sanity,       only : check_seed

  integer(kind=C_INT), intent(out) :: cerrval
  character(kind=C_CHAR, len=1), dimension(80), intent(out) :: cerrmsg

  type(airfoil_type) :: buffer_foil
  integer :: errval, pointst, pointsb
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

! Split up seed airfoil into upper and lower surfaces

  call get_split_points(buffer_foil, pointst, pointsb, symmetrical)
  allocate(xseedt(pointst))
  allocate(zseedt(pointst))
  allocate(xseedb(pointsb))
  allocate(zseedb(pointsb))
  call split_airfoil(buffer_foil, xseedt, xseedb, zseedt, zseedb, symmetrical)

! Deallocate buffer airfoil (no longer needed)

  call deallocate_airfoil(buffer_foil)

! Allocate memory for airfoil analysis

  call allocate_airfoil_data()

! Check that seed airfoil passes constraints

  call check_seed(xoffset, zoffset, foilscale, errval, errmsg)

! Convert to C outputs

  call convert_to_c(errval, errmsg, 80, cerrval, cerrmsg)
 
end subroutine initialize

!=============================================================================80
!
! Sets up optimizer data
!
!=============================================================================80
subroutine optimizer_setup(cerrval, cerrmsg) bind(c)

  integer(kind=C_INT), intent(out) :: cerrval
  character(kind=C_CHAR, len=1), dimension(80), intent(out) :: cerrmsg

end subroutine optimizer_setup

!=============================================================================80
!
! Iterates optimizer
!
!=============================================================================80
subroutine iterate(cerrval, cerrmsg) bind(c)

  integer(kind=C_INT), intent(out) :: cerrval
  character(kind=C_CHAR, len=1), dimension(80), intent(out) :: cerrmsg

end subroutine iterate

!=============================================================================80
!
! Deallocates memory
!
!=============================================================================80
subroutine cleanup() bind(c)

  use airfoil_evaluation, only : xseedt, xseedb, zseedt, zseedb,               &
                                 deallocate_airfoil_data

  call deallocate_airfoil_data()
  deallocate(xseedt) 
  deallocate(zseedt) 
  deallocate(xseedb) 
  deallocate(zseedb) 

end subroutine cleanup

end module xoptfoil_interface
