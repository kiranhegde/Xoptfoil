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

  public
  private :: convert_char_to_c, convert_char_to_fortran

  contains

!=============================================================================80
!
! Converts fortran char array to C
!
!=============================================================================80
subroutine convert_char_to_c(msg, msglen, cmsg)

  use iso_c_binding, only : C_CHAR

  integer, intent(in) :: msglen
  character(len=msglen), intent(in) :: msg
  character(kind=C_CHAR, len=1), dimension(msglen), intent(out) :: cmsg

  integer :: i

  do i = 1, msglen
    cmsg(i) = msg(i:i)
  end do

end subroutine convert_char_to_c

!=============================================================================80
!
! Converts C char array to fortran
!
!=============================================================================80
subroutine convert_char_to_fortran(cmsg, msglen, msg)

  use iso_c_binding, only : C_CHAR

  integer, intent(in) :: msglen
  character(kind=C_CHAR, len=1), dimension(msglen), intent(in) :: cmsg
  character(len=msglen), intent(out) :: msg

  integer :: i

  do i = 1, msglen
    msg(i:i) = cmsg(i)
  end do

end subroutine convert_char_to_fortran

!=============================================================================80
!
! Reads inputs from fortran namelist file
!
!=============================================================================80
subroutine read_namelist_inputs(                                               &
                   cinput_file, max_op_points, csearch_type, cglobal_search,   &
                   clocal_search, cseed_airfoil, cairfoil_file, cnaca_digits,  &
                   nfunctions_top, nfunctions_bot, restart, restart_write_freq,&
                   flap_flag, cseed_violation_handling, errval, cerrmsg) bind(c)

  use iso_c_binding, only : C_INT, C_CHAR, C_BOOL
  use input_output,  only : read_inputs

  character(kind=C_CHAR, len=1), dimension(80), intent(in) :: cinput_file
  integer(kind=C_INT), intent(in) :: max_op_points
  character(kind=C_CHAR, len=1), dimension(16), intent(out) :: csearch_type
  character(kind=C_CHAR, len=1), dimension(17), intent(out) :: cglobal_search
  character(kind=C_CHAR, len=1), dimension(7), intent(out) :: clocal_search
  character(kind=C_CHAR, len=1), dimension(10), intent(out) :: cseed_airfoil
  character(kind=C_CHAR, len=1), dimension(80), intent(out) :: cairfoil_file,  &
                                                               cerrmsg
  character(kind=C_CHAR, len=1), dimension(4), intent(out) :: cnaca_digits,    &
                                                        cseed_violation_handling
  integer(kind=C_INT), intent(out) :: nfunctions_top, nfunctions_bot,          &
                                      restart_write_freq, errval
  logical(kind=C_BOOL), intent(out) :: restart
  integer(kind=C_INT), dimension(max_op_points), intent(out) :: flap_flag

  character(80) :: input_file, airfoil_file, errmsg
  character(16) :: search_type
  character(17) :: global_search
  character(7) :: local_search
  character(10) :: seed_airfoil
  character(4) :: naca_digits, seed_violation_handling

  errval = 0
  errmsg = ''

! Convert input_file from C to Fortran style char array
  
  call convert_char_to_fortran(cinput_file, 80, input_file)

! Read Fortran namelist inputs

  call read_inputs(input_file, search_type, global_search, local_search,       &
                   seed_airfoil, airfoil_file, naca_digits, nfunctions_top,    &
                   nfunctions_bot, restart, restart_write_freq, flap_flag,     &
                   seed_violation_handling, errval, errmsg)

! Convert char arrays to C

  call convert_char_to_c(search_type, 16, csearch_type)
  call convert_char_to_c(global_search, 17, cglobal_search)
  call convert_char_to_c(local_search, 7, clocal_search)
  call convert_char_to_c(seed_airfoil, 10, cseed_airfoil)
  call convert_char_to_c(airfoil_file, 80, cairfoil_file)
  call convert_char_to_c(naca_digits, 4, cnaca_digits)
  call convert_char_to_c(seed_violation_handling, 4, cseed_violation_handling)
  call convert_char_to_c(errmsg, 80, cerrmsg)

end subroutine read_namelist_inputs

!=============================================================================80
!
! Reads seed airfoil, allocates memory, checks seed
!
!=============================================================================80
subroutine initialize(cseed_airfoil, cairfoil_file, cnaca_digits,              &
                      nfunctions_top, nfunctions_bot, cseed_violation_handling,&
                      errval, cerrmsg) bind(c)

  use iso_c_binding,      only : C_INT, C_CHAR
  use airfoil_operations, only : get_seed_airfoil, get_split_points,           &
                                 split_airfoil, deallocate_airfoil
  use xfoil_driver,       only : airfoil_type
  use airfoil_evaluation, only : xseedt, zseedt, xseedb, zseedb, symmetrical,  &
                                 allocate_airfoil_data
  use input_sanity,       only : check_seed

  character(kind=C_CHAR, len=1), dimension(10), intent(in) :: cseed_airfoil
  character(kind=C_CHAR, len=1), dimension(80), intent(in) :: cairfoil_file
  character(kind=C_CHAR, len=1), dimension(4), intent(in) :: cnaca_digits,     &
                                                        cseed_violation_handling
  integer(kind=C_INT), intent(in) :: nfunctions_top, nfunctions_bot
  integer(kind=C_INT), intent(out) :: errval
  character(kind=C_CHAR, len=1), dimension(80), intent(out) :: cerrmsg

  type(airfoil_type) :: buffer_foil
  integer :: pointst, pointsb
  character(10) :: seed_airfoil
  character(80) :: airfoil_file, errmsg
  character(4) :: naca_digits, seed_violation_handling
  double precision :: xoffset, zoffset, foilscale

  errval = 0
  errmsg = ''

! Convert C char arrays to Fortran

  call convert_char_to_fortran(cseed_airfoil, 10, seed_airfoil)
  call convert_char_to_fortran(cairfoil_file, 80, airfoil_file)
  call convert_char_to_fortran(cnaca_digits, 4, naca_digits)
  call convert_char_to_fortran(cseed_violation_handling, 4,                    &
                               seed_violation_handling)
  
! Load seed airfoil into memory, including transformations and smoothing

  call get_seed_airfoil(seed_airfoil, airfoil_file, naca_digits, buffer_foil,  &
                        xoffset, zoffset, foilscale, errval, errmsg)

! Return if there was an error

  if (errval /= 0) then
    call convert_char_to_c(errmsg, 80, cerrmsg)
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

  call allocate_airfoil_data(nfunctions_top, nfunctions_bot)

! Check that seed airfoil passes constraints

  call check_seed(xoffset, zoffset, foilscale, seed_violation_handling, errval,&
                  errmsg)

! Convert to C outputs

  call convert_char_to_c(errmsg, 80, cerrmsg)
 
end subroutine initialize

!=============================================================================80
!
! Sets up optimizer data
!
!=============================================================================80
subroutine optimizer_setup(errval, cerrmsg) bind(c)

  use iso_c_binding,      only : C_INT, C_CHAR

  integer(kind=C_INT), intent(out) :: errval
  character(kind=C_CHAR, len=1), dimension(80), intent(out) :: cerrmsg

end subroutine optimizer_setup

!=============================================================================80
!
! Iterates optimizer
!
!=============================================================================80
subroutine iterate(errval, cerrmsg) bind(c)

  use iso_c_binding,      only : C_INT, C_CHAR

  integer(kind=C_INT), intent(out) :: errval
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
