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

module util

! Module with some useful subroutines

  implicit none

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

end module util
