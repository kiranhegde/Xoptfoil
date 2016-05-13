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

module settings

  use optimization_util, only : pso_options_type, ga_options_type,             &
                                ds_options_type
  use xfoil_driver,      only : airfoil_type, xfoil_options_type,              &
                                xfoil_geom_options_type

  implicit none

! Optimization settings

  character(80) :: search_type, global_search, local_search, seed_airfoil,     &
                   airfoil_file
  character(4) :: naca_digits
  integer :: nfunctions_top, nfunctions_bot, restart_write_freq
  double precision :: min_bump_width
  logical :: restart

! Operating settings

  character(8), dimension(max_op_points) :: flap_selection

! Constraints

  character(4) :: seed_violation_handling

! Structures for other settings

  type(pso_options_type) :: pso_options
  type(ga_options_type) :: ga_options
  type(ds_options_type) :: ds_options

! Matchfoils

  logical match_foils
  character(80) :: matchfoil_file

!FIXME: these should be moved somewhere else
! Other global variables

  integer, dimension(:), allocatable :: constrained_dvs

  integer, dimension(max_op_points) :: flap_optimize_points

  double precision, dimension(:), allocatable :: xmatcht, xmatchb, zmatcht,    &
                                                 zmatchb

  character(80) :: output_prefix

end module settings
