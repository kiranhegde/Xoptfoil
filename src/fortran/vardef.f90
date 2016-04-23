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

module vardef

  use optimization_util, only : pso_options_type, ga_options_type,             &
                                ds_options_type
  use xfoil_driver,      only : airfoil_type, xfoil_options_type,              &
                                xfoil_geom_options_type

  implicit none

! Optimization options

  character(80) :: search_type, global_search, local_search, seed_airfoil,     &
                   airfoil_file
  character(4) :: naca_digits
  character(11) :: shape_functions
  integer :: nfunctions_top, nfunctions_bot, restart_write_freq
  double precision :: initial_perturb, min_bump_width
  logical :: restart

! Operating conditions

  integer, parameter :: max_op_points = 30
  integer :: noppoint
  character(7), dimension(max_op_points) :: op_mode
  character(8), dimension(max_op_points) :: flap_selection
  character(9), dimension(max_op_points) :: optimization_type
  double precision, dimension(max_op_points) :: op_point, reynolds, mach,      &
                                                flap_degrees, weighting 
  logical :: use_flap
  double precision :: x_flap, y_flap

! Constraints

  character(4) :: seed_violation_handling
  character(8), dimension(max_op_points) :: moment_constraint_type
  double precision :: min_thickness, max_thickness, min_te_angle,              &
                      curv_threshold, min_flap_degrees, max_flap_degrees
  double precision, dimension(max_op_points) :: min_moment
  logical :: check_curvature, symmetrical
  integer :: max_curv_reverse

! Structures for other options

  type(pso_options_type) :: pso_options
  type(ga_options_type) :: ga_options
  type(ds_options_type) :: ds_options
  type(xfoil_options_type) :: xfoil_options
  type(xfoil_geom_options_type) :: xfoil_geom_options

! Matchfoils

  logical match_foils
  character(80) :: matchfoil_file

! Other global variables

  double precision, dimension(max_op_points) :: scale_factor
  double precision :: growth_allowed
  integer, dimension(:), allocatable :: constrained_dvs

  double precision, dimension(:), allocatable :: xseedt, xseedb, zseedt, zseedb
  integer :: nflap_optimize          ! Number of operating points where flap 
                                     !   setting will be optimized
  integer, dimension(max_op_points) :: flap_optimize_points

  type(airfoil_type) :: curr_foil
  double precision, dimension(:), allocatable :: xmatcht, xmatchb, zmatcht,    &
                                                 zmatchb

  character(80) :: output_prefix

!$omp threadprivate(curr_foil)

end module vardef
