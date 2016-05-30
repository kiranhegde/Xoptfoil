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

module types

! Defines MAX_OP_POINTS

#include "constants.h"

! Module defining derived types for grouping settings

  use ISO_C_BINDING, only : C_INT, C_DOUBLE, C_BOOL, C_CHAR

  implicit none

  type, bind(c) :: optimization_settings_type
    character(kind=C_CHAR) :: search_type(16), global_search(17),              &
                           local_search(7), seed_airfoil(10), airfoil_file(80),&
                           naca_digits(4), shape_functions(11)
    integer(kind=C_INT) :: nfunctions_top, nfunctions_bot
    real(kind=C_DOUBLE) :: initial_perturb, min_bump_width
    logical(kind=C_BOOL) :: restart
    integer(kind=C_INT) :: restart_write_freq
    logical(kind=C_BOOL) :: write_designs
  end type optimization_settings_type

  type, bind(c) :: operating_points_settings_type
    integer(kind=C_INT) :: noppoint
    logical(kind=C_BOOL) :: use_flap
    real(kind=C_DOUBLE) :: x_flap, y_flap
    character(kind=C_CHAR), dimension(7,MAX_OP_POINTS) :: op_mode
    character(kind=C_CHAR), dimension(9,MAX_OP_POINTS) :: optimization_type
    real(kind=C_DOUBLE), dimension(MAX_OP_POINTS) :: op_point, reynolds, mach
    character(kind=C_CHAR), dimension(8,MAX_OP_POINTS) :: flap_selection
    real(kind=C_DOUBLE), dimension(MAX_OP_POINTS) :: flap_degrees, weighting
  end type operating_points_settings_type

  type, bind(c) :: constraints_settings_type
    character(kind=C_CHAR) :: seed_violation_handling(4)
    real(kind=C_DOUBLE) :: min_thickness, max_thickness, min_te_angle
    real(kind=C_DOUBLE) :: min_camber, max_camber
    logical(kind=C_BOOL) :: check_curvature
    integer(kind=C_INT) :: max_curv_reverse_top, max_curv_reverse_bot
    real(kind=C_DOUBLE) :: curv_threshold
    logical(kind=C_BOOL) :: symmetrical
    real(kind=C_DOUBLE) :: max_flap_degrees, min_flap_degrees
    character(kind=C_CHAR), dimension(8,MAX_OP_POINTS) :: moment_constraint_type
    real(kind=C_DOUBLE), dimension(MAX_OP_POINTS) :: min_moment
  end type constraints_settings_type

  type, bind(c) :: initialization_settings_type
    logical(kind=C_BOOL) :: feasible_init
    real(kind=C_DOUBLE) :: feasible_limit
    integer(kind=C_INT) :: feasible_init_attempts
  end type initialization_settings_type

  type, bind(c) :: particle_swarm_settings_type
    integer(kind=C_INT) :: pso_pop
    real(kind=C_DOUBLE) :: pso_tol
    integer(kind=C_INT) :: pso_maxit
    character(kind=C_CHAR) :: pso_convergence_profile(10)
  end type particle_swarm_settings_type

  type, bind(c) :: genetic_algorithm_settings_type
    integer(kind=C_INT) :: ga_pop
    real(kind=C_DOUBLE) :: ga_tol
    integer(kind=C_INT) :: ga_maxit
    character(kind=C_CHAR) :: parents_selection_method(10)
    real(kind=C_DOUBLE) :: parent_fraction, roulette_selection_pressure,       &
                           tournament_fraction, crossover_range_factor,        &
                           mutant_probability, chromosome_mutation_rate,       &
                           mutation_range_factor
  end type genetic_algorithm_settings_type

  type, bind(c) :: simplex_settings_type
    real(kind=C_DOUBLE) :: simplex_tol
    integer(kind=C_INT) :: simplex_maxit
  end type simplex_settings_type

  type, bind(c) :: xfoil_settings_type
    real(kind=C_DOUBLE) :: ncrit, xtript, xtripb
    logical(kind=C_BOOL) :: viscous_mode, silent_mode
    integer(kind=C_INT) :: bl_maxit
    real(kind=C_DOUBLE) :: vaccel
    logical(kind=C_BOOL) :: fix_unconverged, reinitialize
  end type xfoil_settings_type

  type, bind(c) :: xfoil_paneling_settings_type
    integer(kind=C_INT) :: npan
    real(kind=C_DOUBLE) :: cvpar, cterat, ctrrat, xsref1, xsref2, xpref1, xpref2
  end type xfoil_paneling_settings_type

  type, bind(c) :: matchfoil_settings_type
    logical(kind=C_BOOL) :: match_foils
    character(kind=C_CHAR) :: matchfoil_file(80)
  end type matchfoil_settings_type

end module types
