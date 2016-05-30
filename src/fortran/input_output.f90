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

module input_output

! Module with subroutines for reading and writing of files

  implicit none

  contains

!=============================================================================80
!
! Subroutine to read inputs from namelist file
!
!=============================================================================80
subroutine read_inputs(input_file, max_op_points, optimization_settings,       &
                   operating_points_settings, constraints_settings,            &
                   initialization_settings, particle_swarm_settings,           &
                   genetic_algorithm_settings, simplex_settings,               &
                   xfoil_settings, xfoil_paneling_settings, matchfoil_settings,&
                   errval, errmsg)

  use iso_c_binding, only : C_BOOL
  use types, only : optimization_settings_type, operating_points_settings_type,&
                 constraints_settings_type, initialization_settings_type,      &
                 particle_swarm_settings_type, genetic_algorithm_settings_type,&
                 simplex_settings_type, xfoil_settings_type,                   &
                 xfoil_paneling_settings_type, matchfoil_settings_type
  use util, only : convert_char_to_c

  character(*), intent(in) :: input_file
  integer, intent(in) :: max_op_points
  integer, intent(out) :: errval
  character(80), intent(out) :: errmsg

! Derived types to group settings

  type(optimization_settings_type), intent(out) :: optimization_settings
  type(operating_points_settings_type), intent(out) :: operating_points_settings
  type(constraints_settings_type), intent(out) :: constraints_settings
  type(initialization_settings_type), intent(out) :: initialization_settings
  type(particle_swarm_settings_type), intent(out) :: particle_swarm_settings
  type(genetic_algorithm_settings_type), intent(out) ::                        &
                                                      genetic_algorithm_settings
  type(simplex_settings_type), intent(out) :: simplex_settings
  type(xfoil_settings_type), intent(out) :: xfoil_settings
  type(xfoil_paneling_settings_type), intent(out) :: xfoil_paneling_settings
  type(matchfoil_settings_type), intent(out) :: matchfoil_settings

! Optimization settings

  character(16) :: search_type
  character(17) :: global_search
  character(7) :: local_search
  character(10) :: seed_airfoil
  character(80) :: airfoil_file
  character(4) :: naca_digits
  character(11) :: shape_functions
  integer :: nfunctions_top, nfunctions_bot
  double precision :: initial_perturb, min_bump_width
  logical(kind=C_BOOL) :: restart
  integer :: restart_write_freq
  logical(kind=C_BOOL) :: write_designs

! Operating points

  integer :: noppoint
  logical(kind=C_BOOL) :: use_flap
  double precision :: x_flap, y_flap
  character(7), dimension(max_op_points) :: op_mode
  character(9), dimension(max_op_points) :: optimization_type
  double precision, dimension(max_op_points) :: op_point, reynolds, mach
  character(8), dimension(max_op_points) :: flap_selection
  double precision, dimension(max_op_points) :: flap_degrees, weighting

! Constraints

  character(4) :: seed_violation_handling
  double precision :: min_thickness, max_thickness, min_te_angle
  double precision :: min_camber, max_camber
  logical(kind=C_BOOL) :: check_curvature
  integer :: max_curv_reverse_top, max_curv_reverse_bot
  double precision :: curv_threshold
  logical(kind=C_BOOL) :: symmetrical
  double precision :: max_flap_degrees, min_flap_degrees
  character(8), dimension(max_op_points) :: moment_constraint_type
  double precision, dimension(max_op_points) :: min_moment

! Initialization options

  logical(kind=C_BOOL) :: feasible_init
  double precision :: feasible_limit
  integer :: feasible_init_attempts

! Particle swarm options

  integer :: pso_pop
  double precision :: pso_tol
  integer :: pso_maxit
  character(10) :: pso_convergence_profile

! Genetic algorithm options

  integer :: ga_pop
  double precision :: ga_tol
  integer :: ga_maxit
  character(10) :: parents_selection_method
  double precision :: parent_fraction, roulette_selection_pressure,            &
                      tournament_fraction, crossover_range_factor,             &
                      mutant_probability, chromosome_mutation_rate,            &
                      mutation_range_factor

! Simplex options

  double precision :: simplex_tol
  integer :: simplex_maxit

! Xfoil run options

  double precision :: ncrit, xtript, xtripb
  logical(kind=C_BOOL) :: viscous_mode, silent_mode
  integer :: bl_maxit
  double precision :: vaccel
  logical(kind=C_BOOL) :: fix_unconverged, reinitialize

! Xfoil paneling options

  integer :: npan
  double precision :: cvpar, cterat, ctrrat, xsref1, xsref2, xpref1, xpref2

! Matchfoil options

  logical(kind=C_BOOL) :: match_foils
  character(80) :: matchfoil_file
  
! Other variables

  integer :: i, iunit, ioerr, iostat1
  character(30) :: text
  integer :: nbot_actual, nmoment_constraint
  character :: choice

  namelist /optimization_options/ search_type, global_search, local_search,    &
            seed_airfoil, airfoil_file, naca_digits, shape_functions,          &
            nfunctions_top, nfunctions_bot, initial_perturb, min_bump_width,   &
            restart, restart_write_freq, write_designs
  namelist /operating_conditions/ noppoint, op_mode, op_point, reynolds, mach, &
            use_flap, x_flap, y_flap, flap_selection, flap_degrees, weighting, &
            optimization_type 
  namelist /constraints/ min_thickness, max_thickness, moment_constraint_type, &
                         min_moment, min_te_angle, check_curvature,            &
                         max_curv_reverse_top, max_curv_reverse_bot,           &
                         curv_threshold, symmetrical, min_flap_degrees,        &
                         max_flap_degrees, min_camber, max_camber
  namelist /initialization/ feasible_init, feasible_limit,                     &
                            feasible_init_attempts
  namelist /particle_swarm_options/ pso_pop, pso_tol, pso_maxit,               &
                                    pso_convergence_profile
  namelist /genetic_algorithm_options/ ga_pop, ga_tol, ga_maxit,               &
            parents_selection_method, parent_fraction,                         &
            roulette_selection_pressure, tournament_fraction,                  &
            crossover_range_factor, mutant_probability,                        &
            chromosome_mutation_rate, mutation_range_factor
  namelist /simplex_options/ simplex_tol, simplex_maxit
  namelist /xfoil_run_options/ ncrit, xtript, xtripb, viscous_mode,            &
            silent_mode, bl_maxit, vaccel, fix_unconverged, reinitialize
  namelist /xfoil_paneling_options/ npan, cvpar, cterat, ctrrat, xsref1,       &
            xsref2, xpref1, xpref2
  namelist /matchfoil_options/ match_foils, matchfoil_file

  errval = 0
  errmsg = ''

! Open input file

  iunit = 12
  open(unit=iunit, file=input_file, status='old', iostat=ioerr)
  if (ioerr /= 0) then
    errval = 1
    errmsg = 'could not find input file '//trim(input_file)//'.'
    return
  end if

! Set defaults for main namelist options

  search_type = 'global_and_local'
  global_search = 'particle_swarm'
  local_search = 'simplex'
  seed_airfoil = 'four_digit'
  naca_digits = '0012'
  shape_functions = 'hicks-henne'
  min_bump_width = 0.1d0
  nfunctions_top = 4
  nfunctions_bot = 4
  initial_perturb = 0.025d0
  restart = .false.
  restart_write_freq = 20
  write_designs = .true.

! Read main namelist options

  rewind(iunit)
  read(iunit, iostat=iostat1, nml=optimization_options)
  call namelist_check('optimization_options', iostat1, 'warn', errval, errmsg)
  if (errval /= 0) return

! Populate optimization_settings derived type

  call convert_char_to_c(search_type, 16, optimization_settings%search_type)
  call convert_char_to_c(global_search, 17, optimization_settings%global_search)
  call convert_char_to_c(local_search, 7, optimization_settings%local_search)
  call convert_char_to_c(seed_airfoil, 10, optimization_settings%seed_airfoil)
  call convert_char_to_c(airfoil_file, 80, optimization_settings%airfoil_file)
  call convert_char_to_c(naca_digits, 4, optimization_settings%naca_digits)
  call convert_char_to_c(shape_functions, 11,                                  &
                         optimization_settings%shape_functions)
  optimization_settings%nfunctions_top = nfunctions_top
  optimization_settings%nfunctions_bot = nfunctions_bot
  optimization_settings%initial_perturb = initial_perturb
  optimization_settings%min_bump_width = min_bump_width
  optimization_settings%restart = restart
  optimization_settings%restart_write_freq = restart_write_freq
  optimization_settings%write_designs = write_designs

! Check optimization_settings for errors

  call check_optimization_settings(optimization_settings, errval, errmsg)
  if (errval /= 0) return

! Set defaults for operating conditions

  noppoint = 1
  use_flap = .false.
  x_flap = 0.75d0
  y_flap = 0.d0
  op_mode(:) = 'spec-cl'
  op_point(:) = 0.d0
  optimization_type(:) = 'min-drag'
  reynolds(:) = 1.0D+05
  mach(:) = 0.d0
  flap_selection(:) = 'specify'
  flap_degrees(:) = 0.d0
  weighting(:) = 1.d0

! Read operating conditions

  rewind(iunit)
  read(iunit, iostat=iostat1, nml=operating_conditions)
  call namelist_check('operating_conditions', iostat1, 'stop', errval, errmsg)
  if (errval /= 0) return

! Populate operating_points_settings derived type

  operating_points_settings%noppoint = noppoint
  operating_points_settings%use_flap = use_flap
  operating_points_settings%x_flap = x_flap
  operating_points_settings%y_flap = y_flap
  do i = 1, max_op_points
    call convert_char_to_c(op_mode(i), len(op_mode(i)),                        &
                           operating_points_settings%op_mode(:,i))
    call convert_char_to_c(optimization_type(i), len(optimization_type(i)),    &
                           operating_points_settings%optimization_type(:,i))
  end do
  operating_points_settings%op_point = op_point
  operating_points_settings%reynolds = reynolds
  operating_points_settings%mach = mach
  do i = 1, max_op_points
    call convert_char_to_c(flap_selection(i), len(flap_selection(i)),          &
                           operating_points_settings%flap_selection(:,i))
  end do
  operating_points_settings%flap_degrees = flap_degrees
  operating_points_settings%weighting = weighting 

! Operating points

  call check_operating_points_settings(operating_points_settings,              &
                                       max_op_points, errval, errmsg)
  if (errval /= 0) return

! Set defaults for constraints

  seed_violation_handling = 'stop'
  min_thickness = 0.06d0
  max_thickness = 1000.d0
  min_camber = -0.1d0
  max_camber = 0.1d0
  moment_constraint_type(:) = 'use_seed'
  min_moment(:) = -1.d0
  min_te_angle = 5.d0
  check_curvature = .true.
  max_curv_reverse_top = 0
  max_curv_reverse_bot = 1
  curv_threshold = 0.30d0
  symmetrical = .false.
  min_flap_degrees = -5.d0
  max_flap_degrees = 15.d0

! Read constraints

  rewind(iunit)
  read(iunit, iostat=iostat1, nml=constraints)
  call namelist_check('constraints', iostat1, 'stop', errval, errmsg)
  if (errval /= 0) return

! Store operating points where flap setting will be optimized. Also converts
! flap_selection to int array for easier passing to C.

!FIXME: this should go somewhere else, called by initialize
!  nflap_optimize = 0
!  if (use_flap .and. (.not. match_foils)) then
!    do i = 1, noppoint
!      if (flap_selection(i) == 'optimize') then
!        nflap_optimize = nflap_optimize + 1
!        flap_optimize_points(nflap_optimize) = i
!      end if
!    end do
!  end if

! Normalize weightings for operating points

  weighting = weighting/sum(weighting(1:noppoint))

!FIXME: this will have to be done in a different way
! Ask about removing pitching moment constraints for symmetrical optimization

  if (symmetrical) then
    nmoment_constraint = 0
    do i = 1, noppoint
      if (trim(moment_constraint_type(i)) /= 'none')                           &
        nmoment_constraint = nmoment_constraint + 1
    end do
    
    if (nmoment_constraint > 0) choice = ask_moment_constraints()
    if (choice == 'y') moment_constraint_type(:) = 'none'
  end if

! Set default initialization options

  feasible_init = .true.
  feasible_limit = 5.0D+04
  feasible_init_attempts = 1000

! Read initialization parameters

  rewind(iunit)
  read(iunit, iostat=iostat1, nml=initialization)
  call namelist_check('initialization', iostat1, 'warn', errval, errmsg)
  if (errval /= 0) return

! Set default particle swarm options

  pso_pop = 40
  pso_tol = 1.D-04
  pso_maxit = 700
  pso_convergence_profile = 'exhaustive'

! Set default genetic algorithm options

  ga_pop = 80
  ga_tol = 1.D-04
  ga_maxit = 700
  parents_selection_method = 'tournament'
  parent_fraction = 0.5d0
  roulette_selection_pressure = 8.d0
  tournament_fraction = 0.025d0
  crossover_range_factor = 0.5d0
  mutant_probability = 0.4d0
  chromosome_mutation_rate = 0.01d0
  mutation_range_factor = 0.2d0

! Set default simplex search options

  simplex_tol = 1.0D-05
  simplex_maxit = 1000

  if (trim(search_type) == 'global_and_local' .or. trim(search_type) ==        &
      'global') then

!   The number of bottom shape functions actually used (0 for symmetrical)

    if (symmetrical) then
      nbot_actual = 0
    else
      nbot_actual = nfunctions_bot
    end if
  
!FIXME: this should go somewhere else, called by initialize
!!   Set design variables with side constraints
!
!    if (trim(shape_functions) == 'naca') then
!
!!     For NACA, we will only constrain the flap deflection
!
!      allocate(constrained_dvs(nflap_optimize))
!      counter = 0
!      do i = nfunctions_top + nbot_actual + 1,                                 &
!             nfunctions_top + nbot_acutal + nflap_optimize
!        counter = counter + 1
!        constrained_dvs(counter) = i
!      end do
!          
!    else
!
!!     For Hicks-Henne, also constrain bump locations and width
!
!      allocate(constrained_dvs(2*nfunctions_top + 2*nbot_actual +              &
!                               nflap_optimize))
!      counter = 0
!      do i = 1, nfunctions_top + nbot_actual
!        counter = counter + 1
!        idx = 3*(i-1) + 2      ! DV index of bump location, shape function i
!        constrained_dvs(counter) = idx
!        counter = counter + 1
!        idx = 3*(i-1) + 3      ! Index of bump width, shape function i
!        constrained_dvs(counter) = idx
!      end do
!      do i = 3*(nfunctions_top + nbot_actual) + 1,                             &
!             3*(nfunctions_top + nbot_actual) + nflap_optimize
!        counter = counter + 1
!        constrained_dvs(counter) = i
!      end do
!
!    end if

    if (trim(global_search) == 'particle_swarm') then

!     Read PSO options and put them into derived type

      rewind(iunit)
      read(iunit, iostat=iostat1, nml=particle_swarm_options)
      call namelist_check('particle_swarm_options', iostat1, 'warn', errval,   &
                          errmsg)
      if (errval /= 0) return

    else if (trim(global_search) == 'genetic_algorithm') then

!     Read genetic algorithm options and put them into derived type

      rewind(iunit)
      read(iunit, iostat=iostat1, nml=genetic_algorithm_options)
      call namelist_check('genetic_algorithm_options', iostat1, 'warn', errval,&
                          errmsg)
      if (errval /= 0) return
    else

      errval = 1
      errmsg = "global search type '"//trim(global_search)//&
                "' is not available."
      return
     
    end if

  end if

  if (trim(search_type) == 'global_and_local' .or. trim(search_type) ==        &
      'local') then

    if (trim(local_search) == 'simplex') then

!     Read simplex search options and put them into derived type

      rewind(iunit)
      read(iunit, iostat=iostat1, nml=simplex_options)
      call namelist_check('simplex_options', iostat1, 'warn', errval, errmsg)
      if (errval /= 0) return

    else

      errval = 1
      errmsg = "local search type '"//trim(local_search)//&
               "' is not available."
      return
     
    end if

  end if 

! Set default xfoil aerodynamics and paneling options

  ncrit = 9.d0
  xtript = 1.d0
  xtripb = 1.d0
  viscous_mode = .true.
  silent_mode = .true.
  bl_maxit = 100
  vaccel = 0.01d0
  fix_unconverged = .true.
  reinitialize = .true.

  npan = 160
  cvpar = 1.d0
  cterat = 0.15d0
  ctrrat = 0.2d0
  xsref1 = 1.d0
  xsref2 = 1.d0
  xpref1 = 1.d0
  xpref2 = 1.d0

! Read xfoil options

  rewind(iunit)
  read(iunit, iostat=iostat1, nml=xfoil_run_options)
  call namelist_check('xfoil_run_options', iostat1, 'warn', errval, errmsg)
  if (errval /= 0) return
  rewind(iunit)
  read(iunit, iostat=iostat1, nml=xfoil_paneling_options)
  call namelist_check('xfoil_paneling_options', iostat1, 'warn', errval, errmsg)
  if (errval /= 0) return

! Option to match seed airfoil to another instead of aerodynamic optimization

  match_foils = .false.
  matchfoil_file = 'none'
  read(iunit, iostat=iostat1, nml=matchfoil_options)
  call namelist_check('matchfoil_options', iostat1, 'warn', errval, errmsg)
  if (errval /= 0) return

! Close the input file

  close(iunit)

! Populate constraints_settings derived type

  call convert_char_to_c(seed_violation_handling, len(seed_violation_handling),&
                         constraints_settings%seed_violation_handling)
  constraints_settings%min_thickness = min_thickness
  constraints_settings%max_thickness = max_thickness
  constraints_settings%min_camber = min_camber
  constraints_settings%max_camber = max_camber
  constraints_settings%min_te_angle = min_te_angle
  constraints_settings%check_curvature = check_curvature
  constraints_settings%max_curv_reverse_top = max_curv_reverse_top
  constraints_settings%max_curv_reverse_bot = max_curv_reverse_bot
  constraints_settings%curv_threshold = curv_threshold
  constraints_settings%max_flap_degrees = max_flap_degrees
  constraints_settings%min_flap_degrees = min_flap_degrees
  do i = 1, max_op_points
    call convert_char_to_c(moment_constraint_type(i),                          &
                           len(moment_constraint_type(i)),                     &
                           constraints_settings%moment_constraint_type(:,i))
  end do
  constraints_settings%min_moment = min_moment

! Populate initialization_settings derived type

  initialization_settings%feasible_init = feasible_init
  initialization_settings%feasible_limit = feasible_limit
  initialization_settings%feasible_init_attempts = feasible_init_attempts

! Populate particle_swarm_settings derived type

  particle_swarm_settings%pso_pop = pso_pop
  particle_swarm_settings%pso_tol = pso_tol
  particle_swarm_settings%pso_maxit = pso_maxit
  call convert_char_to_c(pso_convergence_profile, len(pso_convergence_profile),&
                         particle_swarm_settings%pso_convergence_profile)
  
! Populate genetic_algorithm_settings derived type

  genetic_algorithm_settings%ga_pop = ga_pop
  genetic_algorithm_settings%ga_tol = ga_tol
  genetic_algorithm_settings%ga_maxit = ga_maxit
  call convert_char_to_c(parents_selection_method,                             &
                         len(parents_selection_method),                        &
                         genetic_algorithm_settings%parents_selection_method)
  genetic_algorithm_settings%parent_fraction = parent_fraction
  genetic_algorithm_settings%roulette_selection_pressure =                     &
                                                     roulette_selection_pressure
  genetic_algorithm_settings%tournament_fraction = tournament_fraction
  genetic_algorithm_settings%crossover_range_factor = crossover_range_factor
  genetic_algorithm_settings%mutant_probability = mutant_probability
  genetic_algorithm_settings%chromosome_mutation_rate = chromosome_mutation_rate
  genetic_algorithm_settings%mutation_range_factor = mutation_range_factor
  
! Populate simplex_settings derived type

  simplex_settings%simplex_tol = simplex_tol
  simplex_settings%simplex_maxit = simplex_maxit

! Populate xfoil_settings derived type

  xfoil_settings%ncrit = ncrit
  xfoil_settings%xtript = xtript
  xfoil_settings%xtripb = xtripb
  xfoil_settings%viscous_mode = viscous_mode
  xfoil_settings%silent_mode = silent_mode
  xfoil_settings%bl_maxit = bl_maxit
  xfoil_settings%vaccel = vaccel
  xfoil_settings%fix_unconverged = fix_unconverged
  xfoil_settings%reinitialize = reinitialize
  
! Populate xfoil_paneling_settings derived type

  xfoil_paneling_settings%npan = npan
  xfoil_paneling_settings%cvpar = cvpar
  xfoil_paneling_settings%cterat = cterat
  xfoil_paneling_settings%ctrrat = ctrrat
  xfoil_paneling_settings%xsref1 = xsref1
  xfoil_paneling_settings%xsref2 = xsref2
  xfoil_paneling_settings%xpref1 = xpref1
  xfoil_paneling_settings%xpref2 = xpref2

! Populate matchfoil_settings derived type

  matchfoil_settings%match_foils = match_foils
  call convert_char_to_c(matchfoil_file, len(matchfoil_file),                  &
                         matchfoil_settings%matchfoil_file)

! Echo namelist options for checking purposes

  write(*,*)
  write(*,*) 'Echoing program options:'
  write(*,*)

! Optimization options namelist

  write(*,'(A)') " &optimization_options"
  write(*,*) " search_type = '"//trim(search_type)//"'"
  write(*,*) " global_search = '"//trim(global_search)//"'"
  write(*,*) " local_search = '"//trim(local_search)//"'"
  write(*,*) " seed_airfoil = '"//trim(seed_airfoil)//"'"
  write(*,*) " airfoil_file = '"//trim(airfoil_file)//"'"
  write(*,*) " naca_digits = '"//trim(naca_digits)//"'"
  write(*,*) " shape_functions = '"//trim(shape_functions)//"'"
  write(*,*) " min_bump_width = ", min_bump_width
  write(*,*) " nfunctions_top = ", nfunctions_top
  write(*,*) " nfunctions_bot = ", nfunctions_bot
  write(*,*) " initial_perturb = ", initial_perturb
  write(*,*) " restart = ", restart
  write(*,*) " restart_write_freq = ", restart_write_freq
  write(*,*) " write_designs = ", write_designs
  write(*,'(A)') " /"
  write(*,*)

! Operating conditions namelist

  write(*,'(A)') " &operating_conditions"
  write(*,*) " noppoint = ", noppoint
  write(*,*) " use_flap = ", use_flap
  write(*,*) " x_flap = ", x_flap
  write(*,*) " y_flap = ", y_flap
  write(*,*)
  do i = 1, noppoint
    write(text,*) i
    text = adjustl(text)
    write(*,*) " optimization_type("//trim(text)//") = '"//                    &
               trim(optimization_type(i))//"'"
    write(*,*) " op_mode("//trim(text)//") = '"//trim(op_mode(i))//"'"
    write(*,*) " op_point("//trim(text)//") = ", op_point(i)
    write(*,'(A,es17.8)') "  reynolds("//trim(text)//") = ", reynolds(i)
    write(*,*) " mach("//trim(text)//") = ", mach(i)
    write(*,*) " flap_selection("//trim(text)//") = '"//                       &
               trim(flap_selection(i))//"'"
    write(*,*) " flap_degrees("//trim(text)//") = ", flap_degrees(i)
    write(*,*) " weighting("//trim(text)//") = ", weighting(i)
    if (i < noppoint) write(*,*)
  end do
  write(*,'(A)') " /"
  write(*,*)

! Constraints namelist

  write(*,'(A)') " &constraints"
  write(*,*) " min_thickness = ", min_thickness
  write(*,*) " max_thickness = ", max_thickness
  do i = 1, noppoint
    write(text,*) i
    text = adjustl(text)
    write(*,*) " moment_constraint_type("//trim(text)//") = "//                &
               trim(moment_constraint_type(i))
    write(*,*) " min_moment("//trim(text)//") = ", min_moment(i)
  end do
  write(*,*) " min_te_angle = ", min_te_angle
  write(*,*) " check_curvature = ", check_curvature
  write(*,*) " max_curv_reverse_top = ", max_curv_reverse_top
  write(*,*) " max_curv_reverse_bot = ", max_curv_reverse_bot
  write(*,*) " curv_threshold = ", curv_threshold
  write(*,*) " symmetrical = ", symmetrical
  write(*,*) " min_flap_degrees = ", min_flap_degrees
  write(*,*) " max_flap_degrees = ", max_flap_degrees
  write(*,*) " min_camber = ", min_camber
  write(*,*) " max_camber = ", max_camber
  write(*,'(A)') " /"
  write(*,*)

! Initialization namelist

  write(*,'(A)') " &initialization"
  write(*,*) " feasible_init = ", feasible_init
  write(*,*) " feasible_limit = ", feasible_limit
  write(*,*) " feasible_init_attempts = ", feasible_init_attempts
  write(*,'(A)') " /"
  write(*,*)

! Optimizer namelists

  if (trim(search_type) == 'global_and_local' .or. trim(search_type) ==        &
      'global') then

    if (trim(global_search) == 'particle_swarm') then

!     Particle swarm namelist

      write(*,'(A)') " &particle_swarm_options"
      write(*,*) " pso_pop = ", pso_pop
      write(*,*) " pso_tol = ", pso_tol
      write(*,*) " pso_maxit = ", pso_maxit
      write(*,*) " pso_convergence_profile = ", pso_convergence_profile
      write(*,'(A)') " /"
      write(*,*)

    else if (trim(global_search) == 'genetic_algorithm') then

!     Genetic algorithm options

      write(*,'(A)') " &genetic_algorithm_options"
      write(*,*) " ga_pop = ", ga_pop
      write(*,*) " ga_tol = ", ga_tol
      write(*,*) " ga_maxit = ", ga_maxit
      write(*,*) " parents_selection_method = ", parents_selection_method
      write(*,*) " parent_fraction = ", parent_fraction
      write(*,*) " roulette_selection_pressure = ", roulette_selection_pressure
      write(*,*) " tournament_fraction = " , tournament_fraction
      write(*,*) " crossover_range_factor = ", crossover_range_factor
      write(*,*) " mutant_probability = ", mutant_probability
      write(*,*) " chromosome_mutation_rate = ", chromosome_mutation_rate
      write(*,*) " mutation_range_factor = ", mutation_range_factor
      write(*,'(A)') " /"
      write(*,*)

    end if

  end if

  if (trim(search_type) == 'global_and_local' .or. trim(search_type) ==        &
      'local') then

    if(trim(local_search) == 'simplex') then

!     Simplex search namelist

      write(*,'(A)') " &simplex_options"
      write(*,*) " simplex_tol = ", simplex_tol
      write(*,*) " simplex_maxit = ", simplex_maxit
      write(*,'(A)') " /"
      write(*,*)

    end if

  end if

! Xfoil run options namelist

  write(*,'(A)') " &xfoil_run_options"
  write(*,*) " ncrit = ", ncrit
  write(*,*) " xtript = ", xtript
  write(*,*) " xtripb = ", xtripb
  write(*,*) " viscous_mode = ", viscous_mode
  write(*,*) " silent_mode = ", silent_mode
  write(*,*) " bl_maxit = ", bl_maxit
  write(*,*) " vaccel = ", vaccel
  write(*,*) " fix_unconverged = ", fix_unconverged
  write(*,*) " reinitialize = ", reinitialize
  write(*,'(A)') " /"
  write(*,*)

! Xfoil paneling options namelist

  write(*,'(A)') " &xfoil_paneling_options"
  write(*,*) " npan = ", npan
  write(*,*) " cvpar = ", cvpar
  write(*,*) " cterat = ", cterat
  write(*,*) " ctrrat = ", ctrrat
  write(*,*) " xsref1 = ", xsref1
  write(*,*) " xsref2 = ", xsref2
  write(*,*) " xpref1 = ", xpref1
  write(*,*) " xpref2 = ", xpref2
  write(*,'(A)') " /"
  write(*,*)

! Matchfoil options

  write(*,'(A)') " &matchfoil_options"
  write(*,*) " match_foils = ", match_foils
  write(*,*) " matchfoil_file = '"//trim(matchfoil_file)//"'"
  write(*,'(A)') " /"
  write(*,*)

! Constraints

  call check_constraints_settings(constraints_settings, noppoint,              &
                                  max_op_points, errval, errmsg)
  if (errval /= 0) return

! Initialization options
    
  call check_initialization_settings(initialization_settings, errval, errmsg)
  if (errval /= 0) return

! Optimizer options

  if (trim(search_type) == 'global' .or.                                       &
      trim(search_type) == 'global_and_local') then

    if (trim(global_search) == 'particle_swarm') then

!     Particle swarm options

      call check_particle_swarm_settings(particle_swarm_settings, errval,      &
                                         errmsg)
      if (errval /= 0) return

    else if (trim(global_search) == 'genetic_algorithm') then

!     Genetic algorithm options

      call check_genetic_algorithm_settings(genetic_algorithm_settings, errval,&
                                            errmsg)
      if (errval /= 0) return

    end if

  end if

  if (trim(search_type) == 'local' .or.                                        &
       trim(search_type) == 'global_and_local') then

!   Simplex options

    call check_simplex_settings(simplex_settings, errval, errmsg)
    if (errval /= 0) return
  
  end if

! XFoil run options

  call check_xfoil_settings(xfoil_settings, errval, errmsg)
  if (errval /= 0) return

! XFoil paneling options

  if (npan < 20) then
    errval = 1
    errmsg = "npan must be >= 20."
    return
  end if
  if (cvpar <= 0.d0) then
    errval = 1
    errmsg = "cvpar must be > 0."
    return
  end if
  if (cterat <= 0.d0) then
    errval = 1
    errmsg = "cterat must be > 0."
    return
  end if
  if (ctrrat <= 0.d0) then
    errval = 1
    errmsg = "ctrrat must be > 0."
    return
  end if
  if (xsref1 < 0.d0) then
    errval = 1
    errmsg = "xsref1 must be >= 0."
    return
  end if
  if (xsref2 < xsref1) then
    errval = 1
    errmsg = "xsref2 must be >= xsref1"
    return
  end if
  if (xsref2 > 1.d0) then
    errval = 1
    errmsg = "xsref2 must be <= 1."
    return
  end if
  if (xpref1 < 0.d0) then
    errval = 1
    errmsg = "xpref1 must be >= 0."
    return
  end if
  if (xpref2 < xpref1) then
    errval = 1
    errmsg = "xpref2 must be >= xpref1"
    return
  end if
  if (xpref2 > 1.d0) then
    errval = 1
    errmsg = "xpref2 must be <= 1."
    return
  end if

end subroutine read_inputs

!=============================================================================80
!
! Subroutine to read inputs from namelist file - for xfoil_only
!
!=============================================================================80
subroutine read_inputs_xfoil_only(input_file, max_op_points, airfoil_file,     &
                                  operating_points_settings, xfoil_settings,   &
                                  xfoil_paneling_settings, errval, errmsg)

  use iso_c_binding, only : C_BOOL
  use types, only : operating_points_settings_type, xfoil_settings_type,       &
                    xfoil_paneling_settings_type

  character(*), intent(in) :: input_file
  integer, intent(in) :: max_op_points
  character(80), intent(out) :: airfoil_file
  integer, intent(out) :: errval
  character(80), intent(out) :: errmsg

! Derived types to group settings

  type(operating_points_settings_type), intent(out) :: operating_points_settings
  type(xfoil_settings_type), intent(out) :: xfoil_settings
  type(xfoil_paneling_settings_type), intent(out) :: xfoil_paneling_settings

! Operating points

  integer :: noppoint
  logical(kind=C_BOOL) :: use_flap
  double precision :: x_flap, y_flap
  character(7), dimension(max_op_points) :: op_mode
  double precision, dimension(max_op_points) :: op_point, reynolds, mach
  double precision, dimension(max_op_points) :: flap_degrees

! Xfoil run options

  double precision :: ncrit, xtript, xtripb
  logical(kind=C_BOOL) :: viscous_mode, silent_mode
  integer :: bl_maxit
  double precision :: vaccel
  logical(kind=C_BOOL) :: fix_unconverged, reinitialize

! Xfoil paneling options

  integer :: npan
  double precision :: cvpar, cterat, ctrrat, xsref1, xsref2, xpref1, xpref2

  integer :: i, iunit, ioerr, iostat1
  character(30) :: text

  namelist /airfoil_to_load/ airfoil_file
  namelist /operating_conditions/ noppoint, op_mode, op_point, reynolds, mach, &
            use_flap, x_flap, y_flap, flap_degrees
  namelist /xfoil_run_options/ ncrit, xtript, xtripb, viscous_mode,            &
            silent_mode, bl_maxit, vaccel, fix_unconverged, reinitialize
  namelist /xfoil_paneling_options/ npan, cvpar, cterat, ctrrat, xsref1,       &
            xsref2, xpref1, xpref2

  errval = 0
  errmsg = ''

! Open input file

  iunit = 12
  open(unit=iunit, file=input_file, status='old', iostat=ioerr)
  if (ioerr /= 0) then
    errval = 1
    errmsg = 'could not find input file '//trim(input_file)//'.'
    return
  end if

! Read airfoil_to_load namelist options

  read(iunit, iostat=iostat1, nml=airfoil_to_load)
  call namelist_check('airfoil_to_load', iostat1, 'stop', errval, errmsg)
  if (errval /= 0) return

! Set defaults for operating conditions

  noppoint = 1
  use_flap = .false.
  x_flap = 0.75d0
  y_flap = 0.d0
  op_mode(:) = 'spec-cl'
  op_point(:) = 0.d0
  reynolds(:) = 1.0D+05
  mach(:) = 0.d0
  flap_degrees = 0.d0

! Read operating conditions

  read(iunit, iostat=iostat1, nml=operating_conditions)
  call namelist_check('operating_conditions', iostat1, 'stop', errval, errmsg)
  if (errval /= 0) return

! Set default xfoil aerodynamics and paneling options

  ncrit = 9.d0
  xtript = 1.d0
  xtripb = 1.d0
  viscous_mode = .true.
  silent_mode = .true.
  bl_maxit = 100
  vaccel = 0.01d0
  fix_unconverged = .true.
  reinitialize = .true.

  npan = 160
  cvpar = 1.d0
  cterat = 0.15d0
  ctrrat = 0.2d0
  xsref1 = 1.d0
  xsref2 = 1.d0
  xpref1 = 1.d0
  xpref2 = 1.d0

! Read xfoil options

  rewind(iunit)
  read(iunit, iostat=iostat1, nml=xfoil_run_options)
  call namelist_check('xfoil_run_options', iostat1, 'warn', errval, errmsg)
  if (errval /= 0) return
  rewind(iunit)
  read(iunit, iostat=iostat1, nml=xfoil_paneling_options)
  call namelist_check('xfoil_paneling_options', iostat1, 'warn', errval, errmsg)
  if (errval /= 0) return

! Close the input file

  close(iunit)

! Echo namelist options for checking purposes

  write(*,*)
  write(*,*) 'Echoing program options:'
  write(*,*)

! airfoil_to_load namelist

  write(*,'(A)') " &airfoil_to_load"
  write(*,*) " airfoil_file = '"//trim(airfoil_file)//"'"
  write(*,'(A)') " /"
  write(*,*)

! Operating conditions namelist

  write(*,'(A)') " &operating_conditions"
  write(*,*) " noppoint = ", noppoint
  write(*,*) " use_flap = ", use_flap
  write(*,*) " x_flap = ", x_flap
  write(*,*) " y_flap = ", y_flap
  write(*,*)
  do i = 1, noppoint
    write(text,*) i
    text = adjustl(text)
    write(*,*) " op_mode("//trim(text)//") = '"//trim(op_mode(i))//"'"
    write(*,*) " op_point("//trim(text)//") = ", op_point(i)
    write(*,'(A,es17.8)') "  reynolds("//trim(text)//") = ", reynolds(i)
    write(*,*) " mach("//trim(text)//") = ", mach(i)
    write(*,*) " flap_degrees("//trim(text)//") = ", flap_degrees(i)
    if (i < noppoint) write(*,*)
  end do
  write(*,'(A)') " /"
  write(*,*)

! Xfoil run options namelist

  write(*,'(A)') " &xfoil_run_options"
  write(*,*) " ncrit = ", ncrit
  write(*,*) " xtript = ", xtript
  write(*,*) " xtripb = ", xtripb
  write(*,*) " viscous_mode = ", viscous_mode
  write(*,*) " silent_mode = ", silent_mode
  write(*,*) " bl_maxit = ", bl_maxit
  write(*,*) " vaccel = ", vaccel
  write(*,*) " fix_unconverged = ", fix_unconverged
  write(*,*) " reinitialize = ", reinitialize
  write(*,'(A)') " /"
  write(*,*)

! Xfoil paneling options namelist

  write(*,'(A)') " &xfoil_paneling_options"
  write(*,*) " npan = ", npan
  write(*,*) " cvpar = ", cvpar
  write(*,*) " cterat = ", cterat
  write(*,*) " ctrrat = ", ctrrat
  write(*,*) " xsref1 = ", xsref1
  write(*,*) " xsref2 = ", xsref2
  write(*,*) " xpref1 = ", xpref1
  write(*,*) " xpref2 = ", xpref2
  write(*,'(A)') " /"
  write(*,*)

! Populate operating_points_settings derived type

  operating_points_settings%noppoint = noppoint
  operating_points_settings%use_flap = use_flap
  operating_points_settings%x_flap = x_flap
  operating_points_settings%y_flap = y_flap
  do i = 1, max_op_points
    call convert_char_to_c(op_mode(i), len(op_mode(i)),                        &
                           operating_points_settings%op_mode(:,i))
  end do
  operating_points_settings%op_point = op_point
  operating_points_settings%reynolds = reynolds
  operating_points_settings%mach = mach
  operating_points_settings%flap_degrees = flap_degrees

! Populate xfoil_settings derived type

  xfoil_settings%ncrit = ncrit
  xfoil_settings%xtript = xtript
  xfoil_settings%xtripb = xtripb
  xfoil_settings%viscous_mode = viscous_mode
  xfoil_settings%silent_mode = silent_mode
  xfoil_settings%bl_maxit = bl_maxit
  xfoil_settings%vaccel = vaccel
  xfoil_settings%fix_unconverged = fix_unconverged
  xfoil_settings%reinitialize = reinitialize
  
! Populate xfoil_paneling_settings derived type

  xfoil_paneling_settings%npan = npan
  xfoil_paneling_settings%cvpar = cvpar
  xfoil_paneling_settings%cterat = cterat
  xfoil_paneling_settings%ctrrat = ctrrat
  xfoil_paneling_settings%xsref1 = xsref1
  xfoil_paneling_settings%xsref2 = xsref2
  xfoil_paneling_settings%xpref1 = xpref1
  xfoil_paneling_settings%xpref2 = xpref2

end subroutine read_inputs_xfoil_only

!=============================================================================80
!
! Prints error and stops or warns for bad namelist read
!
!=============================================================================80
subroutine namelist_check(nmlname, errcode, action_missing_nml, errval, errmsg)

  character(*), intent(in) :: nmlname
  integer, intent(in) :: errcode
  character(*), intent(in) :: action_missing_nml
  integer, intent(out) :: errval
  character(80), intent(out) :: errmsg

  errval = 0
  errmsg = ''

  if (errcode < 0) then
    write(*,*)
    if (trim(action_missing_nml) == 'warn') then
      write(*,'(A)') 'Warning: namelist '//trim(nmlname)//&
                     ' not found in input file.'
      write(*,'(A)') 'Using default values.'
      write(*,*)
    else
      errval = 1
      errmsg = 'namelist '//trim(nmlname)//&
               ' is required and was not found in input file.'
      return
    end if
  else if (errcode > 0) then
    errval = 1
    errmsg = 'unrecognized variable in namelist '//trim(nmlname)//'.'
    return
  else
    continue
  end if

end subroutine namelist_check

!=============================================================================80
!
! Checks optimization_settings type for valid entries
!
!=============================================================================80
subroutine check_optimization_settings(optimization_settings, errval, errmsg)

  use types, only : optimization_settings_type
  use util,  only : convert_char_to_fortran

  type(optimization_settings_type), intent(in) :: optimization_settings
  integer, intent(out) :: errval
  character(80), intent(out) :: errmsg

  character(16) :: search_type
  character(10) :: seed_airfoil
  character(11) :: shape_functions

  errval = 0
  errmsg = ''

! Convert char arrays to fortran strings

  call convert_char_to_fortran(optimization_settings%search_type,              &
                               size(optimization_settings%search_type,1),      &
                               search_type)
  call convert_char_to_fortran(optimization_settings%seed_airfoil,             &
                               size(optimization_settings%seed_airfoil,1),     &
                               seed_airfoil)
  call convert_char_to_fortran(optimization_settings%shape_functions,          &
                               size(optimization_settings%shape_functions,1),  &
                               shape_functions)

! Error checking and setting search algorithm options

  if (trim(search_type) /= 'global_and_local' .and. trim(search_type) /=       &
      'global' .and. trim(search_type) /= 'local') then
    errval = 1
    errmsg = "search_type must be 'global_and_local', 'global', or 'local.'"
    return
  end if

  if (trim(seed_airfoil) /= 'from_file' .and.                                  &
      trim(seed_airfoil) /= 'four_digit') then
    errval = 1
    errmsg = "seed_airfoil must be 'from_file' or 'four_digit'."
    return
  end if
  if (trim(shape_functions) /= 'hicks-henne' .and.                             &
      trim(shape_functions) /= 'naca') then
    errval = 1
    errmsg = "shape_functions must be 'hicks-henne' or 'naca'."
    return
  end if
  if (optimization_settings%nfunctions_top < 1) then
    errval = 1
    errmsg = "nfunctions_top must be > 0."
    return
  end if
  if (optimization_settings%nfunctions_bot < 1) then
    errval = 1
    errmsg = "nfunctions_bot must be > 0."
    return
  end if
  if (optimization_settings%initial_perturb <= 0.d0) then
    errval = 1
    errmsg = "initial_perturb must be > 0."
    return
  end if
  if (optimization_settings%min_bump_width <= 0.d0) then
    errval = 1
    errmsg = "min_bump_width must be > 0."
    return
  end if

end subroutine check_optimization_settings

!=============================================================================80
!
! Checks operating_points_settings type for valid entries
!
!=============================================================================80
subroutine check_operating_points_settings(operating_points_settings,          &
                                           max_op_points, errval, errmsg)

  use types, only : operating_points_settings_type
  use util,  only : convert_char_to_fortran

  type(operating_points_settings_type), intent(in) :: operating_points_settings
  integer, intent(in) :: max_op_points
  integer, intent(out) :: errval
  character(80), intent(out) :: errmsg

  integer :: i
  character(7), dimension(max_op_points) :: op_mode
  character(9), dimension(max_op_points) :: optimization_type
  character(8), dimension(max_op_points) :: flap_selection

  errval = 0
  errmsg = ''

! Convert char arrays to fortran strings

  do i = 1, max_op_points
    call convert_char_to_fortran(operating_points_settings%op_mode(:,i),       &
                                 size(operating_points_settings%op_mode,1),    &
                                 op_mode(i))
    call convert_char_to_fortran(                                              &
                           operating_points_settings%optimization_type(:,i),   &
                           size(operating_points_settings%optimization_type,1),&
                           optimization_type(i))
    call convert_char_to_fortran(                                              &
                              operating_points_settings%flap_selection(:,i),   &
                              size(operating_points_settings%flap_selection,1),&
                              flap_selection(i))
  end do

  if (operating_points_settings%noppoint < 1) then
    errval = 1
    errmsg = "noppoint must be > 0."
    return
  end if
  if ((operating_points_settings%use_flap) .and.                               &
      (operating_points_settings%x_flap <= 0.0)) then
    errval = 1
    errmsg = "x_flap must be > 0."
    return
  end if
  if ((operating_points_settings%use_flap) .and.                               &
      (operating_points_settings%x_flap >= 1.0)) then
    errval = 1
    errmsg = "x_flap must be < 1."
    return
  end if

  do i = 1, operating_points_settings%noppoint
    if (trim(op_mode(i)) /= 'spec-cl' .and. trim(op_mode(i)) /= 'spec-al') then
      errval = 1
      errmsg = "op_mode must be 'spec-al' or 'spec-cl'."
      return
    end if
    if (operating_points_settings%reynolds(i) <= 0.d0) then
      errval = 1
      errmsg = "reynolds must be > 0."
      return
    end if
    if (operating_points_settings%mach(i) < 0.d0) then
      errval = 1
      errmsg = "mach must be >= 0."
      return
    end if
    if (trim(flap_selection(i)) /= 'specify' .and.                             &
        trim(flap_selection(i)) /= 'optimize') then
      errval = 1
      errmsg = "flap_selection must be 'specify' or 'optimize'."
      return
    end if
    if (operating_points_settings%flap_degrees(i) < -90.d0) then
      errval = 1
      errmsg = "flap_degrees must be > -90."
      return
    end if
    if (operating_points_settings%flap_degrees(i) > 90.d0) then
      errval = 1
      errmsg = "flap_degrees must be < 90."
      return
    end if
    if (operating_points_settings%weighting(i) <= 0.d0) then
      errval = 1
      errmsg = "weighting must be > 0."
      return
    end if
    if (trim(optimization_type(i)) /= 'min-drag' .and.                         &
        trim(optimization_type(i)) /= 'max-glide' .and.                        &
        trim(optimization_type(i)) /= 'min-sink' .and.                         &
        trim(optimization_type(i)) /= 'max-lift') then
      errval = 1 
      errmsg = "optimization_type must be 'min-drag', 'max-glide', "//&
               "min-sink', or 'max-lift'."
      return
    end if
  end do

end subroutine check_operating_points_settings

!=============================================================================80
!
! Checks constraints_settings type for valid entries
!
!=============================================================================80
subroutine check_constraints_settings(constraints_settings, noppoint,          &
                                      max_op_points, errval, errmsg)

  use types, only : constraints_settings_type
  use util,  only : convert_char_to_fortran

  type(constraints_settings_type), intent(in) :: constraints_settings
  integer, intent(in) :: noppoint, max_op_points
  integer, intent(out) :: errval
  character(80), intent(out) :: errmsg

  integer :: i
  character(4) :: seed_violation_handling
  character(8), dimension(max_op_points) :: moment_constraint_type

  errval = 0
  errmsg = ''

! Convert char arrays to fortran strings

  call convert_char_to_fortran(constraints_settings%seed_violation_handling,   &
                          size(constraints_settings%seed_violation_handling,1),&
                          seed_violation_handling)
  do i = 1, max_op_points
    call convert_char_to_fortran(                                              &
                           constraints_settings%moment_constraint_type(:,i),   &
                           size(constraints_settings%moment_constraint_type,1),&
                           moment_constraint_type(i))
  end do

  if (trim(seed_violation_handling) /= 'stop' .and.                            &
      trim(seed_violation_handling) /= 'warn') then
    errval = 1
    errmsg = "seed_violation_handling must be 'stop' or 'warn'."
    return
  end if
  if (constraints_settings%min_thickness <= 0.d0) then
    errval = 1
    errmsg = "min_thickness must be > 0."
    return
  end if
  if (constraints_settings%max_thickness <= 0.d0) then
    errval = 1
    errmsg = "max_thickness must be > 0."
    return
  end if
  do i = 1, noppoint
    if (trim(moment_constraint_type(i)) /= 'use_seed' .and.                    &
        trim(moment_constraint_type(i)) /= 'specify' .and.                     &
        trim(moment_constraint_type(i)) /= 'none')  then
      errval = 1
      errmsg = "moment_constraint_type must be 'use_seed', 'specify', "//&
               "or 'none'."
      return
    end if
  end do
  if (constraints_settings%min_te_angle <= 0.d0) then
    errval = 1
    errmsg = "min_te_angle must be > 0."
    return
  end if
  if (constraints_settings%max_curv_reverse_top < 0) then
    errval = 1
    errmsg = "max_curv_reverse_top must be >= 0."
    return
  end if
  if (constraints_settings%max_curv_reverse_bot < 0) then
    errval = 1
    errmsg = "max_curv_reverse_bot must be >= 0."
    return
  end if
  if (constraints_settings%curv_threshold <= 0.d0) then
    errval = 1
    errmsg = "curv_threshold must be > 0."
    return
  end if
  if (constraints_settings%symmetrical)                                        &
    write(*,*) "Mirroring top half of seed airfoil for symmetrical constraint."
  if (constraints_settings%min_flap_degrees >=                                 &
      constraints_settings%max_flap_degrees) then
    errval = 1
    errmsg = "min_flap_degrees must be less than max_flap_degrees."
    return
  end if
  if (constraints_settings%min_flap_degrees <= -90.d0) then
    errval = 1
    errmsg = "min_flap_degrees must be greater than -90."
    return
  end if
  if (constraints_settings%max_flap_degrees >= 90.d0) then
    errval = 1
    errmsg = "max_flap_degrees must be less than 90."
    return
  end if

end subroutine check_constraints_settings

!=============================================================================80
!
! Checks initialization_settings type for valid entries
!
!=============================================================================80
subroutine check_initialization_settings(initialization_settings, errval,      &
                                         errmsg)

  use types, only : initialization_settings_type

  type(initialization_settings_type), intent(in) :: initialization_settings
  integer, intent(out) :: errval
  character(80), intent(out) :: errmsg

  errval = 0
  errmsg = ''

  if ((initialization_settings%feasible_limit <= 0.d0) .and.                   &
      initialization_settings%feasible_init) then
    errval = 1
    errmsg = "feasible_limit must be > 0."
    return
  end if
  if ((initialization_settings%feasible_init_attempts < 1) .and.               &
      initialization_settings%feasible_init) then
    errval = 1
    errmsg = "feasible_init_attempts must be > 0."
    return
  end if

end subroutine check_initialization_settings

!=============================================================================80
!
! Checks particle_swarm_settings type for valid entries
!
!=============================================================================80
subroutine check_particle_swarm_settings(particle_swarm_settings, errval,      &
                                         errmsg)

  use types, only : particle_swarm_settings_type

  type(particle_swarm_settings_type), intent(in) :: particle_swarm_settings
  integer, intent(out) :: errval
  character(80), intent(out) :: errmsg

  character(10) :: pso_convergence_profile

  errval = 0
  errmsg = ''

! Convert char arrays to fortran strings

  call convert_char_to_fortran(particle_swarm_settings%pso_convergence_profile,&
                       size(particle_swarm_settings%pso_convergence_profile,1),&
                       pso_convergence_profile)

  if (particle_swarm_settings%pso_pop < 1) then
    errval = 1
    errmsg = "pso_pop must be > 0."
    return
  end if
  if (particle_swarm_settings%pso_tol <= 0.d0) then
    errval = 1
    errmsg = "pso_tol must be > 0."
    return
  end if
  if (particle_swarm_settings%pso_maxit < 1) then
    errval = 1
    errmsg = "pso_maxit must be > 0."
    return
  end if
  if ( (trim(pso_convergence_profile) /= "quick") .and.                    &
       (trim(pso_convergence_profile) /= "exhaustive") ) then
    errval = 1
    errmsg = "pso_convergence_profile must be 'exhaustive' "//&
             "or 'quick'."
    return
  end if

end subroutine check_particle_swarm_settings

!=============================================================================80
!
! Checks genetic_algorithm_settings type for valid entries
!
!=============================================================================80
subroutine check_genetic_algorithm_settings(genetic_algorithm_settings, errval,&
                                            errmsg)

  use types, only : genetic_algorithm_settings_type

  type(genetic_algorithm_settings_type), intent(in) ::                         &
                                                      genetic_algorithm_settings
  integer, intent(out) :: errval
  character(80), intent(out) :: errmsg

  character(10) :: parents_selection_method

  errval = 0
  errmsg = ''

! Convert char arrays to fortran strings

  call convert_char_to_fortran(                                                &
                   genetic_algorithm_settings%parents_selection_method,        &
                   size(genetic_algorithm_settings%parents_selection_method,1),&
                   parents_selection_method)

  if (genetic_algorithm_settings%ga_pop < 1) then
    errval = 1
    errmsg = "ga_pop must be > 0."
    return
  end if
  if (genetic_algorithm_settings%ga_tol <= 0.d0) then
    errval = 1
    errmsg = "ga_tol must be > 0."
    return
  end if
  if (genetic_algorithm_settings%ga_maxit < 1) then
    errval = 1
    errmsg = "ga_maxit must be > 0."
    return
  end if
  if ( (trim(parents_selection_method) /= "roulette") .and.                    &
       (trim(parents_selection_method) /= "tournament") .and.                  &
       (trim(parents_selection_method) /= "random") ) then
    errval = 1
    errmsg = "parents_selection_method must be 'roulette', "//&
             "'tournament', or 'random'."
    return
  end if
  if ( (genetic_algorithm_settings%parent_fraction <= 0.d0) .or.               &
       (genetic_algorithm_settings%parent_fraction > 1.d0) ) then
    errval = 1 
    errmsg = "parent_fraction must be > 0 and <= 1."
    return
  end if
  if (genetic_algorithm_settings%roulette_selection_pressure <= 0.d0) then 
    errval = 1
    errmsg = "roulette_selection_pressure must be > 0."
    return
  end if
  if ( (genetic_algorithm_settings%tournament_fraction <= 0.d0) .or.           &
       (genetic_algorithm_settings%tournament_fraction > 1.d0) ) then
    errval = 1
    errmsg = "tournament_fraction must be > 0 and <= 1."
    return
  end if
  if (genetic_algorithm_settings%crossover_range_factor < 0.d0) then 
    errval = 1
    errmsg = "crossover_range_factor must be >= 0."
    return
  end if
  if ( (genetic_algorithm_settings%mutant_probability < 0.d0) .or.             &
       (genetic_algorithm_settings%mutant_probability > 1.d0) ) then 
    errval = 1
    errmsg = "mutant_probability must be >= 0 and <= 1."
    return
  end if
  if (genetic_algorithm_settings%chromosome_mutation_rate < 0.d0) then 
    errval = 1
    errmsg = "chromosome_mutation_rate must be >= 0."
    return
  end if
  if (genetic_algorithm_settings%mutation_range_factor < 0.d0) then 
    errval = 1
    errmsg = "mutation_range_factor must be >= 0."
    return
  end if

end subroutine check_genetic_algorithm_settings

!=============================================================================80
!
! Checks simplex_settings type for valid entries
!
!=============================================================================80
subroutine check_simplex_settings(simplex_settings, errval, errmsg)

  use types, only : simplex_settings_type

  type(simplex_settings_type), intent(in) :: simplex_settings
  integer, intent(out) :: errval
  character(80), intent(out) :: errmsg

  errval = 0
  errmsg = ''

  if (simplex_settings%simplex_tol <= 0.d0) then
    errval = 1
    errmsg = "simplex_tol must be > 0."
    return
  end if
  if (simplex_settings%simplex_maxit < 1) then
    errval = 1
    errmsg = "simplex_maxit must be > 0."
    return
  end if

end subroutine check_simplex_settings

!=============================================================================80
!
! Checks xfoil_settings type for valid entries
!
!=============================================================================80
subroutine check_xfoil_settings(xfoil_settings, errval, errmsg)

  use types, only : xfoil_settings_type

  type(xfoil_settings_type), intent(in) :: xfoil_settings
  integer, intent(out) :: errval
  character(80), intent(out) :: errmsg

  errval = 0
  errmsg = ''

  if (xfoil_settings%ncrit < 0.d0) then
    errval = 1
    errmsg = "ncrit must be >= 0."
    return
  end if
  if (xfoil_settings%xtript < 0.d0 .or. xfoil_settings%xtript > 1.d0) then
    errval = 1 
    errmsg = "xtript must be >= 0. and <= 1."
    return
  end if
  if (xfoil_settings%xtripb < 0.d0 .or. xfoil_settings%xtripb > 1.d0) then
    errval = 1 
    errmsg = "xtripb must be >= 0. and <= 1."
    return
  end if
  if (xfoil_settings%bl_maxit < 1) then
    errval = 1
    errmsg = "bl_maxit must be > 0."
    return
  end if
  if (xfoil_settings%vaccel < 0.d0) then
    errval = 1
    errmsg = "vaccel must be >= 0."
    return
  end if 

end subroutine check_xfoil_settings

!=============================================================================80
!
! Checks xfoil_paneling_settings type for valid entries
!
!=============================================================================80
subroutine check_xfoil_paneling_settings(xfoil_paneling_settings, errval,      &
                                         errmsg)

  use types, only : xfoil_paneling_settings_type

  type(xfoil_paneling_settings_type), intent(in) :: xfoil_paneling_settings
  integer, intent(out) :: errval
  character(80), intent(out) :: errmsg

  errval = 0
  errmsg = ''

end subroutine check_xfoil_paneling_settings

!=============================================================================80
!
! Asks user to turn off pitching moment constraints
!
!=============================================================================80
function ask_moment_constraints()

  character :: ask_moment_constraints
  logical :: valid_choice

! Get user input

  valid_choice = .false.
  do while (.not. valid_choice)
  
    write(*,*)
    write(*,'(A)') 'Warning: pitching moment constraints not recommended for '
    write(*,'(A)', advance='no') 'symmetrical airfoil optimization. '//&
                                 'Turn them off now? (y/n): '
    read(*,'(A)') ask_moment_constraints

    if ( (ask_moment_constraints == 'y') .or.                                  &
         (ask_moment_constraints == 'Y') ) then
      valid_choice = .true.
      ask_moment_constraints = 'y'
      write(*,*)
      write(*,*) "Setting moment_constraint_type(:) = 'none'."
    else if ( (ask_moment_constraints == 'n') .or.                             &
         (ask_moment_constraints == 'N') ) then
      valid_choice = .true.
      ask_moment_constraints = 'n'
    else
      write(*,'(A)') 'Please enter y or n.'
      valid_choice = .false.
    end if

  end do

end function ask_moment_constraints

end module input_output
