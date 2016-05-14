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

module airfoil_evaluation

! Sets up and evaluates the objective function for an airfoil design

  use xfoil_driver, only : airfoil_type, xfoil_geom_options_type,              &
                           xfoil_options_type

  implicit none

  public  
  private :: aero_objective_function, matchfoil_objective_function, checktol,  &
             maxlift, mindrag, curr_foil

! Required optimization settings

! Operating points

  integer, parameter :: max_op_points = 30
  integer :: noppoint
  character(7), dimension(max_op_points) :: op_mode
  character(9), dimension(max_op_points) :: optimization_type
  double precision, dimension(max_op_points) :: op_point, reynolds, mach,      &
                                                flap_degrees, weighting 
  logical :: use_flap
  double precision :: x_flap, y_flap

!FIXME: this should go somewhere else
character(8), dimension(max_op_points) :: flap_selection

! Constraints

  logical :: check_curvature, symmetrical
  double precision, dimension(max_op_points) :: min_moment
  double precision :: min_thickness, max_thickness, min_te_angle,              &
                      curv_threshold, min_flap_degrees, max_flap_degrees
  integer :: max_curv_reverse
  character(8), dimension(max_op_points) :: moment_constraint_type

! Xfoil options

  type(xfoil_options_type) :: xfoil_options
  type(xfoil_geom_options_type) :: xfoil_geom_options

! Options for matching airfoils

  logical match_foils
  character(80) :: matchfoil_file

! Other variables needed by this module

  double precision, dimension(:), allocatable :: xseedt, xseedb, zseedt, zseedb
  double precision, dimension(:), allocatable :: xmatcht, xmatchb, zmatcht,    &
                                                 zmatchb
  type(airfoil_type) :: curr_foil
  double precision :: growth_allowed
  integer :: nflap_optimize          ! Number of operating points where flap 
                                     !   setting will be optimized
  double precision, dimension(max_op_points) :: scale_factor
  integer, dimension(max_op_points) :: flap_optimize_points

! Variables used to check that XFoil results are repeatable when needed

  double precision :: checktol = 0.2d0
  double precision, dimension(max_op_points) :: maxlift = -100.d0
  double precision, dimension(max_op_points) :: mindrag = 100.d0

!$omp threadprivate(curr_foil)

  contains

!=============================================================================80
!
! Allocates memory for airfoil optimization
!
!=============================================================================80
subroutine allocate_airfoil_data(nfunctions_top, nfunctions_bot)

  use xfoil_driver,       only : xfoil_init
  use parametrization,    only : shape_functions, create_shape_functions
  use airfoil_operations, only : allocate_airfoil

  integer, intent(in) :: nfunctions_top, nfunctions_bot

  double precision, dimension(:), allocatable :: modest, modesb

! Allocate shape function setup arrays

  if (trim(shape_functions) == 'naca') then
    allocate(modest(nfunctions_top))
    allocate(modesb(nfunctions_bot))
  else
    allocate(modest(nfunctions_top*3))
    allocate(modesb(nfunctions_bot*3))
  end if
  modest(:) = 0.d0
  modesb(:) = 0.d0

! Allocate private memory for airfoil optimization on each thread

!$omp parallel default(shared)

! For NACA, this will create the shape functions.  For Hicks-Henne,
! it will just allocate them.

  call create_shape_functions(xseedt, xseedb, modest, modesb, shape_functions, &
                              first_time=.true.)

! Allocate memory for working airfoil on each thread

  curr_foil%npoint = size(xseedt,1) + size(xseedb,1) - 1
  call allocate_airfoil(curr_foil)

! Allocate memory for xfoil

  call xfoil_init()

!$omp end parallel

! Deallocate shape function setup arrays

  deallocate(modest)
  deallocate(modesb)

end subroutine allocate_airfoil_data

!=============================================================================80
!
! Frees memory used during airfoil optimization
!
!=============================================================================80
subroutine deallocate_airfoil_data()

  use parametrization,    only : deallocate_shape_functions
  use airfoil_operations, only : deallocate_airfoil
  use xfoil_driver,       only : xfoil_cleanup

!$omp parallel default(shared)

  call deallocate_shape_functions()
  call deallocate_airfoil(curr_foil)
  call xfoil_cleanup()

!$omp end parallel

end subroutine deallocate_airfoil_data

!=============================================================================80
!
! Generic objective function.  Selects either aero_objective_function or
! matchfoil_objective_function depending on whether match_foils = .true. or
! not.
!
!=============================================================================80
function objective_function(designvars)

  double precision, dimension(:), intent(in) :: designvars
  double precision :: objective_function

  if (match_foils) then
    objective_function = matchfoil_objective_function(designvars)
  else
    objective_function = aero_objective_function(designvars)
  end if

end function objective_function

!=============================================================================80
!
! Objective function with option to not add penalty value (used for seed
! airfoil)
!
!=============================================================================80
function objective_function_nopenalty(designvars)

  double precision, dimension(:), intent(in) :: designvars
  double precision :: objective_function_nopenalty

  if (match_foils) then
    objective_function_nopenalty = matchfoil_objective_function(designvars)
  else
    objective_function_nopenalty =                                             &
                    aero_objective_function(designvars, include_penalty=.false.)
  end if

end function objective_function_nopenalty

!=============================================================================80
!
!  Objective function
!
!  Input: design variables (modes for top and bottom shape functions)
!         include_penalty: optional input to enable/disable penalty function
!  Output: objective function value based on airfoil performance
!
!=============================================================================80
function aero_objective_function(designvars, include_penalty)

  use math_deps,       only : interp_vector, curvature
  use parametrization, only : initial_perturb, shape_functions,                &
                              top_shape_function, bot_shape_function,          &
                              create_airfoil
  use xfoil_driver,    only : run_xfoil
  use xfoil_inc,       only : AMAX

!FIXME: settings is being removed
use settings

  double precision, dimension(:), intent(in) :: designvars
  logical, intent(in), optional :: include_penalty
  double precision :: aero_objective_function

  double precision, dimension(max(size(xseedt,1),size(xseedb,1))) :: x_interp, &
                                                 zt_interp, zb_interp, thickness
  double precision, dimension(size(xseedt,1)) :: zt_new
  double precision, dimension(size(xseedb,1)) :: zb_new
  double precision, dimension(size(xseedt,1)+size(xseedb,1)-1) :: curv
  integer :: nmodest, nmodesb, nptt, nptb, i, dvtbnd1, dvtbnd2, dvbbnd1,       &
             dvbbnd2, ncheckpt, nptint
  double precision :: penaltyval
  double precision :: tegap, growth1, growth2, maxgrowth, len1, len2
  double precision :: panang1, panang2, maxpanang, heightfactor
  integer, dimension(noppoint) :: checkpt_list
  character(7), dimension(noppoint) :: opm_check
  double precision, dimension(noppoint) :: opp_check, re_check, ma_check 
  double precision, dimension(noppoint) :: fd_check
  double precision, dimension(noppoint) :: lift, drag, moment, viscrms
  double precision, dimension(noppoint) :: clcheck, cdcheck, cmcheck, rmscheck
  double precision, dimension(noppoint) :: actual_flap_degrees
  logical, dimension(noppoint) :: checkpt
  double precision :: increment, curv1, curv2
  integer :: nreversals, ndvs
  double precision :: gapallow, maxthick, ffact
  integer :: check_idx, flap_idx, dvcounter
  double precision, parameter :: eps = 1.0D-08
  logical :: penalize

! Needed when calling interp_vector, but any errors are ignored here.

  integer :: errval
  character(80) :: errmsg

  nmodest = size(top_shape_function,1)
  nmodesb = size(bot_shape_function,1)
  nptt = size(xseedt,1)
  nptb = size(xseedb,1)

! Enable / disable penalty function

  penalize = .true.
  if (present(include_penalty)) then
    if (.not. include_penalty) penalize = .false.
  end if

! Set modes for top and bottom surfaces

  dvtbnd1 = 1
  if (trim(shape_functions) == 'naca') then
    dvtbnd2 = nmodest
    dvbbnd2 = nmodest + nmodesb
  else
    dvtbnd2 = nmodest*3
    dvbbnd2 = nmodest*3 + nmodesb*3
  end if
  dvbbnd1 = dvtbnd2 + 1

! Overwrite lower DVs for symmetrical airfoils (they are not used)

  if (symmetrical) then
    dvbbnd1 = 1
    dvbbnd2 = dvtbnd2
  end if

! Create top and bottom surfaces by perturbation of seed airfoil

  call create_airfoil(xseedt, zseedt, xseedb, zseedb,                          &
                      designvars(dvtbnd1:dvtbnd2), designvars(dvbbnd1:dvbbnd2),&
                      zt_new, zb_new, shape_functions, symmetrical)

! Format coordinates in a single loop in derived type

  do i = 1, nptt
    curr_foil%x(i) = xseedt(nptt-i+1)
    curr_foil%z(i) = zt_new(nptt-i+1)
  end do
  do i = 1, nptb-1
    curr_foil%x(i+nptt) = xseedb(i+1)
    curr_foil%z(i+nptt) = zb_new(i+1)
  end do

! Check geometry before running Xfoil: growth rates, LE and TE angles, etc.

  penaltyval = 0.d0
  maxgrowth = 0.d0

  len1 = sqrt((curr_foil%x(2)-curr_foil%x(1))**2.d0 +                          &
              (curr_foil%z(2)-curr_foil%z(1))**2.d0)
  do i = 2, nptt + nptb - 2
    len2 = sqrt((curr_foil%x(i+1)-curr_foil%x(i))**2.d0 +                      &
                (curr_foil%z(i+1)-curr_foil%z(i))**2.d0)
    growth1 = len2/len1
    growth2 = len1/len2
    if (max(growth1,growth2) > maxgrowth) maxgrowth = max(growth1,growth2)
    len1 = len2
  end do

! Penalty for too large growth rate

  penaltyval = penaltyval + max(0.d0,maxgrowth-growth_allowed)/1.d0

! Penalty for too blunt leading edge

  panang1 = atan((zt_new(2)-zt_new(1))/(xseedt(2)-xseedt(1))) *                &
            180.d0/acos(-1.d0)
  panang2 = atan((zb_new(1)-zb_new(2))/(xseedb(2)-xseedb(1))) *                &
            180.d0/acos(-1.d0)
  maxpanang = max(panang2,panang1)
  penaltyval = penaltyval + max(0.d0,maxpanang-89.99d0)/0.01d0

! Penalty for too sharp leading edge

  penaltyval = penaltyval + max(0.d0,abs(panang1-panang2)-20.d0)/5.d0

! Interpolate bottom surface to xseedt points (to check thickness)

  if (xseedt(nptt) <= xseedb(nptb)) then
    nptint = nptt
    call interp_vector(xseedb, zb_new, xseedt, zb_interp(1:nptt), errval,      &
                       errmsg)
    x_interp(1:nptt) = xseedt
    zt_interp(1:nptt) = zt_new  
  else
    nptint = nptb
    call interp_vector(xseedt, zt_new, xseedb, zt_interp(1:nptb), errval,      &
                       errmsg)
    x_interp(1:nptb) = xseedb
    zb_interp(1:nptb) = zb_new
  end if

! Compute thickness parameters

  tegap = zt_new(nptt) - zb_new(nptb)
  maxthick = 0.d0
  heightfactor = tan(min_te_angle*acos(-1.d0)/180.d0/2.d0)

  do i = 2, nptint - 1

!   Thickness array and max thickness

    thickness(i) = zt_interp(i) - zb_interp(i)
    if (thickness(i) > maxthick) maxthick = thickness(i)

!   Check if thinner than specified wedge angle on back half of airfoil

    if (xseedt(i) > 0.5d0) then
      gapallow = tegap + 2.d0 * heightfactor * (x_interp(nptint) -             &
                                                x_interp(i))
      if (thickness(i) < gapallow)                                             &
        penaltyval = penaltyval + (gapallow - thickness(i))/0.001d0
    end if

  end do

! Penalties for max thickness too low or high

  penaltyval = penaltyval + max(0.d0,min_thickness-maxthick)/0.1d0
  penaltyval = penaltyval + max(0.d0,maxthick-max_thickness)/0.1d0

! Check for curvature reversals

  if (check_curvature) then

!   Compute curvature

    curv = curvature(curr_foil%npoint, curr_foil%x, curr_foil%z)

!   Check number of reversals that exceed the threshold

    nreversals = 0
    curv1 = 0.d0

    do i = 2, nptt + nptb - 2

      if (abs(curv(i)) >= curv_threshold) then
        curv2 = curv(i)
        if (curv2*curv1 < 0.d0) nreversals = nreversals + 1
        curv1 = curv2
      end if

    end do

    penaltyval = penaltyval + max(0.d0,dble(nreversals-max_curv_reverse))

  end if

! Check that number of flap optimize points are correct

  ndvs = size(designvars,1)
  if (nflap_optimize /= (ndvs - dvbbnd2)) then
    write(*,*) "Wrong number of design variables for flap deflections."
    write(*,*) "Please report this bug."
    stop
  end if

! Get actual flap angles based on design variables
! Also add a penalty for flap deflections outside the specified bounds

  ffact = initial_perturb/(max_flap_degrees - min_flap_degrees)
  actual_flap_degrees(1:noppoint) = flap_degrees(1:noppoint)
  dvcounter = dvbbnd2 + 1
  do i = 1, nflap_optimize
    flap_idx = flap_optimize_points(i)
    actual_flap_degrees(flap_idx) = designvars(dvcounter)/ffact
    penaltyval = penaltyval +                                                  &
                 max(0.d0,actual_flap_degrees(flap_idx)-max_flap_degrees)
    penaltyval = penaltyval +                                                  &
                 max(0.d0,min_flap_degrees-actual_flap_degrees(flap_idx))
    dvcounter = dvcounter + 1
  end do

! Exit if geometry and flap angles don't check out

  if ( (penaltyval > eps) .and. penalize ) then
    aero_objective_function = penaltyval*1.0D+06
    return
  end if

! Analyze airfoil at requested operating conditions with Xfoil

  call run_xfoil(curr_foil, xfoil_geom_options, op_point(1:noppoint),          &
                 op_mode(1:noppoint), reynolds(1:noppoint), mach(1:noppoint),  &
                 use_flap, x_flap, y_flap, actual_flap_degrees(1:noppoint),    &
                 xfoil_options, lift, drag, moment, viscrms)

! Determine if points need to be checked for xfoil consistency

  ncheckpt = 0
  checkpt_list(:) = 0
  checkpt(:) = .false.
  do i = 1, noppoint

!   Don't check the very first design

    if (maxlift(1) == -100.d0) exit

    if ((lift(i) > (1.d0 + checktol)*maxlift(i)) .or.                          &
        (drag(i) < (1.d0 - checktol)*mindrag(i))) then

      checkpt(i) = .true.
      ncheckpt = ncheckpt + 1
      checkpt_list(i) = ncheckpt
      opm_check(ncheckpt) = op_mode(i)
      opp_check(ncheckpt) = op_point(i)
      ma_check(ncheckpt) = mach(i)
      fd_check(ncheckpt) = actual_flap_degrees(i)

!     Perturb Reynolds number slightly to check that XFoil result is 
!     repeatable

      re_check(ncheckpt) = 0.997d0*reynolds(i)
 
    end if

  end do

! Analyze airfoil at perturbed operating points to check for repeatability

  anychecked: if (ncheckpt > 0) then

    call run_xfoil(curr_foil, xfoil_geom_options, opp_check(1:ncheckpt),       &
                   opm_check(1:ncheckpt), re_check(1:ncheckpt),                &
                   ma_check(1:ncheckpt), use_flap, x_flap, y_flap,             &
                   fd_check(1:ncheckpt), xfoil_options, clcheck, cdcheck,      &
                   cmcheck, rmscheck)

!   Keep the more conservative of the two runs

    do i = 1, noppoint

      ischecked: if (checkpt(i)) then

        check_idx = checkpt_list(i)

        checklift: if (clcheck(check_idx) < lift(i)) then
          lift(i) = clcheck(check_idx)
        end if checklift

        checkdrag: if (cdcheck(check_idx) > drag(i)) then
          drag(i) = cdcheck(check_idx)
        end if checkdrag

        checkmoment: if (cmcheck(check_idx) < moment(i)) then
          moment(i) = cmcheck(check_idx)
        end if checkmoment

        checkrms: if (rmscheck(check_idx) > viscrms(i)) then
          viscrms(i) = rmscheck(check_idx)
        end if checkrms

      end if ischecked

    end do

  end if anychecked

! Get objective function contribution from aerodynamics (aero performance
! times normalized weight)

  aero_objective_function = 0.d0

  do i = 1, noppoint

!   Extra checks for really bad designs

    if (viscrms(i) >= 1.d0) then
      lift(i) = -0.1d0
      drag(i) = 1000.d0
      moment(i) = -10.d0
    end if

!   Objective function evaluation

    if (trim(optimization_type(i)) == 'min-sink') then

!     Maximize Cl^1.5/Cd

      if (lift(i) > 0.d0) then
        increment = drag(i)/lift(i)**1.5d0*scale_factor(i)
      else
        increment = 1.D9   ! Big penalty for lift <= 0
      end if

    elseif (trim(optimization_type(i)) == 'max-glide') then

!     Maximize Cl/Cd

      if (lift(i) > 0.d0) then
        increment = drag(i)/lift(i)*scale_factor(i)
      else
        increment = 1.D9   ! Big penalty for lift <= 0
      end if

    elseif (trim(optimization_type(i)) == 'min-drag') then

!     Minimize Cd

      increment = drag(i)*scale_factor(i)

    elseif (trim(optimization_type(i)) == 'max-lift') then

!     Maximize Cl (at given angle of attack)

      if (lift(i) > 0.d0) then
        increment = scale_factor(i)/lift(i)
      else
        increment = 1.D9   ! Big penalty for lift <= 0
      end if

    else

      write(*,*)
      write(*,*) "Error: requested optimization_type not recognized."
      stop

    end if

!   Add contribution to the objective function

    aero_objective_function = aero_objective_function + weighting(i)*increment

  end do

! Add penalty for unconverged points

  do i = 1, noppoint
    penaltyval = penaltyval + max(0.d0,viscrms(i)-1.0D-04)/1.0D-04
  end do

! Add penalty for too low moment

  do i = 1, noppoint
    if (trim(moment_constraint_type(i)) /= 'none') then
      penaltyval = penaltyval + max(0.d0,min_moment(i)-moment(i))/0.1d0
    end if
  end do

! Add penalty for too large panel angles

  maxpanang = AMAX
  penaltyval = penaltyval + max(0.0d0,maxpanang-25.d0)/5.d0

! Add all penalties to objective function, and make them very large

  if (penalize) aero_objective_function =                                      &
                aero_objective_function + penaltyval*1.0D+06

! Update maxlift and mindrag only if it is a good design

  if (penaltyval <= eps) then
    do i = 1, noppoint
!$omp critical
      if (lift(i) > maxlift(i)) maxlift(i) = lift(i)
      if (drag(i) < mindrag(i)) mindrag(i) = drag(i)
!$omp end critical
    end do
  end if

!Bug check
!if (aero_objective_function < 0.5) then
!  print *, "penaltyval: ", penaltyval
!  do i = 1, noppoint
!    print *, drag(i), checkpt(i)
!  end do
!end if

end function aero_objective_function

!=============================================================================80
!
! Objective function for matching one airfoil to another (for testing shape
! functions, optimization algorithms, etc.).  Assumes x-values of points line
! up; this should be handled before optimizing.
!
!=============================================================================80
function matchfoil_objective_function(designvars)

  use parametrization, only : shape_functions, top_shape_function,             &
                              bot_shape_function, create_airfoil
  use math_deps,       only : norm_2

  double precision, dimension(:), intent(in) :: designvars
  double precision :: matchfoil_objective_function

  double precision, dimension(size(xseedt,1)) :: zt_new
  double precision, dimension(size(xseedb,1)) :: zb_new
  integer :: nmodest, nmodesb, nptt, nptb, dvtbnd, dvbbnd

  nmodest = size(top_shape_function,1)
  nmodesb = size(bot_shape_function,1)
  nptt = size(xseedt,1)
  nptb = size(xseedb,1)

! Set modes for top and bottom surfaces

  if (trim(shape_functions) == 'naca') then
    dvtbnd = nmodest
    dvbbnd = nmodest + nmodesb
  else
    dvtbnd = nmodest*3
    dvbbnd = nmodest*3 + nmodesb*3
  end if

! Create top and bottom surfaces by perturbation of seed airfoil

  call create_airfoil(xseedt, zseedt, xseedb, zseedb, designvars(1:dvtbnd),    &
                      designvars(dvtbnd+1:dvbbnd), zt_new, zb_new,             &
                      shape_functions, .false.)

! Evaluate the new airfoil, not counting fixed LE and TE points

  matchfoil_objective_function = norm_2(zt_new(2:nptt-1) - zmatcht(2:nptt-1))
  matchfoil_objective_function = matchfoil_objective_function +                &
                                 norm_2(zb_new(2:nptb-1) - zmatchb(2:nptb-1))

end function matchfoil_objective_function

!=============================================================================80
!
! Generic function to write designs. Selects either 
! write_airfoil_optimization_progress or write_matchfoil_optimization_progress
! depending on whether match_foils = .true. or not.
!
!=============================================================================80
function write_function(designvars, designcounter)

  double precision, dimension(:), intent(in) :: designvars
  integer, intent(in) :: designcounter
  integer :: write_function

  if (match_foils) then
    write_function = write_matchfoil_optimization_progress(designvars,         &
                                                           designcounter)
  else
    write_function = write_airfoil_optimization_progress(designvars,           &
                                                         designcounter)
  end if

end function write_function

!=============================================================================80
!
! Writes airfoil coordinates and polars to files during optimization
!
!=============================================================================80
function write_airfoil_optimization_progress(designvars, designcounter)

  use math_deps,       only : interp_vector 
  use parametrization, only : initial_perturb, shape_functions,                &
                              top_shape_function, bot_shape_function,          &
                              create_airfoil
  use xfoil_driver,    only : run_xfoil

!FIXME: settings is being removed
use settings, only : output_prefix

  double precision, dimension(:), intent(in) :: designvars
  integer, intent(in) :: designcounter
  integer :: write_airfoil_optimization_progress

  double precision, dimension(size(xseedt,1)) :: zt_new
  double precision, dimension(size(xseedb,1)) :: zb_new
  integer :: nmodest, nmodesb, nptt, nptb, i, dvtbnd1, dvtbnd2, dvbbnd1,       &
             dvbbnd2 
  double precision, dimension(noppoint) :: lift, drag, moment, viscrms
  double precision, dimension(noppoint) :: actual_flap_degrees
  double precision :: ffact
  integer :: ndvs, flap_idx, dvcounter
 
  character(100) :: foilfile, polarfile, text
  integer :: foilunit, polarunit

  nmodest = size(top_shape_function,1)
  nmodesb = size(bot_shape_function,1)
  nptt = size(xseedt,1)
  nptb = size(xseedb,1)

! Set modes for top and bottom surfaces

  dvtbnd1 = 1
  if (trim(shape_functions) == 'naca') then
    dvtbnd2 = nmodest
    dvbbnd2 = nmodest + nmodesb
  else
    dvtbnd2 = nmodest*3
    dvbbnd2 = nmodest*3 + nmodesb*3
  end if
  dvbbnd1 = dvtbnd2 + 1

! Overwrite lower DVs for symmetrical airfoils (they are not used)

  if (symmetrical) then
    dvbbnd1 = 1
    dvbbnd2 = dvtbnd2
  end if

! Create top and bottom surfaces by perturbation of seed airfoil

  call create_airfoil(xseedt, zseedt, xseedb, zseedb,                          &
                      designvars(dvtbnd1:dvtbnd2), designvars(dvbbnd1:dvbbnd2),&
                      zt_new, zb_new, shape_functions, symmetrical)

! Format coordinates in a single loop in derived type

  do i = 1, nptt
    curr_foil%x(i) = xseedt(nptt-i+1)
    curr_foil%z(i) = zt_new(nptt-i+1)
  end do
  do i = 1, nptb-1
    curr_foil%x(i+nptt) = xseedb(i+1)
    curr_foil%z(i+nptt) = zb_new(i+1)
  end do

! Check that number of flap optimize points are correct

  ndvs = size(designvars,1)
  if (nflap_optimize /= (ndvs - dvbbnd2)) then
    write(*,*) "Wrong number of design variables for flap deflections."
    write(*,*) "Please report this bug."
    stop
  end if

! Get actual flap angles based on design variables

  ffact = initial_perturb/(max_flap_degrees - min_flap_degrees)
  actual_flap_degrees(1:noppoint) = flap_degrees(1:noppoint)
  dvcounter = dvbbnd2 + 1
  do i = 1, nflap_optimize
    flap_idx = flap_optimize_points(i)
    actual_flap_degrees(flap_idx) = designvars(dvcounter)/ffact
    dvcounter = dvcounter + 1
  end do

! Analyze airfoil at requested operating conditions with Xfoil

  call run_xfoil(curr_foil, xfoil_geom_options, op_point(1:noppoint),          &
                 op_mode(1:noppoint), reynolds(1:noppoint), mach(1:noppoint),  &
                 use_flap, x_flap, y_flap, actual_flap_degrees(1:noppoint),    &
                 xfoil_options, lift, drag, moment, viscrms)

! Set output file names and identifiers

  foilfile = trim(output_prefix)//'_design_coordinates.dat'
  polarfile = trim(output_prefix)//'_design_polars.dat'

  foilunit = 13
  polarunit = 14

! Open files and write headers, if necessary

  if (designcounter == 0) then

!   Header for coordinate file

    write(*,*) "Writing coordinates for seed airfoil to file "//               &
               trim(foilfile)//" ..."
    open(unit=foilunit, file=foilfile, status='replace')
    write(foilunit,'(A)') 'title="Airfoil coordinates"'
    write(foilunit,'(A)') 'variables="x" "z"'
    write(foilunit,'(A)') 'zone t="Seed airfoil"'

!   Header for polar file

    write(*,*) "Writing polars for seed airfoil to file "//                    &
               trim(polarfile)//" ..."
    open(unit=polarunit, file=polarfile, status='replace')
    write(polarunit,'(A)') 'title="Airfoil polars"'
    write(polarunit,'(A)') 'variables="cl" "cd"'
    write(polarunit,'(A)') 'zone t="Seed airfoil polar"'

  else

!   Format design counter as string

    write(text,*) designcounter
    text = adjustl(text)

!   Open coordinate file and write zone header

    write(*,*) "  Writing coordinates for design number "//trim(text)//        &
               " to file "//trim(foilfile)//" ..."
    open(unit=foilunit, file=foilfile, status='old', position='append',        &
         err=900)
    write(foilunit,'(A)') 'zone t="Airfoil", SOLUTIONTIME='//trim(text)

    ! Open polar file and write zone header
    
    write(*,*) "  Writing polars for design number "//trim(text)//             &
               " to file "//trim(polarfile)//" ..."
    open(unit=polarunit, file=polarfile, status='old', position='append',      &
         err=901)
    write(polarunit,'(A)') 'zone t="Polars", SOLUTIONTIME='//trim(text)

  end if

! Write coordinates to file

  do i = 1, nptt + nptb - 1
    write(foilunit,'(2es17.8)') curr_foil%x(i), curr_foil%z(i)
  end do

! Write polars to file

  do i = 1, noppoint
    write(polarunit,'(2es17.8)') lift(i), drag(i)
  end do

! Close output files

  close(foilunit)
  close(polarunit)

! Set return value (needed for compiler)

  write_airfoil_optimization_progress = 0
  return

! Warning if there was an error opening design_coordinates file

900 write(*,*) "Warning: unable to open "//trim(foilfile)//". Skipping ..."
  write_airfoil_optimization_progress = 1
  return

! Warning if there was an error opening design_coordinates file

901 write(*,*) "Warning: unable to open "//trim(polarfile)//". Skipping ..."
  write_airfoil_optimization_progress = 2
  return

end function write_airfoil_optimization_progress

!=============================================================================80
!
! Writes airfoil coordinates to foil during optimization to match one airfoil
! to another.
!
!=============================================================================80
function write_matchfoil_optimization_progress(designvars, designcounter)

  use parametrization, only : shape_functions, top_shape_function,             &
                              bot_shape_function, create_airfoil

!FIXME: settings is being removed
use settings, only : output_prefix

  double precision, dimension(:), intent(in) :: designvars
  integer, intent(in) :: designcounter
  integer :: write_matchfoil_optimization_progress

  double precision, dimension(size(xseedt,1)) :: zt_new
  double precision, dimension(size(xseedb,1)) :: zb_new
  integer :: i, nmodest, nmodesb, nptt, nptb, dvtbnd, dvbbnd

  character(100) :: foilfile, text
  integer :: foilunit

  nmodest = size(top_shape_function,1)
  nmodesb = size(bot_shape_function,1)
  nptt = size(xseedt,1)
  nptb = size(xseedb,1)

! Set modes for top and bottom surfaces

  if (trim(shape_functions) == 'naca') then
    dvtbnd = nmodest
    dvbbnd = nmodest + nmodesb
  else
    dvtbnd = nmodest*3
    dvbbnd = nmodest*3 + nmodesb*3
  end if

! Create top and bottom surfaces by perturbation of seed airfoil

  call create_airfoil(xseedt, zseedt, xseedb, zseedb, designvars(1:dvtbnd),    &
                      designvars(dvtbnd+1:dvbbnd), zt_new, zb_new,             &
                      shape_functions, .false.)

! Format coordinates in a single loop in derived type

  do i = 1, nptt
    curr_foil%x(i) = xseedt(nptt-i+1)
    curr_foil%z(i) = zt_new(nptt-i+1)
  end do
  do i = 1, nptb-1
    curr_foil%x(i+nptt) = xseedb(i+1)
    curr_foil%z(i+nptt) = zb_new(i+1)
  end do

! Set output file names and identifiers

  foilfile = trim(output_prefix)//'_design_coordinates.dat'
  foilunit = 13

! Open file and write header, if necessary

  if (designcounter == 0) then

!   Header for coordinate file

    write(*,*) "Writing coordinates for seed airfoil to file "//               &
               trim(foilfile)//" ..."
    open(unit=foilunit, file=foilfile, status='replace')
    write(foilunit,'(A)') 'title="Airfoil coordinates"'
    write(foilunit,'(A)') 'variables="x" "z"'
    write(foilunit,'(A)') 'zone t="Seed airfoil"'

  else

!   Format designcounter as string

    write(text,*) designcounter
    text = adjustl(text)

!   Open coordinate file and write zone header

    write(*,*) "  Writing coordinates for design number "//trim(text)//        &
               " to file "//trim(foilfile)//" ..."
    open(unit=foilunit, file=foilfile, status='old', position='append',        &
         err=910)
    write(foilunit,'(A)') 'zone t="Airfoil", SOLUTIONTIME='//trim(text)

  end if

! Write coordinates to file

  do i = 1, nptt + nptb - 1
    write(foilunit,'(2es17.8)') curr_foil%x(i), curr_foil%z(i)
  end do

! Close output file

  close(foilunit)

! Set return value (needed for compiler)

  write_matchfoil_optimization_progress = 0
  return

! Warning if there was an error opening design_coordinates file

910 write(*,*) "Warning: unable to open "//trim(foilfile)//". Skipping ..."
  write_matchfoil_optimization_progress = 1
  return

end function write_matchfoil_optimization_progress

!=============================================================================80
!
! Cleans up unused designs written prior to a restart
!
!=============================================================================80
function write_function_restart_cleanup(restart_status, global_search,         &
                                        local_search)

!FIXME: settings is being removed
use settings, only : output_prefix

  character(*), intent(in) :: restart_status, global_search, local_search
  integer :: write_function_restart_cleanup

  integer :: restunit, ioerr, step, designcounter, foilunit, polarunit, ncoord
  integer :: i, j
  double precision, dimension(:,:), allocatable :: x, z, lift, drag
  character(100) :: restfile, foilfile, polarfile, text

! Print status

  write(*,*) 'Cleaning up unused designs written after restart save ...'

  restunit = 12
  foilunit = 13
  polarunit = 14

! Read last written design from restart file

  if (trim(restart_status) == 'global_optimization') then
    if (trim(global_search) == 'particle_swarm') then
      restfile = 'restart_pso_'//trim(output_prefix)
    else if (trim(global_search) == 'genetic_algorithm') then
      restfile = 'restart_ga_'//trim(output_prefix)
    end if
  else
    if (trim(local_search) == 'simplex') then
      restfile = 'restart_simplex_'//trim(output_prefix)
    end if
  end if

  open(unit=restunit, file=restfile, status='old', form='unformatted',         &
       iostat=ioerr)
  if (ioerr /= 0) then
    write_function_restart_cleanup = 1
    return
  end if
  read(restunit) step
  read(restunit) designcounter
  close(restunit)

! Allocate size of data arrays

  ncoord = size(xseedt,1) + size(xseedb,1) - 1
  allocate(x(ncoord,designcounter+1))
  allocate(z(ncoord,designcounter+1))
  allocate(lift(noppoint,designcounter+1))
  allocate(drag(noppoint,designcounter+1))

! Open coordinates file

  foilfile = trim(output_prefix)//'_design_coordinates.dat'
  open(unit=foilunit, file=foilfile, status='old', iostat=ioerr)
  if (ioerr /= 0) then
    write_function_restart_cleanup = 2
    return
  end if

! Skip file header

  read(foilunit,*)
  read(foilunit,*)

! Read coordinates for each airfoil

  do i = 1, designcounter + 1
  
!   Skip zone header

    read(foilunit,*)

!   Read coordinates

    do j = 1, ncoord
      read(foilunit,'(2es17.8)') x(j,i), z(j,i)
    end do

  end do

! Close coordinates file

  close(foilunit)

! Re-write coordinates file without the unused designs

  open(unit=foilunit, file=foilfile, status='replace')
  write(foilunit,'(A)') 'title="Airfoil coordinates"'
  write(foilunit,'(A)') 'variables="x" "z"'
  do i = 0, designcounter
!   Write zone header

    if (i == 0) then
      write(foilunit,'(A)') 'zone t="Seed airfoil"'
    else
      write(text,*) i
      text = adjustl(text)
      write(foilunit,'(A)') 'zone t="Airfoil", SOLUTIONTIME='//trim(text)
    end if

!   Write coordinates

    do j = 1, ncoord
      write(foilunit,'(2es17.8)') x(j,i+1), z(j,i+1)
    end do
  end do

! Close coordinates file

  close(foilunit)

! Return now if we're matching airfoils (no aero data)

  if (match_foils) then
    deallocate(x)
    deallocate(z)
    deallocate(lift)
    deallocate(drag)
    write(*,*) 'Finished cleaning up unused designs.'
    write_function_restart_cleanup = 0
    return
  end if

! Open polars file

  polarfile = trim(output_prefix)//'_design_polars.dat'
  open(unit=polarunit, file=polarfile, status='old', iostat=ioerr)
  if (ioerr /= 0) then
    write_function_restart_cleanup = 3
    return
  end if

! Skip file header

  read(polarunit,*)
  read(polarunit,*)

! Read polars for each airfoil

  do i = 1, designcounter + 1
  
!   Skip zone header

    read(polarunit,*)

!   Read polars

    do j = 1, noppoint
      read(polarunit,'(2es17.8)') lift(j,i), drag(j,i)
    end do

  end do

! Close polars file

  close(polarunit)

! Re-write polars file without the unused designs

  open(unit=polarunit, file=polarfile, status='replace')
  write(polarunit,'(A)') 'title="Airfoil polars"'
  write(polarunit,'(A)') 'variables="cl" "cd"'
  do i = 0, designcounter
!   Write zone header

    if (i == 0) then
      write(polarunit,'(A)') 'zone t="Seed airfoil polar"'
    else
      write(text,*) i
      text = adjustl(text)
      write(polarunit,'(A)') 'zone t="Polars", SOLUTIONTIME='//trim(text)
    end if

!   Write polars

    do j = 1, noppoint
      write(polarunit,'(2es17.8)') lift(j,i+1), drag(j,i+1)
    end do
  end do

! Close polars file

  close(polarunit)

! Deallocate data arrays

  deallocate(x)
  deallocate(z)
  deallocate(lift)
  deallocate(drag)

! Print status

  write(*,*) 'Finished cleaning up unused designs.'
  write(*,*)

  write_function_restart_cleanup = 0

end function write_function_restart_cleanup

end module airfoil_evaluation
