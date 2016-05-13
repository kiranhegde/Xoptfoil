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

module input_sanity

  implicit none

  contains

!=============================================================================80
!
! Checks that the seed airfoil passes all constraints, sets scale factors for
! objective functions at each operating point, and optionally sets the
! minimum allowable pitching moment.
!
!=============================================================================80
subroutine check_seed(xoffset, zoffset, foilscale, errval, errmsg)

  use math_deps,          only : interp_vector, curvature
  use xfoil_driver,       only : airfoil_type, run_xfoil
  use xfoil_inc,          only : AMAX
  use airfoil_operations, only : allocate_airfoil, deallocate_airfoil

!FIXME: use only variables that are needed
use airfoil_evaluation
use settings

  double precision, intent(in) :: xoffset, zoffset, foilscale
  integer, intent(out) :: errval
  character(80), intent(out) :: errmsg

  type(airfoil_type) :: check_foil
  double precision, dimension(:), allocatable :: x_interp, thickness
  double precision, dimension(:), allocatable :: zt_interp, zb_interp
  double precision, dimension(size(xseedt,1)+size(xseedb,1)-1) :: curv
  double precision :: penaltyval, tegap, gapallow, maxthick, heightfactor
  double precision :: panang1, panang2, maxpanang, curv1, curv2
  double precision :: checkval, len1, len2, growth1, growth2, xtrans, ztrans
  double precision, dimension(noppoint) :: lift, drag, moment, viscrms
  integer :: i, nptt, nptb, nreversals, nptint 
  character(30) :: text, text2
  character(4) :: stoptype

  errval = 0
  errmsg = ''

  stoptype = seed_violation_handling
  penaltyval = 0.d0
  nptt = size(xseedt,1)
  nptb = size(xseedb,1)

  write(*,*) 'Checking to make sure seed airfoil passes all constraints ...'
  write(*,*)

! Allocate memory for seed airfoil

  check_foil%npoint = nptt + nptb - 1
  call allocate_airfoil(check_foil)

! Get allowable panel growth rate

  growth_allowed = 0.d0

! Top surface growth rates

  len1 = sqrt((xseedt(2)-xseedt(1))**2.d0 + (zseedt(2)-zseedt(1))**2.d0)
  do i = 2, nptt - 1
    len2 = sqrt((xseedt(i+1)-xseedt(i))**2.d0 + (zseedt(i+1)-zseedt(i))**2.d0)
    growth1 = len2/len1
    growth2 = len1/len2
    if (max(growth1,growth2) > growth_allowed)                                 &
        growth_allowed = 1.5d0*max(growth1,growth2)
    len1 = len2
  end do

! Bottom surface growth rates

  len1 = sqrt((xseedb(2)-xseedb(1))**2.d0 + (zseedb(2)-zseedb(1))**2.d0)
  do i = 2, nptb - 1
    len2 = sqrt((xseedb(i+1)-xseedb(i))**2.d0 + (zseedb(i+1)-zseedb(i))**2.d0)
    growth1 = len2/len1
    growth2 = len1/len2
    if (max(growth1,growth2) > growth_allowed)                               &
        growth_allowed = 1.5d0*max(growth1,growth2)
    len1 = len2
  end do

! Format coordinates in a single loop in derived type

  do i = 1, nptt
    check_foil%x(i) = xseedt(nptt-i+1)
    check_foil%z(i) = zseedt(nptt-i+1)
  end do
  do i = 1, nptb-1
    check_foil%x(i+nptt) = xseedb(i+1)
    check_foil%z(i+nptt) = zseedb(i+1)
  end do

! Too blunt or sharp leading edge

  panang1 = atan((zseedt(2)-zseedt(1))/(xseedt(2)-xseedt(1))) *                &
            180.d0/acos(-1.d0)
  panang2 = atan((zseedb(1)-zseedb(2))/(xseedb(2)-xseedb(1))) *                &
            180.d0/acos(-1.d0)
  maxpanang = max(panang2,panang1)
  if (maxpanang > 89.99d0) then
    write(text,'(F8.4)') maxpanang
    text = adjustl(text)
    write(*,*) "LE panel angle: "//trim(text)//" degrees"
    if (stoptype == 'stop') then
      errval = 1
      errmsg = "seed airfoil's leading edge is too blunt."
      return
    else
      call print_warning("seed airfoil's leading edge is too blunt.")
    end if
  end if
  if (abs(panang1 - panang2) > 20.d0) then
    write(text,'(F8.4)') abs(panang1 - panang2)
    text = adjustl(text)
    write(*,*) "LE panel angle: "//trim(text)//" degrees"
    if (stoptype == 'stop') then
      errval = 1
      errmsg = "seed airfoil's leading edge is too sharp."
      return
    else
      call print_warning("seed airfoil's leading edge is too sharp.")
    end if
  end if

! Interpolate either bottom surface to top surface x locations or vice versa
! to determine thickness

  if (xseedt(nptt) <= xseedb(nptb)) then
    allocate(x_interp(nptt))
    allocate(zt_interp(nptt))
    allocate(zb_interp(nptt))
    allocate(thickness(nptt))
    nptint = nptt
    call interp_vector(xseedb, zseedb, xseedt, zb_interp, errval, errmsg)
    if (errval /= 0) return
    x_interp = xseedt
    zt_interp = zseedt
  else
    allocate(x_interp(nptb))
    allocate(zt_interp(nptb))
    allocate(zb_interp(nptb))
    allocate(thickness(nptb))
    nptint = nptb
    call interp_vector(xseedt, zseedt, xseedb, zt_interp, errval, errmsg)
    if (errval /= 0) return
    x_interp = xseedb
    zb_interp = zseedb
  end if

! Compute thickness parameters

  tegap = zseedt(nptt) - zseedb(nptb)
  maxthick = 0.d0
  heightfactor = tan(min_te_angle*acos(-1.d0)/180.d0/2.d0)

  do i = 2, nptint - 1

!   Thickness array and max thickness
    
    thickness(i) = zt_interp(i) - zb_interp(i)
    if (thickness(i) > maxthick) maxthick = thickness(i)

!   Check if thinner than specified wedge angle on back half of airfoil
    
    if (x_interp(i) > 0.5d0) then
      gapallow = tegap + 2.d0 * heightfactor * (x_interp(nptint) -             &
                                                x_interp(i))
      if (thickness(i) < gapallow) then
        xtrans = x_interp(i)/foilscale - xoffset
        write(text,'(F8.4)') xtrans
        text = adjustl(text)
        write(*,*) "Detected too thin at x = "//trim(text)
        penaltyval = penaltyval + (gapallow - thickness(i))/0.001d0
      end if
    end if

  end do

! Free memory

  deallocate(x_interp)
  deallocate(zt_interp)
  deallocate(zb_interp)
  deallocate(thickness)

! Too thin on back half

  if (penaltyval > 0.d0) then
    if (stoptype == 'stop') then
      errval = 1
      errmsg = "seed airfoil is thinner than min_te_angle near the "//&
               "trailing edge."
      return
    else
      call print_warning("seed airfoil is thinner than min_te_angle near "//&
                         "the trailing edge.")
    end if
  end if
  penaltyval = 0.d0

! Max thickness too low

  if (maxthick < min_thickness) then
    write(text,'(F8.4)') maxthick
    text = adjustl(text)
    write(*,*) "Thickness: "//trim(text)
    if (stoptype == 'stop') then
      errval = 1
      errmsg = "seed airfoil violates min_thickness constraint."
      return
    else
      call print_warning("seed airfoil violates min_thickness constraint.")
    end if
  end if

! Max thickness too high

  if (maxthick > max_thickness) then
    write(text,'(F8.4)') maxthick
    text = adjustl(text)
    write(*,*) "Thickness: "//trim(text)
    if (stoptype == 'stop') then
      errval = 1
      errmsg = "seed airfoil violates max_thickness constraint."
      return
    else
      call print_warning("seed airfoil violates max_thickness constraint.")
    end if
  end if

! Check for curvature reversals

  if (check_curvature) then

!   Compute curvature

    curv = curvature(check_foil%npoint, check_foil%x, check_foil%z)

!   Check number of reversals that exceed the threshold

    nreversals = 0
    curv1 = 0.d0

    do i = 2, nptt + nptb - 2

      if (abs(curv(i)) >= curv_threshold) then
        curv2 = curv(i)
        if (curv2*curv1 < 0.d0) then
          xtrans = check_foil%x(i)/foilscale - xoffset
          write(text,'(F8.4)') xtrans
          text = adjustl(text)
          ztrans = check_foil%z(i)/foilscale - zoffset
          write(text2,'(F8.4)') ztrans
          text2 = adjustl(text2)
          write(*,*) "Curvature reversal detected near (x, z) = ("//&
                         trim(text)//", "//trim(text2)//")"
          write(text,'(F8.4)') curv(i)
          text = adjustl(text)
          write(*,*) "Curvature: "//trim(text)
          nreversals = nreversals + 1
        end if
        curv1 = curv2
      end if

    end do

    if (nreversals > max_curv_reverse) then
      if (stoptype == 'stop') then
        errval = 1                                         
        errmsg = "seed airfoil violates max_curv_reverse constraint."
        return
      else
        call print_warning("seed airfoil violates max_curv_reverse constraint.")
      end if
    end if
  end if

! Check for bad combinations of operating conditions and optimization types

  do i = 1, noppoint
    write(text,*) i
    text = adjustl(text)

    if ((op_point(i) == 0.d0) .and. (op_mode(i) == 'spec-cl') .and.            &
        (trim(optimization_type(i)) /= 'min-drag')) then
      errval = 1
      errmsg = "must use 'min-drag' optimization for Cl = 0 (op. point "//&
               trim(text)//")."
      return
    elseif ((op_mode(i) == 'spec-cl') .and.                                    &
            (trim(optimization_type(i)) == 'max-lift')) then
      errval = 1
      errmsg = "cannot use 'max-lift' optimization for specified Cl (op. "//&
               "point "//trim(text)//")."
      return
    end if

  end do

! Analyze airfoil at requested operating conditions with Xfoil

  call run_xfoil(check_foil, xfoil_geom_options, op_point(1:noppoint),         &
                 op_mode(1:noppoint), reynolds(1:noppoint), mach(1:noppoint),  &
                 use_flap, x_flap, y_flap, flap_degrees(1:noppoint),           &
                 xfoil_options, lift, drag, moment, viscrms)

! Check for unconverged points

  do i = 1, noppoint
    if (viscrms(i) > 1.0D-04) then
      write(text,*) i
      text = adjustl(text)
      if (stoptype == 'stop') then
        errval = 1
        errmsg = "Xfoil did not converge for operating point "//&
                 trim(text)//"."
        return
      else
        call print_warning("Xfoil did not converge for operating point "//&
                           trim(text)//".")
      end if
    end if
  end do

! Set moment constraint or check for violation of specified constraint

  do i = 1, noppoint
    if (trim(moment_constraint_type(i)) == 'use_seed') then
      min_moment(i) = moment(i)
    elseif (trim(moment_constraint_type(i)) == 'specify') then
      if (moment(i) < min_moment(i)) then
        write(text,'(F8.4)') moment(i)
        text = adjustl(text)
        write(*,*) "Moment: "//trim(text)
        write(text,*) i
        text = adjustl(text)
        if (stoptype == 'stop') then
          errval = 1
          errmsg = "seed airfoil violates min_moment constraint for "//&
                   "op. point "//trim(text)//"."
          return
        else
          call print_warning("seed airfoil violates min_moment constraint "//&
                             "for op. point "//trim(text)//".")
        end if
      end if
    end if
  end do

! Evaluate objectives to establish scale factors for each point

  do i = 1, noppoint
    if (trim(optimization_type(i)) == 'min-sink') then
      if (lift(i) > 0.d0) then
        checkval = drag(i)/lift(i)**1.5d0
      else
        write(text,*) i
        text = adjustl(text)
        errval = 1
        errmsg = "cannot use min-sink optimization for Cl <= 0 (op. point "//&
                 trim(text)//")."
        return
      end if
    elseif (trim(optimization_type(i)) == 'max-glide') then
      checkval = drag(i)/lift(i)
    elseif (trim(optimization_type(i)) == 'min-drag') then
      checkval = drag(i)
    elseif (trim(optimization_type(i)) == 'max-lift') then
      checkval = 1.d0/lift(i)
    else
      errval = 1
      errmsg = "requested optimization_type for op. point "//trim(text)//&
               " not recognized."  
      return
    end if
    scale_factor(i) = 1.d0/checkval
  end do

! Penalty for too large panel angles

  maxpanang = AMAX
  if (maxpanang > 25.d0) then
    write(text,'(F8.4)') maxpanang
    text = adjustl(text)
    write(*,*) "Max panel angle: "//trim(text)
    if (stoptype == 'stop') then
      errval = 1
      errmsg = "seed airfoil panel angles are too large. Adjust "//&
               "xfoil_paneling_options."
      return
    else
      call print_warning("seed airfoil panel angles are too large. Adjust "//&
                         "xfoil_paneling_options.")
    end if
  end if

! Deallocate memory for seed airfoil

  call deallocate_airfoil(check_foil)

end subroutine check_seed

!=============================================================================80
!
! Prints warning
!
!=============================================================================80
subroutine print_warning(message)

  character(*), intent(in) :: message
  
  write(*,*)
  write(*,'(A)') 'Warning: '//trim(message)
  write(*,*)

end subroutine print_warning

end module input_sanity
