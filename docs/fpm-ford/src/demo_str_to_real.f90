  program demo_str_to_real
   use, intrinsic :: iso_fortran_env, only: real64
   use M_unicode__float, only : str_to_real
   implicit none

   character(len=16)  :: inputs(4) = [&
   & "  -42.75 ", &
   & "+1.23e4  ", &
   & "bad_input", &
   & "3.14D-2  "]
   real(real64)       :: values(4), value
   integer            :: errors(4), error
   integer            :: k
   character(len=80)  :: edge_cases(7) = [ character(len=80) :: &
   "1.79e308             Valid: just under real64 max (err: 0)      ",  &
   "1.80e308             Overflow: just over real64 max (err: 6)    ",  &
   "1e999999999999       Overflow: massive exponent string (err: 6) ",  &
   "-1.80e308            -Overflow: and just over real64 max (err: 6)", &
   "0.0e5000             Valid: zero stays zero (err: 0)            ",  &
   "5.0e-324             Valid: subnormal underflow limit (err: 0)  ",  &
   "1.0e-400             Valid: graceful underflow to 0.0 (err: 0)  " ]
   integer            :: iostat
   character(len=256) :: iomsg

   ! Process the entire array in a single elemental call
   call str_to_real(inputs, values, errors)

   do k = 1, size(inputs)
      print '(A, " -> val: ", G0, ", err: ", I0)', &
      & trim(inputs(k)), values(k), errors(k)
   enddo

   do k = 1, size(edge_cases)
      print *
      call str_to_real(edge_cases(k)(:20), value, error)
      print '(A, " -> val: ", G0, ", err: ", I0, 1x, a)', &
      & trim(edge_cases(k)(:20)), value, error
      print '("Expected:",A)', edge_cases(k)(21:)
      iomsg = '----------------'
      read(edge_cases(k),'(e20.0)',iostat=iostat,iomsg=iomsg)value
      print *, 'from internal read',value,iostat,trim(iomsg)
   enddo
  end program demo_str_to_real
