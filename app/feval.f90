! Here is an interactive REPL / calculator program that reads from stdin,
! supports defining scalar and array variables on the fly, computes symbolic
! derivatives, and provides a help() command—followed by a clear guide
! on passing arrays to and from the module in a larger Fortran codebase.

! Interactive Calculator & REPL 
!
!    Supported Interactive Commands
!
!    help() — Displays full usage instructions, operators, functions, and examples.
!    entities() - shows HTML entity names for operators
!    vars() — Lists all currently defined variables and their scalar/array values.
!
!    name = expr — Evaluates expr and stores the resulting scalar
!    or array in variable name (supports Unicode names like α = 2.5 or x =
!    linspace(1, 5, 5)).
!
!    diff(expr, var) — Symbolically differentiates expr with respect
!    to var and evaluates the derivative at the current variable values.
!
!    expr — Evaluates any expression and prints the scalar or array result.
!
!    quit / exit — Exits the calculator.

program calc_repl
   use, intrinsic :: iso_fortran_env, only: int32, real64, iostat_end
   use M_unicode, only : ctou, utoc
   use M_unicode, only : cp_string, var_value, user_func, user_func_iface, compiled_expr ! Public Types
   use M_unicode, only : compile_expr_cp, eval_compiled, differentiate_compiled ! Public Procedures
   implicit none

   integer, parameter :: MAX_VARS = 100
   type(cp_string) :: var_names(MAX_VARS)
   type(var_value) :: var_vals(MAX_VARS)
   type(user_func) :: custom_fns(2)
   integer :: num_vars = 0

   character(len=1024) :: line
   character(len=:), allocatable :: trimmed, lhs, rhs, expr_part, var_part
   integer :: ios, eq_pos, comma_pos, idx, err
   type(compiled_expr) :: prog, dprog
   real(real64), allocatable :: res(:)

   ! Register built-in REPL helper functions:
   ! 1. seq(start, step, count) -> generates an array [start, start+step, ...]
   !    Note: Since user_func returns a scalar, we register scalar helpers
   !    and handle array constructors [a, b, c] directly in the REPL parser.
   custom_fns(1)%name%cp = utoc("deg2rad")
   custom_fns(1)%nargs   = 1
   custom_fns(1)%fn      => fn_deg2rad

   custom_fns(2)%name%cp = utoc("rad2deg")
   custom_fns(2)%nargs   = 1
   custom_fns(2)%fn      => fn_rad2deg

   print *, "================================================================================"
   print *, " Unicode Fortran Expression Calculator & REPL. Type help() for instructions,    "
   print *, " entities() for Unicode help,  vars() to list variables, or quit.               "
   print *, "================================================================================"

   do
      write(*, '(A)', advance='no') "calc> "
      read(*, '(A)', iostat=ios) line
      if (ios == iostat_end) then
         print *
         exit
      else if (ios /= 0) then
         print *, "Input read error."
         cycle
      end if

      trimmed = trim(adjustl(line))
      if (len(trimmed) == 0) cycle

      if (trimmed == "quit" .or. trimmed == "exit") exit

      if (trimmed == "help()" .or. trimmed == "help") then
         call print_help()
         cycle
      end if
      if (trimmed == "entities()" .or. trimmed == "entities") then
         call print_entities()
         cycle
      end if

      if (trimmed == "vars()" .or. trimmed == "vars") then
         call print_vars(var_names, var_vals, num_vars)
         cycle
      end if

      ! Check for symbolic differentiation command: diff(expr, var)
      if (len(trimmed) > 6) then
         if (trimmed(1:5) == "diff(" .and. trimmed(len(trimmed):len(trimmed)) == ")") then
            comma_pos = index(trimmed, ",", back=.true.)
            if (comma_pos > 6) then
               expr_part = trim(adjustl(trimmed(6:comma_pos-1)))
               var_part  = trim(adjustl(trimmed(comma_pos+1:len(trimmed)-1)))
               idx = find_var(utoc(var_part), var_names, num_vars)
               if (idx == 0) then
                  print *, "Error: Variable '", var_part, "' is not defined."
                  cycle
               end if
               call compile_expr_cp(utoc(expr_part), prog, err, &
                  var_names(1:num_vars), custom_fns)
               if (err /= 0) then
                  call print_error(err)
                  cycle
               end if
               call differentiate_compiled(prog, idx, dprog, err)
               if (err /= 0) then
                  call print_error(err)
                  cycle
               end if
               call eval_compiled(dprog, res, err, var_vals(1:num_vars))
               if (err /= 0) then
                  call print_error(err)
               else
                  call print_result("d/d(" // var_part // ")", res)
               end if
               cycle
            end if
         end if
      end if

      ! Check for variable assignment: name = expr (ignoring '==', '<=', '>=', '/=')
      eq_pos = find_assignment_eq(trimmed)
      if (eq_pos > 0) then
         lhs = trim(adjustl(trimmed(1:eq_pos-1)))
         rhs = trim(adjustl(trimmed(eq_pos+1:)))

         if (len(lhs) == 0 .or. len(rhs) == 0) then
            print *, "Syntax error in assignment."
            cycle
         end if

         ! Check if RHS is an array literal: [1.0, 2.0, 3.0]
         if (rhs(1:1) == "[" .and. rhs(len(rhs):len(rhs)) == "]") then
            call parse_array_literal(rhs(2:len(rhs)-1), res, err)
            if (err /= 0) then
               print *, "Error parsing array literal."
               cycle
            end if
         else
            call compile_expr_cp(utoc(rhs), prog, err, &
               var_names(1:num_vars), custom_fns)
            if (err /= 0) then
               call print_error(err)
               cycle
            end if
            call eval_compiled(prog, res, err, var_vals(1:num_vars))
            if (err /= 0) then
               call print_error(err)
               cycle
            end if
         end if

         call set_var(utoc(lhs), res, var_names, var_vals, num_vars)
         call print_result(lhs, res)
         cycle
      end if

      ! Otherwise evaluate expression directly
      if (trimmed(1:1) == "[" .and. trimmed(len(trimmed):len(trimmed)) == "]") then
         call parse_array_literal(trimmed(2:len(trimmed)-1), res, err)
         if (err /= 0) then
            print *, "Error parsing array literal."
         else
            call print_result("ans", res)
         end if
      else
         call compile_expr_cp(utoc(trimmed), prog, err, &
            var_names(1:num_vars), custom_fns)
         if (err /= 0) then
            call print_error(err)
            cycle
         end if
         call eval_compiled(prog, res, err, var_vals(1:num_vars))
         if (err /= 0) then
            call print_error(err)
         else
            call print_result("ans", res)
         end if
      end if
   end do

contains

   !---------------------------------------------------------------------------
   ! Custom REPL Functions
   !---------------------------------------------------------------------------
   function fn_deg2rad(args) result(r)
      real(real64), intent(in) :: args(:)
      real(real64) :: r
      r = args(1) * acos(-1.0_real64) / 180.0_real64
   end function fn_deg2rad

   function fn_rad2deg(args) result(r)
      real(real64), intent(in) :: args(:)
      real(real64) :: r
      r = args(1) * 180.0_real64 / acos(-1.0_real64)
   end function fn_rad2deg

   !---------------------------------------------------------------------------
   ! Interactive Help Screen
   !---------------------------------------------------------------------------
   subroutine print_entities()
   write(*,'(a)')[character(len=75) :: &
'the HTML entity names (and numeric codes for characters without standard  ', &
'names) for each symbol you requested.                                     ', &
'                                                                          ', &
'Math and Operator Symbols                                                 ', &
'                                                                          ', &
'Character  Name / Description       HTML Entity Name   Numeric Code       ', &
'   −       Minus sign                 minus             8722              ', &
'   ×       Multiplication sign        times             215               ', &
'   ·       Middle dot                 middot            183               ', &
'   ÷       Division sign              divide            247               ', &
'   √       Square root (radical)      radic             8730              ', &
'   ≤       Less-than or equal to      le                8804              ', &
'   ≥       Greater-than or equal to   ge                8805              ', &
'   ≠       Not equal to               ne                8800              ', &
'   π       Greek small letter pi      pi                960               ', &
'   τ       Greek small letter tau     tau               964               ', &
'                                                                          ', &
'Superscript Characters                                                    ', &
'                                                                          ', &
'Character  Name / Description HTML Entity Name   Numeric Code             ', &
'   ⁰  Superscript  zero          None              8304                   ', &
'   ¹  Superscript  one           sup1              185                    ', &
'   ²  Superscript  two           sup2              178                    ', &
'   ³  Superscript  three         sup3              179                    ', &
'   ⁴  Superscript  four          None              8308                   ', &
'   ⁵  Superscript  five          None              8309                   ', &
'   ⁶  Superscript  six           None              8310                   ', &
'   ⁷  Superscript  seven         None              8311                   ', &
'   ⁸  Superscript  eight         None              8312                   ', &
'   ⁹  Superscript  nine          None              8313                   ', &
'   ⁺  Superscript  plus sign     None              8314                   ', &
'   ⁻  Superscript  minus         None              8315                   ', &
' ⁻  ']
   end subroutine print_entities

   subroutine print_help()
      print *
      print *, "=== CALCULATOR & MODULE EXPLORER HELP ==="
      print *, "COMMANDS:"
      print *, "  help()             Show this help message"
      print *, "  vars()             List all defined variables and their values"
      print *, "  x = 5.0            Assign a scalar variable (case-sensitive)"
      print *, "  v = [1, 2, 3, 4]   Assign a 1D array variable"
      print *, "  diff(expr, x)      Symbolically differentiate expr wrt x and evaluate"
      print *, "  quit / exit        Exit the program"
      print *
      print *, "OPERATORS & UNICODE SYMBOLS:"
      print *, "  Arithmetic:        +  -  −  *  ×  ·  /  ÷  ^  **  √"
      print *, "  Superscripts:      x²  x³  10⁻⁴  (supports ⁰-⁹, ⁺, ⁻)"
      print *, "  Comparisons:       <  <=  ≤  >  >=  ≥  ==  /=  ≠  (returns 1.0 or 0.0)"
      print *, "  Array Indexing:    v[1]  v[2]  (1-based indexing)"
      print *, "  Implicit Multiply: 2π, 3(x + 1), 2√x"
      print *
      print *, "BUILT-IN CONSTANTS & FUNCTIONS:"
      print *, "  Constants:         pi, π, τ (2π), e"
      print *, "  1-Arg Math:        sin, cos, tan, asin, acos, atan, sinh, cosh, tanh,"
      print *, "                     exp, log, log10, sqrt, abs, floor, ceiling, nint"
      print *, "  Multi-Arg Math:    atan2(y, x), hypot(x, y), mod(a, p), min(...), max(...)"
      print *, "  Conditional:       if(cond, true_val, false_val)"
      print *, "  Array Reductions:  sum(v), mean(v), norm2(v), dot(u, v), size(v)"
      print *, "  REPL Extras:       deg2rad(deg), rad2deg(rad)"
      print *
      print *, "EXAMPLE SESSION:"
      print *, "  calc> α = 2.5"
      print *, "  calc> x = [1.0, 2.0, 3.0]"
      print *, "  calc> α·x² + sin(π ÷ 2)"
      print *, "  calc> diff(x³ + 2x, x)"
      print *
   end subroutine print_help

   !---------------------------------------------------------------------------
   ! Variable Table Helpers
   !---------------------------------------------------------------------------

   integer function find_var(name_cp, vnames, nvars)
      integer(int32), intent(in) :: name_cp(:)
      type(cp_string), intent(in) :: vnames(:)
      integer, intent(in) :: nvars
      integer :: i

      find_var = 0
      do i = 1, nvars
         if (size(name_cp) == size(vnames(i)%cp)) then
            if (all(name_cp == vnames(i)%cp)) then
               find_var = i
               return
            end if
         end if
      end do
   end function find_var

   subroutine set_var(name_cp, val, vnames, vvals, nvars)
      integer(int32), intent(in) :: name_cp(:)
      real(real64), intent(in) :: val(:)
      type(cp_string), intent(inout) :: vnames(:)
      type(var_value), intent(inout) :: vvals(:)
      integer, intent(inout) :: nvars
      integer :: idx

      idx = find_var(name_cp, vnames, nvars)
      if (idx == 0) then
         if (nvars >= MAX_VARS) then
            print *, "Error: Maximum variable limit reached."
            return
         end if
         nvars = nvars + 1
         idx = nvars
         vnames(idx)%cp = name_cp
      end if
      vvals(idx)%val = val
   end subroutine set_var

   subroutine print_vars(vnames, vvals, nvars)
      type(cp_string), intent(in) :: vnames(:)
      type(var_value), intent(in) :: vvals(:)
      integer, intent(in) :: nvars
      integer :: i
      if (nvars == 0) then
         print *, "No variables defined."
         return
      end if
      do i = 1, nvars
         call print_result(ctou(vnames(i)%cp), vvals(i)%val)
      end do
   end subroutine print_vars

   subroutine print_result(label, res)
      character(len=*), intent(in) :: label
      real(real64), intent(in) :: res(:)
      integer :: i
      if (size(res) == 1) then
         print '(2X, A, " = ", G0.12)', label, res(1)
      else
         write(*, '(2X, A, " = [")', advance='no') label
         do i = 1, size(res)
            if (i > 1) write(*, '(", ")', advance='no')
            write(*, '(G0.10)', advance='no') res(i)
         end do
         print '(A)', "]"
      end if
   end subroutine print_result

!---------------------------------------------------------------------------
! Parse Comma-Separated Array Literal: "1.0, 2.5, 3.0"
!---------------------------------------------------------------------------

   subroutine parse_array_literal(str, arr, ierr)
      character(len=*), intent(in) :: str
      real(real64), allocatable, intent(out) :: arr(:)
      integer, intent(out) :: ierr
      integer :: i, count, start_p, end_p, stat
      real(real64) :: v
      ierr = 0
      if (len_trim(str) == 0) then
         ierr = 1; return
      end if
      count = 1
      do i = 1, len(str)
         if (str(i:i) == ",") count = count + 1
      end do
      allocate(arr(count))
      start_p = 1
      do i = 1, count
         end_p = index(str(start_p:), ",")
         if (end_p == 0) then
            end_p = len(str)
         else
            end_p = start_p + end_p - 2
         end if
         read(str(start_p:end_p), *, iostat=stat) v
         if (stat /= 0) then
            ierr = 6; return
         end if
         arr(i) = v
         start_p = end_p + 2
      end do
   end subroutine parse_array_literal

! Find single '=' used for assignment (skipping '==', '<=', '>=', '/=')
   integer function find_assignment_eq(s)
      character(len=*), intent(in) :: s
      integer :: i
      find_assignment_eq = 0
      do i = 1, len(s)
         if (s(i:i) == "=") then
            if (i > 1) then
               if (s(i-1:i-1) == "<" .or. s(i-1:i-1) == ">" .or. &
                  s(i-1:i-1) == "/" .or. s(i-1:i-1) == "=") cycle
            end if
            if (i < len(s)) then
               if (s(i+1:i+1) == "=") cycle
            end if
            find_assignment_eq = i
            return
         end if
      end do
   end function find_assignment_eq

   subroutine print_error(err)
      integer, intent(in) :: err
      select case (err)
       case (1);  print *, "Error (1): Empty expression."
       case (2);  print *, "Error (2): Unexpected trailing characters."
       case (3);  print *, "Error (3): Division or modulo by zero."
       case (4);  print *, "Error (4): Unexpected end of expression."
       case (5);  print *, "Error (5): Mismatched parentheses () or brackets []."
       case (6);  print *, "Error (6): Invalid numeric literal or superscript."
       case (7);  print *, "Error (7): Unknown variable or constant."
       case (8);  print *, "Error (8): Syntax error."
       case (9);  print *, "Error (9): Unknown function name."
       case (10); print *, "Error (10): Domain error (sqrt of negative value)."
       case (11); print *, "Error (11): Wrong number of function arguments."
       case (12); print *, "Error (12): Missing variable value."
       case (13); print *, "Error (13): Array index must be a scalar."
       case (14); print *, "Error (14): Array index out of bounds."
       case (15); print *, "Error (15): Incompatible array lengths."
       case (16); print *, "Error (16): Construct not supported for differentiation."
       case default; print *, "Error:", err
      end select
   end subroutine print_error

end program calc_repl
