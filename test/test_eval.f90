program test_suite
   use, intrinsic :: iso_fortran_env, only: int32, real64
   use M_unicode, only : ctou, utoc
   use M_unicode, only : cp_string, var_value, user_func, user_func_iface, compiled_expr ! Public Types
   use M_unicode, only : compile_expr_cp, eval_compiled, differentiate_compiled ! Public Procedures
   implicit none

    integer :: passed = 0, failed = 0

    print *, "=========================================================="
    print *, " Running M_unicode__eval Unit Test Suite"
    print *, "=========================================================="

    call test_utf8_roundtrip()
    call test_basic_arithmetic_and_precedence()
    call test_unicode_operators_and_superscripts()
    call test_implicit_multiplication()
    call test_strict_case_sensitivity()
    call test_array_broadcasting_and_indexing()
    call test_array_reductions_and_multiarg()
    call test_custom_user_functions()
    call test_symbolic_differentiation()
    call test_error_handling()

    print *, "=========================================================="
    print '(A, I0, A, I0, A)', " Summary: ", passed, " passed, ", failed, " failed."
    print *, "=========================================================="

    if (failed > 0) error stop 1

contains

    !---------------------------------------------------------------------------
    ! Test Assertion Helpers
    !---------------------------------------------------------------------------
    subroutine assert_near(actual, expected, tol, name)
        real(real64), intent(in) :: actual, expected, tol
        character(len=*), intent(in) :: name
        if (abs(actual - expected) <= tol) then
            passed = passed + 1
        else
            failed = failed + 1
            print *, "[FAIL] ", name, " | Expected:", expected, " Got:", actual
        end if
    end subroutine assert_near

    subroutine assert_array_near(actual, expected, tol, name)
        real(real64), intent(in) :: actual(:), expected(:), tol
        character(len=*), intent(in) :: name
        if (size(actual) /= size(expected)) then
            failed = failed + 1
            print *, "[FAIL] ", name, " | Size mismatch:", size(actual), "vs", size(expected)
            return
        end if
        if (all(abs(actual - expected) <= tol)) then
            passed = passed + 1
        else
            failed = failed + 1
            print *, "[FAIL] ", name, " | Array mismatch"
            print *, "   Expected:", expected
            print *, "   Got     :", actual
        end if
    end subroutine assert_array_near

    subroutine assert_int_eq(actual, expected, name)
        integer, intent(in) :: actual, expected
        character(len=*), intent(in) :: name
        if (actual == expected) then
            passed = passed + 1
        else
            failed = failed + 1
            print *, "[FAIL] ", name, " | Expected:", expected, " Got:", actual
        end if
    end subroutine assert_int_eq

    !---------------------------------------------------------------------------
    ! 1. UTF-8 <-> Codepoint Array Roundtrip
    !---------------------------------------------------------------------------
    subroutine test_utf8_roundtrip()
        character(len=:), allocatable :: orig, back
        integer(int32), allocatable :: cp(:)

        orig = "α + Δx × π ÷ √(y²) − 速度"
        cp = utoc(orig)
        back = ctou(cp)

        if (orig == back) then
            passed = passed + 1
        else
            failed = failed + 1
            print *, "[FAIL] UTF-8 roundtrip mismatch"
        end if
    end subroutine test_utf8_roundtrip

    !---------------------------------------------------------------------------
    ! 2. Standard Precedence, Associativity, and Scientific Notation
    !---------------------------------------------------------------------------
    subroutine test_basic_arithmetic_and_precedence()
        type(compiled_expr) :: prog
        real(real64), allocatable :: res(:)
        integer :: err

        ! Right-associative power: 2 ** 3 ** 2 == 2 ** 9 == 512
        call compile_expr_cp(utoc("2 ** 3 ** 2"), prog, err)
        call assert_int_eq(err, 0, "Compile right-assoc power")
        call eval_compiled(prog, res, err)
        call assert_near(res(1), 512.0_real64, 1.0e-12_real64, "2**3**2 == 512")

        ! Scientific notation & unary negation
        call compile_expr_cp(utoc("-1.5d2 + 2.5e-1 * 4.0"), prog, err)
        call eval_compiled(prog, res, err)
        call assert_near(res(1), -149.0_real64, 1.0e-12_real64, "Scientific notation")
    end subroutine test_basic_arithmetic_and_precedence

    !---------------------------------------------------------------------------
    ! 3. Unicode Operators (×, ÷, −, √, ≤, ≥, ≠) & Postfix Superscripts (², ⁻³)
    !---------------------------------------------------------------------------
    subroutine test_unicode_operators_and_superscripts()
        type(compiled_expr) :: prog
        real(real64), allocatable :: res(:)
        integer :: err

        call compile_expr_cp(utoc("√(3² + 4²) × 10⁻¹ ÷ 2 − 0.25"), prog, err)
        call assert_int_eq(err, 0, "Compile Unicode math symbols")
        call eval_compiled(prog, res, err)
        ! sqrt(25) * 0.1 / 2 - 0.25 = 5 * 0.05 - 0.25 = 0.0
        call assert_near(res(1), 0.0_real64, 1.0e-12_real64, "√(3²+4²)×10⁻¹÷2−0.25")

        ! Unicode comparisons
        call compile_expr_cp(utoc("(π ≤ 3.15) + (τ ≥ 6.28) + (π ≠ e)"), prog, err)
        call eval_compiled(prog, res, err)
        call assert_near(res(1), 3.0_real64, 1.0e-12_real64, "Unicode comparisons == 3.0")
    end subroutine test_unicode_operators_and_superscripts

    !---------------------------------------------------------------------------
    ! 4. Implicit Multiplication
    !---------------------------------------------------------------------------
    subroutine test_implicit_multiplication()
        type(compiled_expr) :: prog
        type(cp_string) :: vnames(2)
        type(var_value) :: vars(2)
        real(real64), allocatable :: res(:)
        integer :: err

        vnames(1)%cp = utoc("x")
        vnames(2)%cp = utoc("y")
        vars(1)%val = [3.0_real64]
        vars(2)%val = [4.0_real64]

        ! 2x + 3(y - 1) + (x + 1)(y - 2) + 2√y
        ! = 6 + 9 + 4*2 + 4 = 27
        call compile_expr_cp(utoc("2x + 3(y - 1) + (x + 1)(y - 2) + 2√y"), prog, err, vnames)
        call assert_int_eq(err, 0, "Compile implicit multiplication")
        call eval_compiled(prog, res, err, vars)
        call assert_near(res(1), 27.0_real64, 1.0e-12_real64, "Implicit multiplication value")
    end subroutine test_implicit_multiplication

    !---------------------------------------------------------------------------
    ! 5. Strict Case Sensitivity (ASCII & Greek)
    !---------------------------------------------------------------------------
    subroutine test_strict_case_sensitivity()
        type(compiled_expr) :: prog
        type(cp_string) :: vnames(4)
        type(var_value) :: vars(4)
        real(real64), allocatable :: res(:)
        integer :: err

        vnames(1)%cp = utoc("x")
        vnames(2)%cp = utoc("X")
        vnames(3)%cp = utoc("α")
        vnames(4)%cp = utoc("Α") ! Capital Greek Alpha (U+0391)

        vars(1)%val = [1.0_real64]
        vars(2)%val = [10.0_real64]
        vars(3)%val = [100.0_real64]
        vars(4)%val = [1000.0_real64]

        call compile_expr_cp(utoc("x + X + α + Α"), prog, err, vnames)
        call assert_int_eq(err, 0, "Compile distinct case variables")
        call eval_compiled(prog, res, err, vars)
        call assert_near(res(1), 1111.0_real64, 1.0e-12_real64, "Distinct case values")

        ! Uppercase function name SIN must fail
        call compile_expr_cp(utoc("SIN(0)"), prog, err)
        call assert_int_eq(err, 7, "Uppercase SIN rejected")
    end subroutine test_strict_case_sensitivity

    !---------------------------------------------------------------------------
    ! 6. Array Broadcasting & 1-Based Subscript Indexing
    !---------------------------------------------------------------------------
    subroutine test_array_broadcasting_and_indexing()
        type(compiled_expr) :: prog
        type(cp_string) :: vnames(2)
        type(var_value) :: vars(2)
        real(real64), allocatable :: res(:)
        integer :: err

        vnames(1)%cp = utoc("v")
        vnames(2)%cp = utoc("c")
        vars(1)%val = [2.0_real64, 4.0_real64, 6.0_real64]
        vars(2)%val = [10.0_real64]

        ! Broadcast scalar c over vector v, add indexed element v[2] (4.0)
        call compile_expr_cp(utoc("v × c + v[2]"), prog, err, vnames)
        call assert_int_eq(err, 0, "Compile array broadcast + index")
        call eval_compiled(prog, res, err, vars)
        call assert_array_near(res, [24.0_real64, 44.0_real64, 64.0_real64], 1.0e-12_real64, &
                               "Broadcast + index result")
    end subroutine test_array_broadcasting_and_indexing

    !---------------------------------------------------------------------------
    ! 7. Array Reductions & Multi-Argument Functions
    !---------------------------------------------------------------------------
    subroutine test_array_reductions_and_multiarg()
        type(compiled_expr) :: prog
        type(cp_string) :: vnames(2)
        type(var_value) :: vars(2)
        real(real64), allocatable :: res(:)
        integer :: err

        vnames(1)%cp = utoc("a")
        vnames(2)%cp = utoc("b")
        vars(1)%val = [3.0_real64, 4.0_real64]
        vars(2)%val = [2.0_real64, 5.0_real64]

        ! dot(a,b) = 6+20 = 26; norm2(a) = 5; sum(b) = 7; mean(a) = 3.5; size(a) = 2
        call compile_expr_cp(utoc("dot(a, b) + norm2(a) + sum(b) + mean(a) + size(a)"), &
                             prog, err, vnames)
        call eval_compiled(prog, res, err, vars)
        call assert_near(res(1), 43.5_real64, 1.0e-12_real64, "Array reductions")

        ! Conditional if() with array broadcasting
call compile_expr_cp(utoc("if(a > 3.5, b, -1.0)"), prog, err, vnames)
call eval_compiled(prog, res, err, vars)
call assert_array_near(res, [-1.0_real64, 5.0_real64], 1.0e-12_real64, "Vectorized if()")
end subroutine test_array_reductions_and_multiarg
!---------------------------------------------------------------------------
! 8. Custom User-Defined Fortran Functions (with Unicode Name)
!---------------------------------------------------------------------------
function custom_poly(args) result(r)
real(real64), intent(in) :: args(:)
real(real64) :: r
r = args(1)**2 + 2.0_real64 * args(2)
end function custom_poly
subroutine test_custom_user_functions()
type(compiled_expr) :: prog
type(user_func) :: ufuncs(1)
type(cp_string) :: vnames(1)
type(var_value) :: vars(1)
real(real64), allocatable :: res(:)
integer :: err
ufuncs(1)%name%cp = utoc("Φ")
ufuncs(1)%nargs   = 2
ufuncs(1)%fn      => custom_poly
vnames(1)%cp = utoc("x")
vars(1)%val  = [1.0_real64, 2.0_real64, 3.0_real64]
! Φ(x, 10) = x^2 + 20 -> [21, 24, 29]
call compile_expr_cp(utoc("Φ(x, 10)"), prog, err, vnames, ufuncs)
call assert_int_eq(err, 0, "Compile custom Unicode function")
call eval_compiled(prog, res, err, vars)
call assert_array_near(res, [21.0_real64, 24.0_real64, 29.0_real64], 1.0e-12_real64, &
"Custom function broadcast")
end subroutine test_custom_user_functions
!---------------------------------------------------------------------------
! 9. Symbolic Differentiation (Scalar & Array)
!---------------------------------------------------------------------------
subroutine test_symbolic_differentiation()
type(compiled_expr) :: prog, dprog
type(cp_string) :: vnames(1)
type(var_value) :: vars(1)
real(real64), allocatable :: dres(:)
real(real64) :: expected(2)
integer :: err
vnames(1)%cp = utoc("x")
vars(1)%val  = [0.5_real64, 1.5_real64]
! f(x) = x^3 + sin(2x) + exp(-x)
! f'(x) = 3x^2 + 2cos(2x) - exp(-x)
call compile_expr_cp(utoc("x³ + sin(2x) + exp(-x)"), prog, err, vnames)
call assert_int_eq(err, 0, "Compile expression for autodiff")
call differentiate_compiled(prog, 1, dprog, err)
call assert_int_eq(err, 0, "Symbolic differentiation succeeded")
call eval_compiled(dprog, dres, err, vars)
expected = 3.0_real64 * vars(1)%val**2 + 2.0_real64 * cos(2.0_real64 * vars(1)%val) &
- exp(-vars(1)%val)
call assert_array_near(dres, expected, 1.0e-12_real64, "Analytical derivative accuracy")
end subroutine test_symbolic_differentiation
!---------------------------------------------------------------------------
! 10. Error Code Verification
!---------------------------------------------------------------------------
subroutine test_error_handling()
type(compiled_expr) :: prog
type(cp_string) :: vnames(2)
type(var_value) :: vars(2)
real(real64), allocatable :: res(:)
integer :: err
vnames(1)%cp = utoc("a")
vnames(2)%cp = utoc("b")
vars(1)%val = [1.0_real64, 2.0_real64]
vars(2)%val = [1.0_real64, 2.0_real64, 3.0_real64]
! Mismatched parentheses (err = 5)
call compile_expr_cp(utoc("(1 + 2"), prog, err)
call assert_int_eq(err, 5, "Error 5: Mismatched parens")
! Division by zero (err = 3)
call compile_expr_cp(utoc("10 / (2 - 2)"), prog, err)
call eval_compiled(prog, res, err)
call assert_int_eq(err, 3, "Error 3: Division by zero")
! Square root of negative number (err = 10)
call compile_expr_cp(utoc("√(-4)"), prog, err)
call eval_compiled(prog, res, err)
call assert_int_eq(err, 10, "Error 10: Negative sqrt")
! Out-of-bounds array index (err = 14)
call compile_expr_cp(utoc("a[5]"), prog, err, vnames)
call eval_compiled(prog, res, err, vars)
call assert_int_eq(err, 14, "Error 14: Out of bounds index")
! Incompatible broadcast shapes (err = 15)
call compile_expr_cp(utoc("a + b"), prog, err, vnames)
call eval_compiled(prog, res, err, vars)
call assert_int_eq(err, 15, "Error 15: Incompatible array shapes")
end subroutine test_error_handling
end program test_suite
