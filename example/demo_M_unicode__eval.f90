     program demo_M_unicode__eval
     use, intrinsic :: iso_fortran_env, only: int32, real64
     use M_unicode, only : ctou, utoc, &
     & compiled_expr, cp_string, var_value, user_func, &
     & compile_expr_cp, differentiate_compiled, eval_compiled

     implicit none

     type(compiled_expr) :: prog, dprog
     type(cp_string) :: var_names(3)
     type(var_value) :: vars(3)
     type(user_func) :: custom_fns(1)
     real(real64), allocatable :: res(:), dres(:)
     integer :: err

     ! 1. Register a custom Unicode function:
     1     σ(x) = logistic sigmoid 1 / (1 + exp(-x))
     !
     custom_fns(1)%name%cp = utoc("σ")
     custom_fns(1)%nargs   = 1
     custom_fns(1)%fn      => sigmoid_fn

     ! 2. Define case-sensitive variables:
     !    x = array [1.0, 2.0, 3.0]
     !    X = scalar [10.0]
     !    α = scalar [2.0]
     var_names(1)%cp = utoc("x")
     var_names(2)%cp = utoc("X")
     var_names(3)%cp = utoc("α")

     vars(1)%val = [1.0_real64, 2.0_real64, 3.0_real64]
     vars(2)%val = [10.0_real64]
     vars(3)%val = [2.0_real64]

     ! 3. Array broadcasting + custom Unicode function + array indexing
     call compile_expr_cp(utoc("σ(α·x − x[2]) + X"), prog, err, &
     & var_names, custom_fns)
     call eval_compiled(prog, res, err, vars)
     print *, "Broadcast + Custom Func:", res   ! 3-element array

     ! 4. Array reductions (dot product, sum, norm2)
     call compile_expr_cp(utoc("sum(x²) + x[3] × X"), &
     & prog, err, var_names)
     call eval_compiled(prog, res, err, vars)
     ! 1^2 + 2^2 + 3^2 + 3*10 = 44.0
     print *, "Reduction + Indexing:", res

     ! 5. Symbolic Differentiation: d/dx (x³ + 2x² + sin(x)) ->
     !    3x² + 4x + cos(x)
     call compile_expr_cp(utoc("x³ + 2x² + sin(x)"), &
     & prog, err, var_names)
     call differentiate_compiled(prog, var_idx=1, dprog=dprog, ierr=err)
     call eval_compiled(dprog, dres, err, vars)
     print *, "Analytical df/dx at x=[1,2,3]:", dres

     contains

     function sigmoid_fn(args) result(y)
        real(real64), intent(in) :: args(:)
        real(real64) :: y
        y = 1.0_real64 / (1.0_real64 + exp(-args(1)))
     end function sigmoid_fn

     end program demo_M_unicode__eval
