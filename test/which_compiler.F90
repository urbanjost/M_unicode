#include "define_compiler.inc"
program pre
   use, intrinsic :: iso_fortran_env, only : real_kinds,real32,real64,real128
   write(*,*)'__COMPILER__',__COMPILER__

   write(*,*)'REAL128 is ',merge('SUPPORTED    ','NOT_SUPPORTED',real128.gt.0)

#ifdef FLOAT128
   write(*,*)'FLOAT128 is defined'
#endif

#ifdef HAS_DT
   write(*,*)'HAS_DT is defined'
#endif

end program pre
