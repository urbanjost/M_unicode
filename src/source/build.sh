#!/bin/bash
cd $(dirname $0)
export GITHUB=TRUE
export DEMO_OUTDIR=../../example/
export DEMO_SUBDIR=FALSE
prep F90 --comment doxygen --verbose -i ../../app/uni.[fF][fF] -o ../../app/uni.f90
GPF_build_module M_unicode__convert
GPF_build_module M_unicode__random
GPF_build_module M_unicode__float
GPF_build_module M_unicode__get
GPF_build_module M_unicode__regex
GPF_build_module M_unicode__eval
GPF_build_module M_unicode
ccall ../../test/test_suite_M_unicode.[fF]90
#mv ../../example/demo_readline.f90 ../../app/demo_readline.f90
exit
