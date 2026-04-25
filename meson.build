project(
    'M_unicode',
    'fortran',
    version : '1.1.0',
    license : 'MIT',
    default_options : [
        'buildtype=debugoptimized',
        'fortran_std=f2008',
        'default_library=both',
    ]
)

M_unicode_src = files(
    'src/M_unicode.F90'
)

M_unicode_lib = library(
    meson.project_name(),
    sources : M_unicode_src,
    version : meson.project_version(),
    install : true,
)

M_unicode_inc = M_unicode_lib.private_dir_include()
M_unicode_dep = declare_dependency(
    link_with : M_unicode_lib,
    include_directories : M_unicode_inc,
)

test(
    'runTests',
    executable(
        'runTests',
        'test/test_M_unicode.f90',
        dependencies : M_unicode_dep,
    ),
)

M_unicode_lic = files(
    'LICENSE',
)
install_data(
    M_unicode_lic,
    install_dir : join_paths(get_option('prefix'), 'share', 'licenses', meson.project_name()),
)

if host_machine.system() == 'windows'
    symbols_file = 'lib'+meson.project_name()+'-'+meson.project_version().split('.')[0]+'.dll.symbols'
    obj_file = 'src_M_unicode.F90.obj'
else
    symbols_file = 'lib'+meson.project_name()+'.so.'+meson.project_version()+'.symbols'
    obj_file = 'src_M_unicode.F90.o'
endif
install_subdir(M_unicode_lib.path()+'.p',
    install_dir: 'include'/meson.project_name(),
    strip_directory: true,
    exclude_files: [
        'depscan.dd',
        meson.project_name()+'-deps.json',
        meson.project_name()+'.dat',
        symbols_file,
        obj_file,
    ]
)

pkg = import('pkgconfig')
pkg.generate(
    name : meson.project_name(),
    description : 'Fortran direct use of UTF-8 encoded Unicode characters',
    version : meson.project_version(),
    libraries : M_unicode_lib,
    subdirs : meson.project_name(),
)
