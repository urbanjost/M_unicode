```text
 This file was compiled by  flang version 22.0.0 (https://github.com/llvm/llvm-project.git 378b6d51de97ce220c042a0823d047a546c82bf6) 
 using the options   /opt/compiler-explorer/clang-llvmflang-trunk/bin/flang
-o /app/output.s
-g /app/example.f90
-L./lib
-Wl,-rpath,./lib

 This file was compiled by  Intel(R) Fortran Compiler for applications running on Intel(R) 64, Version 2024.0.0 Build 20231017 
 using the options  
-g
-o /app/output.s
-gxx-name=/opt/compiler-explorer/gcc-13.2.0/bin/g++
-L./lib
-Wl,-rpath,./lib

 This file was compiled by  GCC version 16.0.0 20250727 (experimental) 
 using the options  
-I build/gfortran_87E2AE0597D39913
-mtune=generic
-march=x86-64
-g
-Wall
-Wextra
-Werror=implicit-interface
-fPIC
-fmax-errors=1
-fbounds-check
-fcheck=array-temps
-fbacktrace
-fcoarray=single
-fimplicit-none
-ffree-form
-J build/gfortran_87E2AE0597D39913

PASSED adjustl:[  this is a string    ][this is a string      ][this is a string      ]
PASSED adjustl:[  ][  ][  ]
PASSED adjustl:[][][]
PASSED adjustl:[ALLFULL][ALLFULL][ALLFULL]
PASSED adjustl:[  😃   ][😃     ][😃     ]
PASSED adjustr:[  this is a string    ][      this is a string][      this is a string]
PASSED adjustr:[  ][  ][  ]
PASSED adjustr:[][][]
PASSED adjustr:[ALLFULL][ALLFULL][ALLFULL]
PASSED adjustr:[  😃   ][     😃][     😃]
PASSED trim:[  this is a string    ][  this is a string][  this is a string]
PASSED trim:[  ][][]
PASSED trim:[][][]
PASSED trim:[ALLFULL][ALLFULL][ALLFULL]
PASSED trim:[  😃   ][  😃][  😃]
PASSED len_trim
PASSED len_trim
PASSED len_trim
PASSED len_trim
PASSED len_trim
PASSED len_trim
PASSED len
PASSED len
PASSED len
PASSED len
PASSED len
PASSED len
PASSED index  can you find me here? :find me
PASSED index  can you find me here? :find me
PASSED index  can you find me here? :not there
PASSED index  can you find me here? :not there
PASSED index short:shortnot
PASSED index short:shortnot
PASSED repeat:[💣💥💣💥💣💥][💣💥💣💥💣💥💣💥💣💥💣💥💣💥💣💥💣💥💣💥💣💥💣💥💣💥💣💥💣💥][💣💥💣💥💣💥💣💥💣💥💣💥💣💥💣💥💣💥💣💥💣💥💣💥💣💥💣💥💣💥]
PASSED repeat:[1234567890][123456789012345678901234567890][123456789012345678901234567890]
PASSED upper
PASSED upper
82 i ı
PASSED lower
PASSED lower
PASSED tokenize:tokens:[parsley][sage][rosemary][thyme]
PASSED tokenize:separators:[,][,][&]
PASSED tokenize:size of begins
PASSED tokenize:size of ends
PASSED tokenize:begins
PASSED tokenize:ends
PASSED tokenize:tokens:[parsley][sage][rosemary][][thyme]
PASSED sort:sort array
PASSED sort:sort array

PASSED convert to ASCII bytes:[Hello World and Ni Hao -- 你好][Hello World and Ni Hao -- 你好][Hello World and Ni Hao -- 你好]
PASSED concatenation:[😃 and 😃and😃😃is it][😃 and 😃and😃😃is it][😃 and 😃and😃😃is it]
PASSED LLE:[ ][TTFF][TTFF]
PASSED LLT:[ ][TTFF][TTFF]
PASSED LNE:[ ][TTTT][TTTT]
PASSED LEQ:[ ][FFFF][FFFF]
PASSED LGT:[ ][FFTT][FFTT]
PASSED LGE:[ ][FFTT][FFTT]
PASSED LLE:[ ][TTTT][TTTT]
PASSED LLT:[ ][FFFF][FFFF]
PASSED LNE:[ ][FFFF][FFFF]
PASSED LEQ:[ ][TTTT][TTTT]
PASSED LGT:[ ][FFFF][FFFF]
PASSED LGE:[ ][TTTT][TTTT]
PASSED split:Más
PASSED split:vale
PASSED split:pájaro
PASSED split:en
PASSED split:mano,
PASSED split:que
PASSED split:ciento
PASSED split:volando.
PASSED split:size
PASSED split: oop all at once
PASSED scan:default
PASSED scan:back=true
PASSED scan:back=false
PASSED scan:no match
PASSED scan:oop test
PASSED scan:oop test
PASSED verify:32‐af43d
original bytes  :七転び八起き。転んでもまた立ち上がる。くじけずに前を向いて歩いていこう。
bytes reversed  :��ㆁ㓁㄁㦁㄁㩭榁㄁㑐咂㍉嫁㚁㑁㘁㏁む㋂㌁㊸䡁㋫矁㾁も㧁㓂㢻肀㍁㷵諅峁㢻胸�
original string :七転び八起き。転んでもまた立ち上がる。くじけずに前を向いて歩いていこう。
string reversed :。うこいてい歩てい向を前にずけじく。るが上ち立たまもでん転。き起八び転七

all passed
```
