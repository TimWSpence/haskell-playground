# First foray in GHC's FFI

Steps to run:
```
hsc2hs test.hcs
gcc -c -Wall -Werror -fpic test.c
gcc -shared -o libtest.so test.o
cp libtest.so /tmp
ghci -L/tmp -ltest
```
