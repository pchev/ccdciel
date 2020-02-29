Interface library to libraw

This library exist because we cannot link a C++ object directly from Pascal.
It export C function we can link from Pascal to load a raw buffer and return 
a pointer to the raw data.
The rest of the processing is in the Pascal code.

This procedure is tested with "LibRaw 201910 snapshot" it may require adjustement with another version.
 
LINUX:
======
This library is not build with CCDciel for Linux, it is part of libpasastro package.

The Makefile create the library libpasraw.so dynamically linked to libraw.
The distribution libraw-devel package must be installed for the compilation.
The distribution libraw package must be installed at runtime.


MAC:
====
First install the latest libraw from source:
 - Install the LibRaw source code, from tar or from git.
 - cd LibRaw
 - make -f Makefile.dist
 - sudo make -f Makefile.dist install

The Makefile.darwin create the library libpasraw.dylib with libraw statically linked.
No libraw.dylib is required at runtime.


WINDOWS:
========
We use cross-compilation from Linux using Mingw.
Install mingw: sudo apt install mingw-w64

First install the latest libraw from source:
 - Install the LibRaw source code, from tar or from git.
 - cd LibRaw
Then for Win64:
 - make -f Makefile.mingw clean
 - make -f Makefile.mingw CXX=x86_64-w64-mingw32-g++ CC=x86_64-w64-mingw32-gcc library
 - mkdir -p $HOME/mingw/w64/include
 - mkdir -p $HOME/mingw/w64/lib
 - cp -R libraw $HOME/mingw/w64/include/
 - cp lib/libraw.a $HOME/mingw/w64/lib/
 
For Win32:
 - make -f Makefile.mingw clean
 - make -f Makefile.mingw CXX=i686-w64-mingw32-g++ CC=i686-w64-mingw32-gcc library
 - mkdir -p $HOME/mingw/w32/include
 - mkdir -p $HOME/mingw/w32/lib
 - cp -R libraw $HOME/mingw/w32/include/
 - cp lib/libraw.a $HOME/mingw/w32/lib/
 
The Makefile.win32 and Makefile.win64 create the library libpasraw.dll with libraw statically linked.
No libraw.dll is required at runtime.

