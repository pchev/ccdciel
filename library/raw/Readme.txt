Interface library to libraw

This library exist because we cannot link a C++ object directly from Pascal.
It export C function we can link from Pascal to load a raw buffer and return 
a pointer to the raw data.
The rest of the processing is in the Pascal code.
 
LINUX:
======
This library is not build with CCDciel for Linux, it is part of libpasastro package.

The Makefile create the library libpasraw.so dynamically linked to libraw.
The distribution libraw-devel package must be installed for the compilation.
The distribution libraw package must be installed at runtime.


MAC:
====
First install the latest libraw from source:
 - wget https://www.libraw.org/data/LibRaw-0.19.5.tar.gz
 - tar xf LibRaw-0.19.5.tar.gz
 - cd LibRaw-0.19.5
 - ./configure CPPFLAGS="-mmacosx-version-min=10.10" 
 - make
 - sudo make install

The Makefile create the library libpasraw.dylib with libraw statically linked.
No libraw.dylib is required at runtime.


WINDOWS:
========
We use cross-compilation from Linux using Mingw.

First install the latest libraw from source:
 - wget https://www.libraw.org/data/LibRaw-0.19.5.tar.gz
 - tar xf LibRaw-0.19.5.tar.gz
 - cd LibRaw-0.19.5
 - Unfortunatelly the mingw makefile do not support cross-compilation so we have to use this trick:
cp Makefile.mingw Makefile.mingw-cross
sed -i 's/g++/\$\(CPP\)/' Makefile.mingw-cross
sed -i 's/gcc/\$\(CC\)/' Makefile.mingw-cross
sed -i 's/ar /\$\(AR\) /' Makefile.mingw-cross
sed -i 's/ranlib/\$\(RANLIB\)/' Makefile.mingw-cross
sed -i 's/-DLIBRAW_NODLL/-DLIBRAW_NODLL -static-libgcc -static-libstdc++/' Makefile.mingw-cross

  Then for Win64:
 - make -f Makefile.mingw-cross clean  
 - make -f Makefile.mingw-cross CPP=x86_64-w64-mingw32-g++ CC=x86_64-w64-mingw32-gcc AR=x86_64-w64-mingw32-ar RANLIB=x86_64-w64-mingw32-ranlib
 - mkdir -p $HOME/mingw/w64/include
 - mkdir -p $HOME/mingw/w64/lib
 - cp -R libraw $HOME/mingw/w64/include/
 - cp lib/libraw.a $HOME/mingw/w64/lib/
 
  For Win32:
 - make -f Makefile.mingw-cross clean  
 - make -f Makefile.mingw-cross CPP=i686-w64-mingw32-g++ CC=i686-w64-mingw32-gcc AR=i686-w64-mingw32-ar RANLIB=i686-w64-mingw32-ranlib
 - mkdir -p $HOME/mingw/w32/include
 - mkdir -p $HOME/mingw/w32/lib
 - cp -R libraw $HOME/mingw/w32/include/
 - cp lib/libraw.a $HOME/mingw/w32/lib/
 
The Makefile.win32 and Makefile.win64 create the library libpasraw.dll with libraw statically linked.
No libraw.dll is required at runtime.

