# Makefile for Linux, FreeBSD, macOS
# See Readme.txt to install libraw 

CFLAGS= -DLIBRAW_NOTHREADS -O3  -w -fPIC $(arch_flags)
OSTYPE = $(shell uname)

ifneq ($(findstring BSD,$(OSTYPE)),)
CXX           = c++
else
CXX           = g++
endif

ifneq ($(findstring Darwin,$(OSTYPE)),)
SO_Name       = libpasraw.dylib
LIBFLAGS      = -Wl,-single_module -dynamiclib
# on macOS, link libraw statically in a single library
LIBS          = /usr/local/lib/libraw.a -mmacosx-version-min=10.10
else
SOName        = libpasraw.so.1
SO_Name       = libpasraw.so.1.1
LIBFLAGS      = -shared -Wl,-soname,$(SOName) 
#LIBFLAGS      = -L/usr/local/lib -I/usr/local/include -shared -Wl,-soname,$(SOName) 
# on Linux, use the standard shared library
LIBS          = -lraw
endif

SRC =	pasraw.cpp 

all: $(SO_Name)

$(SO_Name): $(OBJS)
	$(CXX) $(CFLAGS) $(LIBFLAGS) -o $@ $(SRC) $(LIBS)

clean:
	rm -f *.a *.o $(SO_Name)
