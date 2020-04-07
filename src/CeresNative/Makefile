all: deploy

deploy: CeresNative.so
	cp CeresNative.so ../../libs/Native/Ceres/linux/AMD64/

CeresNative.so: CeresNative.o 
	g++ CeresNative.o -lceres -lgflags -lglog -shared -o CeresNative.so

CeresNative.o:
	g++ -std=c++11 -fPIC -I/usr/include/eigen3 -c CeresNative.cpp  -o CeresNative.o

.PHONY clean:
	rm -f *.o && rm CeresNative.so
