# Add -g for adding debug information.
# Download MinGW GDB and add it to MinGW/bin file
main:
	gcc -c -o bin/WindowEffect.o src/WindowEffect.c
	gcc -c -o bin/CustomFunctionLib.o src/CustomFunctionLib.c
	gcc -o bin/WindowEffect bin/WindowEffect.o bin/CustomFunctionLib.o -mwindows	
	windres -o bin/CustomWindowRes.o src/CustomWindowRes.rc
	gcc -c -o bin/CustomWindow.o src/CustomWindow.c
	gcc -c -o bin/CustomFunctionLib.o src/CustomFunctionLib.c
	gcc -o bin/CustomWindow bin/CustomWindow.o bin/CustomWindowRes.o bin/CustomFunctionLib.o -mwindows	
# test multi thread
	gcc -c -o bin/DataStructLib.o src/DataStructLib.c
	gcc -c -o bin/MultiThreadLib.o src/MultiThreadLib.c
	gcc -c -o bin/testMultiThread.o src/testMultiThread.c
	gcc -o bin/testMultiThread  bin/DataStructLib.o bin/MultiThreadLib.o bin/testMultiThread.o 
	
	gcc -c -o bin/MultiThreadWithFixBufferLib.o src/MultiThreadWithFixBufferLib.c
	gcc -c -o bin/testMultiThreadWithFixBuffer.o src/testMultiThreadWithFixBuffer.c
	gcc -o bin/testMultiThreadWithFixBuffer bin/MultiThreadWithFixBufferLib.o bin/testMultiThreadWithFixBuffer.o 
# test data struct 
	gcc -c -o bin/DataStructLib.o src/DataStructLib.c
	gcc -c -o bin/testDataStruct.o src/testDataStruct.c
	gcc -o bin/testDataStruct bin/DataStructLib.o bin/testDataStruct.o
clean:
	del bin\WindowEffect.o bin\WindowEffect.exe
	del bin\CustomWindowRes.o bin\CustomWindow.o bin\CustomFunctionLib.o bin\CustomWindow.exe
	del bin\MultiThreadLib.o bin\testMultiThread.o bin\testMultiThread.exe
	del bin\MultiThreadWithFixBufferLib.o bin\testMultiThreadWithFixBuffer.o bin\testMultiThreadWithFixBuffer.exe
	del bin\DataStructLib.o bin\testDataStruct.o bin\testDataStruct.exe
all:
