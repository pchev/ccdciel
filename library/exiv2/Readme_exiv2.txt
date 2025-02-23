Linux:

Install the distribution package


Windows x64:

Download the last release file from https://github.com/Exiv2/exiv2/releases
extract and zip the content of the bin directory.


Mac arm64:

The release file cannot be used because it require to install the dependencies from homebrew.
The method bellow make an idependant package.

download from https://github.com/Exiv2/exiv2
install dependencies with Macports
cd exiv2-0.28.4

export MACOSX_DEPLOYMENT_TARGET=11.0
cmake -S . -B build -DCMAKE_BUILD_TYPE=Release -DEXIV2_ENABLE_NLS=OFF -DBUILD_SHARED_LIBS=OFF -DEXIV2_ENABLE_VIDEO=OFF -DEXIV2_ENABLE_DYNAMIC_RUNTIME=OFF -DEXIV2_ENABLE_INIH=OFF -DEXIV2_ENABLE_BROTLI=OFF -DEXIV2_ENABLE_LENSDATA=OFF
cmake --build build

cd build/bin
cp /opt/local/lib/libexpat.1.dylib .
cp /opt/local/lib/libz.1.dylib .
cp /opt/local/lib/libiconv.2.dylib .
install_name_tool -id @rpath/libexpat.1.dylib libexpat.1.dylib
install_name_tool -id @rpath/libz.1.dylib libz.1.dylib
install_name_tool -id @rpath/libiconv.2.dylib libiconv.2.dylib
install_name_tool -change /opt/local/lib/libexpat.1.dylib @executable_path/libexpat.1.dylib exiv2
install_name_tool -change /opt/local/lib/libz.1.dylib @executable_path/libz.1.dylib exiv2
install_name_tool -change /opt/local/lib/libiconv.2.dylib @executable_path/libiconv.2.dylib exiv2
otool -L exiv2
tar cvzf exiv2-mac-arm64.tgz *

