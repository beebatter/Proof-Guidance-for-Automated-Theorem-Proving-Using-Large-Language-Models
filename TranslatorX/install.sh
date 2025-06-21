mkdir -p translatorxbuild
cd translatorxbuild
cmake .. -DCMAKE_BUILD_TYPE=Release -DCMAKE_C_COMPILER=cc -DCMAKE_CXX_COMPILER=c++ -DCMAKE_CXX_FLAGS="-O3 -std=c++11"
make
sudo cp TranslatorX /usr/local/bin

