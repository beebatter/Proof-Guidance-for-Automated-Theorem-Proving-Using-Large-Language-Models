cmake_minimum_required(VERSION 3.5...3.28)


project(TranslatorX)

add_executable(TranslatorX main.cpp)




TARGET_LINK_LIBRARIES(TranslatorX pthread)

install(TARGETS TranslatorX RUNTIME DESTINATION bin)
