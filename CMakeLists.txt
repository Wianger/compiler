cmake_minimum_required(VERSION 3.10)

project(SysYCompiler CXX)

# 设置 C++ 标准为 C++17
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED True)
set(CMAKE_CXX_EXTENSIONS OFF) # 通常建议关闭编译器特定的扩展

# 添加可执行文件目标
# 我们的源文件位于 src/ 目录下
add_executable(compiler 
    src/main.cpp 
    src/lexer.cpp 
    src/token.cpp
)

# 指定头文件所在的目录
# 这样编译器在处理 src/main.cpp 等文件中的 #include "lexer.h" 时能够找到它们
target_include_directories(compiler PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/src)

# 可选：添加编译选项 (例如，开启更多警告)
# target_compile_options(compiler PRIVATE -Wall -Wextra -pedantic)

# 可选：如果需要，可以添加安装指令
# install(TARGETS compiler DESTINATION bin)

# 打印一条消息，指示 CMake 配置完成
message(STATUS "CMake configuration for SysYCompiler completed.")
message(STATUS "Executable target: compiler")
message(STATUS "Source files: src/main.cpp, src/lexer.cpp, src/token.cpp")
message(STATUS "Include directory: ${CMAKE_CURRENT_SOURCE_DIR}/src") 