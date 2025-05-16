#include "compiler.h"
#include <iostream>

void Compiler::compile() {
  auto lexer = Lexer(source);
  auto tokens = lexer.tokenize();
  auto parser = Parser(tokens);
  auto tac = parser.parse();
  for (const auto &quad : tac) {
    std::cout << quad.op << " " << quad.arg1 << " " << quad.arg2 << " "
              << quad.result << std::endl;
  }
}