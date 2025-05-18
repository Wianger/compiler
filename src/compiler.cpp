#include "compiler.h"
#include <fstream> // Required for file output
#include <iostream>

void Compiler::compile() {
  auto lexer = Lexer(source);
  auto tokens = lexer.tokenize();
  auto parser = Parser(tokens);
  auto tac = parser.parse();
  // for (const auto &quad : tac) { // Commented out TAC printing
  //   std::cout << quad.op << " " << quad.arg1 << " " << quad.arg2 << " "
  //             << quad.result << std::endl;
  // }
  auto mips = Mips(tac, parser.getSymbolTable());
  auto mipsCode = mips.generateMips();

  std::ofstream mipsFile("mips.txt"); // Open mips.txt for output
  if (mipsFile.is_open()) {
    for (const auto &line : mipsCode) {
      mipsFile << line << std::endl;
    }
    mipsFile.close();
  } else {
    std::cerr << "Error: Unable to open mips.txt for writing." << std::endl;
  }
}