#include "lexer.h"
#include "token.h" // For tokenTypeToString
#include <fstream>
#include <iostream>
#include <sstream>

// Function to read file content into a string
std::string readFile(const std::string &filepath) {
  std::ifstream file_stream(filepath);
  if (!file_stream.is_open()) {
    std::cerr << "Could not open file: " << filepath << std::endl;
    return "";
  }
  std::stringstream buffer;
  buffer << file_stream.rdbuf();
  return buffer.str();
}

int main(int argc, char *argv[]) {
  if (argc < 2) {
    std::cerr << "Usage: compiler <source_file>" << std::endl;
    return 1;
  }

  std::string source_filepath = argv[1];
  std::string source_code = readFile(source_filepath);

  if (source_code.empty() &&
      source_filepath !=
          "/dev/null") { // Allow /dev/null for empty input testing
    std::cerr << "Source code is empty or file could not be read." << std::endl;
    return 1;
  }

  Lexer lexer(source_code);
  std::vector<Token> tokens = lexer.tokenize();

  for (const auto &token : tokens) {
    std::cout << "Token: " << tokenTypeToString(token.type) << ", Lexeme: '"
              << token.lexeme << "', Line: " << token.line
              << ", Column: " << token.column << std::endl;
  }

  return 0;
}