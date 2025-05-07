#include "ast.h" // Include AST nodes (needed for CompUnitNode and toString methods)
#include "lexer.h"
#include "parser.h"            // Include Parser
#include "semantic_analyzer.h" // <-- Added
#include "token.h"
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

  if (source_code.empty() && source_filepath != "/dev/null") {
    std::cerr << "Source code is empty or file could not be read." << std::endl;
    return 1;
  }

  std::cout << "--- Lexing ---" << std::endl;
  Lexer lexer(source_code);
  std::vector<Token> tokens;
  try {
    tokens = lexer.tokenize();
    // Optional: Print tokens for debugging lexer
    for (const auto &token : tokens) {
      std::cout << "Token: " << tokenTypeToString(token.type) << ", Lexeme: '"
                << token.lexeme << "', Line: " << token.line
                << ", Column: " << token.column << std::endl;
    }
  } catch (const std::runtime_error
               &e) { // Catch potential errors from lexer (e.g. NumberNode stoi)
    std::cerr << "Lexical Error: " << e.what() << std::endl;
    return 1;
  }
  for (const auto &token : tokens) {
    std::cout << "Token: " << tokenTypeToString(token.type) << ", Lexeme: '"
              << token.lexeme << "', Line: " << token.line
              << ", Column: " << token.column << std::endl;
  }
  std::cout << "Lexing completed. Number of tokens: " << tokens.size()
            << std::endl;
  std::cout << "\n--- Parsing ---" << std::endl;

  Parser parser(tokens);
  std::unique_ptr<CompUnitNode> ast_root = nullptr;
  try {
    ast_root = parser.parse();
    std::cout << "Parsing completed successfully." << std::endl;

    if (ast_root) {
      std::cout << "\n--- Abstract Syntax Tree (AST) ---" << std::endl;
      std::cout << ast_root->toString()
                << std::endl; // Keep or comment outas needed

      std::cout << "\n--- Semantic Analysis ---" << std::endl;
      SemanticAnalyzer semantic_analyzer;
      semantic_analyzer.analyze(ast_root.get()); // Pass raw pointer
      std::cout << "Semantic analysis completed successfully." << std::endl;

    } else {
      std::cerr << "Parsing finished, but AST root is null." << std::endl;
    }
  } catch (const ParseError &e) {
    std::cerr << "Syntax Error: " << e.what() << std::endl;
    return 1;                        // Indicate failure
  } catch (const SemanticError &e) { // <-- Catch SemanticError
    std::cerr << "Semantic Error: " << e.what() << std::endl;
    return 1;
  } catch (const std::runtime_error
               &e) { // Catch other potential runtime errors (e.g. from
                     // NumberNode stoi called during parsing)
    std::cerr << "Runtime Error: " << e.what() << std::endl;
    return 1;
  }

  return 0;
}