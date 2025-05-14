#include "../include/lexer.h"
#include <fstream>
#include <iostream>

std::string to_string(TokenType type) {
  switch (type) {
  case TokenType::IDENFR:
    return "IDENFR";
  case TokenType::INTCON:
    return "INTCON";
  case TokenType::STRCON:
    return "STRCON";
  case TokenType::MAINTK:
    return "MAINTK";
  case TokenType::CONSTTK:
    return "CONSTTK";
  case TokenType::INTTK:
    return "INTTK";
  case TokenType::BREAKTK:
    return "BREAKTK";
  case TokenType::CONTINUETK:
    return "CONTINUETK";
  case TokenType::IFTK:
    return "IFTK";
  case TokenType::ELSETK:
    return "ELSETK";
  case TokenType::WHILETK:
    return "WHILETK";
  case TokenType::GETINTTK:
    return "GETINTTK";
  case TokenType::PRINTFTK:
    return "PRINTFTK";
  case TokenType::RETURNTK:
    return "RETURNTK";
  case TokenType::VOIDTK:
    return "VOIDTK";
  case TokenType::NOT:
    return "NOT";
  case TokenType::AND:
    return "AND";
  case TokenType::OR:
    return "OR";
  case TokenType::PLUS:
    return "PLUS";
  case TokenType::MINU:
    return "MINU";
  case TokenType::MULT:
    return "MULT";
  case TokenType::DIV:
    return "DIV";
  case TokenType::MOD:
    return "MOD";
  case TokenType::LSS:
    return "LSS";
  case TokenType::LEQ:
    return "LEQ";
  case TokenType::GRE:
    return "GRE";
  case TokenType::GEQ:
    return "GEQ";
  case TokenType::EQL:
    return "EQL";
  case TokenType::NEQ:
    return "NEQ";
  case TokenType::ASSIGN:
    return "ASSIGN";
  case TokenType::SEMICN:
    return "SEMICN";
  case TokenType::COMMA:
    return "COMMA";
  case TokenType::LPARENT:
    return "LPARENT";
  case TokenType::RPARENT:
    return "RPARENT";
  case TokenType::LBRACK:
    return "LBRACK";
  case TokenType::RBRACK:
    return "RBRACK";
  case TokenType::LBRACE:
    return "LBRACE";
  case TokenType::RBRACE:
    return "RBRACE";
  case TokenType::ERROR:
    return "ERROR";
  }
  return "UNKNOWN";
}

int main() {
  std::ifstream file("../test/testfile1.txt");
  if (!file.is_open()) {
    std::cerr << "Error opening file" << std::endl;
    return 1;
  }
  std::string source((std::istreambuf_iterator<char>(file)),
                     std::istreambuf_iterator<char>());
  Lexer lexer(source);
  lexer.tokenize();
  for (auto &token : lexer.getTokens()) {
    std::cout << "Token: " << token.first
              << ", Type: " << to_string(token.second) << std::endl;
  }
  return 0;
}