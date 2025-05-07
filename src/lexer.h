#ifndef LEXER_H
#define LEXER_H

#include "token.h"
#include <string>
#include <vector>

class Lexer {
public:
  Lexer(const std::string &source);
  std::vector<Token> tokenize(); // 获取所有词法单元

private:
  std::string source_code;
  size_t current_pos;
  int current_line;
  int current_col; // 当前列相对于当前行的起始位置

  char currentChar() const;
  char peekChar() const; // 查看下一个字符，不移动指针
  void advance();        // 移动到下一个字符，并更新行列号

  Token getNextToken(); // 获取单个词法单元

  void skipWhitespace();
  void skipComments(); // 处理 // 和 /* */ 注释
  Token readIdentifierOrKeyword();
  Token readNumber();
  Token readFormatString();
  // 辅助函数来处理可能的多字符运算符，如 ==, !=, <=, >=, &&, ||
  Token processOperatorOrPunctuator();
};

#endif // LEXER_H