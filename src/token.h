#ifndef TOKEN_H
#define TOKEN_H

#include <string>
#include <variant> // For token value if needed later

// 词法单元类型枚举
enum class TokenType {
  // Keywords 关键字
  CONST_KW,    // const
  INT_KW,      // int
  VOID_KW,     // void
  MAIN_KW,     // main
  IF_KW,       // if
  ELSE_KW,     // else
  WHILE_KW,    // while
  BREAK_KW,    // break
  CONTINUE_KW, // continue
  RETURN_KW,   // return
  GETINT_KW,   // getint
  PRINTF_KW,   // printf

  // Identifiers 标识符
  IDENT,

  // Literals 字面量
  INT_CONST,     // 整数常量
  FORMAT_STRING, // 格式化字符串

  // Operators 运算符
  PLUS,        // +
  MINUS,       // -
  MULTIPLY,    // *
  DIVIDE,      // /
  MODULO,      // %
  ASSIGN,      // =
  EQ,          // ==
  NEQ,         // !=
  LT,          // < (less than)
  GT,          // > (greater than)
  LTE,         // <= (less than or equal)
  GTE,         // >= (greater than or equal)
  LOGICAL_AND, // &&
  LOGICAL_OR,  // ||
  LOGICAL_NOT, // !

  // Punctuators 分隔符/标点符号
  LPAREN,    // (
  RPAREN,    // )
  LBRACKET,  // [
  RBRACKET,  // ]
  LBRACE,    // {
  RBRACE,    // }
  COMMA,     // ,
  SEMICOLON, // ;

  // End Of File 文件结束符
  END_OF_FILE,

  // Unknown/Error 未知/错误
  UNKNOWN
};

// 将TokenType转换为字符串，方便调试
std::string tokenTypeToString(TokenType type);

// 词法单元结构体
struct Token {
  TokenType type;
  std::string lexeme; // 词素，即原始文本
  int line;           // 所在行号
  int column;         // 所在列号

  Token(TokenType t, std::string l, int ln, int col)
      : type(t), lexeme(std::move(l)), line(ln), column(col) {}
};

#endif // TOKEN_H