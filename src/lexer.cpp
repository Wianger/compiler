#include "lexer.h"
#include <cctype> // For isalpha, isdigit, isspace
#include <iostream>
#include <stdexcept> // For error reporting
#include <unordered_map>
// Keyword map
static std::unordered_map<std::string, TokenType> keywords = {
    {"const", TokenType::CONST_KW},
    {"int", TokenType::INT_KW},
    {"void", TokenType::VOID_KW},
    {"main", TokenType::MAIN_KW},
    {"if", TokenType::IF_KW},
    {"else", TokenType::ELSE_KW},
    {"while", TokenType::WHILE_KW},
    {"break", TokenType::BREAK_KW},
    {"continue", TokenType::CONTINUE_KW},
    {"return", TokenType::RETURN_KW},
    {"getint", TokenType::GETINT_KW},
    {"printf", TokenType::PRINTF_KW}};

Lexer::Lexer(const std::string &source)
    : source_code(source), current_pos(0), current_line(1), current_col(1) {}

char Lexer::currentChar() const {
  if (current_pos >= source_code.length()) {
    return '\0'; // End of input
  }
  return source_code[current_pos];
}

char Lexer::peekChar() const {
  if (current_pos + 1 >= source_code.length()) {
    return '\0';
  }
  return source_code[current_pos + 1];
}

void Lexer::advance() {
  if (currentChar() == '\n') {
    current_line++;
    current_col = 1;
  } else {
    current_col++;
  }
  current_pos++;
}

void Lexer::skipWhitespace() {
  while (current_pos < source_code.length() && std::isspace(currentChar())) {
    advance();
  }
}

void Lexer::skipComments() {
  // Single-line comment: // ...
  if (currentChar() == '/' && peekChar() == '/') {
    while (current_pos < source_code.length() && currentChar() != '\n') {
      advance();
    }
    // advance(); // consume the newline if not EOF
    return;
  }
  // Multi-line comment: /* ... */
  if (currentChar() == '/' && peekChar() == '*') {
    advance(); // consume /
    advance(); // consume *
    while (current_pos < source_code.length() &&
           !(currentChar() == '*' && peekChar() == '/')) {
      advance();
    }
    if (currentChar() == '*' && peekChar() == '/') {
      advance(); // consume *
      advance(); // consume /
    } else {
      // Unterminated comment, error or just stop? For now, let it be.
      // Consider throwing an error for unterminated comments in a real
      // compiler.
    }
  }
}

Token Lexer::readIdentifierOrKeyword() {
  std::string lexeme_str;
  int start_col = current_col;
  while (current_pos < source_code.length() &&
         (std::isalnum(static_cast<unsigned char>(currentChar())) ||
          currentChar() == '_')) {
    lexeme_str += currentChar();
    advance();
  }
  auto it = keywords.find(lexeme_str);
  if (it != keywords.end()) {
    return Token(it->second, lexeme_str, current_line, start_col);
  }
  return Token(TokenType::IDENT, lexeme_str, current_line, start_col);
}

Token Lexer::readNumber() {
  std::string num_str;
  int start_col = current_col;
  // For simplicity, SysY grammar only mentions IntConst, not floats or
  // hex/octal. It also doesn't explicitly mention negative numbers as a single
  // token, but '-' will be a separate token. Parser will handle unary minus.
  while (current_pos < source_code.length() && std::isdigit(currentChar())) {
    num_str += currentChar();
    advance();
  }
  return Token(TokenType::INT_CONST, num_str, current_line, start_col);
}

Token Lexer::readFormatString() {
  // FormatString → '"'{ Char }'"'
  // Char → FormatChar | NormalChar
  // FormatChar → %d
  // NormalChar → 十进制编码为32,33,40-126的ASCII字符
  std::string str_val;
  int start_line = current_line;
  int start_col = current_col;

  if (currentChar() != '"') { // Should not happen if called correctly
    return Token(TokenType::UNKNOWN, "", start_line, start_col);
  }
  advance(); // Consume the opening "

  while (current_pos < source_code.length() && currentChar() != '"') {
    char c = currentChar();
    if (c == '%') {
      if (peekChar() == 'd') {
        str_val += c;
        advance();
        str_val += currentChar();
        advance();
      } else {
        // Error: invalid format specifier or stray %
        // For now, consume it as a normal char if allowed, or error
        // According to grammar, only %d is a FormatChar. Other % would be
        // invalid if not NormalChar. The NormalChar rule doesn't include '%'.
        // This implies other % are errors. Let's treat a lone '%' as an error
        // for now by breaking or making an UNKNOWN token. For simplicity here,
        // we'll consume it and let it be part of the string, semantic analysis
        // can check validity later. Or make it UNKNOWN. Let's be strict: if not
        // %d, then it's not a valid char in format string as per current rules.
        // However, real printf allows literal % via %%. Grammar doesn't
        // specify. The NormalChar rule is also very specific (32,33,40-126).
        // For robustness, let's allow % as part of the string for now if it's
        // not %d. The grammar implies the content between " " is {Char}. If a
        // char is not FormatChar and not NormalChar, it's an error.
        str_val += c;
        advance();
      }
    } else {
      // Check if c is a NormalChar
      // NormalChar → decimal codes 32, 33, 40-126
      // 32: space, 33: !. 34: " (should be escaped or end string), 35-39: # $ %
      // & ' The grammar says FormatString -> '"' {Char} '"'". So " inside is
      // not directly allowed by {Char} if NormalChar doesn't include ".
      // Standard C allows \" for quote inside string. This grammar doesn't
      // specify. Let's assume for now no escaped quotes, string ends at next ".
      if (c == 32 || c == 33 || (c >= 40 && c <= 126)) {
        str_val += c;
        advance();
      } else if (c == '\n' ||
                 c == '\r') { // Unescaped newline in string literal
        // Error: unescaped newline in string literal
        // For now, report error or stop. Let's create an UNKNOWN token or
        // partial string. For simplicity, end string here.
        return Token(TokenType::UNKNOWN, str_val, start_line,
                     start_col); // Or throw error
      } else {
        // Invalid character in string literal as per grammar
        // For now, consume and add. Semantic phase might catch this.
        // Or make it an error immediately.
        // Let's be strict and error out for characters not in NormalChar or not
        // part of %d std::cerr << "Error: Invalid character in format string: "
        // << c << " at " << current_line << ":" << current_col << std::endl;
        // For lexer, just consume it or return error token.
        str_val += c; // For now, let's be lenient and add it.
        advance();
      }
    }
  }

  if (currentChar() == '"') {
    advance(); // Consume the closing "
    return Token(TokenType::FORMAT_STRING, str_val, start_line, start_col);
  } else {
    // Unterminated string literal
    // std::cerr << "Error: Unterminated format string at " << start_line << ":"
    // << start_col << std::endl;
    return Token(TokenType::UNKNOWN, str_val, start_line,
                 start_col); // Or throw error
  }
}

Token Lexer::processOperatorOrPunctuator() {
  int start_col = current_col;
  char c = currentChar();
  advance(); // Consume the character

  switch (c) {
  case '+':
    return Token(TokenType::PLUS, "+", current_line, start_col);
  case '-':
    return Token(TokenType::MINUS, "-", current_line, start_col);
  case '*':
    return Token(TokenType::MULTIPLY, "*", current_line, start_col);
  case '/':
    // Check for comments first, this might be done before calling this function
    // If not a comment, it's division
    return Token(TokenType::DIVIDE, "/", current_line, start_col);
  case '%':
    return Token(TokenType::MODULO, "%", current_line, start_col);
  case '=':
    if (currentChar() == '=') {
      advance();
      return Token(TokenType::EQ, "==", current_line, start_col);
    }
    return Token(TokenType::ASSIGN, "=", current_line, start_col);
  case '!':
    if (currentChar() == '=') {
      advance();
      return Token(TokenType::NEQ, "!=", current_line, start_col);
    }
    return Token(TokenType::LOGICAL_NOT, "!", current_line, start_col);
  case '<':
    if (currentChar() == '=') {
      advance();
      return Token(TokenType::LTE, "<=", current_line, start_col);
    }
    return Token(TokenType::LT, "<", current_line, start_col);
  case '>':
    if (currentChar() == '=') {
      advance();
      return Token(TokenType::GTE, ">=", current_line, start_col);
    }
    return Token(TokenType::GT, ">", current_line, start_col);
  case '&':
    if (currentChar() == '&') {
      advance();
      return Token(TokenType::LOGICAL_AND, "&&", current_line, start_col);
    }
    // Stray '&' is an error or unknown token based on grammar.
    return Token(TokenType::UNKNOWN, std::string(1, c), current_line,
                 start_col);
  case '|':
    if (currentChar() == '|') {
      advance();
      return Token(TokenType::LOGICAL_OR, "||", current_line, start_col);
    }
    // Stray '|' is an error or unknown token.
    return Token(TokenType::UNKNOWN, std::string(1, c), current_line,
                 start_col);
  case '(':
    return Token(TokenType::LPAREN, "(", current_line, start_col);
  case ')':
    return Token(TokenType::RPAREN, ")", current_line, start_col);
  case '[':
    return Token(TokenType::LBRACKET, "[", current_line, start_col);
  case ']':
    return Token(TokenType::RBRACKET, "]", current_line, start_col);
  case '{':
    return Token(TokenType::LBRACE, "{", current_line, start_col);
  case '}':
    return Token(TokenType::RBRACE, "}", current_line, start_col);
  case ',':
    return Token(TokenType::COMMA, ",", current_line, start_col);
  case ';':
    return Token(TokenType::SEMICOLON, ";", current_line, start_col);
  default:
    return Token(TokenType::UNKNOWN, std::string(1, c), current_line,
                 start_col);
  }
}

Token Lexer::getNextToken() {
  skipWhitespace();
  while (currentChar() == '/' && (peekChar() == '*' || peekChar() == '/')) {
    skipComments();
    skipWhitespace();
  }

  if (current_pos >= source_code.length()) {
    return Token(TokenType::END_OF_FILE, "", current_line, current_col);
  }

  char ch = currentChar();
  int start_col = current_col; // Store start column for the token

  if (std::isalpha(ch) || ch == '_') {
    return readIdentifierOrKeyword();
  }

  if (std::isdigit(ch)) {
    return readNumber();
  }

  if (ch == '"') {
    return readFormatString();
  }

  // Check for operators and punctuators
  // This needs to be robust for multi-char operators like '==', '&&', etc.
  switch (ch) {
  case '+':
  case '-':
  case '*':
  case '%': // single char ops or start of multi-char
  case '=':
  case '!':
  case '<':
  case '>': // potential multi-char
  case '&':
  case '|': // potential multi-char
  case '(':
  case ')':
  case '[':
  case ']':
  case '{':
  case '}':
  case ',':
  case ';':
  case '/': // Could be division or start of a comment
    return processOperatorOrPunctuator();

  default:
    // Unknown character
    advance(); // Consume it
    return Token(TokenType::UNKNOWN, std::string(1, ch), current_line,
                 start_col);
  }
}

std::vector<Token> Lexer::tokenize() {
  std::vector<Token> tokens;
  Token token = getNextToken();
  while (token.type != TokenType::END_OF_FILE) {
    tokens.push_back(token);
    token = getNextToken();
  }
  tokens.push_back(token); // Add EOF token
  return tokens;
}