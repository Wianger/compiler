#include <fstream>
#include <iostream>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <vector>

class LexicalAnalysis {
public:
  // 构造函数和公共方法
  LexicalAnalysis(const std::string &filePath);
  std::vector<std::pair<std::string, std::string>> lexicalanalysis();

  // 错误处理
  struct LexicalError {
    std::string message;
    int line;
    int column;
  };

  // 获取所有收集到的错误
  std::vector<LexicalError> getErrors() const;

private:
  // 基本属性
  std::string code;                                       // 源代码
  std::unordered_map<std::string, std::string> keywords;  // 关键字表
  std::unordered_map<std::string, std::string> operators; // 运算符表
  int currentLine = 1;                                    // 当前行号
  int currentColumn = 1;                                  // 当前列号
  std::vector<LexicalError> errors;                       // 词法错误列表

  // 工具方法
  std::string fileToString(const std::string &filePath);
  void initKeywords(std::unordered_map<std::string, std::string> &keywords);
  void initOperators(std::unordered_map<std::string, std::string> &operators);
  void updatePosition(char c);
  void reportError(const std::string &message);

  // 词法分析辅助方法
  std::pair<std::string, std::string>
  processIdentifier(std::string::iterator &ch);
  std::pair<std::string, std::string> processNumber(std::string::iterator &ch);
  std::pair<std::string, std::string> processString(std::string::iterator &ch);
  std::pair<std::string, std::string>
  processOperator(std::string::iterator &ch);
  bool processComment(std::string::iterator &ch);
};

// 构造函数
LexicalAnalysis::LexicalAnalysis(const std::string &filePath) {
  code = fileToString(filePath);
  initKeywords(keywords);
  initOperators(operators);
}

// 读取文件内容到字符串
std::string LexicalAnalysis::fileToString(const std::string &filePath) {
  std::ifstream file(filePath, std::ios::binary);
  if (!file) {
    std::cout << "File not found: " << filePath << std::endl;
    exit(1);
  }
  return std::string((std::istreambuf_iterator<char>(file)),
                     std::istreambuf_iterator<char>());
}

// 初始化关键字表
void LexicalAnalysis::initKeywords(
    std::unordered_map<std::string, std::string> &keywords) {
  keywords["main"] = "MAINTK";
  keywords["const"] = "CONSTTK";
  keywords["int"] = "INTTK";
  keywords["break"] = "BREAKTK";
  keywords["continue"] = "CONTINUETK";
  keywords["if"] = "IFTK";
  keywords["else"] = "ELSETK";
  keywords["while"] = "WHILETK";
  keywords["getint"] = "GETINTTK";
  keywords["printf"] = "PRINTFTK";
  keywords["return"] = "RETURNTK";
  keywords["void"] = "VOIDTK";
}

// 初始化操作符表
void LexicalAnalysis::initOperators(
    std::unordered_map<std::string, std::string> &operators) {
  operators["="] = "ASSIGN";
  operators["=="] = "EQL";
  operators["!="] = "NEQ";
  operators["<"] = "LSS";
  operators["<="] = "LEQ";
  operators[">"] = "GRE";
  operators[">="] = "GEQ";
  operators["+"] = "PLUS";
  operators["-"] = "MINU";
  operators["*"] = "MULT";
  operators["/"] = "DIV";
  operators["%"] = "MOD";
  operators["&&"] = "AND";
  operators["||"] = "OR";
  operators["!"] = "NOT";
  operators[";"] = "SEMICN";
  operators[","] = "COMMA";
  operators["("] = "LPARENT";
  operators[")"] = "RPARENT";
  operators["["] = "LBRACK";
  operators["]"] = "RBRACK";
  operators["{"] = "LBRACE";
  operators["}"] = "RBRACE";
}

// 行列位置更新
void LexicalAnalysis::updatePosition(char c) {
  if (c == '\n') {
    currentLine++;
    currentColumn = 1;
  } else {
    currentColumn++;
  }
}

// 错误报告
void LexicalAnalysis::reportError(const std::string &message) {
  LexicalError error{message, currentLine, currentColumn};
  errors.push_back(error);
  throw error;
}

// 获取所有错误
std::vector<LexicalAnalysis::LexicalError> LexicalAnalysis::getErrors() const {
  return errors;
}

// 处理标识符
std::pair<std::string, std::string>
LexicalAnalysis::processIdentifier(std::string::iterator &ch) {
  std::string buffer;

  // 收集标识符字符
  do {
    buffer += *ch;
    updatePosition(*ch);
    ++ch;
  } while (ch != code.end() && (isalnum(*ch) || *ch == '_'));

  // 检查是否是关键字
  std::pair<std::string, std::string> token;
  if (keywords.find(buffer) != keywords.end()) {
    token.first = keywords[buffer];
  } else {
    token.first = "IDENFR";
  }
  token.second = buffer;

  return token;
}

// 处理数字
std::pair<std::string, std::string>
LexicalAnalysis::processNumber(std::string::iterator &ch) {
  std::string buffer;

  // 收集数字字符
  do {
    buffer += *ch;
    updatePosition(*ch);
    ++ch;
  } while (ch != code.end() && isdigit(*ch));

  std::pair<std::string, std::string> token;
  token.first = "INTCON";
  token.second = buffer;

  return token;
}

// 处理字符串
std::pair<std::string, std::string>
LexicalAnalysis::processString(std::string::iterator &ch) {
  std::string buffer;

  // 添加开始引号
  buffer += *ch;
  updatePosition(*ch);
  ++ch;

  // 处理字符串内容
  while (ch != code.end() && *ch != '"') {
    if (*ch == '\\' && ch + 1 != code.end()) {
      // 处理转义序列
      updatePosition(*ch);
      ++ch;

      switch (*ch) {
      case 'n':
        buffer += "\\n";
        break;
      case 't':
        buffer += "\\t";
        break;
      case '\\':
        buffer += "\\\\";
        break;
      case '"':
        buffer += "\\\"";
        break;
      default:
        reportError("Invalid escape sequence");
      }
    } else {
      buffer += *ch;
    }

    updatePosition(*ch);
    ++ch;
  }

  // 检查字符串是否正确结束
  if (ch == code.end()) {
    reportError("Unclosed string");
  }

  // 添加结束引号
  buffer += *ch;
  updatePosition(*ch);
  ++ch;

  std::pair<std::string, std::string> token;
  token.first = "STRCON";
  token.second = buffer;

  return token;
}

// 处理操作符
std::pair<std::string, std::string>
LexicalAnalysis::processOperator(std::string::iterator &ch) {
  std::string buffer;

  // 先尝试双字符操作符
  buffer += *ch;
  updatePosition(*ch);
  ++ch;

  if (ch != code.end()) {
    std::string twoCharOp = buffer + *ch;
    if (operators.find(twoCharOp) != operators.end()) {
      buffer = twoCharOp;
      updatePosition(*ch);
      ++ch;
    }
  }

  // 检查操作符是否有效
  std::pair<std::string, std::string> token;
  if (operators.find(buffer) != operators.end()) {
    token.first = operators[buffer];
    token.second = buffer;
  } else {
    reportError("Unknown operator: " + buffer);
  }

  return token;
}

// 处理注释
bool LexicalAnalysis::processComment(std::string::iterator &ch) {
  // 确保有足够的字符判断是否为注释
  if (ch + 1 == code.end() || *(ch + 1) != '/' && *(ch + 1) != '*') {
    return false;
  }

  if (*(ch + 1) == '/') {
    // 单行注释
    updatePosition(*ch);
    ++ch;
    updatePosition(*ch);
    ++ch;

    // 跳过整行
    while (ch != code.end() && *ch != '\n') {
      updatePosition(*ch);
      ++ch;
    }

    return true;
  } else { // *(ch + 1) == '*'
    // 多行注释
    updatePosition(*ch);
    ++ch;
    updatePosition(*ch);
    ++ch;

    // 寻找注释结束
    bool commentClosed = false;
    while (ch != code.end() && !commentClosed) {
      if (*ch == '*' && ch + 1 != code.end() && *(ch + 1) == '/') {
        updatePosition(*ch);
        ++ch;
        updatePosition(*ch);
        ++ch;
        commentClosed = true;
      } else {
        updatePosition(*ch);
        ++ch;
      }
    }

    if (!commentClosed) {
      reportError("未闭合的多行注释");
    }

    return true;
  }
}

// 主词法分析函数
std::vector<std::pair<std::string, std::string>>
LexicalAnalysis::lexicalanalysis() {
  std::vector<std::pair<std::string, std::string>> tokens;

  for (auto ch = code.begin(); ch != code.end();) {
    // 跳过空白字符
    if (isspace(*ch)) {
      updatePosition(*ch);
      ++ch;
      continue;
    }

    try {
      std::pair<std::string, std::string> token;
      bool tokenGenerated = true;

      // 根据首字符类型选择处理方法
      if (isdigit(*ch)) {
        token = processNumber(ch);
      } else if (isalpha(*ch) || *ch == '_') {
        token = processIdentifier(ch);
      } else if (*ch == '"') {
        token = processString(ch);
      } else if (*ch == '/') {
        // 可能是注释或除法操作符
        if (processComment(ch)) {
          tokenGenerated = false;
        } else {
          token = processOperator(ch);
        }
      } else {
        // 其他操作符
        token = processOperator(ch);
      }

      if (tokenGenerated) {
        tokens.push_back(token);
      }
    } catch (const LexicalError &err) {
      // 已通过reportError添加到errors列表，这里只需恢复分析
      // 跳过到下一个可能的有效token起始位置
      while (ch != code.end() && !isspace(*ch)) {
        updatePosition(*ch);
        ++ch;
      }
    }
  }

  return tokens;
}

class SyntaxAnalysis {
public:
  // 构造函数
  SyntaxAnalysis() = default;

  // 公共方法
  void syntaxAnalysis();
  void
  setTokens(const std::vector<std::pair<std::string, std::string>> &tokens);

  // 错误处理结构
  struct SyntaxError {
    std::string message;
    int position; // token在序列中的位置
  };

  // 获取分析过程中收集的所有错误
  std::vector<SyntaxError> getErrors() const;

private:
  // 基础数据
  std::vector<std::pair<std::string, std::string>> tokens;
  std::vector<std::pair<std::string, std::string>>::iterator it;
  std::vector<SyntaxError> errors;
  int currentPosition = 0;

  // 错误处理方法
  void reportError(const std::string &message);
  void synchronize(); // 错误恢复

  // 辅助方法
  bool match(const std::string &expected);
  bool matchType(const std::string &expectedType);
  bool lookAhead(int steps, const std::string &expected);
  bool lookAheadType(int steps, const std::string &expected);
  void consume(const std::string &expected);
  void consumeType(const std::string &expectedType);

  // 语法分析方法
  void CompUnit();  // 编译单元 CompUnit → {Decl} {FuncDef} MainFuncDef
  void Decl();      // 声明 Decl → ConstDecl | VarDecl
  void ConstDecl(); // 常量声明 ConstDecl → 'const' BType ConstDef { ','
                    // ConstDef } ';'
  void BType();     // 基本类型 BType → 'int'
  void
  ConstDef(); // 常数定义 ConstDef → Ident [ '[' ConstExp ']' ] '=' ConstInitVal
  void ConstInitVal(); // 常量初值 ConstInitVal → ConstExp | '{' [ ConstInitVal
                       // { ',' ConstInitVal } ] '}'
  void VarDecl();      // 变量声明 VarDecl → BType VarDef { ',' VarDef } ';'
  void VarDef(); // 变量定义 VarDef → Ident [ '[' ConstExp ']' ] | Ident [ '['
                 // ConstExp ']' ] '=' InitVal
  void
  InitVal(); // 变量初值 InitVal → Exp | '{' [ InitVal { ',' InitVal } ] '}'
  void
  FuncDef(); // 函数定义 FuncDef → FuncType Ident '(' [FuncFParams] ')' Block
  void MainFuncDef(); // 主函数定义 MainFuncDef → 'int' 'main' '(' ')' Block
  void FuncType();    // 函数类型 FuncType → 'void' | 'int'
  void FuncFParams(); // 函数形参表 FuncFParams → FuncFParam { ',' FuncFParam }
  void FuncFParam();  // 函数形参 FuncFParam → BType Ident
  void Block();       // 语句块 Block → '{' { BlockItem } '}'
  void BlockItem();   // 语句块项 BlockItem → Decl | Stmt
  void Stmt(); // 语句 Stmt → LVal '=' Exp ';' | [Exp] ';' | Block | 'if' '(
               // Cond ')' Stmt [ 'else' Stmt ] | 'while' '(' Cond ')' Stmt |
               // 'break' ';' | 'continue' ';' | 'return' [Exp] ';' | LVal =
               // 'getint''('')'';' | 'printf' '('FormatString {',' Exp} ')'';'
  void Exp();  // 表达式 Exp → AddExp
  void Cond(); // 条件表达式 Cond → LOrExp
  void LVal(); // 左值表达式 LVal → Ident ['[' Exp ']']
  void PrimaryExp(); // 基本表达式 PrimaryExp → '(' Exp ')' | LVal | Number
  void Number();     // 数值 Number → IntConst
  void UnaryExp(); // 一元表达式 UnaryExp → PrimaryExp | Ident '(' [FuncRParams]
                   // ')' | UnaryOp UnaryExp
  void UnaryOp();  // 单目运算符 UnaryOp → '+' | '−' | '!'
  void FuncRParams(); // 函数实参表 FuncRParams → Exp { ',' Exp }
  void MulExp();   // 乘除模表达式 MulExp → UnaryExp | MulExp ('*' | '/' | '%')
                   // UnaryExp
  void AddExp();   // 加减表达式 AddExp → MulExp | AddExp ('+' | '−') MulExp
  void RelExp();   // 关系表达式 RelExp → AddExp | RelExp ('<' | '>' | '<=' |
                   // '>=') AddExp
  void EqExp();    // 相等性表达式 EqExp → RelExp | EqExp ('==' | '!=') RelExp
  void LAndExp();  // 逻辑与表达式 LAndExp → EqExp | LAndExp '&&' EqExp
  void LOrExp();   // 逻辑或表达式 LOrExp → LAndExp | LOrExp '||' LAndExp
  void ConstExp(); // 常量表达式 ConstExp → AddExp
  void FormatString(); // 格式化字符串 FormatString → '"'{ Char }'"'
};

#define OUTPUT_SYNTAX_COMPONENT

// 全局输出文件流
std::ofstream outputFile("output.txt");

// 设置词法分析器产生的Token序列
void SyntaxAnalysis::setTokens(
    const std::vector<std::pair<std::string, std::string>> &tokens) {
  this->tokens = tokens;
  this->it = this->tokens.begin();
  this->currentPosition = 0;
  this->errors.clear();
}

// 获取分析过程中收集的所有错误
std::vector<SyntaxAnalysis::SyntaxError> SyntaxAnalysis::getErrors() const {
  return errors;
}

// 报告语法错误
void SyntaxAnalysis::reportError(const std::string &message) {
  SyntaxError error{message, currentPosition};
  errors.push_back(error);

#ifdef DEBUG
  std::cerr << "语法错误（位置" << currentPosition << "）：" << message
            << std::endl;
#endif
}

// 错误恢复 - 寻找下一个可能的同步点
void SyntaxAnalysis::synchronize() {
  // 向前查找分号或者右大括号作为同步点
  while (it != tokens.end()) {
    if (it->second == ";" || it->second == "}") {
      ++it;
      ++currentPosition;
      return;
    }
    ++it;
    ++currentPosition;
  }
}

// 匹配当前token是否为预期值
bool SyntaxAnalysis::match(const std::string &expected) {
  if (it != tokens.end() && it->second == expected) {
    return true;
  }
  return false;
}

// 匹配当前token类型
bool SyntaxAnalysis::matchType(const std::string &expectedType) {
  if (it != tokens.end() && it->first == expectedType) {
    return true;
  }
  return false;
}

// 向前看n步，检查token是否为预期值
bool SyntaxAnalysis::lookAhead(int steps, const std::string &expected) {
  if (it + steps < tokens.end() && (it + steps)->second == expected) {
    return true;
  }
  return false;
}

bool SyntaxAnalysis::lookAheadType(int steps, const std::string &expected) {
  if (it + steps < tokens.end() && (it + steps)->first == expected) {
    return true;
  }
  return false;
}

// 消耗一个预期的token，如果不匹配则报错
void SyntaxAnalysis::consume(const std::string &expected) {
  if (match(expected)) {
#ifdef OUTPUT_SYNTAX_COMPONENT
    outputFile << it->first << " " << it->second << std::endl;
#endif
    ++it;
    ++currentPosition;
  } else {
    reportError("Expecteing '" + expected + "', but found '" +
                (it != tokens.end() ? it->second : "EOF") + "'");
  }
}

void SyntaxAnalysis::consumeType(const std::string &expectedType) {
  if (matchType(expectedType)) {
#ifdef OUTPUT_SYNTAX_COMPONENT
    outputFile << it->first << " " << it->second << std::endl;
#endif
    ++it;
    ++currentPosition;
  } else {
    reportError("Expecteing '" + expectedType + "', but found '" +
                (it != tokens.end() ? it->first : "EOF") + "'");
  }
}

// 语法分析主函数
void SyntaxAnalysis::syntaxAnalysis() {
  try {
    CompUnit();
    if (it != tokens.end()) {
      reportError("There are additional tags after the compilation unit");
    }
  } catch (const std::exception &e) {
    reportError("Syntax analysis interrupted: " + std::string(e.what()));
  }
}

// 编译单元 CompUnit → {Decl} {FuncDef} MainFuncDef
void SyntaxAnalysis::CompUnit() {
  // 处理声明
  while (it != tokens.end() &&
         (match("const") ||
          (match("int") && lookAheadType(1, "IDENFR") && !lookAhead(2, "(")))) {
    try {
      Decl();
    } catch (const std::exception &) {
      synchronize();
    }
  }

  // 处理函数定义
  while (it != tokens.end() &&
         (match("void") || (match("int") && lookAheadType(1, "IDENFR") &&
                            lookAhead(2, "(") && !lookAhead(1, "main")))) {
    try {
      FuncDef();
    } catch (const std::exception &) {
      synchronize();
    }
  }

  // 处理主函数
  try {
    MainFuncDef();
  } catch (const std::exception &) {
    synchronize();
  }

#ifdef OUTPUT_SYNTAX_COMPONENT
  outputFile << "<CompUnit>" << std::endl;
#endif
}

// 声明 Decl → ConstDecl | VarDecl
void SyntaxAnalysis::Decl() {
  if (match("const")) {
    ConstDecl();
  } else {
    VarDecl();
  }
}

// 常量声明 ConstDecl → 'const' BType ConstDef { ',' ConstDef } ';'
void SyntaxAnalysis::ConstDecl() {
  // 匹配 'const'
  consume("const");

  // 匹配 BType
  BType();

  // 匹配 ConstDef
  ConstDef();

  // 匹配 { ',' ConstDef }
  while (match(",")) {
    consume(",");
    ConstDef();
  }

  // 匹配 ';'
  if (match(";")) {
    consume(";");
  } else {
    reportError("Constant declaration missing ';'");
  }

#ifdef OUTPUT_SYNTAX_COMPONENT
  outputFile << "<ConstDecl>" << std::endl;
#endif
}

// 基本类型 BType → 'int'
void SyntaxAnalysis::BType() {
  if (match("int")) {
    consume("int");
  } else {
    reportError("Expected type 'int'");
  }
}

// 常数定义 ConstDef → Ident [ '[' ConstExp ']' ] '=' ConstInitVal
void SyntaxAnalysis::ConstDef() {
  // 匹配 Ident
  if (matchType("IDENFR")) {
    consumeType("IDENFR");
  } else {
    reportError("Expected identifier");
    return;
  }

  // 匹配可选的 [ '[' ConstExp ']' ]
  if (match("[")) {
    consume("[");
    ConstExp();

    if (match("]")) {
      consume("]");
    } else {
      reportError("Array definition missing ']'");
    }
  }

  // 匹配 '='
  if (match("=")) {
    consume("=");
  } else {
    reportError("Constant definition missing '='");
    return;
  }

  // 匹配 ConstInitVal
  ConstInitVal();

#ifdef OUTPUT_SYNTAX_COMPONENT
  outputFile << "<ConstDef>" << std::endl;
#endif
}

// 常量初值 ConstInitVal → ConstExp | '{' [ ConstInitVal { ',' ConstInitVal } ]
// '}'
void SyntaxAnalysis::ConstInitVal() {
  if (match("{")) {
    consume("{");

    if (!match("}")) {
      ConstInitVal();

      while (match(",")) {
        consume(",");
        ConstInitVal();
      }
    }

    if (match("}")) {
      consume("}");
    } else {
      reportError("Constant initialization list missing '}'");
    }
  } else {
    ConstExp();
  }

#ifdef OUTPUT_SYNTAX_COMPONENT
  outputFile << "<ConstInitVal>" << std::endl;
#endif
}

// 变量声明 VarDecl → BType VarDef { ',' VarDef } ';'
void SyntaxAnalysis::VarDecl() {
  // 匹配 BType
  BType();

  // 匹配 VarDef
  VarDef();

  // 匹配 { ',' VarDef }
  while (match(",")) {
    consume(",");
    VarDef();
  }

  // 匹配 ';'
  if (match(";")) {
    consume(";");
  } else {
    reportError("Variable declaration missing ';'");
  }

#ifdef OUTPUT_SYNTAX_COMPONENT
  outputFile << "<VarDecl>" << std::endl;
#endif
}

// 变量定义 VarDef → Ident [ '[' ConstExp ']' ] | Ident [ '[' ConstExp ']' ] '='
// InitVal
void SyntaxAnalysis::VarDef() {
  // 匹配 Ident
  if (matchType("IDENFR")) {
    consumeType("IDENFR");
  } else {
    reportError("Expected identifier");
    return;
  }

  // 匹配可选的 [ '[' ConstExp ']' ]
  if (match("[")) {
    consume("[");
    ConstExp();

    if (match("]")) {
      consume("]");
    } else {
      reportError("Array definition missing ']'");
    }
  }

  // 匹配可选的 '=' InitVal
  if (match("=")) {
    consume("=");
    InitVal();
  }

#ifdef OUTPUT_SYNTAX_COMPONENT
  outputFile << "<VarDef>" << std::endl;
#endif
}

// 变量初值 InitVal → Exp | '{' [ InitVal { ',' InitVal } ] '}'
void SyntaxAnalysis::InitVal() {
  if (match("{")) {
    consume("{");

    if (!match("}")) {
      InitVal();

      while (match(",")) {
        consume(",");
        InitVal();
      }
    }

    if (match("}")) {
      consume("}");
    } else {
      reportError("Initialization list missing '}'");
    }
  } else {
    Exp();
  }

#ifdef OUTPUT_SYNTAX_COMPONENT
  outputFile << "<InitVal>" << std::endl;
#endif
}

// 函数定义 FuncDef → FuncType Ident '(' [FuncFParams] ')' Block
void SyntaxAnalysis::FuncDef() {
  // 匹配 FuncType
  FuncType();

  // 匹配 Ident
  if (matchType("IDENFR")) {
    consumeType("IDENFR");
  } else {
    reportError("Expected function name");
    return;
  }

  // 匹配 '('
  if (match("(")) {
    consume("(");
  } else {
    reportError("Function definition missing '('");
    return;
  }

  // 匹配可选的 FuncFParams
  if (!match(")")) {
    FuncFParams();
  }

  // 匹配 ')'
  if (match(")")) {
    consume(")");
  } else {
    reportError("Function definition missing ')'");
    return;
  }

  // 匹配 Block
  Block();

#ifdef OUTPUT_SYNTAX_COMPONENT
  outputFile << "<FuncDef>" << std::endl;
#endif
}

// 主函数定义 MainFuncDef → 'int' 'main' '(' ')' Block
void SyntaxAnalysis::MainFuncDef() {
  // 匹配 'int'
  if (match("int")) {
    consume("int");
  } else {
    reportError("The main function should start with 'int'");
    return;
  }

  // 匹配 'main'
  if (match("main")) {
    consume("main");
  } else {
    reportError("Missing 'main'");
    return;
  }

  // 匹配 '('
  if (match("(")) {
    consume("(");
  } else {
    reportError("The main function missing '('");
    return;
  }

  // 匹配 ')'
  if (match(")")) {
    consume(")");
  } else {
    reportError("The main function missing ')'");
    return;
  }

  // 匹配 Block
  Block();

#ifdef OUTPUT_SYNTAX_COMPONENT
  outputFile << "<MainFuncDef>" << std::endl;
#endif
}

// 函数类型 FuncType → 'void' | 'int'
void SyntaxAnalysis::FuncType() {
  if (match("void") || match("int")) {
    consume(match("void") ? "void" : "int");
  } else {
    reportError("Expected function type 'void' or 'int'");
  }

#ifdef OUTPUT_SYNTAX_COMPONENT
  outputFile << "<FuncType>" << std::endl;
#endif
}

// 函数形参表 FuncFParams → FuncFParam { ',' FuncFParam }
void SyntaxAnalysis::FuncFParams() {
  // 匹配 FuncFParam
  FuncFParam();

  // 匹配 { ',' FuncFParam }
  while (match(",")) {
    consume(",");
    FuncFParam();
  }

#ifdef OUTPUT_SYNTAX_COMPONENT
  outputFile << "<FuncFParams>" << std::endl;
#endif
}

// 函数形参 FuncFParam → BType Ident
void SyntaxAnalysis::FuncFParam() {
  // 匹配 BType
  BType();

  // 匹配 Ident
  if (matchType("IDENFR")) {
    consumeType("IDENFR");
  } else {
    reportError("Expected parameter identifier");
  }

#ifdef OUTPUT_SYNTAX_COMPONENT
  outputFile << "<FuncFParam>" << std::endl;
#endif
}

// 语句块 Block → '{' { BlockItem } '}'
void SyntaxAnalysis::Block() {
  // 匹配 '{'
  if (match("{")) {
    consume("{");
  } else {
    reportError("Expected '{' to start block");
    return;
  }

  // 匹配 { BlockItem }
  while (it != tokens.end() && !match("}")) {
    try {
      BlockItem();
    } catch (const std::exception &) {
      synchronize();
    }
  }

  // 匹配 '}'
  if (match("}")) {
    consume("}");
  } else {
    reportError("Block missing '}'");
  }

#ifdef OUTPUT_SYNTAX_COMPONENT
  outputFile << "<Block>" << std::endl;
#endif
}

// 语句块项 BlockItem → Decl | Stmt
void SyntaxAnalysis::BlockItem() {
  if (match("const") || match("int")) {
    Decl();
  } else {
    Stmt();
  }
}

// 语句 Stmt → LVal '=' Exp ';' | [Exp] ';' | Block | 'if' '(' Cond ')' Stmt [
// 'else' Stmt ] | 'while' '(' Cond ')' Stmt |
//            'break' ';' | 'continue' ';' | 'return' [Exp] ';' | LVal =
//            'getint''('')'';' | 'printf' '('FormatString {',' Exp} ')'';'
void SyntaxAnalysis::Stmt() {
  if (it == tokens.end()) {
    reportError("Unexpected statement end");
    return;
  }

  if (match("if")) {
    // 处理 if 语句
    consume("if");

    if (match("(")) {
      consume("(");
      Cond();

      if (match(")")) {
        consume(")");
        Stmt();

        if (match("else")) {
          consume("else");
          Stmt();
        }
      } else {
        reportError("if statement missing ')'");
      }
    } else {
      reportError("if statement missing '('");
    }
  } else if (match("while")) {
    // 处理 while 语句
    consume("while");

    if (match("(")) {
      consume("(");
      Cond();

      if (match(")")) {
        consume(")");
        Stmt();
      } else {
        reportError("while statement missing ')'");
      }
    } else {
      reportError("while statement missing '('");
    }
  } else if (match("break") || match("continue")) {
    // 处理 break 和 continue 语句
    consume(match("break") ? "break" : "continue");
    if (match(";")) {
      consume(";");
    } else {
      reportError("break statement missing ';'");
    }
  } else if (match("return")) {
    // 处理 return 语句
    consume("return");

    if (!match(";")) {
      Exp();
    }

    if (match(";")) {
      consume(";");
    } else {
      reportError("return statement missing ';'");
    }
  } else if (match("printf")) {
    // 处理 printf 语句
    consume("printf");

    if (match("(")) {
      consume("(");

      if (matchType("STRCON")) {
        FormatString();

        while (match(",")) {
          consume(",");
          Exp();
        }

        if (match(")")) {
          consume(")");

          if (match(";")) {
            consume(";");
          } else {
            reportError("printf statement missing ';'");
          }
        } else {
          reportError("printf statement missing ')'");
        }
      } else {
        reportError("printf statement needs format string");
      }
    } else {
      reportError("printf statement missing '('");
    }
  } else if (match("{")) {
    // 处理语句块
    Block();
  } else if (match(";")) {
    consume(";");
  } else {
    // 处理赋值语句或表达式语句
    auto tempIt = it;
    auto tempPos = currentPosition;
    bool isAssign = false;

    // 尝试解析左值表达式
    try {
      LVal();

      if (match("=")) {
        isAssign = true;
        consume("=");

        if (match("getint")) {
          consume("getint");

          if (match("(")) {
            consume("(");

            if (match(")")) {
              consume(")");

              if (match(";")) {
                consume(";");
              } else {
                reportError("getint statement missing ';'");
              }
            } else {
              reportError("getint statement missing ')'");
            }
          } else {
            reportError("getint statement missing '('");
          }
        } else {
          Exp();

          if (match(";")) {
            consume(";");
          } else {
            reportError("Assignment statement missing ';'");
          }
        }
      }
    } catch (const std::exception &) {
      // 解析左值失败，回溯
      it = tempIt;
      currentPosition = tempPos;
    }

    if (!isAssign) {
      // 处理可选的表达式语句
      if (!match(";")) {
        Exp();
      }

      if (match(";")) {
        consume(";");
      } else {
        reportError("Expression statement missing ';'");
      }
    }
  }

#ifdef OUTPUT_SYNTAX_COMPONENT
  outputFile << "<Stmt>" << std::endl;
#endif
}

// 表达式 Exp → AddExp
void SyntaxAnalysis::Exp() {
  AddExp();

#ifdef OUTPUT_SYNTAX_COMPONENT
  outputFile << "<Exp>" << std::endl;
#endif
}

// 条件表达式 Cond → LOrExp
void SyntaxAnalysis::Cond() {
  LOrExp();

#ifdef OUTPUT_SYNTAX_COMPONENT
  outputFile << "<Cond>" << std::endl;
#endif
}

// 左值表达式 LVal → Ident ['[' Exp ']']
void SyntaxAnalysis::LVal() {
  if (matchType("IDENFR")) {
    consumeType("IDENFR");

    if (match("[")) {
      consume("[");
      Exp();

      if (match("]")) {
        consume("]");
      } else {
        reportError("Array access missing ']'");
      }
    }
  } else {
    reportError("Expected identifier");
  }

#ifdef OUTPUT_SYNTAX_COMPONENT
  outputFile << "<LVal>" << std::endl;
#endif
}

// 基本表达式 PrimaryExp → '(' Exp ')' | LVal | Number
void SyntaxAnalysis::PrimaryExp() {
  if (it == tokens.end()) {
    reportError("Unexpected expression end");
    return;
  }

  if (match("(")) {
    consume("(");
    Exp();

    if (match(")")) {
      consume(")");
    } else {
      reportError("Expression missing ')'");
    }
  } else if (matchType("INTCON")) {
    Number();
  } else {
    LVal();
  }

#ifdef OUTPUT_SYNTAX_COMPONENT
  outputFile << "<PrimaryExp>" << std::endl;
#endif
}

// 数值 Number → IntConst
void SyntaxAnalysis::Number() {
  if (matchType("INTCON")) {
    consumeType("INTCON");
  } else {
    reportError("Expected integer constant");
  }

#ifdef OUTPUT_SYNTAX_COMPONENT
  outputFile << "<Number>" << std::endl;
#endif
}

// 一元表达式 UnaryExp → PrimaryExp | Ident '(' [FuncRParams] ')' | UnaryOp
// UnaryExp
void SyntaxAnalysis::UnaryExp() {
  if (it == tokens.end()) {
    reportError("Unexpected expression end");
    return;
  }

  if (match("+") || match("-") || match("!")) {
    UnaryOp();
    UnaryExp();
  } else if (matchType("IDENFR")) {
    auto tempIt = it;
    auto tempPos = currentPosition;

    if (lookAhead(1, "(")) {
      // 函数调用
      consumeType("IDENFR");
      consume("(");

      if (!match(")")) {
        FuncRParams();
      }

      if (match(")")) {
        consume(")");
      } else {
        reportError("Function call missing ')'");
      }
    } else {
      // 不是函数调用，回溯
      it = tempIt;
      currentPosition = tempPos;
      PrimaryExp();
    }
  } else {
    PrimaryExp();
  }

#ifdef OUTPUT_SYNTAX_COMPONENT
  outputFile << "<UnaryExp>" << std::endl;
#endif
}

// 单目运算符 UnaryOp → '+' | '−' | '!'
void SyntaxAnalysis::UnaryOp() {
  if (match("+") || match("-") || match("!")) {
    consume(match("+") ? "+" : match("-") ? "-" : "!");
  } else {
    reportError("Expected unary operator");
  }

#ifdef OUTPUT_SYNTAX_COMPONENT
  outputFile << "<UnaryOp>" << std::endl;
#endif
}

// 函数实参表 FuncRParams → Exp { ',' Exp }
void SyntaxAnalysis::FuncRParams() {
  Exp();

  while (match(",")) {
    consume(",");
    Exp();
  }

#ifdef OUTPUT_SYNTAX_COMPONENT
  outputFile << "<FuncRParams>" << std::endl;
#endif
}

// 乘除模表达式 MulExp → UnaryExp | MulExp ('*' | '/' | '%') UnaryExp
void SyntaxAnalysis::MulExp() {
  UnaryExp();

  while (match("*") || match("/") || match("%")) {
#ifdef OUTPUT_SYNTAX_COMPONENT
    outputFile << "<MulExp>" << std::endl;
#endif
    consume(match("*") ? "*" : match("/") ? "/" : "%");
    UnaryExp();
  }

#ifdef OUTPUT_SYNTAX_COMPONENT
  outputFile << "<MulExp>" << std::endl;
#endif
}

// 加减表达式 AddExp → MulExp | AddExp ('+' | '−') MulExp
void SyntaxAnalysis::AddExp() {
  MulExp();

  while (match("+") || match("-")) {
#ifdef OUTPUT_SYNTAX_COMPONENT
    outputFile << "<AddExp>" << std::endl;
#endif
    consume(match("+") ? "+" : "-");
    MulExp();
  }

#ifdef OUTPUT_SYNTAX_COMPONENT
  outputFile << "<AddExp>" << std::endl;
#endif
}

// 关系表达式 RelExp → AddExp | RelExp ('<' | '>' | '<=' | '>=') AddExp
void SyntaxAnalysis::RelExp() {
  AddExp();

  while (match("<") || match(">") || match("<=") || match(">=")) {
#ifdef OUTPUT_SYNTAX_COMPONENT
    outputFile << "<RelExp>" << std::endl;
#endif
    consume(match("<") ? "<" : match(">") ? ">" : match("<=") ? "<=" : ">=");
    AddExp();
  }

#ifdef OUTPUT_SYNTAX_COMPONENT
  outputFile << "<RelExp>" << std::endl;
#endif
}

// 相等性表达式 EqExp → RelExp | EqExp ('==' | '!=') RelExp
void SyntaxAnalysis::EqExp() {
  RelExp();

  while (match("==") || match("!=")) {
#ifdef OUTPUT_SYNTAX_COMPONENT
    outputFile << "<EqExp>" << std::endl;
#endif
    consume(match("==") ? "==" : "!=");
    RelExp();
  }

#ifdef OUTPUT_SYNTAX_COMPONENT
  outputFile << "<EqExp>" << std::endl;
#endif
}

// 逻辑与表达式 LAndExp → EqExp | LAndExp '&&' EqExp
void SyntaxAnalysis::LAndExp() {
  EqExp();

  while (match("&&")) {
#ifdef OUTPUT_SYNTAX_COMPONENT
    outputFile << "<LAndExp>" << std::endl;
#endif
    consume("&&");
    EqExp();
  }

#ifdef OUTPUT_SYNTAX_COMPONENT
  outputFile << "<LAndExp>" << std::endl;
#endif
}

// 逻辑或表达式 LOrExp → LAndExp | LOrExp '||' LAndExp
void SyntaxAnalysis::LOrExp() {
  LAndExp();

  while (match("||")) {
#ifdef OUTPUT_SYNTAX_COMPONENT
    outputFile << "<LOrExp>" << std::endl;
#endif
    consume("||");
    LAndExp();
  }

#ifdef OUTPUT_SYNTAX_COMPONENT
  outputFile << "<LOrExp>" << std::endl;
#endif
}

// 常量表达式 ConstExp → AddExp
void SyntaxAnalysis::ConstExp() {
  AddExp();

#ifdef OUTPUT_SYNTAX_COMPONENT
  outputFile << "<ConstExp>" << std::endl;
#endif
}

// 格式化字符串 FormatString → '"'{ Char }'"'
void SyntaxAnalysis::FormatString() {
  if (matchType("STRCON")) {
    consumeType("STRCON");
  } else {
    reportError("Expected string constant");
  }
}

int main() {
  try {
    // 词法分析
    LexicalAnalysis lexical("testfile.txt");
    auto tokens = lexical.lexicalanalysis();

    // 语法分析
    SyntaxAnalysis syntax;
    syntax.setTokens(tokens);

    syntax.syntaxAnalysis();

  } catch (const char *msg) {
    std::cerr << "Error: " << msg << std::endl;
    return 1;
  } catch (const std::exception &e) {
    std::cerr << "Exception: " << e.what() << std::endl;
    return 1;
  }

  return 0;
}
