#include "lexer.h"
#include "symbol_table.h"

struct Quadruple {
  std::string op; // "+", "*", "=", etc
  std::string arg1;
  std::string arg2; // 可为 "" 表示 unary
  std::string result;
};

enum class ExpType { Num, Temp, Void };

class Parser {
public:
  Parser(std::vector<Token> tokens)
      : tokens(tokens), position(0), tempCount(0), labelCount(0),
        printfstrCount(0) {
    currentTable = std::make_shared<SymbolTable>();
  }
  std::vector<Quadruple> parse();

private:
  std::vector<Token> tokens;
  std::vector<Quadruple> tac;
  size_t position;
  int tempCount;
  int labelCount;
  int printfstrCount;
  std::shared_ptr<SymbolTable> currentTable;

  Token currentToken() const;
  Token peek(int offset) const;
  bool match(const TokenType &type, int offset = 0);
  void consume(const TokenType &type);
  void splitStringByDelimiter(const std::string &str,
                              const std::string &delimiter);
  int getTempVar();
  int getLabel();
  int getPrintfStrCount();
  void enterScope();
  void exitScope();
  void emit(const std::string &op = "", const std::string &arg1 = "",
            const std::string &arg2 = "", const std::string &result = "");
  void preprocess();

  void CompUnit();
  void Decl(bool);
  void FuncDef();
  void MainFuncDef();
  void Block(bool isfunc = false, std::vector<std::string> params = {});
  void Stmt(int startlabel = 0, int endlabel = 0);
  std::pair<ExpType, int> AddExp();
  std::pair<ExpType, int> LOrExp();
  std::pair<ExpType, int> MulExp();
  std::pair<ExpType, int> UnaryExp();
  std::pair<ExpType, int> PrimaryExp();
  std::pair<ExpType, int> LAndExp();
  std::pair<ExpType, int> EqExp();
  std::pair<ExpType, int> RelExp();
};