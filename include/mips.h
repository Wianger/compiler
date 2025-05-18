#include "parser.h"
class Mips {
public:
  Mips(std::vector<Quadruple> &tac, std::shared_ptr<SymbolTable> table);
  std::vector<std::string> generateMips();

private:
  std::vector<Quadruple> tac;
  std::shared_ptr<SymbolTable> table;
  std::vector<std::string> mipsCode;
  std::vector<std::string> data, text;
  int labelCount = 0;
  int tempCount = 0;

  std::string indent(int level);
  void buildin_func();
  void generateDataSection();
  void generateTextSection();
};