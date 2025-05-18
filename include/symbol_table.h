#include <memory>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <vector>

enum class SymbolType { INT, ARRAY, FUNC, STR };
enum class RetType { INT, VOID };

class SymbolTable;

class Symbol {
public:
  Symbol(const std::string &name, SymbolType type, bool isglobal = false)
      : name(name), type(type), isGlobal(isglobal) {}

  std::string getName() const { return name; }
  SymbolType getType() const { return type; }
  RetType getRetType() const { return retType; }
  bool isGlobalVar() const { return isGlobal; }
  int getOffset() const { return offset; }
  void setOffset(int off) { offset = off; }
  std::shared_ptr<SymbolTable> getTable() const { return table; }
  void setTable(std::shared_ptr<SymbolTable> tbl) { table = tbl; }
  int getValue() const { return value; }
  std::vector<int> getArray() const { return array; }
  void setSize(int sz) { size = sz; }
  void setValue(int val) {
    isInit = true;
    value = val;
  }
  void setArray(const std::vector<int> &arr) {
    isInit = true;
    array = arr;
  }
  int getSize() const { return size; }
  void setRetType(RetType ret) { retType = ret; }
  bool isInitValue() const { return isInit; }
  void setStr(const std::string &s) {
    isInit = true;
    str = s;
  }
  std::string getStr() const { return str; }
  void setBlockSize(int size) { blockSize = size; }
  int getBlockSize() const { return blockSize; }

private:
  std::string name;
  SymbolType type;
  RetType retType;
  bool isGlobal;
  int offset = 0;
  bool isInit = false;
  int value = 0;
  int size = 0;
  int blockSize = 0;
  std::vector<int> array;
  std::string str;
  std::shared_ptr<SymbolTable> table;
};

class SymbolTable {
public:
  SymbolTable(std::shared_ptr<SymbolTable> st = nullptr)
      : parent(st), offset(0) {}
  void insert(const std::string &name, std::shared_ptr<Symbol> symbol) {
    if (find(name) != nullptr)
      throw std::runtime_error("Duplicate declaration of " + name);
    if (symbol->getType() != SymbolType::FUNC && !symbol->isGlobalVar()) {
      if (symbol->getType() == SymbolType::ARRAY) {
        symbol->setOffset(offset);
        offset += 4 * symbol->getSize();
      } else {
        symbol->setOffset(offset);
        offset += 4;
      }
    }
    symbol->setTable(std::make_shared<SymbolTable>(*this));
    table[name] = symbol;
  }
  std::shared_ptr<Symbol> find(const std::string &name) const {
    auto it = table.find(name);
    if (it != table.end())
      return it->second;
    return nullptr;
  }
  std::shared_ptr<Symbol> lookup(const std::string &name) {
    auto it = table.find(name);
    if (it != table.end())
      return it->second;
    if (parent != nullptr)
      return parent->lookup(name);
    return nullptr;
  }
  int Offset() { return offset; }
  void addChild(std::shared_ptr<SymbolTable> child) {
    children.push_back(child);
  }
  std::shared_ptr<SymbolTable> getParent() const { return parent; }
  std::unordered_map<std::string, std::shared_ptr<Symbol>> getTable() {
    return table;
  }

private:
  std::unordered_map<std::string, std::shared_ptr<Symbol>> table;
  int offset = 0;
  std::shared_ptr<SymbolTable> parent;
  std::vector<std::shared_ptr<SymbolTable>> children;
};