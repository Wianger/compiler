#include "symbol_table.h"
#include <iostream>
#include <stdexcept>

std::string symbolKindToString(SymbolKind kind) {
  switch (kind) {
  case SymbolKind::CONSTANT:
    return "CONSTANT";
  case SymbolKind::VARIABLE:
    return "VARIABLE";
  case SymbolKind::FUNCTION:
    return "FUNCTION";
  case SymbolKind::UNKNOWN:
    return "UNKNOWN";
  default:
    return "INVALID_KIND";
  }
}

std::string dataTypeToString(DataType type) {
  switch (type) {
  case DataType::INT:
    return "INT";
  case DataType::VOID:
    return "VOID";
  case DataType::INT_ARRAY:
    return "INT_ARRAY";
  case DataType::UNKNOWN:
    return "UNKNOWN";
  default:
    return "INVALID_TYPE";
  }
}

SymbolTable::SymbolTable()
    : current_scope_level(0) { // Global scope is initially level 0
  scope_stack_
      .emplace_front(); // Create the global scope for variables/constants
  initializeBuiltinFunctions();
}

void SymbolTable::enterScope() {
  current_scope_level++;
  scope_stack_.emplace_front();
}

void SymbolTable::exitScope() {
  if (!scope_stack_.empty()) {
    scope_stack_.pop_front();
  }
  if (current_scope_level > 0) { // Ensure scope level doesn't go below global
    current_scope_level--;
  }
}

bool SymbolTable::addSymbol(const SymbolEntry &entry) {
  if (scope_stack_.empty()) {
    throw std::runtime_error(
        "SymbolTable Error: No current scope to add symbol '" + entry.name +
        "'.");
  }
  // SymbolEntry should have its scope_level set correctly by the caller using
  // getCurrentScopeLevel()
  Scope &currentScope = scope_stack_.front();
  if (currentScope.count(entry.name)) {
    return false; // Symbol already exists in the current scope
  }
  currentScope[entry.name] = entry;
  // The scope_level in the entry should ideally be set when the SymbolEntry is
  // created. currentScope[entry.name].scope_level = this->current_scope_level;
  // // If not already set
  return true;
}

// Functions are always global in SysY
bool SymbolTable::addFunctionSymbol(const SymbolEntry &entry) {
  if (entry.kind != SymbolKind::FUNCTION) {
    return false;
  }
  if (global_function_scope_.count(entry.name)) {
    return false; // Function already exists globally
  }
  global_function_scope_[entry.name] = entry;
  global_function_scope_[entry.name].scope_level =
      0; // Functions are at global scope level 0
  return true;
}

SymbolEntry *SymbolTable::lookupSymbol(const std::string &name) {
  for (auto &scope : scope_stack_) {
    auto it = scope.find(name);
    if (it != scope.end()) {
      return &it->second;
    }
  }
  return nullptr;
}

SymbolEntry *SymbolTable::lookupSymbolCurrentScope(const std::string &name) {
  if (scope_stack_.empty()) {
    return nullptr;
  }
  Scope &currentScope = scope_stack_.front();
  auto it = currentScope.find(name);
  if (it != currentScope.end()) {
    return &it->second;
  }
  return nullptr;
}

SymbolEntry *SymbolTable::lookupFunction(const std::string &name) {
  auto it = global_function_scope_.find(name);
  if (it != global_function_scope_.end()) {
    return &it->second;
  }
  return nullptr;
}

void SymbolTable::initializeBuiltinFunctions() {
  // Add getint
  // Using the constructor: SymbolEntry(name, ret_type, params, line, scope)
  // For functions, scope_level in SymbolEntry is 0 (global)
  SymbolEntry getint_entry("getint", DataType::INT, {}, 0, 0);
  // Ensure kind is set if not by constructor, though this constructor should
  // handle it. The constructor provided was: (std::string n, DataType ret_type,
  // std::vector<std::pair<DataType, Token>> params, int line, int scope) which
  // sets kind = SymbolKind::FUNCTION.
  global_function_scope_[getint_entry.name] = getint_entry;

  // Add printf
  SymbolEntry printf_entry("printf", DataType::VOID, {}, 0, 0);
  // Parameters for printf are custom-checked by its statement node visitor.
  global_function_scope_[printf_entry.name] = printf_entry;
}