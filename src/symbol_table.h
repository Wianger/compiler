#ifndef SYMBOL_TABLE_H
#define SYMBOL_TABLE_H

#include "ast.h"   // For InitializerNode (optional, if storing AST parts)
#include "token.h" // For Token in parameters
#include <list>    // For stack of scopes
#include <memory>
#include <optional>
#include <sstream> // For toString methods
#include <string>
#include <unordered_map> // For individual scope tables
#include <vector>

// Forward declaration for AST nodes if needed for types (e.g., array size
// expression) struct Expression; // If array_size_expr_node is stored in
// SymbolEntry

enum class SymbolKind { UNKNOWN, CONSTANT, VARIABLE, FUNCTION };

// SysY types are simple: int, void, or int array.
// For arrays, we might also want to store dimension or size if known.
enum class DataType {
  UNKNOWN,
  INT,
  VOID, // Only for function return types
  INT_ARRAY
  // We could add FUNCTION_TYPE if we want to store full function signatures as
  // types
};

std::string symbolKindToString(SymbolKind kind);
std::string dataTypeToString(DataType type);

struct SymbolEntry {
  std::string name;
  SymbolKind kind = SymbolKind::UNKNOWN;
  DataType type =
      DataType::UNKNOWN; // For variables, constants, function return type
  int line_defined = 0;
  int scope_level = 0; // 0 for global, 1 for first level of nesting, etc.

  // For constants/variables
  bool is_const = false;
  std::optional<int>
      constant_value; // Store the value for evaluated integer constants
  // std::optional<std::unique_ptr<InitializerNode>> initial_value_ast; // If
  // storing AST For simplicity now, we might not store the full AST in symbol
  // entry during first pass of semantic analysis

  // For arrays
  bool is_array = false;
  std::optional<int>
      array_static_size; // If size is a known constant at compile time

  // For functions
  DataType return_type_func = DataType::UNKNOWN;
  std::vector<std::pair<DataType, Token>>
      parameters; // List of (type, name_token) for params
                  // Name token is useful for error messages or if params are
                  // also symbols

  // Constructor for simple var/const
  SymbolEntry(std::string n, SymbolKind k, DataType t, int line, int scope,
              bool cnst = false)
      : name(std::move(n)), kind(k), type(t), line_defined(line),
        scope_level(scope), is_const(cnst) {
    if (type == DataType::INT_ARRAY)
      is_array = true;
    else
      is_array = false;
  }

  // Constructor for function
  SymbolEntry(std::string n, DataType ret_type,
              std::vector<std::pair<DataType, Token>> params, int line,
              int scope)
      : name(std::move(n)), kind(SymbolKind::FUNCTION), type(DataType::UNKNOWN),
        line_defined(line), scope_level(scope), return_type_func(ret_type),
        parameters(std::move(params)) {
    is_array = false; // Functions are not arrays
  }

  SymbolEntry() = default;

  std::string toString() const {
    std::ostringstream oss;
    oss << "SymbolEntry: { Name: " << name
        << ", Kind: " << symbolKindToString(kind)
        << ", Type: " << dataTypeToString(type); // General type
    if (is_const)
      oss << ", IsConst: true";
    if (is_array) {
      oss << ", IsArray: true";
      if (array_static_size.has_value())
        oss << ", Size: " << array_static_size.value();
      else
        oss << ", Size: <dynamic_or_unknown>";
    }
    if (kind == SymbolKind::FUNCTION) {
      oss << ", ReturnType: " << dataTypeToString(return_type_func);
      oss << ", Params: [";
      for (size_t i = 0; i < parameters.size(); ++i) {
        oss << dataTypeToString(parameters[i].first) << " "
            << parameters[i].second.lexeme;
        if (i < parameters.size() - 1)
          oss << ", ";
      }
      oss << "]";
    }
    oss << ", Line: " << line_defined << ", Scope: " << scope_level << " }";
    return oss.str();
  }
};

class SymbolTable {
public:
  SymbolTable();

  void enterScope();
  void exitScope();

  bool addSymbol(const SymbolEntry &entry);
  bool addFunctionSymbol(const SymbolEntry &entry);

  SymbolEntry *lookupSymbol(const std::string &name);
  SymbolEntry *lookupSymbolCurrentScope(const std::string &name);
  SymbolEntry *lookupFunction(const std::string &name);

  int getCurrentScopeLevel() const { return current_scope_level; }

private:
  using Scope = std::unordered_map<std::string, SymbolEntry>;

  std::list<Scope> scope_stack_;
  Scope global_function_scope_;

  int current_scope_level;
  void
  initializeBuiltinFunctions(); // For printf, getint if treated as functions
};

#endif // SYMBOL_TABLE_H