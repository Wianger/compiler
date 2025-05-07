#include "semantic_analyzer.h"
#include <iostream> // For debug messages (optional)

SemanticAnalyzer::SemanticAnalyzer()
    : has_main_function_(false), current_function_def_(nullptr),
      loop_depth_(0) {
  // symbol_table_ is default constructed, which initializes its own global
  // scope.
}

void SemanticAnalyzer::analyze(CompUnitNode *ast_root) {
  if (!ast_root) {
    std::cerr << "Semantic Analyzer: AST root is null. Cannot analyze."
              << std::endl;
    return;
  }
  has_main_function_ = false;
  current_function_def_ = nullptr;
  loop_depth_ = 0;

  try {
    visit(ast_root);
    if (!has_main_function_) {
      throw SemanticError("Main function ('int main()') definition not found.",
                          ast_root->line_number);
    }
  } catch (const SemanticError &e) {
    // std::cerr << "Semantic Error caught in analyze(): " << e.what() <<
    // std::endl;
    throw;
  }
}

// --- Visitor Method Placeholders ---

void SemanticAnalyzer::visit(CompUnitNode *node) {
  if (!node)
    return;
  // Global scope for variables/constants is already entered by SymbolTable
  // constructor (level 0 for entries, level 1 for table state). Functions are
  // added to a separate global function scope in symbol_table_.

  for (auto &decl : node->declarations) {
    if (!decl)
      continue;
    // Dispatch based on actual type of Declaration
    if (auto *const_decl_node = dynamic_cast<ConstDeclNode *>(decl.get())) {
      visit(const_decl_node, true); // true for is_global
    } else if (auto *var_decl_node = dynamic_cast<VarDeclNode *>(decl.get())) {
      visit(var_decl_node, true); // true for is_global
    } else {
      throw SemanticError(
          "Unknown type of global declaration found in CompUnit.",
          decl->line_number);
    }
  }
  for (auto &func_def : node->function_definitions) {
    if (func_def)
      visit(func_def.get());
  }
  if (node->main_function_definition) {
    visit(node->main_function_definition.get());
  } else {
    throw SemanticError("Main function definition is missing in CompUnit.",
                        node->line_number);
  }
}

void SemanticAnalyzer::visit(ConstDeclNode *node, bool is_global) {
  if (!node)
    return;
  for (auto &const_def : node->const_defs) {
    if (const_def)
      visit(const_def.get(), is_global);
  }
}

void SemanticAnalyzer::visit(ConstDefNode *node, bool is_global) {
  if (!node)
    return;
  std::string name = node->identifier.lexeme;
  int line = node->identifier.line;
  if (symbol_table_.lookupSymbolCurrentScope(name)) {
    throw SemanticError("Redeclaration of constant '" + name + "'", line);
  }
  DataType symbol_type = DataType::INT;
  bool is_array = false;
  std::optional<int> array_size = std::nullopt;
  if (node->array_size_expr) {
    if (!node->array_size_expr.value()) {
      throw SemanticError(
          "Constant array '" + name + "' has null size expression.", line);
    }
    is_array = true;
    symbol_type = DataType::INT_ARRAY;
    std::optional<int> evaluated_size =
        evaluateConstExpression(node->array_size_expr.value().get());
    if (!evaluated_size.has_value()) {
      throw SemanticError("Array size for constant array '" + name +
                              "' is not a constant expression.",
                          node->array_size_expr.value()->line_number);
    }
    if (evaluated_size.value() <= 0) {
      throw SemanticError("Array size for constant array '" + name +
                              "' must be positive.",
                          node->array_size_expr.value()->line_number);
    }
    array_size = evaluated_size;
  }
  if (!node->initializer) {
    throw SemanticError("Constant '" + name + "' must be initialized.", line);
  }
  DataType init_type = visit(node->initializer.get(), true, is_global, name);
  if (is_array) {
    if (init_type != DataType::INT_ARRAY &&
        !(node->initializer->is_array_initializer)) {
      throw SemanticError("Initializer for constant array '" + name +
                              "' is not an array initializer.",
                          node->initializer->line_number);
    }
    if (array_size.has_value() && node->initializer->is_array_initializer) {
      if (node->initializer->array_element_inits.size() >
          static_cast<size_t>(array_size.value())) {
        throw SemanticError(
            "Too many elements in initializer for constant array '" + name +
                "' (expected at most " + std::to_string(array_size.value()) +
                ").",
            node->initializer->line_number);
      }
    }
  } else { // Not an array, so it's a simple integer constant
    if (init_type != DataType::INT) {
      throw SemanticError("Initializer for constant '" + name +
                              "' must be an integer expression.",
                          node->initializer->line_number);
    }
    // For simple int constants, try to evaluate and store the value.
    // The visit(InitializerNode, ...) ensures its sub-expression is a
    // const_expression.
    if (node->initializer
            ->single_init_expr) { // Should be true if init_type is INT
      // We need to call evaluateConstExpression on the *actual* expression
      // inside the initializer. The is_const_init = true in
      // visit(InitializerNode) would have already validated it's evaluatable.
      // Temporarily remove the throw from evaluateConstExpression for this call
      // or make it more nuanced. For now, let's assume evaluateConstExpression
      // is updated to return nullopt on failure rather than always throwing for
      // non-NumberNodes. This specific call to evaluateConstExpression will
      // happen *before* the full recursive one is ready. So, we depend on the
      // fact that InitializerNode for a const int should contain a NumberNode
      // or something simple. This is a bit of a chicken-and-egg, the full
      // evaluateConstExpression will handle more cases. For now, let's just
      // attempt it on the single_init_expr. The visit(InitializerNode) already
      // checked its const-ness.
    } else {
      // This case should not be reached if init_type was INT from a non-array
      // initializer.
      throw SemanticError(
          "Internal error: Constant '/" + name +
              "/' with INT type has no single initializer expression.",
          line);
    }
  }
  SymbolEntry entry(name, SymbolKind::CONSTANT, symbol_type, line,
                    symbol_table_.getCurrentScopeLevel(), true);
  entry.is_array = is_array;
  if (is_array) {
    entry.array_static_size = array_size;
  } else {
    // If it's a simple constant integer, and initializer has a single
    // expression, try to evaluate it. The evaluateConstExpression will be
    // updated to handle more cases. This is the point where we store the value.
    if (node->initializer && node->initializer->single_init_expr) {
      // The call to visit(InitializerNode, true, ...) already confirmed it's a
      // const expr of INT type. Now, we try to get its actual value for
      // storage. This assumes evaluateConstExpression will be robust enough
      // soon.
      std::optional<int> val =
          evaluateConstExpression(node->initializer->single_init_expr.get());
      if (val.has_value()) {
        entry.constant_value = val.value();
      } else {
        // This would mean evaluateConstExpression failed even though
        // visit(InitializerNode) said it was a const int. This could happen if
        // evaluateConstExpression is not yet fully capable. For array
        // dimensions, we throw. For const int definitions, if we can't evaluate
        // it here, it's an issue.
        throw SemanticError(
            "Could not evaluate the constant initializer for '" + name +
                "' to a value, even though it was declared const int.",
            node->initializer->line_number);
      }
    }
  }

  if (!symbol_table_.addSymbol(entry)) {
    throw SemanticError("Redeclaration of constant '" + name +
                            "' (symbol table add failed).",
                        line);
  }
}

DataType SemanticAnalyzer::visit(InitializerNode *node, bool is_const_init,
                                 bool is_global_context_for_const_exp,
                                 const std::string &var_name_for_error) {
  if (!node) {
    throw SemanticError("Null InitializerNode encountered for '" +
                            var_name_for_error + "'.",
                        0);
  }
  int line = node->line_number;
  if (node->is_array_initializer) {
    for (const auto &elem_expr_ptr : node->array_element_inits) {
      if (!elem_expr_ptr) {
        throw SemanticError("Null expression in array initializer for '" +
                                var_name_for_error + "'.",
                            line);
      }
      DataType elem_type =
          getExpressionType(elem_expr_ptr.get(),
                            is_global_context_for_const_exp && is_const_init);
      if (elem_type != DataType::INT) {
        throw SemanticError("Array initializer element for '" +
                                var_name_for_error +
                                "' is not an integer type.",
                            elem_expr_ptr->line_number);
      }
      if (is_const_init) {
        if (!evaluateConstExpression(elem_expr_ptr.get()).has_value()) {
          throw SemanticError("Array initializer element for constant '" +
                                  var_name_for_error +
                                  "' is not a constant expression.",
                              elem_expr_ptr->line_number);
        }
      }
    }
    return DataType::INT_ARRAY;
  } else {
    if (!node->single_init_expr) {
      throw SemanticError("Single initializer expression is null for '" +
                              var_name_for_error + "'.",
                          line);
    }
    DataType expr_type =
        getExpressionType(node->single_init_expr.get(),
                          is_global_context_for_const_exp && is_const_init);
    if (expr_type != DataType::INT) {
      throw SemanticError("Initializer for '" + var_name_for_error +
                              "' is not an integer type.",
                          node->single_init_expr->line_number);
    }
    if (is_const_init) {
      if (!evaluateConstExpression(node->single_init_expr.get()).has_value()) {
        throw SemanticError("Initializer for constant '" + var_name_for_error +
                                "' is not a constant expression.",
                            node->single_init_expr->line_number);
      }
    }
    return DataType::INT;
  }
}

void SemanticAnalyzer::visit(VarDeclNode *node, bool is_global) {
  if (!node)
    return;
  for (auto &var_def : node->var_defs) {
    if (var_def)
      visit(var_def.get(), is_global);
  }
}

void SemanticAnalyzer::visit(VarDefNode *node, bool is_global) {
  if (!node)
    return;
  std::string name = node->identifier.lexeme;
  int line = node->identifier.line;
  if (symbol_table_.lookupSymbolCurrentScope(name)) {
    throw SemanticError("Redeclaration of variable '" + name + "'", line);
  }
  DataType symbol_type = DataType::INT;
  bool is_array = false;
  std::optional<int> array_size = std::nullopt;
  if (node->array_size_expr) {
    if (!node->array_size_expr.value()) {
      throw SemanticError(
          "Variable array '" + name + "' has null size expression.", line);
    }
    is_array = true;
    symbol_type = DataType::INT_ARRAY;
    std::optional<int> evaluated_size =
        evaluateConstExpression(node->array_size_expr.value().get());
    if (!evaluated_size.has_value()) {
      throw SemanticError("Array size for variable '" + name +
                              "' is not a constant expression.",
                          node->array_size_expr.value()->line_number);
    }
    if (evaluated_size.value() <= 0) {
      throw SemanticError("Array size for variable '" + name +
                              "' must be positive.",
                          node->array_size_expr.value()->line_number);
    }
    array_size = evaluated_size;
  }
  if (node->initializer && node->initializer.has_value()) {
    if (!node->initializer.value()) {
      throw SemanticError("Variable '" + name + "' has null initializer node.",
                          line);
    }
    DataType init_type =
        visit(node->initializer.value().get(), false, is_global, name);
    if (is_array) {
      if (init_type != DataType::INT_ARRAY &&
          !(node->initializer.value()->is_array_initializer)) {
        throw SemanticError("Initializer for array variable '" + name +
                                "' is not an array initializer.",
                            node->initializer.value()->line_number);
      }
      if (array_size.has_value() &&
          node->initializer.value()->is_array_initializer) {
        if (node->initializer.value()->array_element_inits.size() >
            static_cast<size_t>(array_size.value())) {
          throw SemanticError(
              "Too many elements in initializer for array variable '" + name +
                  "'.",
              node->initializer.value()->line_number);
        }
      }
    } else {
      if (init_type != DataType::INT) {
        throw SemanticError("Initializer for variable '" + name +
                                "' must be an integer expression.",
                            node->initializer.value()->line_number);
      }
    }
  }
  SymbolEntry entry(name, SymbolKind::VARIABLE, symbol_type, line,
                    symbol_table_.getCurrentScopeLevel(), false);
  entry.is_array = is_array;
  if (is_array) {
    entry.array_static_size = array_size;
  }
  if (!symbol_table_.addSymbol(entry)) {
    throw SemanticError("Redeclaration of variable '" + name +
                            "' (symbol table add failed).",
                        line);
  }
}

void SemanticAnalyzer::visit(FuncDefNode *node) {
  if (!node)
    return;

  std::string func_name = node->func_name_ident.lexeme;
  int line = node->func_name_ident.line;

  if (symbol_table_.lookupFunction(func_name)) {
    throw SemanticError("Redefinition of function '" + func_name + "'.", line);
  }

  DataType return_type_enum;
  if (node->return_type == FunctionReturnType::INT) {
    return_type_enum = DataType::INT;
  } else if (node->return_type == FunctionReturnType::VOID) {
    return_type_enum = DataType::VOID;
  } else {
    throw SemanticError(
        "Invalid AST return type enum for function '" + func_name + "'.", line);
  }
  current_expected_return_type_ = return_type_enum;
  current_function_def_ = node;

  // Use existing SymbolEntry.parameters: std::vector<std::pair<DataType,
  // Token>>
  std::vector<std::pair<DataType, Token>> param_signature_for_symbol_entry;

  SymbolEntry func_entry(func_name, SymbolKind::FUNCTION, DataType::UNKNOWN,
                         line, 0);
  func_entry.return_type_func = return_type_enum;

  symbol_table_.enterScope();

  for (const auto &param_ast_node_ptr : node->parameters) {
    if (!param_ast_node_ptr)
      continue;
    const FuncParamNode *param_ast_node = param_ast_node_ptr.get();

    std::string param_name = param_ast_node->identifier.lexeme;
    int param_line = param_ast_node->identifier.line;

    DataType param_data_type;
    if (param_ast_node->is_array_param) {
      param_data_type = DataType::INT_ARRAY;
    } else {
      param_data_type = DataType::INT;
    }
    if (param_ast_node->type_token.type != TokenType::INT_KW) {
      throw SemanticError(
          "Parameter '" + param_name + "' in function '" + func_name +
              "' has non-INT base type token: " +
              tokenTypeToString(param_ast_node->type_token.type),
          param_ast_node->type_token.line);
    }

    if (symbol_table_.lookupSymbolCurrentScope(param_name)) {
      throw SemanticError("Redeclaration of parameter '" + param_name +
                              "' in function '" + func_name + "'.",
                          param_line);
    }

    param_signature_for_symbol_entry.emplace_back(param_data_type,
                                                  param_ast_node->identifier);

    SymbolEntry param_symbol(param_name, SymbolKind::VARIABLE, param_data_type,
                             param_line, symbol_table_.getCurrentScopeLevel(),
                             false);
    param_symbol.is_array = param_ast_node->is_array_param;
    if (param_ast_node->is_array_param) {
      param_symbol.array_static_size = std::nullopt;
    }
    if (!symbol_table_.addSymbol(param_symbol)) {
      throw SemanticError("Failed to add parameter '" + param_name +
                              "' to symbol table for function '" + func_name +
                              "'.",
                          param_line);
    }
  }

  func_entry.parameters = param_signature_for_symbol_entry;

  if (!symbol_table_.addFunctionSymbol(func_entry)) {
    throw SemanticError("Failed to add function symbol '" + func_name +
                            "' (addFunctionSymbol failed).",
                        line);
  }

  if (!node->body) {
    throw SemanticError("Function '" + func_name + "' has no body.", line);
  }
  visit(node->body.get(), true, nullptr);

  symbol_table_.exitScope();
  current_expected_return_type_ = std::nullopt;
  current_function_def_ = nullptr;
}

void SemanticAnalyzer::visit(MainFuncDefNode *node) {
  if (!node)
    return;

  if (has_main_function_) {
    throw SemanticError("Redefinition of main function.", node->line_number);
  }
  has_main_function_ = true;
  current_expected_return_type_ = DataType::INT;
  // current_function_def_ remains nullptr for main, or we could set it to a
  // pseudo-node if its fields were needed by other rules uniformly.
  // For now, return type is handled by current_expected_return_type_.

  SymbolEntry main_func_entry("main", SymbolKind::FUNCTION, DataType::UNKNOWN,
                              node->line_number, 0);
  main_func_entry.return_type_func = DataType::INT;
  // main has no parameters by SysY spec for 'int main () Block'
  main_func_entry.parameters = {};

  if (symbol_table_.lookupFunction("main")) {
    // This might occur if a regular function was named 'main' and processed
    // before MainFuncDef. Or, if somehow MainFuncDef is processed twice.
    throw SemanticError("Function 'main' is already defined (possibly as a "
                        "regular function, or duplicate main).",
                        node->line_number);
  }
  if (!symbol_table_.addFunctionSymbol(main_func_entry)) {
    throw SemanticError(
        "Failed to add 'main' function symbol to global function scope.",
        node->line_number);
  }

  symbol_table_.enterScope(); // New scope for main function body
  if (!node->body) {
    throw SemanticError("Main function has no body.", node->line_number);
  }
  visit(node->body.get(), true, nullptr); // true: is_function_block
  symbol_table_.exitScope();

  current_expected_return_type_ = std::nullopt;
}

void SemanticAnalyzer::visit(BlockNode *node, bool is_function_block,
                             SymbolTable *function_params_scope /*unused*/) {
  if (!node)
    return;
  bool new_scope_entered_here = false;
  if (!is_function_block) { // Nested block always creates a new scope
    symbol_table_.enterScope();
    new_scope_entered_here = true;
  }
  // If is_function_block, the scope is assumed to be entered by the calling
  // FuncDefNode/MainFuncDefNode visitor. And params_scope (if provided) would
  // have been used to populate it by the caller.

  for (auto &item : node->items) {
    if (!item)
      continue;
    if (auto *decl_node = dynamic_cast<Declaration *>(item.get())) {
      if (auto *cd_node = dynamic_cast<ConstDeclNode *>(decl_node)) {
        visit(cd_node, false); // false for is_global
      } else if (auto *vd_node = dynamic_cast<VarDeclNode *>(decl_node)) {
        visit(vd_node, false); // false for is_global
      }
    } else if (auto *stmt_node = dynamic_cast<Statement *>(item.get())) {
      visit(stmt_node); // This call should now be fine with the dispatcher
                        // declared
    } else {
      throw SemanticError("Unknown BlockItem type.", item->line_number);
    }
  }

  if (new_scope_entered_here) {
    symbol_table_.exitScope();
  }
}

// Generic dispatcher for Statements - ensure this is present and matches
// declaration
void SemanticAnalyzer::visit(Statement *stmt) {
  if (!stmt)
    return;
  if (auto *node = dynamic_cast<AssignStmtNode *>(stmt)) {
    visit(node);
  } else if (auto *node = dynamic_cast<ExprStmtNode *>(stmt)) {
    visit(node);
  } else if (auto *node = dynamic_cast<IfStmtNode *>(stmt)) {
    visit(node);
  } else if (auto *node = dynamic_cast<WhileStmtNode *>(stmt)) {
    visit(node);
  } else if (auto *node = dynamic_cast<BreakStmtNode *>(stmt)) {
    visit(node);
  } else if (auto *node = dynamic_cast<ContinueStmtNode *>(stmt)) {
    visit(node);
  } else if (auto *node = dynamic_cast<ReturnStmtNode *>(stmt)) {
    visit(node);
  } else if (auto *node = dynamic_cast<PrintfStmtNode *>(stmt)) {
    visit(node);
  } else if (auto *node = dynamic_cast<BlockNode *>(stmt)) {
    visit(node, false, nullptr);
  } else {
    throw SemanticError("Unknown Statement type in semantic analysis dispatch.",
                        stmt->line_number);
  }
}

void SemanticAnalyzer::visit(AssignStmtNode *node) {
  if (!node) {
    throw SemanticError("Null AssignStmtNode encountered.", 0);
  }
  // Corrected member names based on ast.h: lval and expression
  if (!node->lval) { // Corrected from l_val
    throw SemanticError("Assignment statement has no LValue.",
                        node->line_number);
  }
  if (!node->expression) { // Corrected from r_val
    throw SemanticError("Assignment statement has no RValue expression.",
                        node->line_number);
  }

  DataType lval_type = getExpressionType(node->lval.get(), false, true);

  if (lval_type != DataType::INT) {
    throw SemanticError("LValue in assignment must be an integer type. Cannot "
                        "assign to type '" +
                            dataTypeToString(lval_type) + "'.",
                        node->lval->line_number);
  }

  DataType rval_type =
      getExpressionType(node->expression.get()); // Corrected from r_val

  if (rval_type != DataType::INT) {
    throw SemanticError(
        "RValue in assignment must be an integer type. Got type '" +
            dataTypeToString(rval_type) + "'.",
        node->expression->line_number); // Corrected from r_val
  }
}

void SemanticAnalyzer::visit(ExprStmtNode *node) {
  if (!node)
    return;
  if (node->expression && node->expression.has_value()) {
    getExpressionType(node->expression.value().get());
  }
}
void SemanticAnalyzer::visit(IfStmtNode *node) {
  if (!node)
    return;

  if (!node->condition) {
    throw SemanticError("If statement missing condition.", node->line_number);
  }
  DataType cond_type = getExpressionType(node->condition.get());
  if (cond_type != DataType::INT) {
    throw SemanticError("If statement condition must be an integer type.",
                        node->condition->line_number);
  }

  if (node->then_stmt) {
    visit(node->then_stmt.get());
  } else {
    throw SemanticError("If statement missing 'then' branch.",
                        node->line_number);
  }

  if (node->else_stmt) {
    if (node->else_stmt.value()) {
      visit(node->else_stmt.value().get());
    }
  }
}
void SemanticAnalyzer::visit(WhileStmtNode *node) {
  if (!node)
    return;

  if (!node->condition) {
    throw SemanticError("While statement missing condition.",
                        node->line_number);
  }
  DataType cond_type = getExpressionType(node->condition.get());
  if (cond_type != DataType::INT) {
    throw SemanticError("While statement condition must be an integer type.",
                        node->condition->line_number);
  }

  loop_depth_++;
  if (node->body_stmt) {
    visit(node->body_stmt.get());
  } else {
    throw SemanticError("While statement missing body.", node->line_number);
  }
  loop_depth_--;
}
void SemanticAnalyzer::visit(BreakStmtNode *node) {
  if (!node)
    return;
  if (loop_depth_ == 0) {
    throw SemanticError("'break' statement not within a loop.",
                        node->line_number);
  }
}
void SemanticAnalyzer::visit(ContinueStmtNode *node) {
  if (!node)
    return;
  if (loop_depth_ == 0) {
    throw SemanticError("'continue' statement not within a loop.",
                        node->line_number);
  }
}
void SemanticAnalyzer::visit(ReturnStmtNode *node) {
  if (!node)
    return;

  if (!current_expected_return_type_.has_value()) {
    throw SemanticError("'return' statement outside of a function.",
                        node->line_number);
  }

  DataType expected_type = current_expected_return_type_.value();

  if (expected_type == DataType::VOID) {
    if (node->return_expression && node->return_expression.has_value()) {
      if (node->return_expression.value()) {
        throw SemanticError("Cannot return a value from a void function.",
                            node->return_expression.value()->line_number);
      } else {
        throw SemanticError("Cannot return a value from a void function.",
                            node->line_number);
      }
    }
  } else if (expected_type == DataType::INT) {
    if (!node->return_expression || !node->return_expression.has_value() ||
        !node->return_expression.value()) {
      throw SemanticError("Non-void function must return a value.",
                          node->line_number);
    }
    DataType actual_return_type =
        getExpressionType(node->return_expression.value().get());
    if (actual_return_type != DataType::INT) {
      throw SemanticError("Return type mismatch: expected INT, got " +
                              dataTypeToString(actual_return_type) + ".",
                          node->return_expression.value()->line_number);
    }
  } else {
    throw SemanticError("Internal error: Unexpected function return type "
                        "encountered during return statement analysis.",
                        node->line_number);
  }
}
void SemanticAnalyzer::visit(PrintfStmtNode *node) {
  if (!node) {
    throw SemanticError("Null PrintfStmtNode encountered.", 0);
  }

  const std::string &fmt_str = node->format_string_token.lexeme;
  int line = node->format_string_token.line;

  size_t expected_args = 0;
  for (size_t i = 0; i < fmt_str.length(); ++i) {
    if (fmt_str[i] == '%') {
      if (i + 1 < fmt_str.length() && fmt_str[i + 1] == 'd') {
        expected_args++;
        i++;
      } else if (i + 1 < fmt_str.length() && fmt_str[i + 1] == '%') {
        i++;
      } else {
        throw SemanticError(
            "Invalid or unsupported format specifier in printf string: '" +
                fmt_str.substr(i) + "'. Only %d and %% are supported.",
            line);
      }
    }
  }

  if (node->arguments.size() != expected_args) {
    throw SemanticError(
        "Argument count mismatch for printf. Format string expects " +
            std::to_string(expected_args) + " arguments, but got " +
            std::to_string(node->arguments.size()) + ".",
        line);
  }

  for (const auto &expr_ptr : node->arguments) {
    if (!expr_ptr) {
      throw SemanticError("Null expression provided as argument to printf.",
                          line);
    }
    DataType arg_type = getExpressionType(expr_ptr.get());
    if (arg_type != DataType::INT) {
      throw SemanticError(
          "Argument type mismatch for printf. Expected INT for %d, got " +
              dataTypeToString(arg_type) + ".",
          expr_ptr->line_number);
    }
  }
}

DataType SemanticAnalyzer::getExpressionType(Expression *expr,
                                             bool is_global_const_context,
                                             bool is_assignment_lhs) {
  if (!expr) {
    throw SemanticError("Null expression encountered while getting type.", 0);
  }

  if (auto *node = dynamic_cast<NumberNode *>(expr)) {
    return visit(node);
  } else if (auto *node = dynamic_cast<LValNode *>(expr)) {
    return visit(node, is_assignment_lhs);
  } else if (auto *node = dynamic_cast<UnaryOpNode *>(expr)) {
    return visit(node, is_global_const_context);
  } else if (auto *node = dynamic_cast<BinaryOpNode *>(expr)) {
    return visit(node, is_global_const_context);
  } else if (auto *node = dynamic_cast<FunctionCallNode *>(expr)) {
    return visit(node);
  } else {
    throw SemanticError("Unknown Expression type in getExpressionType.",
                        expr->line_number);
  }
  return DataType::UNKNOWN; // Should be unreachable
}

DataType SemanticAnalyzer::visit(NumberNode *node) {
  if (!node)
    return DataType::UNKNOWN; // Should not happen
  return DataType::INT;
}
DataType SemanticAnalyzer::visit(LValNode *node, bool is_assignment_lhs) {
  if (!node) {
    throw SemanticError("Null LValNode encountered in semantic analysis.", 0);
  }

  const std::string name = node->identifier.lexeme;
  int line = node->identifier.line;

  const SymbolEntry *symbol = symbol_table_.lookupSymbol(name);
  if (!symbol) {
    throw SemanticError("Use of undeclared identifier '" + name + "'.", line);
  }

  if (is_assignment_lhs && symbol->is_const) {
    throw SemanticError("Assignment to constant variable '" + name + "'.",
                        line);
  }

  if (node->index_expr) {
    if (!symbol->is_array) {
      throw SemanticError("Indexing non-array type variable '" + name + "'.",
                          line);
    }
    DataType index_type_val = getExpressionType(node->index_expr.get());
    if (index_type_val != DataType::INT) {
      throw SemanticError("Array index for '" + name + "' is not an integer.",
                          node->index_expr->line_number);
    }
    return DataType::INT;
  } else {
    return symbol->type;
  }
}
DataType SemanticAnalyzer::visit(UnaryOpNode *node,
                                 bool is_global_const_context) {
  if (!node) {
    throw SemanticError("Null UnaryOpNode encountered.", 0);
  }
  if (!node->operand) {
    throw SemanticError("UnaryOpNode has null operand.", node->line_number);
  }

  DataType operand_type =
      getExpressionType(node->operand.get(), is_global_const_context);

  switch (node->op_token.type) {
  case TokenType::PLUS:
  case TokenType::MINUS:
  case TokenType::LOGICAL_NOT:
    if (operand_type != DataType::INT) {
      throw SemanticError("Operand for unary operator '" +
                              tokenTypeToString(node->op_token.type) +
                              "' must be an integer.",
                          node->operand->line_number);
    }
    return DataType::INT;
  default:
    throw SemanticError("Unsupported unary operator '" +
                            tokenTypeToString(node->op_token.type) +
                            "' in semantic analysis.",
                        node->line_number);
  }
}
DataType SemanticAnalyzer::visit(BinaryOpNode *node,
                                 bool is_global_const_context) {
  if (!node) {
    throw SemanticError("Null BinaryOpNode encountered.", 0);
  }
  if (!node->left || !node->right) {
    throw SemanticError("BinaryOpNode has null operand(s).", node->line_number);
  }

  DataType left_type =
      getExpressionType(node->left.get(), is_global_const_context);
  DataType right_type =
      getExpressionType(node->right.get(), is_global_const_context);

  switch (node->op_token.type) {
  case TokenType::PLUS:
  case TokenType::MINUS:
  case TokenType::MULTIPLY:
  case TokenType::DIVIDE:
  case TokenType::MODULO:
  case TokenType::LT:
  case TokenType::GT:
  case TokenType::LTE:
  case TokenType::GTE:
  case TokenType::EQ:
  case TokenType::NEQ:
  case TokenType::LOGICAL_AND:
  case TokenType::LOGICAL_OR:
    if (left_type != DataType::INT || right_type != DataType::INT) {
      throw SemanticError("Operands for binary operator '" +
                              tokenTypeToString(node->op_token.type) +
                              "' must both be integers. Got " +
                              dataTypeToString(left_type) + " and " +
                              dataTypeToString(right_type) + ".",
                          node->line_number);
    }
    return DataType::INT;
  default:
    throw SemanticError("Unsupported binary operator '" +
                            tokenTypeToString(node->op_token.type) +
                            "' in semantic analysis.",
                        node->line_number);
  }
}
DataType SemanticAnalyzer::visit(FunctionCallNode *node) {
  if (!node) {
    throw SemanticError("Null FunctionCallNode encountered.", 0);
  }

  std::string func_name = node->func_ident.lexeme;
  int call_line = node->func_ident.line;

  const SymbolEntry *func_symbol = symbol_table_.lookupFunction(func_name);
  if (!func_symbol) {
    throw SemanticError("Call to undefined function '" + func_name + "'.",
                        call_line);
  }

  if (func_symbol->kind != SymbolKind::FUNCTION) {
    throw SemanticError("'" + func_name + "' is not a function.", call_line);
  }

  if (node->arguments.size() != func_symbol->parameters.size()) {
    throw SemanticError(
        "Argument count mismatch for function '" + func_name + "'. Expected " +
            std::to_string(func_symbol->parameters.size()) + ", got " +
            std::to_string(node->arguments.size()) + ".",
        call_line);
  }

  for (size_t i = 0; i < node->arguments.size(); ++i) {
    if (!node->arguments[i]) {
      throw SemanticError("Null argument expression in call to function '" +
                              func_name + "' at argument position " +
                              std::to_string(i + 1) + ".",
                          call_line);
    }
    DataType arg_type = getExpressionType(node->arguments[i].get());
    DataType expected_param_type = func_symbol->parameters[i].first;
    if (arg_type != expected_param_type) {
      throw SemanticError("Type mismatch for argument " +
                              std::to_string(i + 1) + " in call to function '" +
                              func_name + "'. Expected " +
                              dataTypeToString(expected_param_type) + ", got " +
                              dataTypeToString(arg_type) + ".",
                          node->arguments[i]->line_number);
    }
  }

  return func_symbol->return_type_func;
}

std::optional<int> SemanticAnalyzer::evaluateConstExpression(Expression *expr) {
  if (!expr) {
    return std::nullopt;
  }

  if (auto num_node = dynamic_cast<NumberNode *>(expr)) {
    return num_node->value;
  } else if (auto lval_node = dynamic_cast<LValNode *>(expr)) {
    if (lval_node->index_expr) {
      return std::nullopt;
    }
    const SymbolEntry *symbol =
        symbol_table_.lookupSymbol(lval_node->identifier.lexeme);
    if (symbol && symbol->is_const && symbol->type == DataType::INT &&
        symbol->constant_value.has_value()) {
      return symbol->constant_value.value();
    } else {
      return std::nullopt;
    }
  } else if (auto unary_node = dynamic_cast<UnaryOpNode *>(expr)) {
    std::optional<int> operand_val =
        evaluateConstExpression(unary_node->operand.get());
    if (operand_val.has_value()) {
      switch (unary_node->op_token.type) {
      case TokenType::PLUS:
        return operand_val.value();
      case TokenType::MINUS:
        return -operand_val.value();
      case TokenType::LOGICAL_NOT:
        return (operand_val.value() == 0) ? 1 : 0;
      default:
        return std::nullopt;
      }
    } else {
      return std::nullopt;
    }
  } else if (auto binary_node = dynamic_cast<BinaryOpNode *>(expr)) {
    std::optional<int> left_val =
        evaluateConstExpression(binary_node->left.get());
    std::optional<int> right_val =
        evaluateConstExpression(binary_node->right.get());

    if (left_val.has_value() && right_val.has_value()) {
      int l = left_val.value();
      int r = right_val.value();
      switch (binary_node->op_token.type) {
      case TokenType::PLUS:
        return l + r;
      case TokenType::MINUS:
        return l - r;
      case TokenType::MULTIPLY:
        return l * r;
      case TokenType::DIVIDE:
        if (r == 0) {
          throw SemanticError("Division by zero in constant expression.",
                              binary_node->line_number);
        }
        return l / r;
      case TokenType::MODULO:
        if (r == 0) {
          throw SemanticError("Modulo by zero in constant expression.",
                              binary_node->line_number);
        }
        return l % r;
      case TokenType::LT:
        return (l < r) ? 1 : 0;
      case TokenType::GT:
        return (l > r) ? 1 : 0;
      case TokenType::LTE:
        return (l <= r) ? 1 : 0;
      case TokenType::GTE:
        return (l >= r) ? 1 : 0;
      case TokenType::EQ:
        return (l == r) ? 1 : 0;
      case TokenType::NEQ:
        return (l != r) ? 1 : 0;
      case TokenType::LOGICAL_AND:
        return (l != 0 && r != 0) ? 1 : 0;
      case TokenType::LOGICAL_OR:
        return (l != 0 || r != 0) ? 1 : 0;
      default:
        return std::nullopt;
      }
    } else {
      return std::nullopt;
    }
  } else {
    return std::nullopt;
  }
}