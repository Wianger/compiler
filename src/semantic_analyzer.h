#ifndef SEMANTIC_ANALYZER_H
#define SEMANTIC_ANALYZER_H

#include "ast.h"
#include "symbol_table.h"
#include <optional>  // For std::optional
#include <stdexcept> // For semantic errors
#include <string>
#include <vector>

// Custom exception for semantic errors
class SemanticError : public std::runtime_error {
public:
  SemanticError(const std::string &message, int line_num = 0)
      : std::runtime_error(message + (line_num > 0
                                          ? " (at or near line " +
                                                std::to_string(line_num) + ")"
                                          : "")),
        line(line_num) {}
  int getLine() const { return line; }

private:
  int line;
};

class SemanticAnalyzer {
public:
  SemanticAnalyzer();

  void analyze(CompUnitNode *ast_root);

private:
  SymbolTable symbol_table_;
  bool has_main_function_ = false;
  FuncDefNode *current_function_def_ = nullptr;
  int loop_depth_ = 0;
  std::optional<DataType> current_expected_return_type_;

  // Visitor methods for each AST node type
  void visit(CompUnitNode *node);
  void visit(ConstDeclNode *node, bool is_global);
  void visit(ConstDefNode *node, bool is_global);
  DataType visit(InitializerNode *node, bool is_const_init,
                 bool is_global_context_for_const_exp,
                 const std::string &var_name_for_error);

  void visit(VarDeclNode *node, bool is_global);
  void visit(VarDefNode *node, bool is_global);

  void visit(FuncDefNode *node);

  void visit(MainFuncDefNode *node);

  void visit(BlockNode *node, bool is_function_block = false,
             SymbolTable *function_params_scope = nullptr);

  // Add back the generic Statement dispatcher declaration
  void visit(Statement *stmt);

  // Statements
  void visit(AssignStmtNode *node);
  void visit(ExprStmtNode *node);
  void visit(IfStmtNode *node);
  void visit(WhileStmtNode *node);
  void visit(BreakStmtNode *node);
  void visit(ContinueStmtNode *node);
  void visit(ReturnStmtNode *node);
  void visit(PrintfStmtNode *node);

  // Expressions - these typically return the DataType of the expression
  DataType visit(NumberNode *node);
  DataType visit(LValNode *node,
                 bool is_assignment_lhs =
                     false); // is_assignment_lhs to check if const is assigned
  DataType visit(UnaryOpNode *node, bool is_global_const_context);
  DataType visit(BinaryOpNode *node, bool is_global_const_context);
  DataType visit(FunctionCallNode *node);

  std::optional<int> evaluateConstExpression(Expression *expr);
  DataType getExpressionType(Expression *expr,
                             bool is_global_const_context = false,
                             bool is_assignment_lhs = false);
};

#endif // SEMANTIC_ANALYZER_H