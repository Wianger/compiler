#ifndef AST_H
#define AST_H

#include "token.h"   // For TokenType, Token
#include <algorithm> // for std::replace_if in string replace (optional)
#include <memory>    // For std::unique_ptr
#include <optional> // For optional parts (like else branch, or optional expression)
#include <sstream>   // Needed for ostringstream
#include <stdexcept> // For std::stoi error handling
#include <string>
#include <vector>

// Forward declarations to handle potential circular dependencies in unique_ptr
// usage
struct Expression;
struct Statement;
struct Declaration;
struct BlockItemNode; // Common base for items in a block (Decl or Stmt)
struct LValNode;
struct InitializerNode;
struct BlockNode;
struct FuncParamNode;
struct FuncDefNode;
struct MainFuncDefNode;
struct ConstDefNode;
struct VarDefNode;

// Helper function for indentation
inline std::string indent(int level, const std::string &baseIndent = "  ") {
  std::string s;
  for (int i = 0; i < level; ++i) {
    s += baseIndent;
  }
  return s;
}

// Base class for all AST nodes
struct ASTNode {
  virtual ~ASTNode() = default;
  int line_number =
      0; // Stores the line number of the beginning of this syntactic construct

  explicit ASTNode(int line = 0) : line_number(line) {}

  // Pure virtual function for converting AST node to string representation
  virtual std::string toString(int indentLevel = 0) const = 0;
};

// ----- Expressions -----
struct Expression : public ASTNode {
  using ASTNode::ASTNode; // Inherit constructor
};

// Number -> IntConst
struct NumberNode : public Expression {
  Token token; // The INT_CONST token itself
  int value;

  explicit NumberNode(Token t) : Expression(t.line), token(std::move(t)) {
    try {
      value = std::stoi(this->token.lexeme);
    } catch (const std::out_of_range &oor) {
      // In a real compiler, this would be an error reported to the user.
      // For now, we might throw or set to a default/error value.
      // Let's throw for now, parser can catch it.
      throw std::runtime_error("Lexical error: Integer literal '" +
                               this->token.lexeme + "' at line " +
                               std::to_string(this->token.line) +
                               " is out of range.");
    } catch (const std::invalid_argument &ia) {
      // Should ideally not happen if lexer is correct
      throw std::runtime_error("Lexical error: Invalid integer literal '" +
                               this->token.lexeme + "' at line " +
                               std::to_string(this->token.line) + ".");
    }
  }

  std::string toString(int indentLevel = 0) const override {
    std::ostringstream oss;
    oss << indent(indentLevel) << "NumberNode (Value: " << value
        << ", Line: " << line_number << ")\n";
    return oss.str();
  }
};

// LVal -> Ident ['[' Exp ']']
struct LValNode : public Expression {
  Token identifier;                       // The IDENT token
  std::unique_ptr<Expression> index_expr; // Optional: array index expression

  LValNode(Token id, std::unique_ptr<Expression> idx = nullptr)
      : Expression(id.line), identifier(std::move(id)),
        index_expr(std::move(idx)) {}

  std::string toString(int indentLevel = 0) const override {
    std::ostringstream oss;
    oss << indent(indentLevel) << "LValNode (Ident: " << identifier.lexeme;
    if (index_expr) {
      oss << ", Indexed, Line: " << line_number << ") [\n";
      oss << indent(indentLevel + 1) << "Index:\n";
      oss << index_expr->toString(indentLevel + 2);
      oss << indent(indentLevel) << "]\n";
    } else {
      oss << ", Line: " << line_number << ")\n";
    }
    return oss.str();
  }
};

// UnaryExp -> PrimaryExp | Ident '(' [FuncRParams] ')' | UnaryOp UnaryExp
// UnaryOp -> '+' | '-' | '!'
struct UnaryOpNode : public Expression {
  Token op_token; // The operator token (+, -, !)
  std::unique_ptr<Expression> operand;

  UnaryOpNode(Token op, std::unique_ptr<Expression> expr)
      : Expression(op.line), op_token(std::move(op)), operand(std::move(expr)) {
  }

  std::string toString(int indentLevel = 0) const override {
    std::ostringstream oss;
    oss << indent(indentLevel) << "UnaryOpNode (Op: " << op_token.lexeme
        << ", Line: " << line_number << ") {\n";
    if (operand) {
      oss << operand->toString(indentLevel + 1);
    } else {
      oss << indent(indentLevel + 1) << "Operand: <null>\n";
    }
    oss << indent(indentLevel) << "}\n";
    return oss.str();
  }
};

// For AddExp, MulExp, RelExp, EqExp, LAndExp, LOrExp
struct BinaryOpNode : public Expression {
  Token op_token; // The operator token
  std::unique_ptr<Expression> left;
  std::unique_ptr<Expression> right;

  BinaryOpNode(std::unique_ptr<Expression> l, Token op,
               std::unique_ptr<Expression> r)
      : Expression(op.line), op_token(std::move(op)), left(std::move(l)),
        right(std::move(r)) {}

  std::string toString(int indentLevel = 0) const override {
    std::ostringstream oss;
    oss << indent(indentLevel) << "BinaryOpNode (Op: " << op_token.lexeme
        << ", Line: " << line_number << ") {\n";
    oss << indent(indentLevel + 1) << "Left:\n";
    if (left)
      oss << left->toString(indentLevel + 2);
    else
      oss << indent(indentLevel + 2) << "<null>\n";
    oss << indent(indentLevel + 1) << "Right:\n";
    if (right)
      oss << right->toString(indentLevel + 2);
    else
      oss << indent(indentLevel + 2) << "<null>\n";
    oss << indent(indentLevel) << "}\n";
    return oss.str();
  }
};

// FuncRParams -> Exp { ',' Exp }
// Part of UnaryExp: Ident '(' [FuncRParams] ')'
struct FunctionCallNode : public Expression {
  Token func_ident; // The IDENT token for function name
  std::vector<std::unique_ptr<Expression>> arguments;

  FunctionCallNode(Token id, std::vector<std::unique_ptr<Expression>> args)
      : Expression(id.line), func_ident(std::move(id)),
        arguments(std::move(args)) {}

  std::string toString(int indentLevel = 0) const override {
    std::ostringstream oss;
    oss << indent(indentLevel)
        << "FunctionCallNode (Name: " << func_ident.lexeme
        << ", Line: " << line_number << ") {\n";
    if (!arguments.empty()) {
      oss << indent(indentLevel + 1) << "Arguments:\n";
      for (const auto &arg : arguments) {
        if (arg)
          oss << arg->toString(indentLevel + 2);
        else
          oss << indent(indentLevel + 2) << "<null_arg>\n";
      }
    } else {
      oss << indent(indentLevel + 1) << "Arguments: <none>\n";
    }
    oss << indent(indentLevel) << "}\n";
    return oss.str();
  }
};

// ----- Block Items (Base for Declarations and Statements within a block) -----
struct BlockItemNode : public ASTNode {
  using ASTNode::ASTNode;
};

// ----- Statements -----
struct Statement : public BlockItemNode {
  using BlockItemNode::BlockItemNode;
};

// Block -> '{' {BlockItem} '}'
struct BlockNode : public Statement {
  std::vector<std::unique_ptr<BlockItemNode>>
      items; // Can contain Declarations or Statements

  BlockNode(int line, std::vector<std::unique_ptr<BlockItemNode>> block_items)
      : Statement(line), items(std::move(block_items)) {}

  std::string toString(int indentLevel = 0) const override {
    std::ostringstream oss;
    oss << indent(indentLevel) << "BlockNode (Line: " << line_number << ") {\n";
    if (items.empty()) {
      oss << indent(indentLevel + 1) << "<empty_block>\n";
    } else {
      for (const auto &item : items) {
        if (item)
          oss << item->toString(indentLevel + 1);
        else
          oss << indent(indentLevel + 1) << "<null_item>\n";
      }
    }
    oss << indent(indentLevel) << "}\n";
    return oss.str();
  }
};

// Stmt -> LVal '=' Exp ';'
struct AssignStmtNode : public Statement {
  std::unique_ptr<LValNode> lval;
  std::unique_ptr<Expression> expression;

  AssignStmtNode(int line, std::unique_ptr<LValNode> lv,
                 std::unique_ptr<Expression> expr)
      : Statement(line), lval(std::move(lv)), expression(std::move(expr)) {}

  std::string toString(int indentLevel = 0) const override {
    std::ostringstream oss;
    oss << indent(indentLevel) << "AssignStmtNode (Line: " << line_number
        << ") {\n";
    oss << indent(indentLevel + 1) << "LValue:\n";
    if (lval)
      oss << lval->toString(indentLevel + 2);
    else
      oss << indent(indentLevel + 2) << "<null_lval>\n";
    oss << indent(indentLevel + 1) << "Expression:\n";
    if (expression)
      oss << expression->toString(indentLevel + 2);
    else
      oss << indent(indentLevel + 2) << "<null_expr>\n";
    oss << indent(indentLevel) << "}\n";
    return oss.str();
  }
};

// Stmt -> [Exp] ';'
struct ExprStmtNode : public Statement {
  std::optional<std::unique_ptr<Expression>> expression;

  ExprStmtNode(int line,
               std::optional<std::unique_ptr<Expression>> expr = std::nullopt)
      : Statement(line), expression(std::move(expr)) {}

  std::string toString(int indentLevel = 0) const override {
    std::ostringstream oss;
    oss << indent(indentLevel) << "ExprStmtNode (Line: " << line_number
        << ") {\n";
    if (expression && expression.value()) {
      oss << expression.value()->toString(indentLevel + 1);
    } else {
      oss << indent(indentLevel + 1) << "<empty_expr_stmt_or_nullopt>\n";
    }
    oss << indent(indentLevel) << "}\n";
    return oss.str();
  }
};

// Stmt -> 'if' '(' Cond ')' Stmt [ 'else' Stmt ]
struct IfStmtNode : public Statement {
  std::unique_ptr<Expression>
      condition; // Cond will be parsed as an Expression (LOrExp)
  std::unique_ptr<Statement> then_stmt;
  std::optional<std::unique_ptr<Statement>> else_stmt;

  IfStmtNode(int line, std::unique_ptr<Expression> cond,
             std::unique_ptr<Statement> then_s,
             std::optional<std::unique_ptr<Statement>> else_s = std::nullopt)
      : Statement(line), condition(std::move(cond)),
        then_stmt(std::move(then_s)), else_stmt(std::move(else_s)) {}

  std::string toString(int indentLevel = 0) const override {
    std::ostringstream oss;
    oss << indent(indentLevel) << "IfStmtNode (Line: " << line_number
        << ") {\n";
    oss << indent(indentLevel + 1) << "Condition:\n";
    if (condition)
      oss << condition->toString(indentLevel + 2);
    else
      oss << indent(indentLevel + 2) << "<null_cond>\n";
    oss << indent(indentLevel + 1) << "ThenBranch:\n";
    if (then_stmt)
      oss << then_stmt->toString(indentLevel + 2);
    else
      oss << indent(indentLevel + 2) << "<null_then>\n";
    if (else_stmt && else_stmt.value()) {
      oss << indent(indentLevel + 1) << "ElseBranch:\n";
      oss << else_stmt.value()->toString(indentLevel + 2);
    } else {
      oss << indent(indentLevel + 1) << "ElseBranch: <none>\n";
    }
    oss << indent(indentLevel) << "}\n";
    return oss.str();
  }
};

// Stmt -> 'while' '(' Cond ')' Stmt
struct WhileStmtNode : public Statement {
  std::unique_ptr<Expression>
      condition; // Cond will be parsed as an Expression (LOrExp)
  std::unique_ptr<Statement> body_stmt;

  WhileStmtNode(int line, std::unique_ptr<Expression> cond,
                std::unique_ptr<Statement> body)
      : Statement(line), condition(std::move(cond)),
        body_stmt(std::move(body)) {}

  std::string toString(int indentLevel = 0) const override {
    std::ostringstream oss;
    oss << indent(indentLevel) << "WhileStmtNode (Line: " << line_number
        << ") {\n";
    oss << indent(indentLevel + 1) << "Condition:\n";
    if (condition)
      oss << condition->toString(indentLevel + 2);
    else
      oss << indent(indentLevel + 2) << "<null_cond>\n";
    oss << indent(indentLevel + 1) << "Body:\n";
    if (body_stmt)
      oss << body_stmt->toString(indentLevel + 2);
    else
      oss << indent(indentLevel + 2) << "<null_body>\n";
    oss << indent(indentLevel) << "}\n";
    return oss.str();
  }
};

// Stmt -> 'break' ';'
struct BreakStmtNode : public Statement {
  explicit BreakStmtNode(int line) : Statement(line) {}

  std::string toString(int indentLevel = 0) const override {
    return indent(indentLevel) +
           "BreakStmtNode (Line: " + std::to_string(line_number) + ")\n";
  }
};

// Stmt -> 'continue' ';'
struct ContinueStmtNode : public Statement {
  explicit ContinueStmtNode(int line) : Statement(line) {}

  std::string toString(int indentLevel = 0) const override {
    return indent(indentLevel) +
           "ContinueStmtNode (Line: " + std::to_string(line_number) + ")\n";
  }
};

// Stmt -> 'return' [Exp] ';'
struct ReturnStmtNode : public Statement {
  std::optional<std::unique_ptr<Expression>> return_value;

  ReturnStmtNode(int line,
                 std::optional<std::unique_ptr<Expression>> val = std::nullopt)
      : Statement(line), return_value(std::move(val)) {}

  std::string toString(int indentLevel = 0) const override {
    std::ostringstream oss;
    oss << indent(indentLevel) << "ReturnStmtNode (Line: " << line_number
        << ") {\n";
    if (return_value && return_value.value()) {
      oss << indent(indentLevel + 1) << "ReturnValue:\n";
      oss << return_value.value()->toString(indentLevel + 2);
    } else {
      oss << indent(indentLevel + 1) << "ReturnValue: <void_or_nullopt>\n";
    }
    oss << indent(indentLevel) << "}\n";
    return oss.str();
  }
};

// Stmt -> LVal = 'getint''('')'';'
struct GetIntStmtNode : public Statement {
  std::unique_ptr<LValNode> lval;

  GetIntStmtNode(int line, std::unique_ptr<LValNode> lv)
      : Statement(line), lval(std::move(lv)) {}

  std::string toString(int indentLevel = 0) const override {
    std::ostringstream oss;
    oss << indent(indentLevel) << "GetIntStmtNode (Line: " << line_number
        << ") {\n";
    oss << indent(indentLevel + 1) << "LValue (Target):\n";
    if (lval)
      oss << lval->toString(indentLevel + 2);
    else
      oss << indent(indentLevel + 2) << "<null_lval>\n";
    oss << indent(indentLevel) << "}\n";
    return oss.str();
  }
};

// Stmt -> 'printf' '('FormatString {',' Exp} ')'';'
struct PrintfStmtNode : public Statement {
  Token format_string_token; // The FORMAT_STRING token
  std::vector<std::unique_ptr<Expression>> arguments;

  PrintfStmtNode(int line, Token fmt_str_token,
                 std::vector<std::unique_ptr<Expression>> args)
      : Statement(line), format_string_token(std::move(fmt_str_token)),
        arguments(std::move(args)) {}

  std::string toString(int indentLevel = 0) const override {
    std::ostringstream oss;
    std::string format_str_lexeme = format_string_token.lexeme;
    // 간단한 문자열 정리 (예: \n을 \\n으로 표시)
    // Replace newline characters with literal "\n" for cleaner single-line
    // display in the format string part
    size_t pos = 0;
    while ((pos = format_str_lexeme.find('\n', pos)) != std::string::npos) {
      format_str_lexeme.replace(pos, 1, "\\n");
      pos += 2; // length of "\n"
    }

    oss << indent(indentLevel) << "PrintfStmtNode (Format: \""
        << format_str_lexeme << "\", Line: " << line_number << ") {\n";
    if (!arguments.empty()) {
      oss << indent(indentLevel + 1) << "Arguments:\n";
      for (const auto &arg : arguments) {
        if (arg)
          oss << arg->toString(indentLevel + 2);
        else
          oss << indent(indentLevel + 2) << "<null_arg>\n";
      }
    } else {
      oss << indent(indentLevel + 1) << "Arguments: <none>\n";
    }
    oss << indent(indentLevel) << "}\n";
    return oss.str();
  }
};

// ----- Initializers -----
// InitVal -> Exp | '{' [ InitVal { ',' InitVal } ] '}' -> for 1D array: '{' [
// Exp { ',' Exp } ] '}' ConstInitVal -> ConstExp | '{' [ ConstInitVal { ','
// ConstInitVal } ] '}' -> for 1D array: '{' [ ConstExp { ',' ConstExp } ] '}'
struct InitializerNode : public ASTNode {
  bool is_array_initializer = false;
  std::unique_ptr<Expression> single_init_expr; // Used if !is_array_initializer
  std::vector<std::unique_ptr<Expression>>
      array_element_inits; // Used if is_array_initializer (for 1D arrays)

  // Constructor for single expression initializer
  InitializerNode(int line, std::unique_ptr<Expression> expr)
      : ASTNode(line), single_init_expr(std::move(expr)) {}

  // Constructor for 1D array initializer (list of expressions)
  InitializerNode(int line, std::vector<std::unique_ptr<Expression>> expr_list)
      : ASTNode(line), is_array_initializer(true),
        array_element_inits(std::move(expr_list)) {}

  std::string toString(int indentLevel = 0) const override {
    std::ostringstream oss;
    oss << indent(indentLevel) << "InitializerNode (Line: " << line_number
        << ") {\n";
    if (is_array_initializer) {
      oss << indent(indentLevel + 1) << "Type: ArrayInitializer\n";
      if (array_element_inits.empty()) {
        oss << indent(indentLevel + 1) << "Elements: <empty_array_init>\n";
      } else {
        oss << indent(indentLevel + 1) << "Elements:\n";
        for (const auto &elem : array_element_inits) {
          if (elem)
            oss << elem->toString(indentLevel + 2);
          else
            oss << indent(indentLevel + 2) << "<null_elem>\n";
        }
      }
    } else {
      oss << indent(indentLevel + 1) << "Type: SingleExpressionInitializer\n";
      if (single_init_expr) {
        oss << single_init_expr->toString(indentLevel + 1);
      } else {
        oss << indent(indentLevel + 1) << "<null_single_expr>\n";
      }
    }
    oss << indent(indentLevel) << "}\n";
    return oss.str();
  }
};

// ----- Definitions (parts of Declarations) -----
// ConstDef -> Ident [ '[' ConstExp ']' ] '=' ConstInitVal
struct ConstDefNode : public ASTNode {
  Token identifier;
  std::optional<std::unique_ptr<Expression>> array_size_expr; // ConstExp
  std::unique_ptr<InitializerNode> initializer;               // ConstInitVal

  ConstDefNode(int line, Token id, std::unique_ptr<InitializerNode> init,
               std::optional<std::unique_ptr<Expression>> size = std::nullopt)
      : ASTNode(line), identifier(std::move(id)),
        array_size_expr(std::move(size)), initializer(std::move(init)) {}

  std::string toString(int indentLevel = 0) const override {
    std::ostringstream oss;
    oss << indent(indentLevel) << "ConstDefNode (Ident: " << identifier.lexeme;
    if (array_size_expr && array_size_expr.value()) {
      oss << "[ARRAY], Line: " << line_number << ") {\n";
      oss << indent(indentLevel + 1) << "ArraySize:\n";
      oss << array_size_expr.value()->toString(indentLevel + 2);
    } else {
      oss << ", Line: " << line_number << ") {\n";
    }
    oss << indent(indentLevel + 1) << "Initializer:\n";
    if (initializer)
      oss << initializer->toString(indentLevel + 2);
    else
      oss << indent(indentLevel + 2) << "<null_init>\n";
    oss << indent(indentLevel) << "}\n";
    return oss.str();
  }
};

// VarDef -> Ident [ '[' ConstExp ']' ] | Ident [ '[' ConstExp ']' ] '=' InitVal
struct VarDefNode : public ASTNode {
  Token identifier;
  std::optional<std::unique_ptr<Expression>> array_size_expr;  // ConstExp
  std::optional<std::unique_ptr<InitializerNode>> initializer; // InitVal

  VarDefNode(
      int line, Token id,
      std::optional<std::unique_ptr<Expression>> size = std::nullopt,
      std::optional<std::unique_ptr<InitializerNode>> init = std::nullopt)
      : ASTNode(line), identifier(std::move(id)),
        array_size_expr(std::move(size)), initializer(std::move(init)) {}

  std::string toString(int indentLevel = 0) const override {
    std::ostringstream oss;
    oss << indent(indentLevel) << "VarDefNode (Ident: " << identifier.lexeme;
    if (array_size_expr && array_size_expr.value()) {
      oss << "[ARRAY], Line: " << line_number << ") {\n";
      oss << indent(indentLevel + 1) << "ArraySize:\n";
      oss << array_size_expr.value()->toString(indentLevel + 2);
    } else {
      oss << ", Line: " << line_number << ") {\n";
    }
    if (initializer && initializer.value()) {
      oss << indent(indentLevel + 1) << "Initializer:\n";
      oss << initializer.value()->toString(indentLevel + 2);
    } else {
      oss << indent(indentLevel + 1) << "Initializer: <none_or_nullopt>\n";
    }
    oss << indent(indentLevel) << "}\n";
    return oss.str();
  }
};

// ----- Declarations (which are also BlockItems) -----
struct Declaration : public BlockItemNode {
  using BlockItemNode::BlockItemNode;
};

// ConstDecl -> 'const' BType ConstDef { ',' ConstDef } ';'
struct ConstDeclNode : public Declaration {
  // BType is implicitly 'int' as per grammar BType -> 'int'
  std::vector<std::unique_ptr<ConstDefNode>> const_defs;

  ConstDeclNode(int line, std::vector<std::unique_ptr<ConstDefNode>> defs)
      : Declaration(line), const_defs(std::move(defs)) {}

  std::string toString(int indentLevel = 0) const override {
    std::ostringstream oss;
    oss << indent(indentLevel) << "ConstDeclNode (Line: " << line_number
        << ") {\n";
    if (const_defs.empty()) {
      oss << indent(indentLevel + 1) << "<empty_const_decl>\n";
    } else {
      for (const auto &def : const_defs) {
        if (def)
          oss << def->toString(indentLevel + 1);
        else
          oss << indent(indentLevel + 1) << "<null_def>\n";
      }
    }
    oss << indent(indentLevel) << "}\n";
    return oss.str();
  }
};

// VarDecl -> BType VarDef { ',' VarDef } ';'
struct VarDeclNode : public Declaration {
  // BType is implicitly 'int'
  std::vector<std::unique_ptr<VarDefNode>> var_defs;

  VarDeclNode(int line, std::vector<std::unique_ptr<VarDefNode>> defs)
      : Declaration(line), var_defs(std::move(defs)) {}

  std::string toString(int indentLevel = 0) const override {
    std::ostringstream oss;
    oss << indent(indentLevel) << "VarDeclNode (Line: " << line_number
        << ") {\n";
    if (var_defs.empty()) {
      oss << indent(indentLevel + 1) << "<empty_var_decl>\n";
    } else {
      for (const auto &def : var_defs) {
        if (def)
          oss << def->toString(indentLevel + 1);
        else
          oss << indent(indentLevel + 1) << "<null_def>\n";
      }
    }
    oss << indent(indentLevel) << "}\n";
    return oss.str();
  }
};

// ----- Function Related (Top-level definitions, not BlockItems) -----
// FuncType -> 'void' | 'int'
enum class FunctionReturnType { VOID, INT };

// MOVE THIS DEFINITION HERE - BEFORE FuncDefNode uses it
inline std::string funcReturnTypeToString(FunctionReturnType rt) {
  return rt == FunctionReturnType::VOID ? "void" : "int";
}

struct FuncParamNode : public ASTNode {
  Token identifier;

  FuncParamNode(int line, Token id)
      : ASTNode(line), identifier(std::move(id)) {}

  std::string toString(int indentLevel = 0) const override {
    std::ostringstream oss;
    oss << indent(indentLevel)
        << "FuncParamNode (Type: int, Name: " << identifier.lexeme
        << ", Line: " << line_number << ")\n";
    return oss.str();
  }
};

struct FuncDefNode : public ASTNode {
  FunctionReturnType return_type;
  Token func_name_ident;
  std::vector<std::unique_ptr<FuncParamNode>> parameters;
  std::unique_ptr<BlockNode> body;

  FuncDefNode(int line, FunctionReturnType rt, Token name,
              std::vector<std::unique_ptr<FuncParamNode>> params,
              std::unique_ptr<BlockNode> blk)
      : ASTNode(line), return_type(rt), func_name_ident(std::move(name)),
        parameters(std::move(params)), body(std::move(blk)) {}

  std::string toString(int indentLevel = 0) const override {
    std::ostringstream oss;
    oss << indent(indentLevel)
        << "FuncDefNode (Name: " << func_name_ident.lexeme << ", ReturnType: "
        << funcReturnTypeToString(return_type) // This call should now be valid
        << ", Line: " << line_number << ") {\n";
    if (!parameters.empty()) {
      oss << indent(indentLevel + 1) << "Parameters:\n";
      for (const auto &param : parameters) {
        if (param)
          oss << param->toString(indentLevel + 2);
        else
          oss << indent(indentLevel + 2) << "<null_param>\n";
      }
    } else {
      oss << indent(indentLevel + 1) << "Parameters: <none>\n";
    }
    oss << indent(indentLevel + 1) << "Body:\n";
    if (body)
      oss << body->toString(indentLevel + 2);
    else
      oss << indent(indentLevel + 2) << "<null_body>\n";
    oss << indent(indentLevel) << "}\n";
    return oss.str();
  }
};

// MainFuncDef -> 'int' 'main' '(' ')' Block
struct MainFuncDefNode
    : public ASTNode { // Not a BlockItem, a top-level definition
  std::unique_ptr<BlockNode> body;

  MainFuncDefNode(int line, std::unique_ptr<BlockNode> blk)
      : ASTNode(line), body(std::move(blk)) {}

  std::string toString(int indentLevel = 0) const override {
    std::ostringstream oss;
    oss << indent(indentLevel)
        << "MainFuncDefNode (Name: main, ReturnType: int, Line: " << line_number
        << ") {\n";
    oss << indent(indentLevel + 1) << "Body:\n";
    if (body)
      oss << body->toString(indentLevel + 2);
    else
      oss << indent(indentLevel + 2) << "<null_body>\n";
    oss << indent(indentLevel) << "}\n";
    return oss.str();
  }
};

// ----- Compilation Unit (The Root of the AST) -----
// CompUnit -> {Decl} {FuncDef} MainFuncDef
struct CompUnitNode : public ASTNode {
  std::vector<std::unique_ptr<Declaration>> declarations; // Global declarations
  std::vector<std::unique_ptr<FuncDefNode>> function_definitions;
  std::unique_ptr<MainFuncDefNode> main_function_definition;

  CompUnitNode(int line, std::vector<std::unique_ptr<Declaration>> decls,
               std::vector<std::unique_ptr<FuncDefNode>> func_defs,
               std::unique_ptr<MainFuncDefNode> main_func_def)
      : ASTNode(line), declarations(std::move(decls)),
        function_definitions(std::move(func_defs)),
        main_function_definition(std::move(main_func_def)) {}

  std::string toString(int indentLevel = 0) const override {
    std::ostringstream oss;
    oss << indent(indentLevel) << "CompUnitNode (Line: " << line_number
        << ") {\n";

    if (!declarations.empty()) {
      oss << indent(indentLevel + 1) << "GlobalDeclarations:\n";
      for (const auto &decl : declarations) {
        if (decl)
          oss << decl->toString(indentLevel + 2);
        else
          oss << indent(indentLevel + 2) << "<null_decl>\n";
      }
    } else {
      oss << indent(indentLevel + 1) << "GlobalDeclarations: <none>\n";
    }

    if (!function_definitions.empty()) {
      oss << indent(indentLevel + 1) << "FunctionDefinitions:\n";
      for (const auto &func_def : function_definitions) {
        if (func_def)
          oss << func_def->toString(indentLevel + 2);
        else
          oss << indent(indentLevel + 2) << "<null_func_def>\n";
      }
    } else {
      oss << indent(indentLevel + 1) << "FunctionDefinitions: <none>\n";
    }

    oss << indent(indentLevel + 1) << "MainFunctionDefinition:\n";
    if (main_function_definition) {
      oss << main_function_definition->toString(indentLevel + 2);
    } else {
      oss << indent(indentLevel + 2)
          << "<null_main_func_def_ERROR>\n"; // Should always exist
    }

    oss << indent(indentLevel) << "}\n";
    return oss.str();
  }
};

#endif // AST_H