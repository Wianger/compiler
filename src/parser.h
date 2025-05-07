#ifndef PARSER_H
#define PARSER_H

#include "ast.h" // Include our AST node definitions
#include "token.h"
#include <memory>    // For std::unique_ptr
#include <stdexcept> // For runtime_error
#include <vector>

class Parser {
public:
  explicit Parser(std::vector<Token> tokens);

  std::unique_ptr<CompUnitNode> parse(); // Main entry point

private:
  std::vector<Token> tokens;
  size_t current_token_idx = 0;

  // Helper methods
  const Token &currentToken() const; // Get current token without consuming
  const Token &peekToken(size_t offset = 1) const; // Look ahead
  Token consumeToken(); // Consume current token and advance
  Token expectToken(TokenType expected_type); // Consume if match, else error
  bool matchToken(TokenType type); // Check current token type without consuming
  bool checkCurrentTokenType(TokenType type) const; // Check current token type
  bool checkPeekTokenType(TokenType type,
                          size_t offset = 1) const; // Check next token type

  // --- Recursive Descent Parsing Methods for Grammar Rules ---
  // Each method will parse a part of the grammar and return a corresponding AST
  // node.

  // CompUnit -> {Decl} {FuncDef} MainFuncDef
  std::unique_ptr<CompUnitNode> parseCompUnit();

  // Decl -> ConstDecl | VarDecl
  std::unique_ptr<Declaration> parseDecl();
  std::unique_ptr<ConstDeclNode> parseConstDecl();
  // BType -> 'int' (handled by checking for INT_KW)
  std::unique_ptr<ConstDefNode> parseConstDef();
  std::unique_ptr<InitializerNode>
  parseConstInitVal(); // ConstInitVal -> ConstExp | '{' [ ConstInitVal { ','
                       // ConstInitVal } ] '}'

  std::unique_ptr<VarDeclNode> parseVarDecl();
  std::unique_ptr<VarDefNode> parseVarDef();
  std::unique_ptr<InitializerNode>
  parseInitVal(); // InitVal -> Exp | '{' [ InitVal { ',' InitVal } ] '}'

  // FuncDef -> FuncType Ident '(' [FuncFParams] ')' Block
  std::unique_ptr<FuncDefNode> parseFuncDef();
  FunctionReturnType parseFuncType();
  std::vector<std::unique_ptr<FuncParamNode>> parseFuncFParams();
  std::unique_ptr<FuncParamNode> parseFuncFParam();

  // MainFuncDef -> 'int' 'main' '(' ')' Block
  std::unique_ptr<MainFuncDefNode> parseMainFuncDef();

  // Block -> '{' {BlockItem} '}'
  std::unique_ptr<BlockNode> parseBlock();
  // BlockItem -> Decl | Stmt
  std::unique_ptr<BlockItemNode> parseBlockItem();

  // Stmt -> ...
  std::unique_ptr<Statement> parseStmt();
  std::unique_ptr<LValNode> parseLVal(); // Moved here as it's a prefix for
                                         // multiple Stmt types and also an Exp

  // Expressions (following precedence and associativity)
  // Exp -> AddExp
  // ConstExp -> AddExp
  std::unique_ptr<Expression> parseExp(); // Alias for parseAddExp
  std::unique_ptr<Expression>
  parseConstExp(); // Alias for parseAddExp (semantic check later)
  std::unique_ptr<Expression> parseCond(); // Cond -> LOrExp

  // Precedence: LOrExp -> LAndExp -> EqExp -> RelExp -> AddExp -> MulExp ->
  // UnaryExp -> PrimaryExp
  std::unique_ptr<Expression> parseLOrExp();
  std::unique_ptr<Expression> parseLAndExp();
  std::unique_ptr<Expression> parseEqExp();
  std::unique_ptr<Expression> parseRelExp();
  std::unique_ptr<Expression> parseAddExp();
  std::unique_ptr<Expression> parseMulExp();
  std::unique_ptr<Expression> parseUnaryExp();
  std::unique_ptr<Expression> parsePrimaryExp();
  // Number -> IntConst (handled in parsePrimaryExp)
  // UnaryOp -> '+' | '-' | '!' (handled in parseUnaryExp)
  std::vector<std::unique_ptr<Expression>>
  parseFuncRParams(); // For function calls

  // Error reporting utility
  //[[noreturn]]
  void error(const std::string &message, const Token &error_token);
  //[[noreturn]]
  void error(const std::string &message); // Uses currentToken
};

// Custom exception for parsing errors
class ParseError : public std::runtime_error {
public:
  explicit ParseError(const std::string &message)
      : std::runtime_error(message) {}
};

#endif // PARSER_H