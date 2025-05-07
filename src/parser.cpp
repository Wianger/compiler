#include "parser.h"
#include <string> // For std::to_string in error messages

Parser::Parser(std::vector<Token> t_tokens) : tokens(std::move(t_tokens)) {
  // Filter out UNKNOWN tokens? Or let parser handle them.
  // For now, assume lexer provides a clean stream or parser handles UNKNOWN.
  // Add a dummy EOF token if not already present, to simplify boundary checks.
  if (tokens.empty() || tokens.back().type != TokenType::END_OF_FILE) {
    int line = tokens.empty() ? 1 : tokens.back().line;
    int col = tokens.empty() ? 1 : tokens.back().column + 1;
    tokens.emplace_back(TokenType::END_OF_FILE, "", line, col);
  }
}

const Token &Parser::currentToken() const {
  if (current_token_idx >= tokens.size()) {
    // Should not happen if EOF is always present and not consumed past.
    // Return the last token (EOF) if out of bounds.
    return tokens.back();
  }
  return tokens[current_token_idx];
}

const Token &Parser::peekToken(size_t offset) const {
  if (current_token_idx + offset >= tokens.size()) {
    return tokens.back(); // EOF
  }
  return tokens[current_token_idx + offset];
}

Token Parser::consumeToken() {
  const Token &current = currentToken();
  if (current.type != TokenType::END_OF_FILE) { // Don't advance past EOF
    current_token_idx++;
  }
  return current; // Return the token that was consumed
}

Token Parser::expectToken(TokenType expected_type) {
  Token current = currentToken();
  if (current.type == expected_type) {
    consumeToken();
    return current;
  }
  error("Expected token " + tokenTypeToString(expected_type) + " but got " +
            tokenTypeToString(current.type),
        current);
  // error() should throw, so this part is unreachable if error() is
  // [[noreturn]] or throws. If not, we need a way to signal failure. For now,
  // assume error() throws ParseError
  return current; // Should be unreachable if error throws
}

bool Parser::matchToken(TokenType type) {
  if (checkCurrentTokenType(type)) {
    consumeToken();
    return true;
  }
  return false;
}

bool Parser::checkCurrentTokenType(TokenType type) const {
  return !tokens.empty() && current_token_idx < tokens.size() &&
         currentToken().type == type;
}

bool Parser::checkPeekTokenType(TokenType type, size_t offset) const {
  return !tokens.empty() && (current_token_idx + offset) < tokens.size() &&
         peekToken(offset).type == type;
}

// Error reporting
void Parser::error(const std::string &message, const Token &token_ref) {
  throw ParseError("Parser Error at Line " + std::to_string(token_ref.line) +
                   ", Column " + std::to_string(token_ref.column) + ": " +
                   message + " (Lexeme: '" + token_ref.lexeme + "')");
}

void Parser::error(const std::string &message) {
  error(message, currentToken());
}

// Main parsing entry point
std::unique_ptr<CompUnitNode> Parser::parse() {
  current_token_idx = 0;
  try {
    return parseCompUnit();
  } catch (const ParseError &e) {
    // For now, rethrow. In a real compiler, you might collect multiple errors.
    // std::cerr << e.what() << std::endl; // Example of logging
    throw;
  }
}

// Replace the parseCompUnit placeholder with this:
std::unique_ptr<CompUnitNode> Parser::parseCompUnit() {
  int start_line = currentToken().line;
  std::vector<std::unique_ptr<Declaration>> global_decls;
  std::vector<std::unique_ptr<FuncDefNode>> func_defs;
  std::unique_ptr<MainFuncDefNode> main_func_def = nullptr;

  while (true) {
    if (checkCurrentTokenType(TokenType::END_OF_FILE)) {
      break; // End of input tokens
    }

    // Lookahead logic:
    Token t0 = currentToken();
    Token t1 = peekToken(1); // Token after current
    Token t2 = peekToken(2); // Token two after current

    if (t0.type == TokenType::CONST_KW) {
      global_decls.push_back(parseConstDecl());
    } else if (t0.type == TokenType::INT_KW) {
      if (t1.type == TokenType::MAIN_KW && t2.type == TokenType::LPAREN) {
        if (main_func_def) {
          error("Multiple main function definitions found.", t1);
        }
        main_func_def = parseMainFuncDef();
        // After main, we typically expect EOF. Loop will break if so.
        // If not EOF, the next iteration will error or parse if valid (though
        // grammar implies main is last)
      } else if (t1.type == TokenType::IDENT && t2.type == TokenType::LPAREN) {
        func_defs.push_back(parseFuncDef());
      } else {
        global_decls.push_back(parseVarDecl());
      }
    } else if (t0.type == TokenType::VOID_KW) {
      if (t1.type == TokenType::IDENT && t2.type == TokenType::LPAREN) {
        func_defs.push_back(parseFuncDef());
      } else {
        error(
            "Expected identifier and '(' after 'void' for function definition.",
            t1);
        break; // Unrecoverable, stop parsing CompUnit
      }
    } else {
      // If main_func_def has been parsed and we still have tokens, it's an
      // error
      if (main_func_def && t0.type != TokenType::END_OF_FILE) {
        error("Unexpected tokens after main function definition: " +
                  tokenTypeToString(t0.type) + " ('" + t0.lexeme + "')",
              t0);
        break;
      }
      // If not EOF and not a recognized global item, it's an error
      if (t0.type != TokenType::END_OF_FILE) { // Check again as main_func_def
                                               // might have consumed to EOF
        error("Unexpected token at global scope: " +
                  tokenTypeToString(t0.type) + " ('" + t0.lexeme + "')",
              t0);
        break; // Unrecoverable
      }
      // If it was EOF, the main loop condition will catch it.
    }
  }

  if (!main_func_def) {
    // If EOF was reached and no main function, error at the last sensible token
    // or a generic one.
    Token error_token = (tokens.size() > 1 &&
                         tokens[tokens.size() - 2].type != TokenType::UNKNOWN)
                            ? tokens[tokens.size() - 2]
                            : tokens.back();
    error("Main function ('int main() { ... }') definition not found in "
          "compilation unit.",
          error_token);
  }

  return std::make_unique<CompUnitNode>(start_line, std::move(global_decls),
                                        std::move(func_defs),
                                        std::move(main_func_def));
}

// --- Placeholder implementations for parsing methods ---
// These will be filled in one by one.

std::unique_ptr<Declaration> Parser::parseDecl() {
  int line = currentToken().line;
  if (checkCurrentTokenType(TokenType::CONST_KW)) {
    return parseConstDecl();
  } else if (checkCurrentTokenType(TokenType::INT_KW)) {
    return parseVarDecl();
  } else {
    error("Expected 'const' or 'int' to start a declaration.", currentToken());
    return nullptr;
  }
}

std::unique_ptr<ConstDeclNode> Parser::parseConstDecl() {
  Token const_tok = expectToken(TokenType::CONST_KW);
  expectToken(TokenType::INT_KW); // BType is 'int'

  std::vector<std::unique_ptr<ConstDefNode>> const_defs;
  const_defs.push_back(parseConstDef());

  while (matchToken(TokenType::COMMA)) { // Consumes ',' if present
    const_defs.push_back(parseConstDef());
  }

  expectToken(TokenType::SEMICOLON);
  return std::make_unique<ConstDeclNode>(const_tok.line, std::move(const_defs));
}

std::unique_ptr<ConstDefNode> Parser::parseConstDef() {
  Token ident_tok = expectToken(TokenType::IDENT);
  int line = ident_tok.line;

  std::optional<std::unique_ptr<Expression>> array_size_expr = std::nullopt;
  if (matchToken(TokenType::LBRACKET)) { // Consumes '[' if present
    array_size_expr = parseConstExp();   // ConstExp -> AddExp
    expectToken(TokenType::RBRACKET);
  }

  expectToken(TokenType::ASSIGN); // Expect '='
  std::unique_ptr<InitializerNode> initializer = parseConstInitVal();

  return std::make_unique<ConstDefNode>(line, ident_tok, std::move(initializer),
                                        std::move(array_size_expr));
}

std::unique_ptr<InitializerNode> Parser::parseConstInitVal() {
  int line = currentToken().line;
  if (matchToken(
          TokenType::LBRACE)) { // Consumes '{' if present (Array Initializer)
    std::vector<std::unique_ptr<Expression>> elements;
    if (!checkCurrentTokenType(
            TokenType::RBRACE)) {            // If not empty initializer {}
      elements.push_back(parseConstExp());   // First element is ConstExp
      while (matchToken(TokenType::COMMA)) { // Consumes ',' if present
        elements.push_back(parseConstExp()); // Subsequent elements are ConstExp
      }
    }
    expectToken(TokenType::RBRACE);
    return std::make_unique<InitializerNode>(line, std::move(elements));
  } else { // Single ConstExpression Initializer
    std::unique_ptr<Expression> const_expr = parseConstExp();
    return std::make_unique<InitializerNode>(line, std::move(const_expr));
  }
}

std::unique_ptr<VarDeclNode> Parser::parseVarDecl() {
  Token int_tok = expectToken(TokenType::INT_KW); // BType is 'int'

  std::vector<std::unique_ptr<VarDefNode>> var_defs;
  var_defs.push_back(parseVarDef());

  while (matchToken(TokenType::COMMA)) { // Consumes ',' if present
    var_defs.push_back(parseVarDef());
  }

  expectToken(TokenType::SEMICOLON);
  return std::make_unique<VarDeclNode>(int_tok.line, std::move(var_defs));
}

std::unique_ptr<VarDefNode> Parser::parseVarDef() {
  Token ident_tok = expectToken(TokenType::IDENT);
  int line = ident_tok.line;

  std::optional<std::unique_ptr<Expression>> array_size_expr = std::nullopt;
  if (matchToken(TokenType::LBRACKET)) { // Consumes '[' if present
    array_size_expr = parseConstExp();   // Array size must be a ConstExp
    expectToken(TokenType::RBRACKET);
  }

  std::optional<std::unique_ptr<InitializerNode>> initializer = std::nullopt;
  if (matchToken(TokenType::ASSIGN)) { // Consumes '=' if present
    initializer = parseInitVal();
  }

  return std::make_unique<VarDefNode>(
      line, ident_tok, std::move(array_size_expr), std::move(initializer));
}

std::unique_ptr<InitializerNode> Parser::parseInitVal() {
  int line = currentToken().line;
  if (matchToken(
          TokenType::LBRACE)) { // Consumes '{' if present (Array Initializer)
    std::vector<std::unique_ptr<Expression>> elements;
    if (!checkCurrentTokenType(
            TokenType::RBRACE)) {            // If not empty initializer {}
      elements.push_back(parseExp());        // First element is Exp
      while (matchToken(TokenType::COMMA)) { // Consumes ',' if present
        elements.push_back(parseExp());      // Subsequent elements are Exp
      }
    }
    expectToken(TokenType::RBRACE);
    return std::make_unique<InitializerNode>(line, std::move(elements));
  } else { // Single Expression Initializer
    std::unique_ptr<Expression> expr = parseExp();
    return std::make_unique<InitializerNode>(line, std::move(expr));
  }
}

std::unique_ptr<FuncDefNode> Parser::parseFuncDef() {
  // FuncType Ident '(' [FuncFParams] ')' Block
  int start_line =
      currentToken()
          .line; // Line of FuncType token before consumption by parseFuncType
  FunctionReturnType func_type = parseFuncType();
  Token func_name_ident = expectToken(TokenType::IDENT);

  expectToken(TokenType::LPAREN);

  std::vector<std::unique_ptr<FuncParamNode>> params_vec;
  if (!checkCurrentTokenType(TokenType::RPAREN)) {
    params_vec = parseFuncFParams();
  }

  expectToken(TokenType::RPAREN);
  std::unique_ptr<BlockNode> body = parseBlock();

  return std::make_unique<FuncDefNode>(start_line, func_type, func_name_ident,
                                       std::move(params_vec), std::move(body));
}

FunctionReturnType Parser::parseFuncType() {
  Token type_token = currentToken();
  if (matchToken(TokenType::VOID_KW)) {
    return FunctionReturnType::VOID;
  } else if (matchToken(TokenType::INT_KW)) {
    return FunctionReturnType::INT;
  } else {
    error("Expected function return type ('void' or 'int') but got " +
              tokenTypeToString(type_token.type),
          type_token);
    return FunctionReturnType::INT; // Should be unreachable
  }
}

std::vector<std::unique_ptr<FuncParamNode>> Parser::parseFuncFParams() {
  std::vector<std::unique_ptr<FuncParamNode>> params;
  params.push_back(parseFuncFParam());

  while (matchToken(TokenType::COMMA)) {
    params.push_back(parseFuncFParam());
  }
  return params;
}

std::unique_ptr<FuncParamNode> Parser::parseFuncFParam() {
  // FuncFParam -> BType Ident [ '[' ']' { '[' ConstExp ']' } ]
  // For Phase 1, we only support BType Ident [ '[' ']' ]

  Token btype_token = expectToken(TokenType::INT_KW); // BType is 'int'
  Token ident_token = expectToken(TokenType::IDENT);
  int line_for_node =
      btype_token
          .line; // Or ident_token.line, depends on desired line number source

  bool is_array = false;
  // std::vector<std::unique_ptr<Expression>> higher_dims; // For future
  // multi-dim support

  if (matchToken(TokenType::LBRACKET)) { // Consumes '[' if present
    // For Phase 1, we expect an immediate ']' for a simple array parameter like
    // 'name[]'
    expectToken(TokenType::RBRACKET); // Consumes ']'
    is_array = true;

    // Future (Phase 2) logic for handling more dimensions like name[][ConstExp]
    // would go here: while (matchToken(TokenType::LBRACKET)) {
    //   higher_dims.push_back(parseConstExp());
    //   expectToken(TokenType::RBRACKET);
    // }
  }

  // Constructor for FuncParamNode (from ast.h) is:
  // FuncParamNode(int line, Token type_tok, Token id, bool is_arr)
  return std::make_unique<FuncParamNode>(line_for_node, btype_token,
                                         ident_token, is_array);
}

std::unique_ptr<MainFuncDefNode> Parser::parseMainFuncDef() {
  Token int_tok = expectToken(TokenType::INT_KW);
  // The line number for MainFuncDefNode can be taken from the 'int' token.
  int line = int_tok.line;

  expectToken(TokenType::MAIN_KW);
  expectToken(TokenType::LPAREN);
  expectToken(TokenType::RPAREN);

  std::unique_ptr<BlockNode> body =
      parseBlock(); // parseBlock is already implemented

  return std::make_unique<MainFuncDefNode>(line, std::move(body));
}

std::unique_ptr<BlockNode> Parser::parseBlock() {
  Token lbrace_token = expectToken(TokenType::LBRACE); // Consume '{'
  std::vector<std::unique_ptr<BlockItemNode>> items;

  // Keep parsing BlockItems until we see '}' or EOF (error case for
  // unterminated block)
  while (!checkCurrentTokenType(TokenType::RBRACE) &&
         !checkCurrentTokenType(TokenType::END_OF_FILE)) {
    items.push_back(parseBlockItem());
  }

  expectToken(TokenType::RBRACE); // Consume '}' or error if EOF
  return std::make_unique<BlockNode>(lbrace_token.line, std::move(items));
}

std::unique_ptr<BlockItemNode> Parser::parseBlockItem() {
  // BlockItem -> Decl | Stmt
  // Decl starts with 'const' BType or BType (which is 'int' in SysY).
  // Inside a block, 'int main()' or 'int func()' definitions are not expected.
  if (checkCurrentTokenType(TokenType::CONST_KW) ||
      checkCurrentTokenType(TokenType::INT_KW)) {
    // This assumes 'const' or 'int' at the start of a block item is a
    // declaration.
    return parseDecl();
  } else {
    return parseStmt();
  }
}

std::unique_ptr<Statement> Parser::parseStmt() {
  int line = currentToken().line;

  // Case 1: Block statement
  if (checkCurrentTokenType(TokenType::LBRACE)) {
    return parseBlock();
  }
  // Case 2: 'if' statement
  else if (matchToken(TokenType::IF_KW)) {
    Token if_tok =
        tokens[current_token_idx - 1]; // The 'if' token (already consumed)

    expectToken(TokenType::LPAREN);
    std::unique_ptr<Expression> condition = parseCond(); // Cond -> LOrExp
    expectToken(TokenType::RPAREN);

    std::unique_ptr<Statement> then_stmt = parseStmt();

    std::optional<std::unique_ptr<Statement>> else_stmt_opt = std::nullopt;
    if (matchToken(TokenType::ELSE_KW)) { // Consumed 'else' if present
      else_stmt_opt = parseStmt();
    }

    return std::make_unique<IfStmtNode>(if_tok.line, std::move(condition),
                                        std::move(then_stmt),
                                        std::move(else_stmt_opt));
  }
  // Case 3: 'while' statement - IMPLEMENT THIS NOW
  else if (matchToken(TokenType::WHILE_KW)) {
    Token while_tok =
        tokens[current_token_idx - 1]; // The 'while' token (already consumed)

    expectToken(TokenType::LPAREN);
    std::unique_ptr<Expression> condition = parseCond(); // Cond -> LOrExp
    expectToken(TokenType::RPAREN);

    std::unique_ptr<Statement> body_stmt = parseStmt();

    return std::make_unique<WhileStmtNode>(while_tok.line, std::move(condition),
                                           std::move(body_stmt));
  }
  // Case 4: 'break' statement
  else if (matchToken(TokenType::BREAK_KW)) {
    Token break_tok = tokens[current_token_idx - 1]; // The 'break' token itself
    expectToken(TokenType::SEMICOLON);
    return std::make_unique<BreakStmtNode>(break_tok.line);
  }
  // Case 5: 'continue' statement
  else if (matchToken(TokenType::CONTINUE_KW)) {
    Token continue_tok = tokens[current_token_idx - 1]; // The 'continue' token
    expectToken(TokenType::SEMICOLON);
    return std::make_unique<ContinueStmtNode>(continue_tok.line);
  }
  // Case 6: 'return' statement
  else if (matchToken(TokenType::RETURN_KW)) {
    Token return_tok = tokens[current_token_idx - 1];
    std::optional<std::unique_ptr<Expression>> return_expr_opt = std::nullopt;
    if (!checkCurrentTokenType(TokenType::SEMICOLON)) {
      return_expr_opt = parseExp();
    }
    expectToken(TokenType::SEMICOLON);
    return std::make_unique<ReturnStmtNode>(return_tok.line,
                                            std::move(return_expr_opt));
  }
  // Case 7: 'printf' statement - IMPLEMENT THIS NOW
  else if (matchToken(TokenType::PRINTF_KW)) {
    Token printf_tok =
        tokens[current_token_idx - 1]; // The 'printf' token (already consumed)

    expectToken(TokenType::LPAREN);
    Token format_string_tok = expectToken(TokenType::FORMAT_STRING);

    std::vector<std::unique_ptr<Expression>> arguments;
    // Check for arguments only if the next token is a comma
    // The grammar {',' Exp} means zero or more additional expressions after the
    // format string. The first expression is not preceded by a comma if it's
    // the only one after format string according to standard C printf, but SysY
    // grammar is FormatString {',' Exp}. This implies format string is
    // mandatory, then zero or more COMMA Exp pairs.
    while (checkCurrentTokenType(TokenType::COMMA)) {
      consumeToken(); // Consume ','
      arguments.push_back(parseExp());
    }

    expectToken(TokenType::RPAREN);
    expectToken(TokenType::SEMICOLON);

    return std::make_unique<PrintfStmtNode>(printf_tok.line, format_string_tok,
                                            std::move(arguments));
  }
  // Case 8: Empty Statement (just a semicolon)
  else if (checkCurrentTokenType(TokenType::SEMICOLON)) {
    Token semi_token = consumeToken(); // Consume ';'
    return std::make_unique<ExprStmtNode>(semi_token.line, std::nullopt);
  }
  // Case 9: Assignment or Expression Statement (LVal = getint() is now part of
  // normal assignment)
  else {
    int expr_line = currentToken().line;
    Token expression_start_token =
        currentToken(); // For error reporting if LHS is not LVal
    std::unique_ptr<Expression> expr_node =
        parseExp(); // This will be the LVal on LHS

    if (matchToken(TokenType::ASSIGN)) { // Consumed '=' if present
      if (!expr_node) {
        error("Internal error: parseExp returned null before assignment.",
              expression_start_token);
      }

      // Check if the parsed expression is actually an LValNode
      LValNode *lval_raw_ptr = dynamic_cast<LValNode *>(expr_node.get());
      if (!lval_raw_ptr) {
        error(
            "Left-hand side of assignment (before '=') is not a valid LValue.",
            expression_start_token);
      }
      // If it is, release it from expr_node and take ownership in lval_node
      std::unique_ptr<LValNode> lval_node(
          static_cast<LValNode *>(expr_node.release()));

      // The special check for GETINT_KW after ASSIGN is removed here.
      // The RHS is now always parsed as a general expression by parseExp().
      std::unique_ptr<Expression> rhs_expr =
          parseExp(); // This will parse getint() into a FunctionCallNode if
                      // applicable
      expectToken(TokenType::SEMICOLON);
      return std::make_unique<AssignStmtNode>(
          lval_node->line_number, // Or use expression_start_token.line
          std::move(lval_node), std::move(rhs_expr));

    } else if (checkCurrentTokenType(
                   TokenType::SEMICOLON)) { // If ';' follows the expression
                                            // (Expression Statement)
      consumeToken();                       // Consume ';'
      if (!expr_node) {
        error("Internal error: null expression before semicolon for expression "
              "statement.",
              expression_start_token);
      }
      return std::make_unique<ExprStmtNode>(expr_node->line_number,
                                            std::move(expr_node));
    } else {
      error("Expected ';' or '=' after expression in statement, but got " +
                tokenTypeToString(currentToken().type) + " ('" +
                currentToken().lexeme + "')",
            currentToken());
      return nullptr; // Should be unreachable
    }
  }
}

std::unique_ptr<LValNode> Parser::parseLVal() {
  Token ident_token = expectToken(TokenType::IDENT);
  std::unique_ptr<Expression> index_expr = nullptr;

  if (checkCurrentTokenType(TokenType::LBRACKET)) {
    consumeToken();                   // Consume '['
    index_expr = parseExp();          // Parse the index expression
    expectToken(TokenType::RBRACKET); // Expect and consume ']'
  }
  return std::make_unique<LValNode>(ident_token, std::move(index_expr));
}

std::unique_ptr<Expression> Parser::parseExp() {
  // Exp should be the entry point for the full expression hierarchy,
  // starting from the lowest precedence operator (Logical OR).
  // Exp -> LOrExp (if we allow logical ops in general expressions)
  // Original was Exp -> AddExp, which was incorrect for handling operators
  // with lower precedence than Add/Sub.
  return parseLOrExp();
}

std::unique_ptr<Expression> Parser::parseConstExp() {
  // ConstExp -> Exp (semantically checked later)
  // We still parse it as a full expression, starting from LOrExp.
  // The semantic checker will later verify if it evaluates to a constant.
  return parseLOrExp();
}

std::unique_ptr<Expression> Parser::parseCond() {
  // Cond -> LOrExp
  return parseLOrExp();
}

// Expression parsing hierarchy (placeholders initially)
std::unique_ptr<Expression> Parser::parseLOrExp() {
  // LOrExp -> LAndExp { '||' LAndExp }
  auto left_node = parseLAndExp(); // Parse the first LAndExp

  while (checkCurrentTokenType(TokenType::LOGICAL_OR)) {
    Token op_token = consumeToken();  // Consume '||'
    auto right_node = parseLAndExp(); // Parse the LAndExp on the right
    left_node = std::make_unique<BinaryOpNode>(std::move(left_node), op_token,
                                               std::move(right_node));
  }
  return left_node;
}

std::unique_ptr<Expression> Parser::parseLAndExp() {
  // LAndExp -> EqExp { '&&' EqExp }
  auto left_node = parseEqExp(); // Parse the first EqExp

  while (checkCurrentTokenType(TokenType::LOGICAL_AND)) {
    Token op_token = consumeToken(); // Consume '&&'
    auto right_node = parseEqExp();  // Parse the EqExp on the right
    left_node = std::make_unique<BinaryOpNode>(std::move(left_node), op_token,
                                               std::move(right_node));
  }
  return left_node;
}

std::unique_ptr<Expression> Parser::parseEqExp() {
  // EqExp -> RelExp { ('==' | '!=') RelExp }
  auto left_node = parseRelExp(); // Parse the first RelExp

  while (checkCurrentTokenType(TokenType::EQ) ||
         checkCurrentTokenType(TokenType::NEQ)) {
    Token op_token = consumeToken(); // Consume the operator
    auto right_node = parseRelExp(); // Parse the RelExp on the right
    left_node = std::make_unique<BinaryOpNode>(std::move(left_node), op_token,
                                               std::move(right_node));
  }
  return left_node;
}

std::unique_ptr<Expression> Parser::parseRelExp() {
  // RelExp -> AddExp { ('<' | '>' | '<=' | '>=') AddExp }
  auto left_node = parseAddExp(); // Parse the first AddExp

  while (checkCurrentTokenType(TokenType::LT) ||
         checkCurrentTokenType(TokenType::GT) ||
         checkCurrentTokenType(TokenType::LTE) ||
         checkCurrentTokenType(TokenType::GTE)) {
    Token op_token = consumeToken(); // Consume the operator
    auto right_node = parseAddExp(); // Parse the AddExp on the right
    left_node = std::make_unique<BinaryOpNode>(std::move(left_node), op_token,
                                               std::move(right_node));
  }
  return left_node;
}

std::unique_ptr<Expression> Parser::parseAddExp() {
  // AddExp -> MulExp { ('+' | '-') MulExp }
  auto left_node = parseMulExp(); // Parse the first MulExp

  while (checkCurrentTokenType(TokenType::PLUS) ||
         checkCurrentTokenType(TokenType::MINUS)) {
    Token op_token = consumeToken(); // Consume the operator
    auto right_node = parseMulExp(); // Parse the MulExp on the right
    left_node = std::make_unique<BinaryOpNode>(std::move(left_node), op_token,
                                               std::move(right_node));
  }
  return left_node;
}

std::unique_ptr<Expression> Parser::parseMulExp() {
  // MulExp -> UnaryExp { ('*' | '/' | '%') UnaryExp }
  auto left_node = parseUnaryExp(); // Parse the first UnaryExp

  while (checkCurrentTokenType(TokenType::MULTIPLY) ||
         checkCurrentTokenType(TokenType::DIVIDE) ||
         checkCurrentTokenType(TokenType::MODULO)) {
    Token op_token = consumeToken();   // Consume the operator
    auto right_node = parseUnaryExp(); // Parse the UnaryExp on the right
    left_node = std::make_unique<BinaryOpNode>(std::move(left_node), op_token,
                                               std::move(right_node));
  }
  return left_node;
}

std::unique_ptr<Expression> Parser::parseUnaryExp() {
  // UnaryExp → PrimaryExp | Ident '(' [FuncRParams] ')' | Getint '(' ')' |
  // UnaryOp UnaryExp UnaryOp → '+' | '−' | '!'
  int line = currentToken().line;

  // Case 1: UnaryOp UnaryExp
  if (checkCurrentTokenType(TokenType::PLUS) ||
      checkCurrentTokenType(TokenType::MINUS) ||
      checkCurrentTokenType(TokenType::LOGICAL_NOT)) {
    Token op_token = consumeToken();
    auto operand = parseUnaryExp(); // Recursive call for unary op
    return std::make_unique<UnaryOpNode>(op_token, std::move(operand));
  }
  // Case 2: Function Call (Ident '(' [FuncRParams] ')' OR Getint '(' ')')
  else if ((checkCurrentTokenType(TokenType::IDENT) ||
            checkCurrentTokenType(TokenType::GETINT_KW)) &&
           checkPeekTokenType(TokenType::LPAREN)) {
    Token func_name_token = consumeToken(); // Consume IDENT or GETINT_KW

    expectToken(TokenType::LPAREN);
    std::vector<std::unique_ptr<Expression>> args;

    if (func_name_token.type == TokenType::GETINT_KW) {
      // getint() has no arguments. Ensure RPAREN follows.
    } else { // IDENT case, parse arguments if any
      if (!checkCurrentTokenType(TokenType::RPAREN)) {
        args = parseFuncRParams();
      }
    }
    expectToken(TokenType::RPAREN);
    return std::make_unique<FunctionCallNode>(func_name_token, std::move(args));
  }
  // Case 3: PrimaryExp
  else {
    return parsePrimaryExp();
  }
}

std::unique_ptr<Expression> Parser::parsePrimaryExp() {
  // PrimaryExp → '(' Exp ')' | LVal | Number
  int line = currentToken().line;
  if (checkCurrentTokenType(TokenType::LPAREN)) {
    consumeToken(); // Consume '('
    auto exp_node = parseExp();
    expectToken(TokenType::RPAREN); // Expect and consume ')'
    return exp_node;
  } else if (checkCurrentTokenType(TokenType::INT_CONST)) {
    Token num_token = consumeToken();
    // NumberNode constructor handles std::stoi and potential errors
    return std::make_unique<NumberNode>(num_token);
  } else if (checkCurrentTokenType(TokenType::IDENT)) {
    // In PrimaryExp, an IDENT must be the start of an LVal.
    // Function calls are handled in parseUnaryExp by looking for IDENT followed
    // by LPAREN.
    return parseLVal();
  } else {
    error("Unexpected token in PrimaryExp: expected '(', INT_CONST, or IDENT, "
          "but got " +
          tokenTypeToString(currentToken().type) + " ('" +
          currentToken().lexeme + "')");
    return nullptr; // Should be unreachable due to error throwing
  }
}

std::vector<std::unique_ptr<Expression>> Parser::parseFuncRParams() {
  // FuncRParams → Exp { ',' Exp }
  std::vector<std::unique_ptr<Expression>> args;

  if (!checkCurrentTokenType(
          TokenType::RPAREN)) { // If not ')', there must be at least one Exp
    args.push_back(parseExp());
    while (checkCurrentTokenType(TokenType::COMMA)) {
      consumeToken(); // Consume ','
      args.push_back(parseExp());
    }
  }
  return args;
}
