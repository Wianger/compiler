#include "parser.h"
#include <iostream>
#include <memory>

std::string tokenTypeToString(TokenType type) {
  switch (type) {
  case TokenType::IDENFR:
    return "IDENFR";
  case TokenType::INTCON:
    return "INTCON";
  case TokenType::STRCON:
    return "STRCON";
  case TokenType::MAINTK:
    return "MAINTK";
  case TokenType::CONSTTK:
    return "CONSTTK";
  case TokenType::INTTK:
    return "INTTK";
  case TokenType::BREAKTK:
    return "BREAKTK";
  case TokenType::CONTINUETK:
    return "CONTINUETK";
  case TokenType::IFTK:
    return "IFTK";
  case TokenType::ELSETK:
    return "ELSETK";
  case TokenType::WHILETK:
    return "WHILETK";
  case TokenType::GETINTTK:
    return "GETINTTK";
  case TokenType::PRINTFTK:
    return "PRINTFTK";
  case TokenType::RETURNTK:
    return "RETURNTK";
  case TokenType::VOIDTK:
    return "VOIDTK";
  case TokenType::NOT:
    return "NOT";
  case TokenType::AND:
    return "AND";
  case TokenType::OR:
    return "OR";
  case TokenType::PLUS:
    return "PLUS";
  case TokenType::MINU:
    return "MINU";
  case TokenType::MULT:
    return "MULT";
  case TokenType::DIV:
    return "DIV";
  case TokenType::MOD:
    return "MOD";
  case TokenType::LSS:
    return "LSS";
  case TokenType::LEQ:
    return "LEQ";
  case TokenType::GRE:
    return "GRE";
  case TokenType::GEQ:
    return "GEQ";
  case TokenType::EQL:
    return "EQL";
  case TokenType::NEQ:
    return "NEQ";
  case TokenType::ASSIGN:
    return "ASSIGN";
  case TokenType::SEMICN:
    return "SEMICN";
  case TokenType::COMMA:
    return "COMMA";
  case TokenType::LPARENT:
    return "LPARENT";
  case TokenType::RPARENT:
    return "RPARENT";
  case TokenType::LBRACK:
    return "LBRACK";
  case TokenType::RBRACK:
    return "RBRACK";
  case TokenType::LBRACE:
    return "LBRACE";
  case TokenType::RBRACE:
    return "RBRACE";
  case TokenType::ERROR:
    return "ERROR";
  }
  return "UNKNOWN";
}

void Parser::splitStringByDelimiter(const std::string &str,
                                    const std::string &delimiter) {
  size_t start = 0;
  size_t end = str.find(delimiter);
  std::vector<std::string> params;
  while (end != std::string::npos) {
    std::string token = str.substr(start, end - start);
    std::string name = "printfstr" + std::to_string(getPrintfStrCount());
    currentTable->insert(name,
                         std::make_shared<Symbol>(name, SymbolType::STR, true));
    params.push_back(name);
    consume(TokenType::COMMA);
    auto exp = AddExp();
    std::string exp_value = (exp.first == ExpType::Num)
                                ? std::to_string(exp.second)
                                : "t" + std::to_string(exp.second);
    params.push_back(exp_value);
    start = end + delimiter.length();
    end = str.find(delimiter, start);
  }
  std::string lastToken = str.substr(start, end);
  std::string name = "printfstr" + std::to_string(getPrintfStrCount());
  currentTable->insert(name,
                       std::make_shared<Symbol>(name, SymbolType::STR, true));
  params.push_back(name);
  for (const auto &param : params)
    emit("param", param);
  emit("call", "printf", std::to_string(params.size()));
}

void Parser::emit(const std::string &op, const std::string &arg1,
                  const std::string &arg2, const std::string &result) {
  tac.push_back({op, arg1, arg2, result});
}

int Parser::getTempVar() { return tempCount++; }

int Parser::getLabel() { return labelCount++; }

int Parser::getPrintfStrCount() { return printfstrCount++; }

void Parser::enterScope() {
  auto newTable = std::make_shared<SymbolTable>(currentTable);
  currentTable->addChild(newTable);
  currentTable = newTable;
}

void Parser::exitScope() {
  if (currentTable->getParent() != nullptr)
    currentTable = currentTable->getParent();
  else
    throw std::runtime_error("No parent scope to exit");
}

Token Parser::currentToken() const {
  if (position < tokens.size())
    return tokens[position];
  return {"", TokenType::ERROR};
}

Token Parser::peek(int offset) const {
  if (position + offset < tokens.size())
    return tokens[position + offset];
  return {"", TokenType::ERROR};
}

bool Parser::match(const TokenType &type, int offset) {
  if (peek(offset).second == type)
    return true;
  return false;
}

void Parser::consume(const TokenType &type) {
  if (match(type))
    position++;
  else
    throw std::runtime_error("Expected token " + tokenTypeToString(type) +
                             " type not found");
}

std::vector<Quadruple> Parser::parse() {
  preprocess();
  CompUnit();
  return tac;
}

void Parser::CompUnit() {
  while (position < tokens.size() &&
         (match(TokenType::CONSTTK) ||
          (match(TokenType::INTTK) && !match(TokenType::LPARENT, 2))))
    Decl(true);
  while (position < tokens.size() &&
         (match(TokenType::VOIDTK) ||
          (match(TokenType::INTTK) && match(TokenType::IDENFR, 1) &&
           match(TokenType::LPARENT, 2))))
    FuncDef();
  if (match(TokenType::INTTK) && match(TokenType::MAINTK, 1) &&
      match(TokenType::LPARENT, 2))
    MainFuncDef();
  else
    throw std::runtime_error("Main function not found");
}

void Parser::Decl(bool isglobal) {
  if (match(TokenType::CONSTTK))
    consume(TokenType::CONSTTK);
  consume(TokenType::INTTK);
  while (match(TokenType::IDENFR)) {
    std::string name = currentToken().first;
    SymbolType type = SymbolType::INT;
    int size = 0;
    consume(TokenType::IDENFR);
    if (match(TokenType::LBRACK)) {
      type = SymbolType::ARRAY;
      consume(TokenType::LBRACK);
      auto exp = AddExp();
      if (exp.first == ExpType::Num)
        size = exp.second;
      else
        throw std::runtime_error("Array size must be a constant");
      consume(TokenType::RBRACK);
    }
    currentTable->insert(name, std::make_shared<Symbol>(name, type, isglobal),
                         size);
    if (match(TokenType::ASSIGN)) {
      consume(TokenType::ASSIGN);
      if (match(TokenType::LBRACE)) {
        consume(TokenType::LBRACE);
        if (!match(TokenType::RBRACE)) {
          int index = 0;
          auto exp = AddExp();
          std::string exp_value = (exp.first == ExpType::Num)
                                      ? std::to_string(exp.second)
                                      : "t" + std::to_string(exp.second);
          emit("[]=", name, std::to_string(index), exp_value);
          while (match(TokenType::COMMA)) {
            consume(TokenType::COMMA);
            auto exp = AddExp();
            std::string exp_value = (exp.first == ExpType::Num)
                                        ? std::to_string(exp.second)
                                        : "t" + std::to_string(exp.second);
            emit("[]=", name, std::to_string(++index), exp_value);
          }
        }
        consume(TokenType::RBRACE);
      } else {
        auto exp = AddExp();
        std::string exp_value = (exp.first == ExpType::Num)
                                    ? std::to_string(exp.second)
                                    : "t" + std::to_string(exp.second);
        emit("=", name, exp_value);
      }
    }
    if (match(TokenType::COMMA))
      consume(TokenType::COMMA);
  }
  consume(TokenType::SEMICN);
}

void Parser::FuncDef() {
  RetType retType = RetType::INT;
  if (match(TokenType::VOIDTK)) {
    consume(TokenType::VOIDTK);
    retType = RetType::VOID;
  } else
    consume(TokenType::INTTK);

  std::string name = currentToken().first;
  consume(TokenType::IDENFR);
  consume(TokenType::LPARENT);
  emit("func", name);
  std::vector<std::string> params;
  while (match(TokenType::INTTK)) {
    consume(TokenType::INTTK);
    std::string paramName = currentToken().first;
    emit("param", paramName);
    params.push_back(paramName);
    consume(TokenType::IDENFR);
    if (match(TokenType::COMMA))
      consume(TokenType::COMMA);
  }
  consume(TokenType::RPARENT);
  currentTable->insert(
      name, std::make_shared<Symbol>(name, SymbolType::FUNC, false, retType));
  Block(true, params);
  emit("endfunc", name);
}

void Parser::MainFuncDef() {
  consume(TokenType::INTTK);
  consume(TokenType::MAINTK);
  consume(TokenType::LPARENT);
  consume(TokenType::RPARENT);
  emit("func", "main");
  currentTable->insert(
      "main",
      std::make_shared<Symbol>("main", SymbolType::FUNC, false, RetType::INT));
  Block();
  emit("endfunc", "main");
}

void Parser::Block(bool isfunc, std::vector<std::string> params) {
  consume(TokenType::LBRACE);
  enterScope();
  if (isfunc) {
    for (const auto &param : params) {
      currentTable->insert(param,
                           std::make_shared<Symbol>(param, SymbolType::INT));
    }
  }
  while (position < tokens.size() && !match(TokenType::RBRACE)) {
    if (match(TokenType::CONSTTK) || match(TokenType::INTTK))
      Decl(false);
    else
      Stmt();
  }
  consume(TokenType::RBRACE);
  exitScope();
}

void Parser::Stmt(int startlabel, int endlabel) {
  if (match(TokenType::IFTK)) {
    consume(TokenType::IFTK);
    consume(TokenType::LPARENT);
    auto exp = LOrExp();
    consume(TokenType::RPARENT);
    int elselabel = getLabel();
    int endlabel = getLabel();
    std::string exp_value = (exp.first == ExpType::Num)
                                ? std::to_string(exp.second)
                                : "t" + std::to_string(exp.second);
    emit("if", exp_value, "L" + std::to_string(elselabel));
    Stmt();
    emit("goto", "L" + std::to_string(endlabel));
    emit("L" + std::to_string(elselabel));
    if (match(TokenType::ELSETK)) {
      consume(TokenType::ELSETK);
      Stmt();
    }
    emit("L" + std::to_string(endlabel));
  } else if (match(TokenType::WHILETK)) {
    consume(TokenType::WHILETK);
    consume(TokenType::LPARENT);
    auto exp = LOrExp();
    consume(TokenType::RPARENT);
    int startlabel = getLabel();
    int endlabel = getLabel();
    emit("L" + std::to_string(startlabel));
    std::string exp_value = (exp.first == ExpType::Num)
                                ? std::to_string(exp.second)
                                : "t" + std::to_string(exp.second);
    emit("if", exp_value, "L" + std::to_string(endlabel));
    Stmt(startlabel, endlabel);
    emit("goto", "L" + std::to_string(startlabel));
    emit("L" + std::to_string(endlabel));
  } else if (match(TokenType::BREAKTK)) {
    consume(TokenType::BREAKTK);
    consume(TokenType::SEMICN);
    emit("goto", "L" + std::to_string(endlabel));
  } else if (match(TokenType::CONTINUETK)) {
    consume(TokenType::CONTINUETK);
    consume(TokenType::SEMICN);
    emit("goto", "L" + std::to_string(startlabel));
  } else if (match(TokenType::RETURNTK)) {
    consume(TokenType::RETURNTK);
    if (match(TokenType::SEMICN))
      emit("return");
    else {
      auto exp = AddExp();
      if (exp.first != ExpType::Void) {
        std::string exp_value = (exp.first == ExpType::Num)
                                    ? std::to_string(exp.second)
                                    : "t" + std::to_string(exp.second);
        emit("return", exp_value);
      }
    }
    consume(TokenType::SEMICN);
  } else if (match(TokenType::PRINTFTK)) {
    consume(TokenType::PRINTFTK);
    consume(TokenType::LPARENT);
    std::string str = currentToken().first;
    consume(TokenType::STRCON);
    splitStringByDelimiter(str, "%d");
    consume(TokenType::RPARENT);
    consume(TokenType::SEMICN);
  } else if (match(TokenType::LBRACE)) {
    Block();
  } else if (match(TokenType::IDENFR)) {
    int temp_pos = position;
    try {
      std::string name = currentToken().first;
      bool isArray = false;
      std::pair<ExpType, int> index;
      consume(TokenType::IDENFR);
      if (match(TokenType::LBRACK)) {
        isArray = true;
        consume(TokenType::LBRACK);
        index = AddExp();
        consume(TokenType::RBRACK);
      }
      consume(TokenType::ASSIGN);
      if (match(TokenType::GETINTTK)) {
        consume(TokenType::GETINTTK);
        consume(TokenType::LPARENT);
        if (isArray) {
          std::string index_value = (index.first == ExpType::Num)
                                        ? std::to_string(index.second)
                                        : "t" + std::to_string(index.second);
          int tempVar = getTempVar();
          emit("call", "getint", "", "t" + std::to_string(tempVar));
          emit("[]=", name, index_value, "t" + std::to_string(tempVar));
        } else {
          int tempVar = getTempVar();
          emit("call", "getint", "", "t" + std::to_string(tempVar));
          emit("=", name, "t" + std::to_string(tempVar));
        }
        consume(TokenType::RPARENT);
        consume(TokenType::SEMICN);
      } else {
        auto exp = AddExp();
        std::string exp_value = (exp.first == ExpType::Num)
                                    ? std::to_string(exp.second)
                                    : "t" + std::to_string(exp.second);
        if (isArray) {
          std::string index_value = (index.first == ExpType::Num)
                                        ? std::to_string(index.second)
                                        : "t" + std::to_string(index.second);
          emit("[]=", name, index_value, exp_value);
        } else {
          emit("=", name, exp_value);
        }
        consume(TokenType::SEMICN);
      }
    } catch (const std::exception &e) {
      position = temp_pos;
      std::cout << "Error: " << e.what() << std::endl;
      if (!match(TokenType::SEMICN))
        AddExp();
      consume(TokenType::SEMICN);
    }
  } else {
    AddExp();
    consume(TokenType::SEMICN);
  }
}

std::pair<ExpType, int> Parser::AddExp() {
  auto exp1 = MulExp();
  while (match(TokenType::PLUS) || match(TokenType::MINU)) {
    std::string op = currentToken().first;
    consume(currentToken().second);
    auto exp2 = MulExp();
    int result = 0;
    if (exp1.first == ExpType::Num && exp2.first == ExpType::Num) {
      if (op == "+")
        result = exp1.second + exp2.second;
      else
        result = exp1.second - exp2.second;
      exp1 = {ExpType::Num, result};
    } else {
      result = getTempVar();
      std::string exp1_value = (exp1.first == ExpType::Num)
                                   ? std::to_string(exp1.second)
                                   : "t" + std::to_string(exp1.second);
      std::string exp2_value = (exp2.first == ExpType::Num)
                                   ? std::to_string(exp2.second)
                                   : "t" + std::to_string(exp2.second);
      emit(op, exp1_value, exp2_value, "t" + std::to_string(result));
      exp1 = {ExpType::Temp, result};
    }
  }
  return exp1;
}

std::pair<ExpType, int> Parser::MulExp() {
  auto exp1 = UnaryExp();
  while (match(TokenType::MULT) || match(TokenType::DIV) ||
         match(TokenType::MOD)) {
    std::string op = currentToken().first;
    consume(currentToken().second);
    auto exp2 = UnaryExp();
    int result = 0;
    if (exp1.first == ExpType::Num && exp2.first == ExpType::Num) {
      if (op == "*")
        result = exp1.second * exp2.second;
      else if (op == "/")
        result = exp1.second / exp2.second;
      else
        result = exp1.second % exp2.second;
      exp1 = {ExpType::Num, result};
    } else {
      result = getTempVar();
      std::string exp1_value = (exp1.first == ExpType::Num)
                                   ? std::to_string(exp1.second)
                                   : "t" + std::to_string(exp1.second);
      std::string exp2_value = (exp2.first == ExpType::Num)
                                   ? std::to_string(exp2.second)
                                   : "t" + std::to_string(exp2.second);
      emit(op, exp1_value, exp2_value, "t" + std::to_string(result));
      exp1 = {ExpType::Temp, result};
    }
  }
  return exp1;
}

std::pair<ExpType, int> Parser::UnaryExp() {
  if (match(TokenType::PLUS) || match(TokenType::MINU) ||
      match(TokenType::NOT)) {
    std::string op = currentToken().first;
    consume(currentToken().second);
    auto exp = UnaryExp();
    int result = 0;
    if (exp.first == ExpType::Num) {
      if (op == "+")
        result = exp.second;
      else if (op == "-")
        result = -exp.second;
      else
        result = !exp.second;
      return {ExpType::Num, result};
    } else {
      if (op == "+")
        return exp;
      else if (op == "-")
        op = "neg";
      result = getTempVar();
      std::string exp_value = "t" + std::to_string(exp.second);
      emit(op, exp_value, "", "t" + std::to_string(result));
      return {ExpType::Temp, result};
    }
  } else if (match(TokenType::IDENFR) && match(TokenType::LPARENT, 1)) {
    std::string name = currentToken().first;
    auto symbol = currentTable->lookup(name);
    consume(TokenType::IDENFR);
    consume(TokenType::LPARENT);
    if (match(TokenType::RPARENT)) {
      if (symbol && symbol->getType() == SymbolType::FUNC) {
        if (symbol->getRetType() == RetType::VOID) {
          emit("call", name, "0");
        } else {
          int temp = getTempVar();
          emit("call", name, "0", "t" + std::to_string(temp));
          consume(TokenType::RPARENT);
          return {ExpType::Temp, temp};
        }
      } else {
        throw std::runtime_error("Function " + name + " not found");
      }
    } else {
      std::vector<std::string> params;
      auto exp = AddExp();
      std::string exp_value = (exp.first == ExpType::Num)
                                  ? std::to_string(exp.second)
                                  : "t" + std::to_string(exp.second);
      params.push_back(exp_value);
      while (match(TokenType::COMMA)) {
        consume(TokenType::COMMA);
        auto exp2 = AddExp();
        std::string exp_value = (exp2.first == ExpType::Num)
                                    ? std::to_string(exp2.second)
                                    : "t" + std::to_string(exp2.second);
        params.push_back(exp_value);
      }
      if (symbol && symbol->getType() == SymbolType::FUNC) {
        if (symbol->getRetType() == RetType::VOID) {
          for (const auto &param : params)
            emit("param", param);
          emit("call", name, std::to_string(params.size()));
        } else {
          for (const auto &param : params)
            emit("param", param);
          int temp = getTempVar();
          emit("call", name, std::to_string(params.size()),
               "t" + std::to_string(temp));
          consume(TokenType::RPARENT);
          return {ExpType::Temp, temp};
        }
      } else {
        consume(TokenType::RPARENT);
        throw std::runtime_error("Function " + name + " not found");
      }
    }
  } else
    return PrimaryExp();
  return {ExpType::Void, 0};
}

std::pair<ExpType, int> Parser::PrimaryExp() {
  if (match(TokenType::LPARENT)) {
    consume(TokenType::LPARENT);
    auto exp = AddExp();
    consume(TokenType::RPARENT);
    return exp;
  } else if (match(TokenType::IDENFR)) {
    std::string name = currentToken().first;
    consume(TokenType::IDENFR);
    auto symbol = currentTable->lookup(name);
    if (symbol) {
      if (symbol->getType() == SymbolType::INT) {
        int temp = getTempVar();
        emit("=", "t" + std::to_string(temp), name);
        return {ExpType::Temp, temp};
      } else if (symbol->getType() == SymbolType::ARRAY) {
        if (match(TokenType::LBRACK)) {
          consume(TokenType::LBRACK);
          auto exp = AddExp();
          consume(TokenType::RBRACK);
          std::string exp_value = (exp.first == ExpType::Num)
                                      ? std::to_string(exp.second)
                                      : "t" + std::to_string(exp.second);
          int temp = getTempVar();
          emit("=[]", "t" + std::to_string(temp), name, exp_value);
          return {ExpType::Temp, temp};
        }
      }
    } else {
      throw std::runtime_error("Identifier " + name + " not found");
    }
  } else if (match(TokenType::INTCON)) {
    int value = std::stoi(currentToken().first);
    consume(TokenType::INTCON);
    return {ExpType::Num, value};
  } else if (match(TokenType::INTCON)) {
    return {ExpType::Num, std::stoi(currentToken().first)};
  }
  return {ExpType::Void, 0};
}

std::pair<ExpType, int> Parser::LOrExp() {
  auto exp1 = LAndExp();
  while (match(TokenType::OR)) {
    std::string op = currentToken().first;
    consume(currentToken().second);
    auto exp2 = LAndExp();
    int result = 0;
    if (exp1.first == ExpType::Num && exp2.first == ExpType::Num) {
      result = exp1.second || exp2.second;
      exp1 = {ExpType::Num, result};
    } else {
      result = getTempVar();
      std::string exp1_value = (exp1.first == ExpType::Num)
                                   ? std::to_string(exp1.second)
                                   : "t" + std::to_string(exp1.second);
      std::string exp2_value = (exp2.first == ExpType::Num)
                                   ? std::to_string(exp2.second)
                                   : "t" + std::to_string(exp2.second);
      emit(op, exp1_value, exp2_value, "t" + std::to_string(result));
      exp1 = {ExpType::Temp, result};
    }
  }
  return exp1;
}

std::pair<ExpType, int> Parser::LAndExp() {
  auto exp1 = EqExp();
  while (match(TokenType::AND)) {
    std::string op = currentToken().first;
    consume(currentToken().second);
    auto exp2 = EqExp();
    int result = 0;
    if (exp1.first == ExpType::Num && exp2.first == ExpType::Num) {
      result = exp1.second && exp2.second;
      exp1 = {ExpType::Num, result};
    } else {
      result = getTempVar();
      std::string exp1_value = (exp1.first == ExpType::Num)
                                   ? std::to_string(exp1.second)
                                   : "t" + std::to_string(exp1.second);
      std::string exp2_value = (exp2.first == ExpType::Num)
                                   ? std::to_string(exp2.second)
                                   : "t" + std::to_string(exp2.second);
      emit(op, exp1_value, exp2_value, "t" + std::to_string(result));
      exp1 = {ExpType::Temp, result};
    }
  }
  return exp1;
}

std::pair<ExpType, int> Parser::EqExp() {
  auto exp1 = RelExp();
  while (match(TokenType::EQL) || match(TokenType::NEQ)) {
    std::string op = currentToken().first;
    consume(currentToken().second);
    auto exp2 = RelExp();
    int result = 0;
    if (exp1.first == ExpType::Num && exp2.first == ExpType::Num) {
      if (op == "==")
        result = exp1.second == exp2.second;
      else
        result = exp1.second != exp2.second;
      exp1 = {ExpType::Num, result};
    } else {
      result = getTempVar();
      std::string exp1_value = (exp1.first == ExpType::Num)
                                   ? std::to_string(exp1.second)
                                   : "t" + std::to_string(exp1.second);
      std::string exp2_value = (exp2.first == ExpType::Num)
                                   ? std::to_string(exp2.second)
                                   : "t" + std::to_string(exp2.second);
      emit(op, exp1_value, exp2_value, "t" + std::to_string(result));
      exp1 = {ExpType::Temp, result};
    }
  }
  return exp1;
}

std::pair<ExpType, int> Parser::RelExp() {
  auto exp1 = AddExp();
  while (match(TokenType::LSS) || match(TokenType::LEQ) ||
         match(TokenType::GRE) || match(TokenType::GEQ)) {
    std::string op = currentToken().first;
    consume(currentToken().second);
    auto exp2 = AddExp();
    int result = 0;
    if (exp1.first == ExpType::Num && exp2.first == ExpType::Num) {
      if (op == "<")
        result = exp1.second < exp2.second;
      else if (op == "<=")
        result = exp1.second <= exp2.second;
      else if (op == ">")
        result = exp1.second > exp2.second;
      else
        result = exp1.second >= exp2.second;
      exp1 = {ExpType::Num, result};
    } else {
      result = getTempVar();
      std::string exp1_value = (exp1.first == ExpType::Num)
                                   ? std::to_string(exp1.second)
                                   : "t" + std::to_string(exp1.second);
      std::string exp2_value = (exp2.first == ExpType::Num)
                                   ? std::to_string(exp2.second)
                                   : "t" + std::to_string(exp2.second);
      emit(op, exp1_value, exp2_value, "t" + std::to_string(result));
      exp1 = {ExpType::Temp, result};
    }
  }
  return exp1;
}

void Parser::preprocess() {
  std::unordered_map<std::string, int> const_vars;
  std::unordered_map<std::string, std::vector<int>> const_array_vars;
  while (position < tokens.size()) {
    if (match(TokenType::CONSTTK) && !match(TokenType::LBRACK, 3)) {
      consume(TokenType::CONSTTK);
      consume(TokenType::INTTK);
      while (match(TokenType::IDENFR)) {
        std::string name = currentToken().first;
        consume(TokenType::IDENFR);
        consume(TokenType::ASSIGN);
        auto exp = AddExp();
        if (exp.first == ExpType::Num) {
          const_vars[name] = exp.second;
        }
        if (match(TokenType::COMMA))
          consume(TokenType::COMMA);
      }
      consume(TokenType::SEMICN);
    } else if (match(TokenType::IDENFR) && !match(TokenType::LBRACK, 1) &&
               const_vars.find(currentToken().first) != const_vars.end()) {
      Token &token = tokens[position];
      consume(TokenType::IDENFR);
      token.first = std::to_string(const_vars[currentToken().first]);
      token.second = TokenType::INTCON;
    } else
      position++;
  }
  position = 0; // Reset position for parsing
  tempCount = 0;
}
