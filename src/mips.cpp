#include "mips.h"

Mips::Mips(std::vector<Quadruple> &tac, std::shared_ptr<SymbolTable> table)
    : tac(tac), table(table) {}

std::vector<std::string> Mips::generateMips() {
  generateDataSection();
  generateTextSection();
  buildin_func();
  mipsCode.insert(mipsCode.end(), data.begin(), data.end());
  mipsCode.insert(mipsCode.end(), text.begin(), text.end());
  return mipsCode;
}

std::string Mips::indent(int level) { return std::string(level * 4, ' '); }

void Mips::generateDataSection() {
  data.push_back(".data");
  for (auto &tb : table->getTable()) {
    auto &name = tb.first;
    auto &symbol = tb.second;
    if (!symbol->isGlobalVar())
      continue;
    if (symbol->getType() == SymbolType::INT) {
      data.push_back(indent(1) + name + ": .word " +
                     std::to_string(symbol->getValue()));
    } else if (symbol->getType() == SymbolType::ARRAY) {
      if (symbol->isInitValue()) {
        std::string array = indent(1) + name + ": .word ";
        for (int i = 0; i < symbol->getSize(); ++i) {
          array += std::to_string(symbol->getArray()[i]);
          if (i != symbol->getSize() - 1)
            array += ", ";
        }
        data.push_back(array);
      } else {
        data.push_back(indent(1) + name + ": .space " +
                       std::to_string(symbol->getSize() * 4));
      }
    } else if (symbol->getType() == SymbolType::STR) {
      data.push_back(indent(1) + name + ": .asciiz \"" + symbol->getStr() +
                     "\"");
    }
  }
}

void Mips::buildin_func() {
  text.push_back("getint:");
  text.push_back(indent(1) + "li $v0, 5");
  text.push_back(indent(1) + "syscall");
  text.push_back(indent(1) + "jr $ra");

  text.push_back("printstr:");
  text.push_back(indent(1) + "li $v0, 4");
  text.push_back(indent(1) + "syscall");
  text.push_back(indent(1) + "jr $ra");

  text.push_back("printint:");
  text.push_back(indent(1) + "li $v0, 1");
  text.push_back(indent(1) + "syscall");
  text.push_back(indent(1) + "jr $ra");
}

void Mips::generateTextSection() {
  text.push_back(".text");
  text.push_back(".globl main");
  for (int i = 0; i < tac.size(); ++i) {
    auto &quad = tac[i];
    if (quad.op == "func") {
      auto symbol = table->lookup(quad.arg1);
      int paramCount = std::stoi(quad.arg2);
      int blockSize = symbol->getBlockSize();
      text.push_back(quad.arg1 + ":");
      if (quad.arg1 != "main") {
        text.push_back(indent(1) + "addi $sp, $sp, -" +
                       std::to_string(blockSize + 4));
        text.push_back(indent(1) + "sw $ra, " + std::to_string(blockSize - 4) +
                       "($sp)");
      }
    } else if (quad.op == "endfunc") {
      auto symbol = table->lookup(quad.arg1);
      if (quad.arg1 == "main") {
        text.push_back(indent(1) + "li $v0, 10");
        text.push_back(indent(1) + "syscall");
        continue;
      }
      int blockSize = symbol->getBlockSize();
      text.push_back(indent(1) + "lw $ra, " + std::to_string(blockSize - 4) +
                     "($sp)");
      text.push_back(indent(1) + "addi $sp, $sp, " +
                     std::to_string(blockSize + 4));
      text.push_back(indent(1) + "jr $ra");
    }
  }
}