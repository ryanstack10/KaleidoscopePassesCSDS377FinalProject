#include "AST.h"
#include <string.h>
#include <iostream>

// Example pass used to show operations in the code

static std::string print_ast(ExprAST* ast) {
  // Check if this is a binary operation
  BinaryExprAST* binExpr = dynamic_cast<BinaryExprAST*>(ast);
  if (binExpr != nullptr) {
    return "(" + print_ast(binExpr->LHS.get()) + ")" + binExpr->Op + "(" + print_ast(binExpr->RHS.get()) + ")";
  }

  NumberExprAST* numExpr = dynamic_cast<NumberExprAST*>(ast);
  if (numExpr != nullptr) {
    return std::to_string(numExpr->Val);
  }

  VariableExprAST* varExpr = dynamic_cast<VariableExprAST*>(ast);
  if (varExpr != nullptr) {
    return varExpr->Name;
  }

  return "INVALID";
}

void ExamplePass(ModuleAST* TheModule) {
  fprintf(stderr, "This Module has %lu externs and %lu functions!\n\n", TheModule->Externs.size(), TheModule->Functions.size());
  std::map<std::string, std::pair<std::unique_ptr<FunctionAST>, bool>>::iterator fn;
  for (fn = TheModule->Functions.begin(); fn != TheModule->Functions.end(); fn++) {
    std::cout << fn->first << ": " << print_ast(fn->second.first->Body.get()) << std::endl;
  }
}
