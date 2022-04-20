#include "AST.h"
#include <iostream>

static void FoldExpression(ExprAST* Expression) {
  // Check if this is a binary operation
  BinaryExprAST* binExpr = dynamic_cast<BinaryExprAST*>(Expression);
  if (binExpr != nullptr) {
    fprintf(stderr, "FOUND A BINARY EXPRESSION!\n");
    
    // Fold children first
    FoldExpression(binExpr->LHS.get());
    FoldExpression(binExpr->RHS.get());

    // If both the LHS and RHS can be casted to constants, we can fold them
    NumberExprAST* numLHS = dynamic_cast<NumberExprAST*>(binExpr->LHS.get());
    NumberExprAST* numRHS = dynamic_cast<NumberExprAST*>(binExpr->RHS.get());
    if (numLHS != nullptr && numRHS != nullptr) {
      fprintf(stderr, "These two things can be folded at some point\n");
    }

    return;
  }

  // Check if this is a constant number
  NumberExprAST* numExpr = dynamic_cast<NumberExprAST*>(Expression);
  if (numExpr != nullptr) {
    fprintf(stderr, "FOUND A CONSTANT EXPRESSION WITH VALUE %f!\n", numExpr->Val);
    return;
  }

  // fprintf(stderr, "Cannot fold %s, skipping\n", typeid(Expression).name());
}

void ConstantFold(ModuleAST* TheModule) {
  fprintf(stderr, "RUNNING CONSTANT FOLDING\n");
  std::map<std::string, std::pair<std::unique_ptr<FunctionAST>, bool>>::iterator fn;
  for (fn = TheModule->Functions.begin(); fn != TheModule->Functions.end(); fn++) {
    fprintf(stderr, "Attempting to fold the body of %s\n", fn->first.c_str());
    FoldExpression(fn->second.first->Body.get());
  }
}