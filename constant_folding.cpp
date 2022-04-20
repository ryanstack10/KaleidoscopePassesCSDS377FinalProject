#include "AST.h"
#include <iostream>

static ExprAST* FoldExpression(ExprAST* Expression) {
  // Check if this is a binary operation
  BinaryExprAST* binExpr = dynamic_cast<BinaryExprAST*>(Expression);
  if (binExpr != nullptr) {
    // Fold children first
    FoldExpression(binExpr->LHS.get());
    FoldExpression(binExpr->RHS.get());

    // If both the LHS and RHS can be casted to constants, we can fold them
    NumberExprAST* numLHS = dynamic_cast<NumberExprAST*>(binExpr->LHS.get());
    NumberExprAST* numRHS = dynamic_cast<NumberExprAST*>(binExpr->RHS.get());
    if (numLHS != nullptr && numRHS != nullptr) {
      NumberExprAST* combined;
      fprintf(stderr, "  Able to fold %f and %f\n", numLHS->Val, numRHS->Val);
      if (binExpr->Op == '+') {
        combined = new NumberExprAST(numLHS->Val + numRHS->Val);
      }
      if (binExpr->Op == '-') {
        combined = new NumberExprAST(numLHS->Val - numRHS->Val);
      }
      if (binExpr->Op == '*') {
        combined = new NumberExprAST(numLHS->Val * numRHS->Val);
      }
      return combined;
    }

    // Otherwise, we cannot fold this expression, do nothing.
    return Expression;
  }

  // Return Expression if all else fails
  return Expression;
}

void ConstantFold(ModuleAST* TheModule) {
  fprintf(stderr, "RUNNING CONSTANT FOLDING\n");
  std::map<std::string, std::pair<std::unique_ptr<FunctionAST>, bool>>::iterator fn;
  for (fn = TheModule->Functions.begin(); fn != TheModule->Functions.end(); fn++) {
    fprintf(stderr, "Attempting to fold the body of %s\n", fn->first.c_str());
    *(fn->second.first->Body) = *FoldExpression(fn->second.first->Body.get());
  }
}