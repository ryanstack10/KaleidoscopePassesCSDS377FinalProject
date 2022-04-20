#include "AST.h"
#include <iostream>

static ExprAST* FoldExpression(ExprAST* Expression) {
  // Check if this is a binary operation
  BinaryExprAST* binExpr = dynamic_cast<BinaryExprAST*>(Expression);
  if (binExpr != nullptr) {
    // Fold children first
    ExprAST* newLHS = FoldExpression(binExpr->LHS.get());
    binExpr->LHS.release();
    binExpr->LHS.reset(newLHS);
    ExprAST* newRHS = FoldExpression(binExpr->RHS.get());
    binExpr->RHS.release();
    binExpr->RHS.reset(newRHS);

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
  }

  // Check if this is a for loop
  ForExprAST* forExpr = dynamic_cast<ForExprAST*>(Expression);
  if (forExpr != nullptr) {
    // Fold each component of the for-loop
    ExprAST* newStart = FoldExpression(forExpr->Start.get());
    forExpr->Start.release();
    forExpr->Start.reset(newStart);
    ExprAST* newEnd = FoldExpression(forExpr->End.get());
    forExpr->End.release();
    forExpr->End.reset(newEnd);
    ExprAST* newStep = FoldExpression(forExpr->Step.get());
    forExpr->Step.release();
    forExpr->Step.reset(newStep);
    ExprAST* newBody = FoldExpression(forExpr->Body.get());
    forExpr->Body.release();
    forExpr->Body.reset(newBody);
  }

  // Return original expression if all else fails
  return Expression;
}

void ConstantFold(ModuleAST* TheModule) {
  fprintf(stderr, "RUNNING CONSTANT FOLDING\n");

  // Iterate through every function in the module
  std::map<std::string, std::pair<std::unique_ptr<FunctionAST>, bool>>::iterator fn;
  for (fn = TheModule->Functions.begin(); fn != TheModule->Functions.end(); fn++) {
    fprintf(stderr, "Attempting to fold the body of %s\n", fn->first.c_str());
    ExprAST* newFn = FoldExpression(fn->second.first->Body.get());
    fn->second.first->Body.release();
    fn->second.first->Body.reset(newFn);
  }
  std::cout << "Finished constant folding" << std::endl;
}