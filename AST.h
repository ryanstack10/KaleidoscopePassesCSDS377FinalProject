#ifndef AST_H
#define AST_H

#include "llvm/IR/Value.h"
#include <map>
#include <memory>
#include <string>
#include <vector>

using namespace llvm;

//===----------------------------------------------------------------------===//
// Abstract Syntax Tree (aka Parse Tree)
//===----------------------------------------------------------------------===//

namespace {

/// ExprAST - Base class for all expression nodes.
class ExprAST {
public:
  virtual ~ExprAST() = default;

  virtual Value *codegen() = 0;
};

/// NumberExprAST - Expression class for numeric literals like "1.0".
class NumberExprAST : public ExprAST {
public:
  double Val;

  NumberExprAST(double Val) : Val(Val) {}

  Value *codegen() override;
};

/// VariableExprAST - Expression class for referencing a variable, like "a".
class VariableExprAST : public ExprAST {
public:
  std::string Name;

  VariableExprAST(const std::string &Name) : Name(Name) {}

  Value *codegen() override;
  const std::string &getName() const;
};

/// UnaryExprAST - Expression class for a unary operator.
class UnaryExprAST : public ExprAST {
public:
  char Opcode;
  std::unique_ptr<ExprAST> Operand;

  UnaryExprAST(char Opcode, std::unique_ptr<ExprAST> Operand)
      : Opcode(Opcode), Operand(std::move(Operand)) {}

  Value *codegen() override;
};

/// BinaryExprAST - Expression class for a binary operator.
class BinaryExprAST : public ExprAST {
public:
  char Op;
  std::unique_ptr<ExprAST> LHS, RHS;

  BinaryExprAST(char Op, std::unique_ptr<ExprAST> LHS,
                std::unique_ptr<ExprAST> RHS)
      : Op(Op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}

  Value *codegen() override;
};

/// CallExprAST - Expression class for function calls.
class CallExprAST : public ExprAST {
public:
  std::string Callee;
  std::vector<std::unique_ptr<ExprAST>> Args;

  CallExprAST(const std::string &Callee,
              std::vector<std::unique_ptr<ExprAST>> Args)
      : Callee(Callee), Args(std::move(Args)) {}

  Value *codegen() override;
};

/// IfExprAST - Expression class for if/then/else.
class IfExprAST : public ExprAST {
public:
  std::unique_ptr<ExprAST> Cond, Then, Else;

  IfExprAST(std::unique_ptr<ExprAST> Cond, std::unique_ptr<ExprAST> Then,
            std::unique_ptr<ExprAST> Else)
      : Cond(std::move(Cond)), Then(std::move(Then)), Else(std::move(Else)) {}

  Value *codegen() override;
};

/// ForExprAST - Expression class for for/in.
class ForExprAST : public ExprAST {
public:
  std::string VarName;
  std::unique_ptr<ExprAST> Start, End, Step, Body;

  ForExprAST(const std::string &VarName, std::unique_ptr<ExprAST> Start,
             std::unique_ptr<ExprAST> End, std::unique_ptr<ExprAST> Step,
             std::unique_ptr<ExprAST> Body)
      : VarName(VarName), Start(std::move(Start)), End(std::move(End)),
        Step(std::move(Step)), Body(std::move(Body)) {}

  Value *codegen() override;
};

/// VarExprAST - Expression class for var/in
class VarExprAST : public ExprAST {
public:
  std::vector<std::pair<std::string, std::unique_ptr<ExprAST>>> VarNames;
  std::unique_ptr<ExprAST> Body;

  VarExprAST(
      std::vector<std::pair<std::string, std::unique_ptr<ExprAST>>> VarNames,
      std::unique_ptr<ExprAST> Body)
      : VarNames(std::move(VarNames)), Body(std::move(Body)) {}

  Value *codegen() override;
};

/// PrototypeAST - This class represents the "prototype" for a function,
/// which captures its name, and its argument names (thus implicitly the number
/// of arguments the function takes), as well as if it is an operator.
class PrototypeAST {
public:
  std::string Name;
  std::vector<std::string> Args;
  int Operator;
  unsigned Precedence; // Precedence if a binary op.

  PrototypeAST(const std::string &Name, std::vector<std::string> Args,
               int Operator = -1, unsigned Prec = 0)
      : Name(Name), Args(std::move(Args)), Operator(Operator),
        Precedence(Prec) {}

  Function *codegen();
  const std::string &getName() const;

  bool isOperator() const;
  bool isUnaryOp() const;
  bool isBinaryOp() const;

  int getOperator() const;

  std::string getOperatorName() const;

  unsigned getBinaryPrecedence() const;
};

/// FunctionAST - This class represents a function definition itself.
class FunctionAST {
public:
  std::unique_ptr<PrototypeAST> Proto;
  std::unique_ptr<ExprAST> Body;

  FunctionAST(std::unique_ptr<PrototypeAST> Proto,
              std::unique_ptr<ExprAST> Body)
      : Proto(std::move(Proto)), Body(std::move(Body)) {}

  const std::string& getName();

  Function *codegen();
};

/// ModuleAST - This class represents all functions/externs within a file or section of code
class ModuleAST {
public:
  // Maps function name to function prototype
  std::map<std::string, std::unique_ptr<PrototypeAST>> Externs;
  // bool = true if the function is a definition
  //      = false if the function is a top-level expression
  std::map<std::string, std::pair<std::unique_ptr<FunctionAST>, bool>> Functions;

  ModuleAST() = default;

  // Returns a string of codegen for every extern/function
  void codegen();

  PrototypeAST* getFunctionProto(std::string name) const;
  
  // Transfers ownership of the Extern PrototypeAST to ModuleAST
  void addExtern(std::unique_ptr<PrototypeAST> Extern);
  // Transfers ownership of the FunctionAST to ModuleAST
  void addFunction(std::unique_ptr<FunctionAST> Function, bool isDefinition);
};

} // end anonymous namespace

#endif /* AST_H */
