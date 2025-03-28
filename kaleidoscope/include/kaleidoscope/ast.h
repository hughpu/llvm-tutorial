#pragma once

#include <kaleidoscope/kaleidoscopejit.h>
#include <kaleidoscope/token.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/PassManager.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Passes/StandardInstrumentations.h>
#include <llvm/Transforms/InstCombine/InstCombine.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Scalar/GVN.h>
#include <llvm/Transforms/Scalar/Reassociate.h>
#include <llvm/Transforms/Scalar/SimplifyCFG.h>
#include <llvm/Transforms/Utils/Mem2Reg.h>
#include <map>
#include <memory>
#include <string>
#include <vector>

namespace kaleidoscope {

class ExprAST {
public:
  virtual ~ExprAST() = default;
  virtual llvm::Value *codegen() = 0;
};

class NumberExprAST : public ExprAST {
public:
  NumberExprAST(double val) : val_(val) {}
  llvm::Value *codegen() override;

private:
  double val_;
};

class VariableExprAST : public ExprAST {
public:
  VariableExprAST(const std::string &name) : name_(name) {}
  llvm::Value *codegen() override;
  std::string name() { return name_; }

private:
  std::string name_;
};

class BinaryExprAST : public ExprAST {
public:
  BinaryExprAST(char op, std::unique_ptr<ExprAST> lhs,
                std::unique_ptr<ExprAST> rhs)
      : op_(op), lhs_(std::move(lhs)), rhs_(std::move(rhs)) {}
  llvm::Value *codegen() override;

private:
  char op_;
  std::unique_ptr<ExprAST> lhs_;
  std::unique_ptr<ExprAST> rhs_;
};

class UnaryExprAST : public ExprAST {
public:
  UnaryExprAST(char op_code, std::unique_ptr<ExprAST> operand)
      : op_(op_code), operand_(std::move(operand)) {}

  llvm::Value *codegen() override;

private:
  char op_;
  std::unique_ptr<ExprAST> operand_;
};

class CallExprAST : public ExprAST {
public:
  CallExprAST(const std::string &callee,
              std::vector<std::unique_ptr<ExprAST>> args)
      : callee_(callee), args_(std::move(args)) {}
  llvm::Value *codegen() override;

private:
  std::string callee_;
  std::vector<std::unique_ptr<ExprAST>> args_;
};

class PrototypeAST {
public:
  PrototypeAST(const std::string &name, std::vector<std::string> args,
               bool is_operator = false, unsigned prec = 0)
      : name_(name), args_(std::move(args)), is_operator_(is_operator),
        precedence_(prec) {}
  llvm::Function *codegen();
  std::string name() { return name_; }
  bool isUnaryOp() const { return is_operator_ && args_.size() == 1; }
  bool isBinaryOp() const { return is_operator_ && args_.size() == 2; }
  char getOperatorName() const {
    assert(isUnaryOp() || isBinaryOp());
    return name_[name_.size() - 1];
  }
  unsigned getPrecedence() const { return precedence_; }

private:
  std::string name_;
  std::vector<std::string> args_;

  bool is_operator_;
  unsigned precedence_;
};

class FunctionAST {
public:
  FunctionAST(std::unique_ptr<PrototypeAST> proto,
              std::unique_ptr<ExprAST> body)
      : proto_(std::move(proto)), body_(std::move(body)) {}
  llvm::Function *codegen();

private:
  std::unique_ptr<PrototypeAST> proto_;
  std::unique_ptr<ExprAST> body_;
};

class IfExprAST : public ExprAST {
public:
  IfExprAST(std::unique_ptr<ExprAST> cond, std::unique_ptr<ExprAST> then,
            std::unique_ptr<ExprAST> else_)
      : cond_(std::move(cond)), then_(std::move(then)),
        else_(std::move(else_)) {}
  llvm::Value *codegen() override;

private:
  std::unique_ptr<ExprAST> cond_, then_, else_;
};

class ForExprAST : public ExprAST {
public:
  ForExprAST(const std::string &name, std::unique_ptr<ExprAST> start,
             std::unique_ptr<ExprAST> end, std::unique_ptr<ExprAST> step,
             std::unique_ptr<ExprAST> body)
      : name_(name), start_(std::move(start)), end_(std::move(end)),
        step_(std::move(step)), body_(std::move(body)) {}

  llvm::Value *codegen() override;

private:
  std::string name_;
  std::unique_ptr<ExprAST> start_, end_, step_, body_;
};

extern int cur_token;
extern std::map<char, int> binop_precedence;

void MainLoop();
int GetNextToken();
int GetTokPrecedence();
void InitializeModuleAndPassManagers();
void HandleDefinition();
void HandleExtern();
void HandleTopLevelExpression();
llvm::Function *getFunction(std::string name);

std::unique_ptr<ExprAST> LogError(const char *str);
std::unique_ptr<ExprAST> ParseNumberExpr();
std::unique_ptr<ExprAST> ParseExpression();
std::unique_ptr<ExprAST> ParseParenExpr();
std::unique_ptr<ExprAST> ParseIndentifierExpr();
std::unique_ptr<ExprAST> ParsePrimary();
std::unique_ptr<ExprAST> ParseBinOpsRHS(int expr_prec,
                                        std::unique_ptr<ExprAST> lhs);
std::unique_ptr<ExprAST> ParseIfExpr();
std::unique_ptr<ExprAST> ParseForExpr();
std::unique_ptr<PrototypeAST> LogErrorP(const char *str);
std::unique_ptr<PrototypeAST> ParsePrototype();
std::unique_ptr<FunctionAST> ParseDefinition();
std::unique_ptr<PrototypeAST> ParseExtern();
std::unique_ptr<FunctionAST> ParseTopLevelExpr();
std::unique_ptr<ExprAST> ParseUnary();
llvm::AllocaInst *CreateEntryBlockAlloca(llvm::Function *the_func,
                                         llvm::StringRef var_name);

extern std::unique_ptr<llvm::LLVMContext> TheContext;
extern std::unique_ptr<llvm::IRBuilder<>> Builder;
extern std::unique_ptr<llvm::Module> TheModule;
extern std::map<std::string, llvm::AllocaInst *> NamedValues;
extern std::unique_ptr<llvm::FunctionPassManager> TheFPM;
extern std::unique_ptr<llvm::LoopAnalysisManager> TheLAM;
extern std::unique_ptr<llvm::FunctionAnalysisManager> TheFAM;
extern std::unique_ptr<llvm::CGSCCAnalysisManager> TheCGAM;
extern std::unique_ptr<llvm::ModuleAnalysisManager> TheMAM;
extern std::unique_ptr<llvm::PassInstrumentationCallbacks> ThePIC;
extern std::unique_ptr<llvm::StandardInstrumentations> TheSI;
extern std::unique_ptr<llvm::orc::KaleidoscopeJIT> TheJIT;
extern std::map<std::string, std::unique_ptr<PrototypeAST>> FunctionProtos;
extern llvm::ExitOnError ExitOnErr;
} // namespace kaleidoscope
