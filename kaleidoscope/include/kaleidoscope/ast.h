#pragma once

#include <string>
#include <kaleidoscope/token.h>
#include <memory>
#include <vector>
#include <map>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Verifier.h>


namespace kaleidoscope
{
extern std::unique_ptr<llvm::LLVMContext> TheContext;
extern std::unique_ptr<llvm::IRBuilder<>> Builder;
extern std::unique_ptr<llvm::Module> TheModule;
extern std::map<std::string, llvm::Value*> NamedValues;

class ExprAST
{
public:
    virtual ~ExprAST() = default;
    virtual llvm::Value* codegen() = 0;
};

class NumberExprAST : public ExprAST
{
public:
    NumberExprAST(double val) : val_(val) {}
    llvm::Value* codegen() override;

private:
    double val_;
};

class VariableExprAST : public ExprAST
{
public:
    VariableExprAST(const std::string &name) : name_(name) {}
    llvm::Value* codegen() override;
private:
    std::string name_;
};

class BinaryExprAST : public ExprAST
{
public:
    BinaryExprAST(char op, std::unique_ptr<ExprAST> lhs, std::unique_ptr<ExprAST> rhs)
        : op_(op), lhs_(std::move(lhs)), rhs_(std::move(rhs)) {}
    llvm::Value* codegen() override;

private:
    char op_;
    std::unique_ptr<ExprAST> lhs_;
    std::unique_ptr<ExprAST> rhs_;
};

class CallExprAST : public ExprAST
{
public:
    CallExprAST(const std::string &callee, std::vector<std::unique_ptr<ExprAST>> args)
        : callee_(callee), args_(std::move(args)) {}
    llvm::Value* codegen() override;
private:
    std::string callee_;
    std::vector<std::unique_ptr<ExprAST>> args_;

};

class PrototypeAST
{
public:
    PrototypeAST(const std::string &name, std::vector<std::string> args)
        : name_(name), args_(std::move(args)) {}
    llvm::Function* codegen();
    std::string name() { return name_; }

private:
    std::string name_;
    std::vector<std::string> args_;
};

class FunctionAST
{
public:
    FunctionAST(std::unique_ptr<PrototypeAST> proto, std::unique_ptr<ExprAST> body)
        : proto_(std::move(proto)), body_(std::move(body)) {}
    llvm::Function* codegen();
private:
    std::unique_ptr<PrototypeAST> proto_;
    std::unique_ptr<ExprAST> body_;
};


extern int cur_token;
extern std::map<char, int> binop_precedence;

void MainLoop();
int GetNextToken();
int GetTokPrecedence();
void InitializeModule();
void HandleDefinition();
void HandleExtern();
void HandleTopLevelExpression();
std::unique_ptr<ExprAST> LogError(const char *str);
std::unique_ptr<PrototypeAST> LogErrorP(const char *str);
std::unique_ptr<ExprAST> ParseNumberExpr();
std::unique_ptr<ExprAST> ParseExpression();
std::unique_ptr<ExprAST> ParseParenExpr();
std::unique_ptr<ExprAST> ParseIndentifierExpr();
std::unique_ptr<ExprAST> ParsePrimary();
std::unique_ptr<ExprAST> ParseBinOpsRHS(int expr_prec, std::unique_ptr<ExprAST> lhs);
std::unique_ptr<PrototypeAST> ParsePrototype();
std::unique_ptr<FunctionAST> ParseDefinition();
std::unique_ptr<PrototypeAST> ParseExtern();
std::unique_ptr<FunctionAST> ParseTopLevelExpr();
    
}

