#include <kaleidoscope/ast.h>

namespace kaleidoscope {
std::unique_ptr<llvm::LLVMContext> TheContext;
std::unique_ptr<llvm::IRBuilder<>> Builder;
std::unique_ptr<llvm::Module> TheModule;
std::unique_ptr<llvm::FunctionPassManager> TheFPM;
std::unique_ptr<llvm::LoopAnalysisManager> TheLAM;
std::unique_ptr<llvm::FunctionAnalysisManager> TheFAM;
std::unique_ptr<llvm::CGSCCAnalysisManager> TheCGAM;
std::unique_ptr<llvm::ModuleAnalysisManager> TheMAM;
std::unique_ptr<llvm::PassInstrumentationCallbacks> ThePIC;
std::unique_ptr<llvm::StandardInstrumentations> TheSI;
std::unique_ptr<llvm::orc::KaleidoscopeJIT> TheJIT;
std::map<std::string, std::unique_ptr<PrototypeAST>> FunctionProtos;
llvm::ExitOnError ExitOnErr;

std::map<std::string, llvm::Value *> NamedValues;
int cur_token;
std::map<char, int> binop_precedence = {
    // {'=', 2},
    // {';', 2},
    {'<', 10},
    {'+', 20},
    {'-', 20},
    {'*', 40},
};

int GetNextToken() { return cur_token = GetToken(); }

std::unique_ptr<ExprAST> LogError(const char *str) {
  fprintf(stderr, "Error: %s\n", str);
  return nullptr;
}

std::unique_ptr<PrototypeAST> LogErrorP(const char *str) {
  LogError(str);
  return nullptr;
}

std::unique_ptr<ExprAST> ParseNumberExpr() {
  auto node = std::make_unique<NumberExprAST>(num_val);
  GetNextToken();
  return std::move(node);
}

std::unique_ptr<ExprAST> ParseExpression() {
  auto lhs = ParsePrimary();

  if (!lhs)
    return nullptr;

  return ParseBinOpsRHS(0, std::move(lhs));
}

std::unique_ptr<ExprAST> ParseParenExpr() {
  GetNextToken(); // eat '('
  auto val_in_paren = ParseExpression();
  if (!val_in_paren)
    return nullptr;

  if (cur_token != ')')
    return LogError("expected ')'");

  GetNextToken(); // eat ')'
  return val_in_paren;
}

std::unique_ptr<ExprAST> ParseIndentifierExpr() {
  std::string id_name = identitfier_str;
  GetNextToken();

  if (cur_token != '(')
    return std::make_unique<VariableExprAST>(id_name);

  GetNextToken(); // eat '('
  std::vector<std::unique_ptr<ExprAST>> args;
  if (cur_token != ')') {
    while (true) {
      if (auto arg = ParseExpression())
        args.push_back(std::move(arg));
      else
        return nullptr;

      if (cur_token == ')')
        break;
      if (cur_token != ',')
        return LogError("Expected ')' or ',' in argument list");
      GetNextToken();
    }
  }

  GetNextToken(); // eat ')'

  return std::make_unique<CallExprAST>(id_name, std::move(args));
}

std::unique_ptr<ExprAST> ParsePrimary() {
  switch (cur_token) {
  default:
    return LogError("unknown token when expecting an expression");
  case TOK_NUMBER:
    return ParseNumberExpr();
  case TOK_IDENTIFIER:
    return ParseIndentifierExpr();
  case '(':
    return ParseParenExpr();
  case TOK_IF:
    return ParseIfExpr();
  case TOK_FOR:
    return ParseForExpr();
  }
}

int GetTokPrecedence() {
  if (!isascii(cur_token))
    return -1;

  int tok_prec = binop_precedence[cur_token];
  if (tok_prec <= 0)
    return -1;
  return tok_prec;
}

std::unique_ptr<ExprAST> ParseBinOpsRHS(int expr_prec,
                                        std::unique_ptr<ExprAST> lhs) {
  while (true) {
    int tok_prec = GetTokPrecedence();
    if (tok_prec < expr_prec)
      return lhs;

    int binop = cur_token;
    GetNextToken();

    auto rhs = ParsePrimary();
    if (!rhs)
      return nullptr;

    int next_prec = GetTokPrecedence();
    if (tok_prec < next_prec) {
      rhs = ParseBinOpsRHS(next_prec, std::move(rhs));
      if (!rhs)
        return nullptr;
    }

    lhs =
        std::make_unique<BinaryExprAST>(binop, std::move(lhs), std::move(rhs));
  }
}

std::unique_ptr<PrototypeAST> ParsePrototype() {
  if (cur_token != TOK_IDENTIFIER)
    return LogErrorP("Expected function name in prototype");

  std::string fn_name = identitfier_str;
  GetNextToken(); // eat identifier

  if (cur_token != '(')
    return LogErrorP("Expected '(' in prototype");

  std::vector<std::string> arg_names;
  while (GetNextToken() == TOK_IDENTIFIER) {
    arg_names.push_back(identitfier_str);
  }

  if (cur_token != ')')
    return LogErrorP("Expected ')' in prototype");

  GetNextToken(); // eat ')'

  return std::make_unique<PrototypeAST>(fn_name, std::move(arg_names));
}

std::unique_ptr<FunctionAST> ParseDefinition() {
  GetNextToken(); // eat def
  auto proto = ParsePrototype();
  if (!proto)
    return nullptr;

  if (auto expr = ParseExpression())
    return std::make_unique<FunctionAST>(std::move(proto), std::move(expr));

  return nullptr;
}

std::unique_ptr<PrototypeAST> ParseExtern() {
  GetNextToken(); // eat extern
  return ParsePrototype();
}

std::unique_ptr<FunctionAST> ParseTopLevelExpr() {
  if (auto expr = ParseExpression()) {
    auto proto = std::make_unique<PrototypeAST>("__anon_expr",
                                                std::vector<std::string>());
    return std::make_unique<FunctionAST>(std::move(proto), std::move(expr));
  }
  return nullptr;
}

std::unique_ptr<ExprAST> ParseIfExpr() {
  GetNextToken();

  auto cond = ParseExpression();
  if (!cond)
    return nullptr;

  if (cur_token != TOK_THEN)
    return LogError("expected then");
  GetNextToken();

  auto then = ParseExpression();
  if (!then)
    return nullptr;

  if (cur_token != TOK_ELSE)
    return LogError("expected else");
  GetNextToken();

  auto else_ = ParseExpression();
  if (!else_)
    return nullptr;

  return std::make_unique<IfExprAST>(std::move(cond), std::move(then),
                                     std::move(else_));
}

// forexpr ::= 'for' identifier '=' start_expr ',' end_expr (',' step_expr)?
// 'in' body_expr
std::unique_ptr<ExprAST> ParseForExpr() {
  GetNextToken();

  if (cur_token != TOK_IDENTIFIER)
    return LogError("expected identifier after for");

  std::string id_name = identitfier_str;
  GetNextToken();

  if (cur_token != '=') {
    return LogError("expected '=' after for");
  }
  GetNextToken();

  auto start = ParseExpression();
  if (!start)
    return nullptr;

  if (cur_token != ',')
    return LogError("expected ',' after for start value");
  GetNextToken();

  auto end = ParseExpression();
  if (!end)
    return nullptr;

  std::unique_ptr<ExprAST> step;
  if (cur_token == ',') {
    GetNextToken();
    step = ParseExpression();
    if (!step) {
      return nullptr;
    }
  }

  if (cur_token != TOK_IN)
    return LogError("expected 'in' after for");
  GetNextToken();

  auto body = ParseExpression();
  if (!body)
    return nullptr;

  return std::make_unique<ForExprAST>(id_name, std::move(start), std::move(end),
                                      std::move(step), std::move(body));
}

llvm::Value *LogErrorV(const char *Str) {
  fprintf(stderr, "Error: %s\n", Str);
  return nullptr;
}

llvm::Value *NumberExprAST::codegen() {
  return llvm::ConstantFP::get(*TheContext, llvm::APFloat(val_));
}

llvm::Value *VariableExprAST::codegen() {
  llvm::Value *V = NamedValues[name_];
  if (!V)
    return LogErrorV("Unknown variable name");
  return V;
}

llvm::Value *BinaryExprAST::codegen() {
  llvm::Value *lhs_val = lhs_->codegen();
  llvm::Value *rhs_val = rhs_->codegen();
  if (!lhs_val || !rhs_val)
    return nullptr;

  switch (op_) {
  case '+':
    return Builder->CreateFAdd(lhs_val, rhs_val, "addtmp");
  case '-':
    return Builder->CreateFSub(lhs_val, rhs_val, "subtmp");
  case '*':
    return Builder->CreateFMul(lhs_val, rhs_val, "multmp");
  case '<':
    lhs_val = Builder->CreateFCmpULT(lhs_val, rhs_val, "cmptmp");
    return Builder->CreateUIToFP(lhs_val, llvm::Type::getDoubleTy(*TheContext),
                                 "booltmp");
  default:
    return LogErrorV("invalid binary operator");
  }
}

llvm::Function *getFunction(std::string name) {
  if (auto *f = TheModule->getFunction(name))
    return f;
  auto fi = FunctionProtos.find(name);
  if (fi != FunctionProtos.end())
    return fi->second->codegen();

  return nullptr;
}

llvm::Value *CallExprAST::codegen() {
  llvm::Function *callee_f = getFunction(callee_);
  if (!callee_f)
    return LogErrorV(("Unknown function name: " + callee_).c_str());

  if (callee_f->arg_size() != args_.size())
    return LogErrorV("Incorrect # arguments passed");

  std::vector<llvm::Value *> args_val;

  for (unsigned i = 0, e = args_.size(); i != e; ++i) {
    args_val.push_back(args_[i]->codegen());
    if (!args_val.back())
      return nullptr;
  }

  return Builder->CreateCall(callee_f, args_val, "calltmp");
}

llvm::Function *PrototypeAST::codegen() {
  std::vector<llvm::Type *> doubles(args_.size(),
                                    llvm::Type::getDoubleTy(*TheContext));

  llvm::FunctionType *func_type = llvm::FunctionType::get(
      llvm::Type::getDoubleTy(*TheContext), doubles, false);

  llvm::Function *func = llvm::Function::Create(
      func_type, llvm::Function::ExternalLinkage, name_, TheModule.get());

  unsigned idx = 0;
  for (auto &arg : func->args()) {
    arg.setName(args_[idx++]);
  }

  return func;
}

llvm::Function *FunctionAST::codegen() {
  auto &p = *proto_;
  FunctionProtos[p.name()] = std::move(proto_);
  llvm::Function *the_func = getFunction(p.name());

  if (!the_func) {
    the_func = proto_->codegen();
  }

  if (!the_func) {
    return nullptr;
  }

  llvm::BasicBlock *basic_block =
      llvm::BasicBlock::Create(*TheContext, "entry", the_func);
  Builder->SetInsertPoint(basic_block);

  NamedValues.clear();
  for (auto &arg : the_func->args()) {
    NamedValues[std::string(arg.getName())] = &arg;
  }

  if (llvm::Value *ret_val = body_->codegen()) {
    Builder->CreateRet(ret_val);

    llvm::verifyFunction(*the_func);

    TheFPM->run(*the_func, *TheFAM);

    return the_func;
  }

  the_func->eraseFromParent();
  return nullptr;
}

llvm::Value *IfExprAST::codegen() {
  llvm::Value *cond_val = cond_->codegen();
  if (!cond_val) {
    return nullptr;
  }

  cond_val = Builder->CreateFCmpONE(
      cond_val, llvm::ConstantFP::get(*TheContext, llvm::APFloat(0.0)),
      "ifcond");

  llvm::Function *the_func = Builder->GetInsertBlock()->getParent();
  llvm::BasicBlock *then_block =
      llvm::BasicBlock::Create(*TheContext, "then", the_func);
  llvm::BasicBlock *else_block = llvm::BasicBlock::Create(*TheContext, "else");
  llvm::BasicBlock *merge_block =
      llvm::BasicBlock::Create(*TheContext, "ifcont");

  Builder->CreateCondBr(cond_val, then_block, else_block);

  Builder->SetInsertPoint(then_block);
  auto then_val = then_->codegen();
  if (!then_val) {
    return nullptr;
  }

  Builder->CreateBr(merge_block);
  then_block = Builder->GetInsertBlock();

  the_func->getBasicBlockList().insert(the_func->end(), else_block);
  Builder->SetInsertPoint(else_block);
  auto else_val = else_->codegen();
  if (!else_val) {
    return nullptr;
  }

  Builder->CreateBr(merge_block);
  else_block = Builder->GetInsertBlock();

  the_func->getBasicBlockList().insert(the_func->end(), merge_block);
  Builder->SetInsertPoint(merge_block);

  llvm::PHINode *PN =
      Builder->CreatePHI(llvm::Type::getDoubleTy(*TheContext), 2, "iftmp");
  PN->addIncoming(then_val, then_block);
  PN->addIncoming(else_val, else_block);
  return PN;
}

llvm::Value *ForExprAST::codegen() {
  llvm::Value *start_val = start_->codegen();
  if (!start_val)
    return nullptr;

  llvm::BasicBlock *pre_loop_block = Builder->GetInsertBlock();
  llvm::Function *for_scope_func = pre_loop_block->getParent();
  llvm::BasicBlock *loop_block =
      llvm::BasicBlock::Create(*TheContext, "loop", for_scope_func);

  Builder->CreateBr(loop_block);

  Builder->SetInsertPoint(loop_block);
  llvm::PHINode *phi_var =
      Builder->CreatePHI(llvm::Type::getDoubleTy(*TheContext), 2, name_);

  phi_var->addIncoming(start_val, pre_loop_block);

  llvm::Value *old_name_val = NamedValues[name_];
  NamedValues[name_] = phi_var;

  if (!body_->codegen())
    return nullptr;

  llvm::Value *step_val = nullptr;
  if (step_) {
    step_val = step_->codegen();
    if (!step_val)
      return nullptr;
  } else {
    step_val = llvm::ConstantFP::get(*TheContext, llvm::APFloat(1.0));
  }

  llvm::Value *next_var = Builder->CreateFAdd(phi_var, step_val, "nextvar");

  llvm::Value *end_cond = end_->codegen();
  if (!end_cond)
    return nullptr;
  end_cond = Builder->CreateFCmpONE(
      end_cond, llvm::ConstantFP::get(*TheContext, llvm::APFloat(0.0)),
      "loopcond");

  llvm::BasicBlock *end_block = Builder->GetInsertBlock();
  llvm::BasicBlock *after_block =
      llvm::BasicBlock::Create(*TheContext, "afterloop", for_scope_func);
  Builder->CreateCondBr(end_cond, loop_block, after_block);
  phi_var->addIncoming(next_var, end_block);

  Builder->SetInsertPoint(after_block);

  if (old_name_val) {
    NamedValues[name_] = old_name_val;
  } else {
    NamedValues.erase(name_);
  }

  return llvm::Constant::getNullValue(llvm::Type::getDoubleTy(*TheContext));
}

void InitializeModuleAndPassManagers() {
  TheContext = std::make_unique<llvm::LLVMContext>();
  TheModule = std::make_unique<llvm::Module>("my first jit", *TheContext);
  TheModule->setDataLayout(TheJIT->getDataLayout());

  Builder = std::make_unique<llvm::IRBuilder<>>(*TheContext);

  TheFPM = std::make_unique<llvm::FunctionPassManager>();
  TheFAM = std::make_unique<llvm::FunctionAnalysisManager>();
  TheLAM = std::make_unique<llvm::LoopAnalysisManager>();
  TheCGAM = std::make_unique<llvm::CGSCCAnalysisManager>();
  TheMAM = std::make_unique<llvm::ModuleAnalysisManager>();
  ThePIC = std::make_unique<llvm::PassInstrumentationCallbacks>();

  TheSI = std::make_unique<llvm::StandardInstrumentations>(true);
  TheSI->registerCallbacks(*ThePIC, TheFAM.get());

  TheFPM->addPass(llvm::InstCombinePass());
  TheFPM->addPass(llvm::ReassociatePass());
  TheFPM->addPass(llvm::GVNPass());
  TheFPM->addPass(llvm::SimplifyCFGPass());

  llvm::PassBuilder pb;
  pb.registerModuleAnalyses(*TheMAM);
  pb.registerFunctionAnalyses(*TheFAM);
  pb.crossRegisterProxies(*TheLAM, *TheFAM, *TheCGAM, *TheMAM);
}

void HandleDefinition() {
  if (auto fn_ast = ParseDefinition()) {
    if (auto *fn_ir = fn_ast->codegen()) {
      fprintf(stderr, "Read function definition:");
      fn_ir->print(llvm::errs());
      fprintf(stderr, "\n");
      ExitOnErr(TheJIT->addModule(llvm::orc::ThreadSafeModule(
          std::move(TheModule), std::move(TheContext))));
      InitializeModuleAndPassManagers();
    }
  } else {
    GetNextToken();
  }
}

void HandleExtern() {
  if (auto proto_ast = ParseExtern()) {
    if (auto *fn_ir = proto_ast->codegen()) {
      fprintf(stderr, "Read extern:");
      fn_ir->print(llvm::errs());
      fprintf(stderr, "\n");
      FunctionProtos[proto_ast->name()] = std::move(proto_ast);
    }
  } else {
    GetNextToken();
  }
}

void HandleTopLevelExpression() {
  if (auto fn_ast = ParseTopLevelExpr()) {
    if (fn_ast->codegen()) {
      auto resource_tracker = TheJIT->getMainJITDylib().createResourceTracker();
      auto thread_safe_module = llvm::orc::ThreadSafeModule(
          std::move(TheModule), std::move(TheContext));
      ExitOnErr(
          TheJIT->addModule(std::move(thread_safe_module), resource_tracker));
      InitializeModuleAndPassManagers();

      auto expr_symbol = ExitOnErr(TheJIT->lookup("__anon_expr"));
      assert(expr_symbol && "Function not found");

      double (*func_ptr)() =
          reinterpret_cast<double (*)()>(expr_symbol.getAddress());
      fprintf(stderr, "Evaluated to %f\n", func_ptr());
      ExitOnErr(resource_tracker->remove());
    }
  } else {
    GetNextToken();
  }
}

void MainLoop() {
  while (true) {
    fprintf(stderr, "ready> ");
    switch (cur_token) {
    case TOK_EOF:
      return;
    case ';':
      GetNextToken();
      break;
    case TOK_DEF:
      HandleDefinition();
      break;
    case TOK_EXTERN:
      HandleExtern();
      break;
    default:
      HandleTopLevelExpression();
      break;
    }
  }
}

} // namespace kaleidoscope