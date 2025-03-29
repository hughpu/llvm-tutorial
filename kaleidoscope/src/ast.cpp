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

std::map<std::string, llvm::AllocaInst *> NamedValues;
int cur_token;
std::map<char, int> binop_precedence = {
    {'=', 2},
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
  auto lhs = ParseUnary();

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
  case TOK_VAR:
    return ParseVarExpr();
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

    auto rhs = ParseUnary();
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

std::unique_ptr<ExprAST> ParseUnary() {
  if (!isascii(cur_token) || cur_token == '(' || cur_token == ',')
    return ParsePrimary();

  int op_code = cur_token;
  GetNextToken();
  if (auto operand = ParseUnary())
    return std::make_unique<UnaryExprAST>(op_code, std::move(operand));

  return nullptr;
}

std::unique_ptr<PrototypeAST> ParsePrototype() {
  std::string fn_name;
  unsigned kind = 0; // 0 = identifier; 1 = unary; 2 = binary;
  unsigned binary_precedence = 30;

  switch (cur_token) {
  case TOK_IDENTIFIER:
    fn_name = identitfier_str;
    kind = 0;
    GetNextToken(); // eat identifier
    break;
  case TOK_UNARY:
    GetNextToken();
    if (!isascii(cur_token))
      return LogErrorP("Expect unary operator");
    fn_name = "unary";
    fn_name += (char)cur_token;
    kind = 1;
    GetNextToken();
    break;
  case TOK_BINARY:
    GetNextToken();
    if (!isascii(cur_token))
      return LogErrorP("Expect binary operator");
    fn_name = "binary";
    fn_name += (char)cur_token;
    kind = 2;
    GetNextToken();

    if (cur_token == TOK_NUMBER) {
      if (num_val < 1 || num_val > 100)
        return LogErrorP("Invalid precedence suppose to be in range of 0..100");
      binary_precedence = (unsigned)num_val;
      GetNextToken();
    }

    break;
  default:
    return LogErrorP("Expected function name in prototype");
  }

  if (cur_token != '(')
    return LogErrorP("Expected '(' in prototype");

  std::vector<std::string> arg_names;
  while (GetNextToken() == TOK_IDENTIFIER) {
    arg_names.push_back(identitfier_str);
  }

  if (cur_token != ')')
    return LogErrorP("Expected ')' in prototype");

  GetNextToken(); // eat ')'
  if (kind && arg_names.size() != kind)
    return LogErrorP("Invalid number of operands for operator");

  return std::make_unique<PrototypeAST>(fn_name, std::move(arg_names),
                                        kind != 0, binary_precedence);
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

std::unique_ptr<ExprAST> ParseVarExpr() {
  GetNextToken();

  std::vector<std::pair<std::string, std::unique_ptr<ExprAST>>> var_names;

  if (cur_token != TOK_IDENTIFIER)
    return LogError("expected identifier after var");

  while (true) {
    std::string name = identitfier_str;
    GetNextToken();

    std::unique_ptr<ExprAST> init;
    if (cur_token == '=') {
      GetNextToken();

      init = ParseExpression();
      if (!init)
        return nullptr;
    }

    var_names.push_back(std::make_pair(name, std::move(init)));

    if (cur_token != ',')
      break;
    GetNextToken();

    if (cur_token != TOK_IDENTIFIER) {
      return LogError("expected identifier list after var");
    }
  }

  if (cur_token != TOK_IN)
    return LogError("expected 'in' keyword after 'var'");
  GetNextToken();

  auto body = ParseExpression();
  if (!body) {
    return nullptr;
  }

  return std::make_unique<VarExprAST>(std::move(var_names), std::move(body));
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
  llvm::AllocaInst *A = NamedValues[name_];
  if (!A)
    return LogErrorV(("Unknown variable name: " + name_).c_str());
  return Builder->CreateLoad(A->getAllocatedType(), A, name_.c_str());
}

llvm::Value *VarExprAST::codegen() {
  std::vector<std::pair<std::string, llvm::AllocaInst *>> old_bindings;

  llvm::Function *the_func = Builder->GetInsertBlock()->getParent();

  for (const auto &var_pair : var_names_) {
    const std::string &var_name = var_pair.first;
    ExprAST *init = var_pair.second.get();

    llvm::Value *init_val;
    if (init) {
      init_val = init->codegen();
      if (!init_val)
        return nullptr;
    } else {
      init_val = llvm::ConstantFP::get(*TheContext, llvm::APFloat(0.0));
    }

    llvm::AllocaInst *alloca = CreateEntryBlockAlloca(the_func, var_name);
    Builder->CreateStore(init_val, alloca);

    if (NamedValues.find(var_name) != NamedValues.end()) {
      old_bindings.emplace_back(var_name, NamedValues[var_name]);
    }

    NamedValues[var_name] = alloca;
  }

  llvm::Value *body_val = body_->codegen();
  if (!body_val)
    return nullptr;

  for (const auto &old_binding : old_bindings) {
    NamedValues[old_binding.first] = old_binding.second;
  }

  return body_val;
}

llvm::Value *UnaryExprAST::codegen() {
  llvm::Value *operand_value = operand_->codegen();
  if (!operand_value)
    return nullptr;

  llvm::Function *func = getFunction(std::string("unary") + op_);
  if (!func)
    return LogErrorV("Unknown unary operator");

  return Builder->CreateCall(func, operand_value, "unop");
}

llvm::Value *BinaryExprAST::codegen() {
  llvm::Value *lhs_val = lhs_->codegen();
  llvm::Value *rhs_val = rhs_->codegen();
  if (!lhs_val || !rhs_val)
    return nullptr;

  if (op_ == '=') {
    VariableExprAST *lhs_eq = static_cast<VariableExprAST *>(lhs_.get());
    if (!lhs_eq) {
      return LogErrorV("destination of '=' must be variable");
    }
    llvm::AllocaInst *lhs_var = NamedValues[lhs_eq->name()];
    if (!lhs_var) {
      return LogErrorV("unknown variable name");
    }
    Builder->CreateStore(rhs_val, lhs_var);
    return rhs_val;
  }

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
    break;
  }

  llvm::Function *F = getFunction(std::string("binary") + op_);
  assert(F && "binary operator not found!");

  llvm::Value *ops[2] = {lhs_val, rhs_val};
  return Builder->CreateCall(F, ops, "binops");
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

  // if (!the_func) {
  //   the_func = proto_->codegen();
  // }

  if (!the_func) {
    return nullptr;
  }

  if (p.isBinaryOp())
    binop_precedence[p.getOperatorName()] = p.getPrecedence();

  llvm::BasicBlock *basic_block =
      llvm::BasicBlock::Create(*TheContext, "entry", the_func);
  Builder->SetInsertPoint(basic_block);

  NamedValues.clear();
  for (auto &arg : the_func->args()) {
    llvm::AllocaInst *alloc = CreateEntryBlockAlloca(the_func, arg.getName());
    Builder->CreateStore(&arg, alloc);
    NamedValues[std::string(arg.getName())] = alloc;
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
  // entry
  llvm::Function *the_func = Builder->GetInsertBlock()->getParent();
  llvm::AllocaInst *alloca = CreateEntryBlockAlloca(the_func, name_);

  llvm::Value *start_val = start_->codegen();
  if (!start_val)
    return nullptr;

  Builder->CreateStore(start_val, alloca);
  llvm::AllocaInst *old_name_alloc = NamedValues[name_];
  NamedValues[name_] = alloca;

  llvm::BasicBlock *loop_block =
      llvm::BasicBlock::Create(*TheContext, "loop", the_func);
  llvm::BasicBlock *after_block =
      llvm::BasicBlock::Create(*TheContext, "afterloop", the_func);
  llvm::BasicBlock *pre_block =
      llvm::BasicBlock::Create(*TheContext, "preloop", the_func);

  // pre
  Builder->CreateBr(pre_block);
  Builder->SetInsertPoint(pre_block);

  llvm::Value *end_cond = end_->codegen();
  if (!end_cond)
    return nullptr;
  end_cond = Builder->CreateFCmpONE(
      end_cond, llvm::ConstantFP::get(*TheContext, llvm::APFloat(0.0)),
      "loopcond");
  Builder->CreateCondBr(end_cond, loop_block, after_block);

  // body
  Builder->SetInsertPoint(loop_block);
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
  llvm::Value *cur_var =
      Builder->CreateLoad(alloca->getAllocatedType(), alloca, name_.c_str());
  llvm::Value *next_var = Builder->CreateFAdd(cur_var, step_val, "nextvar");
  Builder->CreateStore(next_var, alloca);
  Builder->CreateBr(pre_block);

  // after
  Builder->SetInsertPoint(after_block);

  if (old_name_alloc) {
    NamedValues[name_] = old_name_alloc;
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

  TheFPM->addPass(llvm::PromotePass());
  TheFPM->addPass(llvm::InstCombinePass());
  TheFPM->addPass(llvm::ReassociatePass());
  TheFPM->addPass(llvm::GVNHoistPass());
  TheFPM->addPass(llvm::GVNSinkPass());
  TheFPM->addPass(llvm::SimplifyCFGPass());

  llvm::PassBuilder pb;
  pb.registerModuleAnalyses(*TheMAM);
  pb.registerFunctionAnalyses(*TheFAM);
  pb.crossRegisterProxies(*TheLAM, *TheFAM, *TheCGAM, *TheMAM);
}

llvm::AllocaInst *CreateEntryBlockAlloca(llvm::Function *the_func,
                                         llvm::StringRef var_name) {
  llvm::IRBuilder<> temp_builder(&the_func->getEntryBlock(),
                                 the_func->getEntryBlock().begin());
  return temp_builder.CreateAlloca(llvm::Type::getDoubleTy(*TheContext),
                                   nullptr, var_name);
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