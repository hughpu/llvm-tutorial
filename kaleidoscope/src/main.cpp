#include <kaleidoscope/ast.h>
#include <kaleidoscope/token.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Target/TargetMachine.h>

int main()
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    kaleidoscope::InitializeModuleAndPassManagers();

    fprintf(stderr, "ready> ");
    kaleidoscope::GetNextToken();

    kaleidoscope::MainLoop();
    
    kaleidoscope::TheModule->print(llvm::errs(), nullptr);
    return 0;
}