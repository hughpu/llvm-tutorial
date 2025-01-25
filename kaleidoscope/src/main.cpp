#include <kaleidoscope/ast.h>
#include <kaleidoscope/token.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Target/TargetMachine.h>
#include <immintrin.h>

int main()
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    fprintf(stderr, "ready> ");
    kaleidoscope::GetNextToken();

    kaleidoscope::TheJIT = kaleidoscope::ExitOnErr(llvm::orc::KaleidoscopeJIT::Create());
    kaleidoscope::InitializeModuleAndPassManagers();

    kaleidoscope::MainLoop();
    
    kaleidoscope::TheModule->print(llvm::errs(), nullptr);
    return 0;
}