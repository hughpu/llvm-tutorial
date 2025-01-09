#include <kaleidoscope/ast.h>
#include <kaleidoscope/token.h>

int main()
{
    kaleidoscope::InitializeModuleAndPassManagers();

    fprintf(stderr, "ready> ");
    kaleidoscope::GetNextToken();

    kaleidoscope::MainLoop();
    
    kaleidoscope::TheModule->print(llvm::errs(), nullptr);
    return 0;
}