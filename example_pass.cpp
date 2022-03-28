#include "AST.h"

void ExamplePass(ModuleAST* TheModule) {
  fprintf(stderr, "This Module has %lu externs and %lu functions!\n\n",
    TheModule->Externs.size(), TheModule->Functions.size());
}
