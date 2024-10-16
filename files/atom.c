#include <stdio.h>
#include <stdlib.h>

#include "lexer.h" 
#include "utils.h"
#include "lexer.c"
#include "utils.c"
#include "parser.h"
#include "parser.c"
#include "ad.c"
#include "ad.h"
#include "at.c"
#include "at.h"
#include "vm.c"
#include "vm.h"
#include "gc.c"
#include "gc.h"

int main()
{
    
    char *inbuf=loadFile("tests/testgc.c"); 
    puts(inbuf);
    Token *tokens=tokenize(inbuf);
    free(inbuf);
    showTokens(tokens);
    pushDomain();
    vmInit();
    parse(tokens);
    showDomain(symTable, "global");
    Instr *test=genTestProgramTema();
    run(test);

    Symbol *symMain=findSymbolInDomain(symTable,"main");

    if(!symMain)err("missing main function");

    Instr *entryCode=NULL;
    addInstr(&entryCode,OP_CALL)->arg.instr=symMain->fn.instr;
    addInstr(&entryCode,OP_HALT);
    run(entryCode);
    dropDomain();
    return 0;
}