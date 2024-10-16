#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdbool.h>

#include "parser.h"
#include "ad.h"
#include "at.h"
#include "gc.h"

Token *iTk;		// the iterator in the tokens list
Token *consumedTk;		// the last consumed token
Symbol *owner=NULL;

bool consume(int code);
bool structDef();
bool varDef();
bool typeBase(Type *t);
bool arrayDecl(Type *t);
bool fnDef();
bool fnParam();
bool stm();
bool stmCompound(bool newDomain);
bool expr(Ret *r);
bool exprAssign(Ret *r);
bool exprOr(Ret *r);
bool exprAnd(Ret *r);
bool exprEq(Ret *r);
bool exprRel(Ret *r);
bool exprAdd(Ret *r);
bool exprMul(Ret *r);
bool exprCast(Ret *r);
bool exprUnary(Ret *r);
bool exprPostfix(Ret *r);
bool exprPrimary(Ret *r);

bool exprOrPrim(Ret *r);
bool exprAndPrim(Ret *r);
bool exprEqPrim(Ret *r);
bool exprRelPrim(Ret *r);
bool exprAddPrim(Ret *r);
bool exprMulPrim(Ret *r);
bool exprPostfixPrim(Ret *r);

void tkerr(const char *fmt,...){
	fprintf(stderr,"error in line %d: ",iTk->line);
	va_list va;
	va_start(va,fmt);
	vfprintf(stderr,fmt,va);
	va_end(va);
	fprintf(stderr,"\n");
	exit(EXIT_FAILURE);
	}

bool consume(int code){
	if(iTk->code==code){
		consumedTk=iTk;
		iTk=iTk->next;
		return true;
		}
	return false;
	}

bool structDef(){
	Token *start = iTk;
	if(consume(STRUCT)){
		if(consume(ID)){
			Token *tkName=consumedTk;
			if(consume(LACC)){
				Symbol *s = findSymbolInDomain(symTable, tkName->text);
				if(s)
				{
					tkerr("symbol redefinition: %s", tkName->text);
				}
				s=addSymbolToDomain(symTable, newSymbol(tkName->text, SK_STRUCT));
				s->type.tb=TB_STRUCT;
				s->type.s=s;
				s->type.n=-1;
				pushDomain();
				owner=s;
				for(;;){
					if(varDef()){}
					else break;
				}
				if(consume(RACC)){
					if(consume(SEMICOLON)){
						owner=NULL;
						dropDomain();
						return true;
					} else {
						tkerr("Lipseste ';' dupa definirea structurii");
					}
				} else {
					tkerr("Lipseste '}' la finalul structurii");
				}
			} //else tkerr("Lipseste acolada de inceput");
		} else {
			tkerr("Lipseste numele structurii");
		}
	}
	
	iTk=start;
	return false;
}

bool varDef(){
	Token *start=iTk;
	Type t;
	if(typeBase(&t)){
		if(consume(ID)){
			Token *tkName = consumedTk;
			if(arrayDecl(&t)){
				if(t.n==0){
					tkerr("a vector variable must have a specified dimension");
				}
			}
			if(consume(SEMICOLON)){
				Symbol *var=findSymbolInDomain(symTable, tkName->text);
				if(var){
					tkerr("symbol redefinition: %s", tkName->text);
				}
				var=newSymbol(tkName->text, SK_VAR);
				var->type=t;
				var->owner=owner;
				addSymbolToDomain(symTable, var);
				if(owner){
					switch(owner->kind){
						case SK_FN:
							var->varIdx=symbolsLen(owner->fn.locals);
							addSymbolToList(&owner->fn.locals, dupSymbol(var));
							break;
						case SK_STRUCT:
							var->varIdx=typeSize(&owner->type);
							addSymbolToList(&owner->structMembers, dupSymbol(var));
							break;
						 default:
						 	break;
					}
				} else {
					var->varMem=safeAlloc(typeSize(&t));
				}
				return true;
			} else {
				tkerr("Lipseste ';' dupa definirea variabilei");
			}
		} else {
			tkerr("Lipseste numele variabilei");
		}
	}
	iTk=start;
	return false;
}


// typeBase: TYPE_INT | TYPE_DOUBLE | TYPE_CHAR | STRUCT ID
bool typeBase(Type *t){
	Token *start=iTk;
	t->n=-1;
	if(consume(TYPE_INT)){
		t->tb=TB_INT;
		return true;
		}
	if(consume(TYPE_DOUBLE)){
		t->tb=TB_DOUBLE;
		return true;
		}
	if(consume(TYPE_CHAR)){
		t->tb=TB_CHAR;
		return true;
		}
	if(consume(STRUCT)){
		if(consume(ID)){
			Token *tkName=consumedTk;
			t->tb=TB_STRUCT;
			t->s=findSymbol(tkName->text);
			if(!t->s){
				tkerr("structura nedefinita: %s", tkName->text);
			}
			return true;
			} else {
				tkerr("Lipseste numele variabilei");
			}
		}
		iTk=start;
	return false;
}

// arrayDecl: LBRACKET INT? RBRACKET
bool arrayDecl(Type *t) {
	Token *start=iTk;
    if (consume(LBRACKET)) 
	{
        if (consume(INT)) {
			Token *tkSize=consumedTk;
			t->n=tkSize->i;
		} else{
			t->n=0;
		}
		if(consume(RBRACKET))
		{
			return true;
		} else {
			tkerr("Lipseste ']' la declararea tabloului");
		}
	}
	iTk=start;
    return false;
}

/**
 * fnDef: ( typeBase | VOID ) ID
 *				 LPAR ( fnParam ( COMMA fnParam )* )? RPAR
 *				 stmCompound
*/
bool fnDef(){
	Type t;
	Token *start=iTk;
	Instr *startInstr = owner ? lastInstr(owner->fn.instr) : NULL;
	if(typeBase(&t)){
		if(consume(ID)){
			Token *tkName=consumedTk;
			if(consume(LPAR)){
				Symbol *fn=findSymbolInDomain(symTable, tkName->text);
				if(fn){
					tkerr("symbol redefinition: %s", tkName->text);
				}
				fn=newSymbol(tkName->text, SK_FN);
				fn->type=t;
				addSymbolToDomain(symTable, fn);
				owner=fn;
				pushDomain();
				if(fnParam()){
					for(;;){
						if(consume(COMMA)){
							if(fnParam()){}
							else tkerr("Lipseste parametrul functiei dupa virgula/parametru invalid");
						}
						else break;
					}
				}
				if(consume(RPAR)){
					addInstr(&fn->fn.instr,OP_ENTER);
					if(stmCompound(false)){
						fn->fn.instr->arg.i=symbolsLen(fn->fn.locals);
						if(fn->type.tb==TB_VOID){
							addInstrWithInt(&fn->fn.instr, OP_RET_VOID, symbolsLen(fn->fn.params));
						}
						dropDomain();
						owner=NULL;
						return true;
					} else {
						tkerr("Lipseste corpul functiei");
					}
				} else {
					tkerr("Lipseste ')' la finalul functiei");
				}
			}
		} else {
			tkerr("Lipseste numele functiei");
		}
	} else if(consume(VOID)){
		t.tb=TB_VOID;
		if(consume(ID)){
			Token *tkName=consumedTk;
			if(consume(LPAR)){
				Symbol *fn=findSymbolInDomain(symTable, tkName->text);
				if(fn){
					tkerr("symbol redefinition: %s", tkName->text);
				}
				fn=newSymbol(tkName->text, SK_FN);
				fn->type=t;
				addSymbolToDomain(symTable, fn);
				owner=fn;
				pushDomain();
				if(fnParam()){
					for(;;){
						if(consume(COMMA)){
							if(fnParam()){}
							else tkerr("Lipseste parametrul functiei dupa virgula/parametru invalid");
						}
						else break;
					}
				}
				if(consume(RPAR)){
					addInstr(&fn->fn.instr, OP_ENTER);
					if(stmCompound(false)){
						fn->fn.instr->arg.i=symbolsLen(fn->fn.locals);
						if(fn->type.tb==TB_VOID){
							addInstrWithInt(&fn->fn.instr, OP_RET_VOID, symbolsLen(fn->fn.params));
						}
						dropDomain();
						owner=NULL;
						return true;
					} else {
						tkerr("Lipseste corpul functiei");
					}
				} else {
					tkerr("Lipseste ')' la finalul functiei");
				}
			}
		} else {
			tkerr("Lipseste numele functiei");
		}

	}
	iTk=start;
	if(owner){
		delInstrAfter(startInstr);
	}
	return false;
}

/**
 * fnParam: typeBase ID arrayDecl?
*/
bool fnParam(){
	Type t;
	Token *start=iTk;
	if(typeBase(&t)){
		if(consume(ID)){
			Token *tkName=consumedTk;
			if(arrayDecl(&t)){
				t.n=0;
			}
			Symbol *param=findSymbolInDomain(symTable, tkName->text);
			if(param){
				tkerr("symbol redefinition: %s", tkName->text);
			}
			param=newSymbol(tkName->text, SK_PARAM);
			param->type=t;
			param->owner=owner;
			param->paramIdx=symbolsLen(owner->fn.params);
			addSymbolToDomain(symTable, param);
			addSymbolToList(&owner->fn.params, dupSymbol(param));
			return true;
		} else {
			tkerr("Lipseste numele parametrului");
		}
	}
	iTk=start;
	return false;
}

/**
 * stm: stmCompound
 *		 | IF LPAR expr RPAR stm ( ELSE stm )?
 *		 | WHILE LPAR expr RPAR stm
 *		 | RETURN expr? SEMICOLON
 *		 | expr? SEMICOLON
*/
bool stm() { //definirea conditiilor if, else, while, return
	Token *start = iTk;
    Instr *startInstr = owner ? lastInstr(owner->fn.instr) : NULL;
    Ret rCond, rExpr;

    if (stmCompound(true)){
        return true;
    } 
    else if (consume(IF)){
        if (consume(LPAR)) {
            if (expr(&rCond)) {
                if (!canBeScalar(&rCond)) {
                    tkerr("The if condition must be a scalar value");
                }
                if (consume(RPAR)){
                    addRVal(&owner->fn.instr, rCond.lval, &rCond.type);
                    Type intType = {TB_INT, NULL, -1};
                    insertConvIfNeeded(lastInstr(owner->fn.instr), &rCond.type, &intType);
                    Instr *ifJF = addInstr(&owner->fn.instr, OP_JF);

                    if (stm()){
                        if (consume(ELSE)){
                            Instr *ifJMP = addInstr(&owner->fn.instr, OP_JMP);
                            ifJF->arg.instr = addInstr(&owner->fn.instr, OP_NOP);

                            if (stm()){
                                ifJMP->arg.instr = addInstr(&owner->fn.instr, OP_NOP);
                            } 
                            else{
                                tkerr("Missing statement after 'else'");
                            }
                        } 
                        else{
                            ifJF->arg.instr = addInstr(&owner->fn.instr, OP_NOP);
                        }

                        return true;
                    } else{
                        tkerr("Missing statement after 'if'");
                    }
                } 
                else{
                    tkerr("Missing ')' after expression in 'if'");
                }
            } 
            else{
                tkerr("Missing expression in 'if'");
            }
        } 
        else{
            tkerr("Missing '(' after 'if'");
        }
    } 
    else 
    if (consume(WHILE)){
        Instr *beforeWhileCond = lastInstr(owner->fn.instr);

        if (consume(LPAR)) {
            if (expr(&rCond)) {
                if (!canBeScalar(&rCond)){
                    tkerr("The while condition must be a scalar value");
                }
                if (consume(RPAR)){
                    addRVal(&owner->fn.instr, rCond.lval, &rCond.type);
                    Type intType = {TB_INT, NULL, -1};
                    insertConvIfNeeded(lastInstr(owner->fn.instr), &rCond.type, &intType);
                    Instr *whileJF = addInstr(&owner->fn.instr, OP_JF);

                    if (stm()){
                        addInstr(&owner->fn.instr, OP_JMP)->arg.instr = beforeWhileCond->next;
                        whileJF->arg.instr = addInstr(&owner->fn.instr, OP_NOP);

                        return true;
                    } 
                    else{
                        tkerr("Missing statement after 'while'");
                    }
                } 
                else{
                    tkerr("Missing ')' after expression in 'while'");
                }
            }
            else{
                tkerr("Missing expression in 'while'");
            }
        } 
        else{
            tkerr("Missing '(' after 'while'");
        }
    } 
    else 
    if (consume(RETURN)){
        if (expr(&rExpr)){
            if (owner->type.tb == TB_VOID){
                tkerr("A void function cannot return a value");
            }

            if (!canBeScalar(&rExpr)){
                tkerr("The return value must be a scalar value");
            }

            if (!convTo(&rExpr.type, &owner->type)){
                tkerr("Cannot convert the return expression type to the function "
                      "return type");
            }

            addRVal(&owner->fn.instr, rExpr.lval, &rExpr.type);
            insertConvIfNeeded(lastInstr(owner->fn.instr), &rExpr.type, &owner->type);
            addInstrWithInt(&owner->fn.instr, OP_RET, symbolsLen(owner->fn.params));
        } 
        else{
            if (owner->type.tb != TB_VOID){
                tkerr("A non-void function must return a value");
            }

            addInstr(&owner->fn.instr, OP_RET_VOID);
        }
        if (consume(SEMICOLON)){
            return true;
        } 
        else{
            tkerr("Missing ';' after 'return'");
        }
    } else{
        if (expr(&rExpr)){
            if (rExpr.type.tb != TB_VOID){
                addInstr(&owner->fn.instr, OP_DROP);
            }
        }

        if (consume(SEMICOLON)){
            return true;
        }
    }

    iTk = start;
    if (owner){
        delInstrAfter(startInstr);
    }

    return false;
}

/**
 * stmCompound: LACC ( varDef | stm )* RACC
*/
bool stmCompound(bool newDomain){
	Token *start=iTk;
	if(consume(LACC)){
		if(newDomain){
			pushDomain();
		}
		for(;;){
			if(varDef() || stm()){}
			else break;
		}
		if(consume(RACC)){
			if(newDomain){
				dropDomain();
			}
			return true;
		} else {
			tkerr("Lipseste '}'");
		}
	}
	iTk=start;
	return false;
}

/**
 * expr: exprAssign
*/
bool expr(Ret *r){
	Token *start = iTk;
	if(exprAssign(r)){
		return true;
	}
	iTk=start;
	return false;
}

/**
 * exprAssign: exprUnary ASSIGN exprAssign | exprOr
*/
bool exprAssign(Ret *r) { //tratarea cazului de assign =, sa fie ambele parti prezente, si dreapta, si stanga
	Token *start = iTk;
    Instr *startInstr = owner ? lastInstr(owner->fn.instr) : NULL;
    Ret rDst;

    if (exprUnary(&rDst)){
        if (consume(ASSIGN)){
            if (exprAssign(r)){
                if (!rDst.lval){
                    tkerr("The assign destination must be a left-value");
                }

                if (rDst.ct){
                    tkerr("The assign destination cannot be constant");
                }

                if (!canBeScalar(&rDst)){
                    tkerr("The assign destination must be scalar");
                }

                if (!canBeScalar(r)){
                    tkerr("The assign source must be scalar");
                }

                if (!convTo(&r->type, &rDst.type)){
                    tkerr("The assign source cannot be converted to destination");
                }

                r->lval = false;
                r->ct = true;

                addRVal(&owner->fn.instr, r->lval, &r->type);
                insertConvIfNeeded(lastInstr(owner->fn.instr), &r->type, &rDst.type);
                switch (rDst.type.tb){
                    case TB_INT:
                        addInstr(&owner->fn.instr, OP_STORE_I);
                        break;
                    case TB_DOUBLE:
                        addInstr(&owner->fn.instr, OP_STORE_F);
                        break;
                    default : break;
                }

                return true;
            } 
            else{
                tkerr("Missing expression after '='");
            }
        }
    }

    iTk = start;
    if (owner){
        delInstrAfter(startInstr);
    }

    if (exprOr(r)){
        return true;
    }

    iTk = start;
    if (owner) 
    {
        delInstrAfter(startInstr);
    }

    return false;
}


/**
 * exprOr: exprOr OR exprAnd | exprAnd =>
 * exprOr: exprAnd exprOrPrim
 * exprOrPrim: Or exprAnd exprOrPrim
*/
bool exprOr(Ret *r){
	Token *start=iTk;
	if(exprAnd(r)){
		if(exprOrPrim(r)){
			return true;
		}
	}
	iTk=start;
	return false;
}

bool exprOrPrim(Ret *r){
	
	if(consume(OR)){
		Ret right;
		if(exprAnd(&right)){
			Type tDst;
			if(!arithTypeTo(&r->type, &right.type, &tDst)){
				tkerr("invalid operand type for ||");
			}
			*r=(Ret){{TB_INT, NULL, -1}, false, true};
			if(exprOrPrim(r)){
				return true;
			}
		} else {
			tkerr("Lipseste expresia dupa semnul '||'");
		}
	}
	return true; //asta-i epsilon din formula
}

/**
 * exprAnd: exprAnd AND exprEq | exprEq =>
 * exprAnd: exprEq exprAndPrim
 * exprAndPrim: AND exprEq exprAndPrim
*/
bool exprAnd(Ret *r){
	Token *start=iTk;
	if(exprEq(r)){
		if(exprAndPrim(r)){
			return true;
		}
	}
	iTk=start;
	return false;
}

bool exprAndPrim(Ret *r){
	if(consume(AND)){
		Ret right;
		if(exprEq(&right)){
			Type tDst;
			if(!arithTypeTo(&r->type, &right.type, &tDst)){
				tkerr("invalid operand type for &&");
			}
			*r=(Ret){{TB_INT, NULL, -1}, false, true};
			if(exprAndPrim(r)){
				return true;
			}
		} else {
			tkerr("Lipseste expresia dupa semnul '&&'");
		}
	}
	return true; //asta-i epsilon din formula
}

/**
 * exprEq: exprEq ( EQUAL | NOTEQ ) exprRel | exprRel =>
 * exprEq: exprRel exprEqPrim
 * exprEqPrim: (EQUAL | NOTEQ) exprRel exprEqPrim
*/

bool exprEq(Ret *r){
	Token *start = iTk;
	if(exprRel(r)){
		if(exprEqPrim(r)){
			return true;
		}
	}
	iTk=start;
	return false;
}

bool exprEqPrim(Ret *r){
	if(consume(EQUAL)){
		Ret right;
		if(exprRel(&right)){
			Type tDst;
			if(!arithTypeTo(&r->type, &right.type, &tDst)){
				tkerr("invalid operand type for ==");
			}
			*r=(Ret){{TB_INT, NULL, -1}, false, true};
			if(exprEqPrim(r)){
				return true;
			}
		} else {
			tkerr("Lipseste expresia dupa '=='");
		}
	} else if(consume(NOTEQ)){
		Ret right;
		if(exprRel(&right)){
			Type tDst;
			if(!arithTypeTo(&r->type, &right.type, &tDst)){
				tkerr("invalid operand type for !=");
			}
			*r=(Ret){{TB_INT, NULL, -1}, false, true};
			if(exprEqPrim(r)){
				return true;
			}
		} else {
			tkerr("Lipseste expresia dupa '!='");
		}
	}
	return true; //asta-i epsilon din functie
}

/**
 * exprRel: exprRel ( LESS | LESSEQ | GREATER | GREATEREQ ) exprAdd | exprAdd =>
 * exprRel: exprAdd exprRelPrim
 * exprRelPrim: ( LESS | LESSEQ | GREATER | GREATEREQ ) exprAdd exprRelPrim
*/
bool exprRel(Ret *r){
	Token *start=iTk;
	Instr *startInstr = owner ? lastInstr(owner->fn.instr) : NULL;
	if(exprAdd(r)){
		if(exprRelPrim(r)){
			return true;
		}
	}
	iTk=start;
	if(owner){
		delInstrAfter(startInstr);
	}
	return false;
}

bool exprRelPrim(Ret *r){
    Token *op;
    if (consume(LESS) || consume(LESSEQ) || consume(GREATER) ||consume(GREATEREQ)){
        Ret right;

        op = consumedTk;
        Instr *lastLeft = lastInstr(owner->fn.instr);
        addRVal(&owner->fn.instr, r->lval, &r->type);

        if (exprAdd(&right)){
            Type tDst;
            if (!arithTypeTo(&r->type, &right.type, &tDst)) {
                tkerr("Invalid operand type for <, <=, >,>=");
            }

            addRVal(&owner->fn.instr, right.lval, &right.type);
            insertConvIfNeeded(lastLeft, &r->type, &tDst);
            insertConvIfNeeded(lastInstr(owner->fn.instr), &right.type, &tDst);
            switch (op->code){
                case LESS:
                    switch (tDst.tb){
                        case TB_INT:
                            addInstr(&owner->fn.instr, OP_LESS_I);
                            break;
                        case TB_DOUBLE:
                            addInstr(&owner->fn.instr, OP_LESS_F);
                            break;
                        default : break;
                    }
                    break;
                default : break;
            }

            *r = (Ret){{TB_INT, NULL, -1}, false, true};

            if (exprRelPrim(r)){
                return true;
            }
        } 
        else{
            tkerr("Invalid expression after comparison");
        }
    }

    return true;
}
/**
 * exprAdd: exprAdd ( ADD | SUB ) exprMul | exprMul =>
 * exprAdd: exprMul exprAddPrim
 * exprAddPrim: (ADD | SUB) exprMul exprAddPrim
*/
bool exprAdd(Ret *r){
	Token *start=iTk;
	Instr *startInstr = owner ? lastInstr(owner->fn.instr) : NULL;
	if(exprMul(r)){
		if(exprAddPrim(r)){
			return true;
		}
	}
	iTk=start;
	if(owner){
		delInstrAfter(startInstr);
	}
	return false;
}

bool exprAddPrim(Ret *r){
    if (consume(ADD) || consume(SUB)){
        Ret right;

        Token *op = consumedTk;
        Instr *lastLeft = lastInstr(owner->fn.instr);
        addRVal(&owner->fn.instr, r->lval, &r->type);

        if (exprMul(&right)) {
            Type tDst;
            if (!arithTypeTo(&r->type, &right.type, &tDst)){
                tkerr("Invalid operand type for + or -");
            }

            addRVal(&owner->fn.instr, right.lval, &right.type);
            insertConvIfNeeded(lastLeft, &r->type, &tDst);
            insertConvIfNeeded(lastInstr(owner->fn.instr), &right.type, &tDst);
            switch (op->code){
                case ADD:
                    switch (tDst.tb){
                        case TB_INT:
                            addInstr(&owner->fn.instr, OP_ADD_I);
                            break;
                        case TB_DOUBLE:
                            addInstr(&owner->fn.instr, OP_ADD_D);
                            break;
                        default : break;
                    }
                    break;
                case SUB:
                    switch (tDst.tb){
                        case TB_INT:
                            addInstr(&owner->fn.instr, OP_SUB_I);
                            break;
                        case TB_DOUBLE:
                            addInstr(&owner->fn.instr, OP_SUB_F);
                            break;
                        default : break;
                    }
                    break;
                default : break;
            }

            *r = (Ret){tDst, false, true};

            if (exprAddPrim(r)){
                return true;
            }
        } 
        else 
        {
            tkerr("Invalid expression after operation");
        }
    }

    return true;
}

/**
 * exprMul: exprMul ( MUL | DIV ) exprCast | exprCast =>
 * exprMul: exprCast exprMulPrim
 * exprMulPrim: (MUL | DIV) exprCast exprMulPrim
*/
bool exprMul(Ret *r){
	Token *start=iTk;
	Instr *startInstr = owner ? lastInstr(owner->fn.instr) :NULL;
	if(exprCast(r)){
		if(exprMulPrim(r)){
			return true;
		}
	}
	iTk=start;
	if(owner){
		delInstrAfter(startInstr);
	}
	return false;
}

bool exprMulPrim(Ret *r){

    if (consume(MUL) || consume(DIV)){
        Ret right;

        Token *op = consumedTk;
        Instr *lastLeft = lastInstr(owner->fn.instr);
        addRVal(&owner->fn.instr, r->lval, &r->type);

        if (exprCast(&right)){
            Type tDst;

            if (!arithTypeTo(&r->type, &right.type, &tDst)){
                tkerr("Invalid operand type for * or /");
            }

            addRVal(&owner->fn.instr, right.lval, &right.type);
            insertConvIfNeeded(lastLeft, &r->type, &tDst);
            insertConvIfNeeded(lastInstr(owner->fn.instr), &right.type, &tDst);
            switch (op->code){
                case MUL:
                    switch (tDst.tb){
                        case TB_INT:
                            addInstr(&owner->fn.instr, OP_MUL_I);
                            break;
                        case TB_DOUBLE:
                            addInstr(&owner->fn.instr, OP_MUL_F);
                            break;
                        default : break;
                    }
                    break;
                case DIV:
                    switch (tDst.tb){
                        case TB_INT:
                            addInstr(&owner->fn.instr, OP_DIV_I);
                            break;
                        case TB_DOUBLE:
                            addInstr(&owner->fn.instr, OP_DIV_F);
                            break;
                        default : break;
                    }
                    break;
                default : break;
            }

            *r = (Ret){tDst, false, true};

            if (exprMulPrim(r)){
                return true;
            }
        } 
        else{
            tkerr("Invalid expression after operation");
        }
    }

    return true;
}

/**
 * exprCast: LPAR typeBase arrayDecl? RPAR exprCast | exprUnary
*/
bool exprCast(Ret *r){
	Token *start=iTk;
	if(consume(LPAR)){
		Type t;
		Ret op;
		if(typeBase(&t)){
			if(arrayDecl(&t)){}
			if(consume(RPAR)){
				if(exprCast(&op)){
					if(t.tb==TB_STRUCT){
						tkerr("cannot convert to a struct type");
					}
					if(op.type.tb==TB_STRUCT){
						tkerr("cannot convert a struct");
					}
					if(op.type.n>=0 && t.n<0){
						tkerr("an array can be converted only to another array");
					}
					if(op.type.n<0 && t.n>=0){
						tkerr("a scalar can be converted only to another scalar");
						*r=(Ret){t,false,true};
					}
					return true;
				}
			} else {
				tkerr("Lipseste ')'");
			}
		} else {
			tkerr("Lipseste expresia dupa semnul '}'");
		}
	}
	if(exprUnary(r)){
		return true;
	}
	iTk=start;
	return false;
}

/**
 * exprUnary: ( SUB | NOT ) exprUnary | exprPostfix
*/
bool exprUnary(Ret *r){
	Token *start=iTk;
	if(consume(SUB)){
		if(exprUnary(r)){
			if(!canBeScalar(r)){
				tkerr("unary - must have a scalar operand");
			}
			r->lval=false;
			r->ct=true;
			return true;
		} else {
			tkerr("Expresie invalida dupa '-'");
		}
	} else if(consume(NOT)){
		if(exprUnary(r)){
			if(!canBeScalar(r)){
				tkerr("unary ! must have a scalar operand");
			}
			r->lval=false;
			r->ct=true;
			return true;
		} else {
			tkerr("Expresie invalida dupa '!'");
		}
		iTk=start;
	}
	if(exprPostfix(r)){
		return true;
	}
	iTk=start;
	return false;
}

/**
 * exprPostfix: exprPostfix LBRACKET expr RBRACKET
 *		| exprPostfix DOT ID
 *		| exprPrimary
 =>
 exprPostfix: exprPrimary exprPostfixPrim
 exprPostfixPrim: LBRACKET expr RBRACKET exprPostfixPrim | DOT ID exprPostfixPrim
*/

bool exprPostfix(Ret *r){
	Token *start=iTk;
	if(exprPrimary(r)){
		if(exprPostfixPrim(r)){
			return true;
		}
	}
	iTk=start;
	return false;
}

bool exprPostfixPrim(Ret *r){
    Token *start = iTk;
 
    if(consume(LBRACKET))
	{
        Ret idx;
        if(expr(&idx))
		{
            if(consume(RBRACKET))
			{
                  if(r->type.n<0)
                  {
                    tkerr("only an array can be indexed");
                  }
                  Type tInt={TB_INT,NULL,-1};
                  if(!convTo(&idx.type,&tInt))
                  {
                    tkerr("the index is not convertible to int");
                  }
                  r->type.n=-1;
                  r->lval=true;
                  r->ct=false;
                if(exprPostfixPrim(r))
				{
                    return true;
                }else tkerr("expresie invalida dupa ]\n");
            }else tkerr("lipseste ] dupa expresie\n");
        }
        iTk = start;
    }
    if(consume(DOT))
	{
        if(consume(ID))
		{
            Token *tkName=consumedTk;
             if (r->type.tb != TB_STRUCT) 
             {
				tkerr("a field can only be selected from a struct");
			}
			Symbol *s = findSymbolInList(r->type.s->structMembers, tkName->text);
            if (!s)
             {
				tkerr("the structure %s does not have a field %s",r->type.s->name,tkName->text);
			}
			*r = (Ret){s->type,true,s->type.n>=0};
            if(exprPostfixPrim(r))
			{
                return true;
            }else tkerr("lipseste expresia de dupa numele campului\n");
        }else tkerr("lipseste numele campului ce se doreste a fi cautat\n");
        iTk = start;
    }
    return true;//epsilon
 
}

/**
 * exprPrimary: ID ( LPAR ( expr ( COMMA expr )* )? RPAR )?
 *		| INT | DOUBLE | CHAR | STRING | LPAR expr RPAR
*/
bool exprPrimary(Ret *r) { //myFunction(1, "hello", 3.14)
	Token *start = iTk;
    Instr *startInstr = owner ? lastInstr(owner->fn.instr) : NULL;

    if (consume(ID)){
        Token *tkName = consumedTk;
        Symbol *s = findSymbol(tkName->text);

        if (!s){
            tkerr("Undefined id: %s", tkName->text);
        }

        if (consume(LPAR)){
            if (s->kind != SK_FN){
                tkerr("Only a function can be called");
            }

            Ret rArg;
            Symbol *param = s->fn.params;

            if (expr(&rArg)){
                if (!param){
                    tkerr("Too many arguments in function call");
                }

                if (!convTo(&rArg.type, &param->type)){
                    tkerr("In call, cannot convert the argument type to the parameter type");
                }

                addRVal(&owner->fn.instr, rArg.lval, &rArg.type);
                insertConvIfNeeded(lastInstr(owner->fn.instr), &rArg.type, &param->type);

                param = param->next;

                for (;;) {
                    if (consume(COMMA)){
                        if (expr(&rArg)){
                            if (!param){
                                tkerr("Too many arguments in function call");
                            }

                            if (!convTo(&rArg.type, &param->type)){
                                tkerr("In call, cannot convert the argument type to the parameter type");
                            }

                            addRVal(&owner->fn.instr, rArg.lval, &rArg.type);
                            insertConvIfNeeded(lastInstr(owner->fn.instr), &rArg.type, &param->type);

                            param = param->next;
                        } 
                        else{
                            tkerr("Missing expression after ',' in function call");
                        }
                    } 
                    else{
                        break;
                    }
                }
            }
            if (consume(RPAR)){
                if (param){
                    tkerr("Too few arguments in function call");
                }

                *r = (Ret){s->type, false, true};

                if (s->fn.extFnPtr){
                    addInstr(&owner->fn.instr, OP_CALL_EXT)->arg.extFnPtr = s->fn.extFnPtr;
                } 
                else{
                    addInstr(&owner->fn.instr, OP_CALL)->arg.instr = s->fn.instr;
                }

                return true;
            } 
            else{
                tkerr("Missing ')' in function call");
            }


        } 
        else{
            if (s->kind == SK_FN){
                tkerr("A function can only be called");
            }

            *r = (Ret){s->type, true, s->type.n >= 0};

            if (s->kind == SK_VAR){
                if (s->owner == NULL) {// global variables
                    addInstr(&owner->fn.instr, OP_ADDR)->arg.p = s->varMem;
                } 
                else{// local variables
                    switch (s->type.tb){
                        case TB_INT:
                            addInstrWithInt(&owner->fn.instr, OP_FPADDR_I, s->varIdx + 1);
                            break;
                        case TB_DOUBLE:
                            addInstrWithInt(&owner->fn.instr, OP_FPADDR_F, s->varIdx + 1);
                            break;
                        default : break;
                    }
                }
            }

            if (s->kind == SK_PARAM){
                switch (s->type.tb){
                    case TB_INT:
                        addInstrWithInt(&owner->fn.instr, OP_FPADDR_I, s->paramIdx - symbolsLen(s->owner->fn.params) - 1);
                        break;
                    case TB_DOUBLE:
                        addInstrWithInt(&owner->fn.instr, OP_FPADDR_F, s->paramIdx - symbolsLen(s->owner->fn.params) - 1);
                        break;
                    default : break;
                }
            }
        }
        return true;
    } 
    else if (consume(INT)){
        *r = (Ret){{TB_INT, NULL, -1}, false, true};

        Token *ct = consumedTk;
        addInstrWithInt(&owner->fn.instr, OP_PUSH_I, ct->i);
        return true;
    } 
    else if (consume(DOUBLE)){
        *r = (Ret){{TB_DOUBLE, NULL, -1}, false, true};

        Token *ct = consumedTk;
        addInstrWithDouble(&owner->fn.instr, OP_PUSH_D, ct->d);
        return true;
    } 
    else if (consume(CHAR)){
        *r = (Ret){{TB_CHAR, NULL, -1}, false, true};
        return true;
    } 
    else if (consume(STRING)){
        *r = (Ret){{TB_CHAR, NULL, 0}, false, true};
        return true;
    } 
    else if (consume(LPAR)){
        if (expr(r)){
            if (consume(RPAR)){
                return true;
            } 
            else{
                tkerr("Missing ')' after expression");
            }
        }
    }

    iTk = start;

    if (owner){
        delInstrAfter(startInstr);
    }
    
    return false;
}


// unit: ( structDef | fnDef | varDef )* END
bool unit(){
	for(;;){
		if(structDef()){}
		else if(fnDef()){}
		else if(varDef()){}
		else break; 
		}
	if(consume(END)){
		return true;
		} else {
			tkerr("Syntax error");
		}
	return false;
	}

void parse(Token *tokens){
	iTk=tokens;
	if(!unit())tkerr("syntax error");
	}
