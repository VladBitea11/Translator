#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>

#include "lexer.h"
#include "utils.h"

Token *tokens;	// single linked list of tokens
Token *lastTk;		// the last token in list

int line=1;		// the current line in the input file

// adds a token to the end of the tokens list and returns it
// sets its code and line
Token *addTk(int code){
	Token *tk=safeAlloc(sizeof(Token));
	tk->code=code;
	tk->line=line;
	tk->next=NULL;
	if(lastTk){
		lastTk->next=tk;
		}else{
		tokens=tk;
		}
	lastTk=tk;
	return tk;
	}

char *extract(const char *begin,const char *end){
	
	//Calculez lungimea substringului extras
	size_t length = end - begin;	
	//Aloc memorie pentru substringul extras
	char *extracted = (char *)safeAlloc(length+1); //+1 de la terminatorul NULL
	//Copiez substringul din stringul original in substringul extras
	strncpy(extracted, begin, length);
	//Adauga terminatorul NULL
	extracted[length]='\0';
	//returneaza substringul extras
	return extracted;

}


Token *tokenize(const char *pch){
	const char *start;
	Token *tk;
	char *endptr; //Pointer pentru strtol
	for(;;){
		switch(*pch){
			case ' ':case '\t':pch++;break;
			case '\r':		// handles different kinds of newlines (Windows: \r\n, Linux: \n, MacOS, OS X: \r or \n)
				if(pch[1]=='\n')pch++;
				// fallthrough to \n
			case '\n':
				line++;
				pch++;
				break;
			case '\0':addTk(END);return tokens;
			case ',':addTk(COMMA);pch++;break;
			case '=':
				if(pch[1]=='='){
					addTk(EQUAL);
					pch+=2;
					}else{
					addTk(ASSIGN);
					pch++;
					}
				break;
			case '(': addTk(LPAR); pch++; break;
			case ')': addTk(RPAR); pch++; break;
			case '{': addTk(LACC); pch++; break;
			case '}': addTk(RACC); pch++; break;
			case '[': addTk(LBRACKET); pch++; break;
			case ']': addTk(RBRACKET); pch++; break;
			case ';': addTk(SEMICOLON); pch++; break;
			case '+': addTk(ADD); pch++; break;
			case '-': addTk(SUB); pch++; break;
			case '*': addTk(MUL); pch++; break;
			case '/':
				if(pch[1] == '/'){
					pch += 2;
					while(*pch && *pch != '\n') pch++; //Ignora restul liniei
				} else if (pch[1] == '*'){
					pch+=2;
					while(*pch && !(*pch =='*' && pch[1] == '/')) pch++; //Ignora tot textul
					pch+=2;
				} else {
					addTk(DIV);
					pch++;
				}
				break;
				case '.': addTk(DOT); pch++; break;
				case '&':
						if(pch[1]=='&'){
							addTk(AND);
							pch+=2;
						} else {
							err("Lipseste al doilea: %c", *pch);
						}
						break;
				case '|':
						if(pch[1]=='|'){
							addTk(OR);
							pch+=2;
						} else {
							err("Lipseste al doilea: %c", *pch);
						}
						break;
				case '!':
						if(pch[1]=='='){
							addTk(NOTEQ);
							pch+=2;
						} else {
							addTk(NOT);
							pch++;
						}
						break;
				case '<':
						if(pch[1]=='='){
							addTk(LESSEQ);
							pch+=2;
						} else {
							addTk(LESS);
							pch++;
						}
						break;
				case '>':
						if(pch[1]=='='){
							addTk(GREATEREQ);
							pch+=2;
						} else {
							addTk(GREATER);
							pch++;
						}
						break;
				case '\'':
						if(pch[2] == '\''){
							addTk(CHAR)->c = pch[1];
							pch+=3;
						} else {
							err("lipsa caracter: %c", *pch);
						}
						break;
				case '"':
						for(start = ++pch; *pch && *pch != '"' && *pch != '\0'; ++pch){}
							if(*pch == '"'){
								addTk(STRING)->text = extract(start, pch);
								++pch; //Trecem peste a doua ghilimea dubla
							} else {
								err("unclosed string literal"); //testat si \0
							}
						
						break;
			default:
				if(isalpha(*pch)||*pch=='_'){
					for(start=pch++;isalnum(*pch)||*pch=='_';pch++){}
					char *text=extract(start,pch);
					if(strcmp(text, "int")==0)addTk(TYPE_INT);
					else if(strcmp(text,"char")==0)addTk(TYPE_CHAR);
					else if(strcmp(text,"double")==0)addTk(TYPE_DOUBLE);
					else if(strcmp(text,"else")==0)addTk(ELSE);
					else if(strcmp(text, "if")==0)addTk(IF);
					else if(strcmp(text, "return")==0)addTk(RETURN);
					else if(strcmp(text, "struct")==0)addTk(STRUCT);
					else if(strcmp(text, "void")==0)addTk(VOID);
					else if(strcmp(text, "while")==0)addTk(WHILE);
					else if(strcmp(text, "else")==0)addTk(ELSE);
					else{
						tk=addTk(ID);
						tk->text=text;
						}
					} else if (isdigit(*pch) || *pch =='-'){
						int punct = 0;
						int exp = 0;
						for(start=pch; isdigit(*pch) || *pch=='.' || *pch=='e' || *pch=='E' || *pch=='+' || *pch=='-'; pch++){
							if(*pch=='.'){
								if(!isdigit(*(pch+1)))
								{
									err("numar invalid");
								}
								if(punct || exp) {
									err("numar invalid");
								}
								punct=1;
							} else if(*pch == 'e' || *pch == 'E'){
								if(!isdigit(*(pch+1))){
									err("numar invalid");
								}
								if(exp){
									err("numar invalid");
								}
								exp=1;
							} else if(*pch == '-' || *pch == '+'){
								if(!isdigit(*(pch+1)))
								{
									err("numar invalid");
								}
							}
						}
						char *text = extract(start, pch);
						if(punct || exp){
							double val = strtod(text, &endptr);
							if(endptr != text){
								addTk(DOUBLE)->d=val;
							} else {
								err("numar invalid");
							}
						} else {
							long val = strtol(text, &endptr, 10);
							if(endptr!=text){
								addTk(INT)->i = (int)val;
							} else {
								err("numar invalid");
							}
						}
					} else err("invalid char1: %c (%d)", *pch, *pch);
				
			}
		}
	}

void showTokens(const Token *tokens){
	for(const Token *tk=tokens;tk;tk=tk->next){
		printf("%d\t",tk->line);
		switch(tk->code){
			case ID:
					printf("ID:%s\n", tk->text);
					break;
			case TYPE_CHAR:
					printf("TYPE_CHAR\n");
					break;
			case TYPE_DOUBLE:
					printf("TYPE_DOUBLE\n");
					break;
			case ELSE:
					printf("ELSE\n");
					break;
			case IF:
					printf("IF\n");
					break;
			case TYPE_INT:
					printf("TYPE_INT\n");
					break;
			case RETURN:
					printf("RETURN\n");
					break;
			case STRUCT:
					printf("STRUCT\n");
					break;
			case VOID:
					printf("VOID\n");
					break;
			case WHILE:
					printf("WHILE\n");
					break;
			case INT:
					printf("INT:%d\n", tk->i);
					break;
			case DOUBLE:
					printf("DOUBLE:%f\n", tk->d);
					break;
			case CHAR:
					printf("CHAR:%c\n", tk->c);
					break;
			case STRING:
					printf("STRING:%s\n", tk->text);
					break;
			case COMMA:
					printf("COMMA\n");
					break;
			case SEMICOLON:
					printf("SEMICOLON\n");
					break;
			case LPAR:
					printf("LPAR\n");
					break;
			case RPAR:
					printf("RPAR\n");
					break;
			case LBRACKET:
					printf("LBRACKET\n");
					break;
			case RBRACKET:
					printf("RBRACKET\n");
					break;
			case LACC:
					printf("LACC\n");
					break;
			case RACC:
					printf("RACC\n");
					break;
			case END:
					printf("END\n");
					break;
			case ADD:
					printf("ADD\n");
					break;
			case SUB:
					printf("SUB\n");
					break;
			case MUL:
					printf("MUL\n");
					break;
			case DIV:
					printf("DIV\n");
					break;
			case DOT:
					printf("DOT\n");
					break;
			case AND:
					printf("AND\n");
					break;
			case OR:
					printf("OR\n");
					break;
			case NOT:
					printf("NOT\n");
					break;
			case ASSIGN:
					printf("ASSIGN\n");
					break;
			case EQUAL:
					printf("EQUAL\n");
					break;
			case NOTEQ:
					printf("NOTEQ\n");
					break;
			case LESS:
					printf("LESS\n");
					break;
			case LESSEQ:
					printf("LESSEQ\n");
					break;
			case GREATER:
					printf("GREATER\n");
					break;		
			case GREATEREQ:
					printf("GREATEREQ\n");
					break;		
		}
		}
	}
