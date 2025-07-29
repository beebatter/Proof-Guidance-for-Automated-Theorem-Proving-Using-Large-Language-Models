#ifndef TPTP_CPP
#define TPTP_CPP

#include "stdlib.h"
#include "tptp.h"
#include "tree.h"
#include "list.h"




Tptp::Tptp(void) {
    debug=false;
    fileOK=false;
    tradError=false;
    axiomNumber=conjectureNumber=functionNumber=variableNumber=lastPrintedComment=constNumber=0;
}

Tptp::Tptp(const char *fn, bool d) {
    init(fn,d);
}    

void Tptp::init(const char *fn, bool d) {
    debug=d;
    fileOK=false;
    tradError=false;
    
    //carlos sousa 10-02-2023
    //axiomNumber=conjectureNumber=functionNumber=variableNumber=lastPrintedComment=0;
    axiomNumber=conjectureNumber=functionNumber=variableNumber=lastPrintedComment=constNumber=0;
    //carlos sousa 10-02-2023
    
    
    
    outFile.open(fn);
    fileOK=outFile.is_open();
}

Tptp::~Tptp() {
    outFile.close();
}

bool Tptp::getTradError(void) const {
    return tradError;
}

void Tptp::write(const string &s) {
    if(!fileOK) return;
    outFile<<s;
    if(debug) cout<<s;
}

void Tptp::newLine(void)  {
    if(!fileOK) return;
    outFile<<endl;
    if (debug) cout<<endl;
}

string  Tptp::setVarTrad( string  s) {
    if (isupper(s.at(0))) return s;
    if(islower(s.at(0))) {
        char c=toupper(s.at(0));
        s.at(0)=c;
        return s;
    }
    variableName="P9v" + my_to_string(variableNumber);
    variableNumber++;
    return variableName;
}


string  Tptp::setConstTrad(void) {
    constName="c" + my_to_string(constNumber);
    constNumber++;
    return constName;
}


void Tptp::printComments(TList &c, ulong from, ulong until) {
    listNode *aux=c.getHead();

    while (aux && (aux->data.getLine()<=until || until==-1)) { //não vale a pena continuar a correr a lista depois do limite superior
        if(aux->data.getLine()>=from && (aux->data.getLine()<=until || until==-1)  && !aux->data.getPrinted()) {
                write(aux->data.getId());
                aux->data.setPrinted(true);
                newLine();
        }
        aux=aux->next;
    }
    
    lastPrintedComment=until;
}

uint Tptp::getLastPrintedComment(void) const {
    return lastPrintedComment;
}


void Tptp::equation(const TTree &t,const TList &f, const TList &v, const TList &c) {
    equation(t.getRoot(), f,v,c);
}

void Tptp::equation(treeNode * r,const TList &f, const TList & v, const TList &c) {
 
 treeNode *aux;
 if (r==NULL) return;

 if((r->data.getType()==ttEqual) || r->data.getType()==ttNotEqual || (r->data.getOperatorType()==otConnective && ( r->data.getNotation()==ntInfix ||r->data.getNotation()==ntInfix_right|| r->data.getNotation()==ntInfix_left )) )   { //é o igual, ou é uma connectiva
                if(r->data.getType()==ttImplication || r->data.getType()==ttEquivalence) {
                        write("(");
                        equation(r->left,f,v,c);
                        write(")");
                        write(tradConnectives(r->data));
                        write("(");
                        equation(r->right,f,v,c);
                        write(")");
                }
                else {
                        equation(r->left,f,v,c);
                        write(tradConnectives(r->data));
                        equation(r->right,f,v,c);                    
                }
    }
 
 else 
    if( (r->data.getType()==ttFunction || r->data.getNotation()==ntPrefix || r->data.getNotation()==ntPostfix) && (r->data.getOperatorType()!=otConnective) && r->data.getType()!=ttTrue && r->data.getType()!=ttFalse ) { //as funções são sempre prefixas
        if (f.findTradById(r->data.getId())=="") tradError=true;
        write(f.findTradById(r->data.getId())+"(");   
        aux=r->parameters->nextParameter;
        while(aux) {
                    equation(aux,f,v,c);
                    if (aux->nextParameter) write(",");
                    aux=aux->nextParameter;
        }
        write(")");
    }

 else if(r->data.getNotation()==ntInfix) {
            if (f.findTradById(r->data.getId())=="") tradError=true;
            write(f.findTradById(r->data.getId())+"(");
            equation(r->left,f,v,c);
            if (r->right!=NULL) {
                write(",");
                equation(r->right,f,v,c);
            }
            write(")"); 
    }
    
   else 
    if(r->data.getType()==ttNegation && r->data.getOperatorType()==otConnective && r->data.getNotation()==ntPrefix) { //connective negação 
        if (f.findTradById(r->data.getId())=="") tradError=true;
        write(f.findTradById(r->data.getId())+"(");   
        equation(r->left,f,v,c);
        write(")");
    }  

 else 
    if(r->data.getType()==ttVariable) {
        if(v.findTradById(r->data.getId())=="") tradError=true;
        write(v.findTradById(r->data.getId())); //variaveis
    }
    else   
        if(r->data.getType()==ttConstant || r->data.getType()==ttInteger || r->data.getType()==ttTrue || r->data.getType()==ttFalse) {//constantes---vamos ter de traduzir as constantes para usar em tptp (com o eprover) c1, c2, c3, c4, etc
            if(c.findTradById(r->data.getId())=="") tradError=true;
            write(c.findTradById(r->data.getId())); //variaveis
        }
    else write(r->data.getId()); 
}



string Tptp::incAxiomName(void) {
    axiomName="axiom_name_" + my_to_string(axiomNumber);
    axiomNumber++;
    return axiomName;
}

string Tptp::incConjectureName(void) {
    conjectureName="conjecture_name_" + my_to_string(conjectureNumber);
    conjectureNumber++;
    return conjectureName;
}

string Tptp::setFunctionTrad(const string s) {
 if( isalpha(s.at(0)) ) return s;
 functionName="p9f" + my_to_string(functionNumber);
 functionNumber++;
 return functionName;
}

string Tptp::tradConnectives(TP9Token t) {
   if(t.getType()==ttImplication) return "=>";
   if(t.getType()==ttEqual) return "=";
   if(t.getType()==ttNotEqual) return "!=";
   if(t.getType()==ttEquivalence) return "<=>";
   if(t.getType()==ttNegation) return "~";
   if(t.getType()==ttBackImplication) return "<=";
   return t.getId();
}

//agora vai-se receber uma lista de quantificadores por fórmula
//e não a lista geral
void Tptp::writeQuantifiers(const TList &quantifiers) {
    //as variáveis estão quantificadas correctamente...o sintático é que tem de fazer isso ou seja a quantificação das variáveis
    string universal="![", existential="?[";
    uint i;
    listNode *aux=quantifiers.getHead();
    while (aux) {
                    if(aux->data.getVType()==vtUniversal) universal = universal + setVarTrad(aux->data.getId()) + ",";
                    else existential = existential + setVarTrad(aux->data.getId()) + ",";
                    aux=aux->next;
    }
    i=universal.length();
    if(i>2) {
                universal.at(i-1)=']';
                write(universal+":");
    }
    
    i=existential.length();
    if(i>2) {
                existential.at(i-1)=']';
                write(existential+":");
    }
}


void Tptp::axioms(const TTree &formula, const TList &functions, const TList &variables, const TList &constants, const TList & quantifiers) {
  write("fof("); 
  write(incAxiomName());
  write(",");
  write("axiom,");
  writeQuantifiers(quantifiers);
  write("(");
  equation(formula, functions, variables, constants);
  write(")).");
  newLine();
}

void Tptp::conjectures(const TTree &formula, const TList &functions, const TList &variables, const TList &constants, const TList & quantifiers) {
  write("fof(");
  write(incAxiomName());
  write(",");
  write("conjecture,");
  writeQuantifiers(quantifiers);
  write("(");
  equation(formula, functions, variables,constants);
  write(")).");     
  newLine();
}



void Tptp::tradFunctions(TList &functions) {
  listNode *aux=  functions.getHead();
  write("%Function Translation");
  newLine();    



  while (aux) {
        aux->data.setTrad(setFunctionTrad(aux->data.getId()));
        write("%Original:" + aux->data.getId() + " Translation:" + aux->data.getTrad());
        newLine();
        aux=aux->next;
    }
  write("%End of Function Translation");
  newLine();
  functionNumber=0;  
}


void Tptp::tradVariables(TList &variables) {
    listNode *aux=variables.getHead();
    write("%Variable Translation");
    newLine();
    while(aux) {
                aux->data.setTrad(setVarTrad(aux->data.getId()));
                write("%Original:" + aux->data.getId() + " Translation:" + aux->data.getTrad());
                newLine();
                aux=aux->next;               
    }
    write("%End of Variable Translation");
    newLine();
    variableNumber=0;  
}

void Tptp::tradConstants(TList &constants) {
    listNode *aux=constants.getHead();
    write("%Constant Translation");
    newLine();
    while(aux) {
                
                if(aux->data.getType()==ttTrue) aux->data.setTrad("$t");
                else if(aux->data.getType()==ttFalse) aux->data.setTrad("$f");
                else aux->data.setTrad(setConstTrad());
                write("%Original:" + aux->data.getId() + " Translation:" + aux->data.getTrad());
                newLine();
                aux=aux->next;               
    }
    write("%End of Constant Translation");
    newLine();
    constNumber=0;  
    
}

#endif
