#include "global.h"
#include "sintatic.h"
#include <iostream>
#include <stdlib.h>
#include <string.h>
#include <string>


using namespace std;






TSyntatic::TSyntatic(const char *inf, const char *ef, const bool d) {
    fACount=0;
    fSCount=0;
    fGCount=0;
    contaPar=0;
    errorFile.open(ef);
    lexer.Init(inf); //initialize lexer with input file
    debug=d;
    mode=NO_MODE;
    assmuptionsLine=0;
    goalsLine=0;
    sosLine=0;
    hintsLine=0;
}


 TSyntatic::~TSyntatic(){
      errorFile.close();
}


void TSyntatic::setErrorFile(const char* namef) {
    errorFile.open(namef);
}



void TSyntatic::errorMessage(const string s, const ulong l, const ulong c) {
    errorFile<<s<<"-Line:"<<l<<"-Column:"<<c<<endl;
    if(debug) cout<<s<<"-Line:"<<l<<"-Column:"<<c<<endl;
}




_byte TSyntatic::status(void) {
    
    if (errorFile && lexer.fok()) return 0;
     
    if (!lexer.fok()) return IN_FILE_ERROR;
    
    if (!errorFile) return ERROR_FILE_ERROR;
    
    return NO_ERRORS;
    
}


ulong TSyntatic::getAssumptionsLine(void) const {
    return assmuptionsLine;
}

ulong TSyntatic::getGoalsLine(void) const {
    return goalsLine;
}


ulong TSyntatic::getSosLine(void) const {
    return sosLine;
}


ulong TSyntatic::getHintsLine(void) const {
    return hintsLine;
}



TList & TSyntatic::getCommentsList() {
    return comments;
}



//mas o token já está ferrado
void TSyntatic::getComments(void ){
     comments+=token; //insere na lista de comentários
     token=lexer.nextToken();
}

bool TSyntatic::assignConstant(const TP9Token &t) {
    TP9TokenType aux=t.getType();
    return (aux==ttMaxSeconds || aux==ttStartSize || aux==ttEndSize || aux==ttIncrement || aux==ttDomainSize || aux==ttIterate || aux==ttMaxModels || aux==ttMaxSecondsPer || aux==ttMaxMegs || aux==ttFalse_Part || aux==ttFalse_Part ||aux==ttTrue_Part || aux==ttOrder || aux==ttEq_defs || aux==ttAge_part || aux==ttWeight_part || aux==ttLiteral_selection || aux==ttBacksub_check || aux==ttMaxWeight || aux==ttSos_limit || aux==ttFold_denial_max ||
    aux==ttNewConstants  || aux==ttSk_constant_weight || aux==ttProp_atom_weight || aux==ttSkolem_penalty || aux==ttNest_penalty || aux==ttStats || aux==ttEval_limit
    );
}

bool TSyntatic::getAssign(string &e, long &l, long &c) {
    bool iter=false;
    
    
    token=lexer.nextToken();
    if(token.getType()==ttOpenPar) {
        token=lexer.nextToken();
       
         if(assignConstant(token) ) {
                     
                     if(token.getType()==ttIterate) iter=true;
    
                     token=lexer.nextToken();
                     if (token.getType()==ttComma) {
                         token=lexer.nextToken();
                         if(token.getId()=="-") token=lexer.nextToken(); //negative integers
                         
                         if ( (token.getType()==ttInteger && !iter) || ( token.getType()==ttQuotedSymbol  && iter) || token.getType()==ttFunction || token.getType()==ttVariable) {
                                        token=lexer.nextToken();//continuar depois da vŕgula, espera-se de seguida um close par e um ponto
                                        
                                        if (token.getType()==ttClosePar) {
                                            token=lexer.nextToken();
                                            if(token.getType()==ttPoint) {
                                                token=lexer.nextToken();
                                                
                                            }
                                            else {
                                                    e="Point parentheses expected";
                                                    l=token.getLine();
                                                    c=token.getColumn();
                                                    return true;
                                                
                                            }
                                        }
                                        else {
                                                e="Close parentheses expected";
                                                l=token.getLine();
                                                c=token.getColumn();
                                                return true;
                                        }
                        }

                        else {
                                if (iter) e="String expected";
                                else e="Integer expected";  
                                
                                l=token.getLine();
                                c=token.getColumn();
                                return true; 
                        }
                         
                     }else {
                            e="Comma expected";            
                            l=token.getLine();
                            c=token.getColumn();
                            return true; 
                    }
        } else {
                e="Assign constant expected";            
                l=token.getLine();
                c=token.getColumn();
                return true; 
        }
        
    } else {
            e="Open parentheses expected";            
            l=token.getLine();
            c=token.getColumn();
            return true; 
    }
    return false;

    
}

bool TSyntatic::getSet(string &e, long &l, long &c,bool sc) {
    TP9Token aux;
    
    token=lexer.nextToken();
    if (token.getType()==ttOpenPar) {
        token=lexer.nextToken();
       
        //if(token.getType()==?????) { //que tipos de token é que set e o clear aceitam??????
         if(true) {
            //no entanto há que guardar o token que vem
            aux=token; 
            token=lexer.nextToken();
            if(token.getType()==ttClosePar) {
                token=lexer.nextToken();
                if(token.getType()==ttPoint) {
                        token=lexer.nextToken();
                    
                }else {
                    e="Point parentheses expected";            
                    l=token.getLine();
                    c=token.getColumn();
                    return true;
                    
                }
                
            }else {
                    e="Close parentheses expected";            
                    l=token.getLine();
                    c=token.getColumn();
                    return true;
                
            }
        }
        else {
                e="variable expected expected";            
                l=token.getLine();
                c=token.getColumn();
                return true;
        }
        
    } else {
            e="Open parentheses expected";            
            l=token.getLine();
            c=token.getColumn();
            return true;
    }
    
    if(aux.getType()==ttPrologStyleVariables) {
        if (sc) lexer.setProlog();
        else lexer.clearProlog();
    }
    
    return false; //no error
}


bool TSyntatic::getClear(string &e, long &l, long &c) {
    return getSet(e,l,c,false); //para já é igual, só muda o token inicial, mas há que passar false
}


bool TSyntatic::isRedeclarable(const TP9Token &token) const{
   
    return      token.getType()==ttConjunctionName ||
                token.getType()==ttDisjunctionName ||
                token.getType()==ttTrueName ||
                token.getType()==ttFalseName ||
                token.getType()==ttNegationName ||
                token.getType()==ttImplicationName ||
                token.getType()==ttBackwardImplicationName ||
                token.getType()==ttEquivalence||
                token.getType()==ttUniversalQuantificationName ||
                token.getType()==ttExistentialQuantificationName ||
                token.getType()==ttEqualityName ||
                token.getType()==ttNegatedEqualityName ||
                token.getType()==ttAttributeName;
}


bool TSyntatic::isPrecedence(const TP9Token &t) const{
    switch (t.getType()) {
        case ttInfixName:
        case ttInfixLeftName:
        case ttInfixRightName:
        case ttPrefixName:
        case ttOrdinaryName:
        case ttPostfixName: return true;
        default:;
    }
    return false;
}





bool TSyntatic::getRedeclare(string &e, long &l, long &c) {
  TP9TokenType ty;
  string s;
    token=lexer.nextToken();
    
    if (token.getType()==ttOpenPar) {
        token=lexer.nextToken();
        
        switch(token.getType()) {
            
            case ttConjunctionName:                 ty=ttConjunction; break;
            case ttDisjunctionName:                 ty=ttDisjunction; break;
            case ttTrueName:                        ty=ttTrue; break;
            case ttFalseName:                       ty=ttFalse; break;
            case ttNegationName:                    ty=ttNegation ; break;
            case ttImplicationName:                 ty=ttImplication; break;
            case ttBackwardImplicationName:         ty=ttBackImplication; break;
            case ttEquivalenceName:                 ty=ttEquivalence; break;
            case ttUniversalQuantificationName:     ty=ttAll; break;
            case ttExistentialQuantificationName:   ty=ttExists; break;
            case ttEqualityName:                    ty=ttEqual; break;
            case ttNegatedEqualityName:             ty=ttNotEqual; break;
            case ttAttributeName:                   ty=ttAtribute; break;
            default:;
            
        }
        
        if (isRedeclarable(token)) {
            
            token=lexer.nextToken();
            if(token.getType()==ttComma) {
                token=lexer.nextToken();
                if (token.getType()==ttQuotedSymbol) {
                    s=token.getId();
                    token=lexer.nextToken();
                    if(token.getType()==ttClosePar) {
                        token=lexer.nextToken();
                        
                        if(token.getType()==ttPoint) {
                            //tudo ok, é tempo de redeclarar o symbolo
                            lexer.redeclare(ty,s);
                            token=lexer.nextToken();
                        }
                        else {
                                e="Point expected";            
                                l=token.getLine();
                                c=token.getColumn();
                                return true; 
                        }
                    }else {
                            e="Close parentheses expected";            
                            l=token.getLine();
                            c=token.getColumn();
                            return true; 
                    }
                }else {
                        e="Symbol expected";            
                        l=token.getLine();
                        c=token.getColumn();
                        return true;  
                }
            }else {
                    e="Comma expected";            
                    l=token.getLine();
                    c=token.getColumn();
                    return true;  
            }
        }else {
                e="Redeclarable built-in symbol expected";            
                l=token.getLine();
                c=token.getColumn();
                return true;  
        }
    } else {
            e="Open parentheses expected";            
            l=token.getLine();
            c=token.getColumn();
            return true;  
    }
    return false;
}


bool TSyntatic::getIf(string &e, long &l, long &c) {

    token=lexer.nextToken();
    
    if(token.getType()==ttOpenPar) {
            token=lexer.nextToken();
            token=lexer.nextToken();
            
            if(token.getType()==ttClosePar) {
                token=lexer.nextToken();
                
                if(token.getType()==ttPoint) {
                        token=lexer.nextToken();
                }
                else {
                        e="Point parentheses expected";
                        l=token.getLine();
                        c=token.getColumn();
                        return true;
                    
                }
                
                
                
               
            } else {
                    e="Close parentheses expected";
                    l=token.getLine();
                    c=token.getColumn();
                    return true;
                
            }
            
            
        
        
    } else {
           e="Open parentheses expected";
           l=token.getLine();
           c=token.getColumn();
           return true;
  }
  return false;  
}

bool TSyntatic::getEndIf(string &e, long &l, long &c) {

     token=lexer.nextToken();
     if(token.getType()==ttPoint) {
                token=lexer.nextToken();
     }
     else {
            e="Point parentheses expected";
            l=token.getLine();
            c=token.getColumn();
            return true;
                    
    }
    return false;
    
}



bool TSyntatic::getFunctionOrder(string &e, long &l, long &c) {
    token=lexer.nextToken();
    if(token.getType()==ttOpenPar) {
    
      do{  
            token=lexer.nextToken();
            if(token.getType()==ttPoint ||token.getType()==ttComments || token.getType()==ttEOF ) {
                e="Close parentheses expected";
                l=token.getLine();
                c=token.getColumn();
                return true;
                
            }
            
      } while(token.getType()!=ttClosePar);
        
      token=lexer.nextToken();
      if(token.getType()!=ttPoint) {
           e="Point expected";
           l=token.getLine();
           c=token.getColumn();
            
          return true;
      }
      token=lexer.nextToken();  
      return false;  
    }
    else {
           e="Open parentheses expected";
           l=token.getLine();
           c=token.getColumn();
           return true;
        
    }
    
    
    
}


bool TSyntatic::getOp(string &e, long &l, long &c) {
   
    TP9Token aux;
    TP9NotationType nt;
    uint val;
    aux=token;
    string s;
    token=lexer.nextToken();
    if(token.getType()==ttOpenPar) {
            token=lexer.nextToken();
            if(token.getType()==ttInteger) {
                val=my_stoi(token.getId());
                token=lexer.nextToken();
                if(token.getType()==ttComma) {
                    token=lexer.nextToken();
                    if(isPrecedence(token)) {
                            nt=token.getNotation(token.getId());
                            token=lexer.nextToken();
                            if(token.getType()==ttComma) {
                                 token=lexer.nextToken();   //aqui vem um quoted symbol, mas l'dentro tem de vir um builtin
                                 s=token.getId();
                                 removeQuotes(s);
                                 if(lexer.isBuiltIn(s)) {
                                    //change properties in builin list
                                    lexer.changeBuiltInProperties(s, nt,val); 
                                     
                                 }else {
                                        e="BuiltIn Symbol expected";
                                        l=token.getLine();
                                        c=token.getColumn();
                                        return true;
                                 }
                                 token=lexer.nextToken();
                                 if(token.getType()==ttClosePar) {
                                    token=lexer.nextToken();
                                    if(token.getType()==ttPoint) {
                                    token=lexer.nextToken();
                                    //tudo bem, vamos redefinir o symbolo
                                    lexer.redefine(aux.getType(), nt,val);
                                    
                                    }else {
                                            e="Point expected";
                                            l=token.getLine();
                                            c=token.getColumn();
                                            return true;
                                    }
                                }else {
                                        e="Close parentheses expected";
                                        l=token.getLine();
                                        c=token.getColumn();
                                        return true;
                                }   
                            }else {
                                    e="Comma expected";
                                    l=token.getLine();
                                    c=token.getColumn();
                                    return true;
                                }
                    }else {
                            e="Precedence expected";
                            l=token.getLine();
                            c=token.getColumn();
                            return true;
                        }
                    
                }else {
                        e="Comma expected";
                        l=token.getLine();
                        c=token.getColumn();
                        return true;
                    }
            }else {
                    e="Integer expected";
                    l=token.getLine();
                    c=token.getColumn();
                    return true;
                }
    }else {
            e="Open parentheses expected";            
            l=token.getLine();
            c=token.getColumn();
            return true;       
        }
    return false;
}

bool TSyntatic::getIgnoreTokens(string &e, long &l, long &c) {
    bool error=false;

    while ( (token.getType()==ttComments || token.getType()==ttAssign || token.getType()==ttSet || token.getType()==ttClear || 
        token.getType()==ttRedeclare || token.getType()==ttOp || token.getType()==ttIf  || token.getType()==ttEndIf || token.getType()==ttFunction_order) && (!error) ) {
        
        switch(token.getType()) {
            case ttComments : getComments(); break;
            case ttAssign : error=getAssign(e,l,c); break;
            case ttSet: error=getSet(e,l,c,true); break; //set
            case ttClear: error=getClear(e,l,c); break;
            case ttRedeclare: error=getRedeclare(e,l,c);break;
            case ttOp: error=getOp(e,l,c); break;
            case ttIf: error=getIf(e,l,c); break; 
            case ttEndIf: error=getEndIf(e,l,c); break;
            case ttFunction_order: error=getFunctionOrder(e,l,c); break;
            default:;
        }
    }
    return !error; //para retornar true se ok
}


uint TSyntatic::getFormulasDeclaration(string &e, long &l, long &c) { //retorna o modo - NO_MODE, SOS_MODE, GOALS_MODE, ASSUMPTIONS_MODE
    uint res=NO_MODE; 
    if (token.getType()==ttFormulas) {
         token=lexer.nextToken();
         if (token.getType()==ttOpenPar) {
                    token=lexer.nextToken();
                    if (token.getType()==ttAssumptions || token.getType()==ttSos || token.getType()==ttGoals || token.getType()==ttHints) {
                        if(token.getType()==ttAssumptions)   {assmuptionsLine=token.getLine() ;res=ASSUMPTIONS_MODE;}
                        if(token.getType()==ttGoals) {goalsLine=token.getLine() ; res=GOALS_MODE;}
                        if(token.getType()==ttSos)  {sosLine=token.getLine(); res=SOS_MODE;}
                        if(token.getType()==ttHints) {hintsLine=token.getLine(); res=HINTS_MODE;}
                        
                        
                        
                        token=lexer.nextToken();
                        if(token.getType()==ttClosePar) {
                            token=lexer.nextToken();
                            if(token.getType()!=ttPoint) {
                                e="point expected";
                                l=token.getLine();
                                c=token.getColumn();
                                res=NO_MODE;
                                
                            } else { 
                                        token=lexer.nextToken();//forra a bomba
                                        
                            }
                            
                        } else {
                                e="Close parentheses expected";
                                l=token.getLine();
                                c=token.getColumn();
                                res=NO_MODE;
                        }
                        
                    } else {
                            e="Assumptions, sos, hints or goals expected parentheses expected";
                            l=token.getLine();
                            c=token.getColumn();
                            res=NO_MODE;
                    }
                    
                } else {
                        e="Open parentheses expected";
                        l=token.getLine();
                        c=token.getColumn();
                        res=NO_MODE;
                }    
        
    } else {
                e="Formulas expected";
                l=token.getLine();
                c=token.getColumn();
                res=NO_MODE;
    }
     
    return res;     
}



bool TSyntatic::getEndOfListDeclaration(string &e, long &l, long &c) {
    
    if(token.getType()==ttEndOfList) {
        token=lexer.nextToken();
        if (token.getType()!=ttPoint) {
            e="Point expected";
            l=token.getLine();
            c=token.getColumn();
            return false;
            
        } else {
            token=lexer.nextToken();   
            return true;
        }
    } else  {
              e="end_of_list expected";
              l=token.getLine();
              c=token.getColumn();
              return false;
        }
}



bool TSyntatic::testAndSetOperator(TP9Token &t) {
    listNode *aux;
    aux=functions.getById(t.getId());
    if(aux) return 
        (aux->data.getOperatorType()==otOperator);
    t.setOperatorType(otOperator);
    lexer.changeBuiltInOperatorType(t.getId(), otOperator); //altera no builtin também
    lexer.changeBuiltInArity(t.getId(), t.getArity()); //e também a ariedade    
    functions.insertNew(t);
    return true;
}




bool TSyntatic::testAndSetPredicate(TP9Token &t) {
    listNode *aux;
    aux=functions.getById(t.getId());
    if(aux) return (aux->data.getOperatorType()==otPredicate);
    t.setOperatorType(otPredicate);
    lexer.changeBuiltInOperatorType(t.getId(), otPredicate); //altera no builtin também
    functions.insertNew(t);
    return true;
}

uint TSyntatic::checkArity(const TList &l, const TP9Token & t) {
    listNode *aux;
    aux=l.getById(t.getId());
    if (!aux) return -1;
    else return aux->data.getArity();
}


treeNode * TSyntatic::functionPredicate(string &e, long &l, long &c, bool &error) {
treeNode *aux, *aux1, *aux2,*aux3;

uint farity=0;    

    //aqui vamos tratar de funções que são sempre prefixas....
    aux=TTree::newNode(token); //the root node is the function symbol
    aux1=TTree::newNode(token);//the node to put parameters
    aux1->data.setId("Parameters of:" + aux->data.getId() + "->");
    aux->parameters=aux1;
    token=lexer.nextToken();
    if(token.getType()!=ttOpenPar) { //go for parameters
                e="Open parentheses expected";
                l=token.getLine();
                c=token.getColumn();
                error=true;
                return NULL;
    }
   
   token=lexer.nextToken(); 
   
   do{
        if(token.getType()==ttComma) token=lexer.nextToken();
        farity++;
        aux2=term(e,l,c,error); //vai buscar o termo
        aux3=aux->parameters;
        while(aux3->nextParameter) //Acha a posição na lista de parâmetros
            aux3=aux3->nextParameter;
        aux3->nextParameter=aux2;
    }while(token.getType()==ttComma);
    
    aux->data.setArity(farity);
    if(token.getType()!=ttClosePar) {
        e="Close parentheses expected";
        l=token.getLine();
        c=token.getColumn();
        error=true;
        return NULL;
    }
   
   
  
   token=lexer.nextToken();
   return aux;
    
}

treeNode * TSyntatic::functionTerm(TP9Token &t, string & e, long & l, long & c, bool & error) {
 treeNode *aux, *aux1, *aux2,*aux3;
 TP9Token taux;
 uint farity=0;    

    //aqui vamos tratar de funções que são sempre prefixas....
    aux=TTree::newNode(token); //the root node is the function symbol
    aux1=TTree::newNode(token);//the node to put parameters
    aux1->data.setId("Parameters of:" + aux->data.getId() + "->");
    aux->parameters=aux1;
    token=lexer.nextToken();
    
    if(token.getType()!=ttOpenPar) { //go for parameters
                e="Open parentheses expected";
                l=token.getLine();
                c=token.getColumn();
                error=true;
                return NULL;
    }
   
   token=lexer.nextToken(); 
   
   do{
        if(token.getType()==ttComma) token=lexer.nextToken();
        farity++;
        aux2=term(e,l,c,error); //vai buscar o termo
        aux3=aux->parameters;
        while(aux3->nextParameter) //Acha a posição na lista de parâmetros
            aux3=aux3->nextParameter;
        aux3->nextParameter=aux2;
    }while(token.getType()==ttComma);
    
    aux->data.setArity(farity);
    if(token.getType()!=ttClosePar) {
        e="Close parentheses expected";
        l=token.getLine();
        c=token.getColumn();
        error=true;
        return NULL;
    }

    taux=aux->data;
    if (!testAndSetOperator(taux)) {
        e="This symbol: " + taux.getId() + " cannot be used as relation and function";
        l=aux->data.getLine();
        c=aux->data.getColumn();
        error=true;
        return NULL;
    }
        
    if(aux->data.getArity() != checkArity(functions, aux->data)) {
        e="Arity error";
        l=aux->data.getLine();
        c=aux->data.getColumn();
        error=true;
        return NULL;
    }
  
   token=lexer.nextToken();
   return aux;
}



treeNode * TSyntatic::prefixOperatorPredicate(string & e, long &l, long &c, bool &error) {
    treeNode *aux, *aux1, *aux2,*aux3;
    TP9Token taux;
    uint farity=0;

        aux=TTree::newNode(token); 
        aux1=TTree::newNode(token);
        aux1->data.setId("Parameters of:" + aux->data.getId() + "->");
        aux->parameters=aux1;
        token=lexer.nextToken(); //aqui pode ser parentisis ou nãp
        //se não for parentisis, tem de ter ariedade 1
        if(token.getType()!=ttOpenPar) {
            aux2=term(e,l,c,error);
            farity++;
            aux1->nextParameter=aux2;
            aux->data.setArity(farity);
        }
        else {
                token=lexer.nextToken();
                do{
                    if(token.getType()==ttComma) token=lexer.nextToken();
                    aux2=term(e,l,c,error); //vai buscar o termo
                    farity++;
                    aux3=aux->parameters;
                    while(aux3->nextParameter) //Acha a posição na lista de parâmetros
                        aux3=aux3->nextParameter;
                    aux3->nextParameter=aux2;
                } while(token.getType()==ttComma);

                aux->data.setArity(farity);
                if(token.getType()!=ttClosePar) {
                                                  e="Close parentheses expected";
                                                  l=token.getLine();
                                                  c=token.getColumn();
                                                  error=true;
                                                    return NULL;
                }
        
            token=lexer.nextToken();
        }
        taux=aux->data;
       
        if(!testAndSetPredicate(taux)) { 
            e="This symbol: " + taux.getId() + " cannot be used as relation and function";
            l=token.getLine();
            c=token.getColumn();
            error=true;
            return NULL;
        }
        if(checkArity(functions, aux->data)!=farity) { //depois bate-se a calculada com a que existe
                e="Arity error";
                l=aux->data.getLine();
                c=aux->data.getColumn();
                error=true;
                return NULL;
         } 
    return aux;
    
}


treeNode * TSyntatic::prefixOperatorTerm(string & e, long & l, long & c, bool & error) {
    treeNode *aux, *aux1, *aux2,*aux3;
    TP9Token taux;
    uint farity=0;

        aux=TTree::newNode(token); 
        aux1=TTree::newNode(token);
        aux1->data.setId("Parameters of:" + aux->data.getId() + "->");
        aux->parameters=aux1;
        token=lexer.nextToken(); //aqui pode ser parentisis ou nãp
        //se não for parentisis, tem de ter ariedade 1
        if(token.getType()!=ttOpenPar) {
            aux2=term(e,l,c,error);
            farity++;
            aux1->nextParameter=aux2;
            aux->data.setArity(farity);
        }
        else {
                token=lexer.nextToken();
                do{
                    if(token.getType()==ttComma) token=lexer.nextToken();
                    aux2=term(e,l,c,error); //vai buscar o termo
                    farity++;
                    aux3=aux->parameters;
                    while(aux3->nextParameter) //Acha a posição na lista de parâmetros
                        aux3=aux3->nextParameter;
                    aux3->nextParameter=aux2;
                } while(token.getType()==ttComma);

                aux->data.setArity(farity);
                if(token.getType()!=ttClosePar) {
                                                  e="Close parentheses expected";
                                                  l=token.getLine();
                                                  c=token.getColumn();
                                                  error=true;
                                                    return NULL;
                }
        
            token=lexer.nextToken();
        }
        taux=aux->data;
        if(!testAndSetOperator(taux)) { 
            e="This symbol: " + taux.getId() + " cannot be used as relation and function";
            l=token.getLine();
            c=token.getColumn();
            error=true;
            return NULL;
        }
        if(checkArity(functions, aux->data)!=farity) { //depois bate-se a calculada com a que existe
                e="Arity error";
                l=aux->data.getLine();
                c=aux->data.getColumn();
                error=true;
                return NULL;
         } 
    return aux;
}





treeNode * TSyntatic::postFixOperatorTerm(bool found, treeNode *first, string &e, long &l, long &c, bool &error) {
    treeNode *aux, *aux1,*aux2,*last;
    TP9Token taux;
    uint farity;
    
    if (found) {//o postfixoperator foi já encontrado e a ariedade vai ser 1
        aux=TTree::newNode(token);
        aux1=TTree::newNode(token);
        aux1->data.setId("Parameters of:" + aux->data.getId() + "->");
        aux->parameters=aux1;
        aux1->nextParameter=first;
        token.setArity(1);
        taux=token;
        farity=1;
    }    
    
    //o opstfixoperator ainda não foi encontrado, mas estamos numa posição de o ir encontrar
    if(!found) { //o token trás uma vŕgula neste momento
      farity=1;
       //vamos chamar termo a termo até aparecer o parentisis a fechar
       aux1= TTree::newNode(token);
       aux1->nextParameter=first;
       last=first;
       token=lexer.nextToken();
       
      do{
            if(token.getType()==ttComma) token=lexer.nextToken();
            aux2=term(e,l,c,error); //vai buscar o termo
            last->nextParameter=aux2;
            last=aux2;
            farity++;
        } while(token.getType()==ttComma);
        
       if(token.getType()!=ttClosePar) {
             e="Close parentheses expected";
             l=token.getLine();
             c=token.getColumn();
             error=true;
            return NULL;
           
       }
       contaPar--;
       token=lexer.nextToken();
      
       if (token.getNotation()==ntPostfix && lexer.isTermOperator(token) ) {
            aux=TTree::newNode(token);
            aux->parameters=aux1;
            taux=token;
            taux.setArity(farity);
            aux1->data.setId("Parameters of:" + aux->data.getId() + "->");
       }
      else {
                e="Postfix operator expected";
                l=token.getLine();
                c=token.getColumn();
                error=true;
                return NULL;
        }
    }    
   
     if(!testAndSetOperator(taux)) { 
            e="This symbol: " + taux.getId() + " cannot be used as relation and function";
            l=token.getLine();
            c=token.getColumn();
            error=true;
            return NULL;
     }
     if(checkArity(functions, aux->data)!=farity) { //depois bate-se a calculada com a que existe
                e="Arity error";
                l=aux->data.getLine();
                c=aux->data.getColumn();
                error=true;
                return NULL;
     } 
   
   token=lexer.nextToken();
   return aux; 
}




treeNode * TSyntatic::term(string &e, long &l, long &c, bool &error) {
    treeNode *aux,*aux1,*aux2,*aux3,*aux4;
    aux=NULL;

    
    //prefixos----------------------------------------------------------------------------------------------------------------------------
    if ( (token.getNotation()==ntPrefix && lexer.isTermOperator(token) ) || token.getType()==ttFunction ) {
        if (token.getNotation()==ntPrefix && lexer.isTermOperator(token)) 
            aux= prefixOperatorTerm(e,l,c,error);
        
        //funções operadoras que para já também são prefixas
        if (token.getType()==ttFunction) {

            TP9Token taux;
            aux= functionTerm(taux,e,l,c,error);     //se vier um parentisis
            
            
            
        }    
        
    
        while (token.getNotation()==ntPostfix && lexer.isTermOperator(token)) //um unário postfixo depois de uma variável
                    aux=postFixOperatorTerm(true,aux,e,l,c,error);    //sabe-se que aqui a ariedade tem de ser 1 e que o postfixToken já foi encontrado
        

        if(token.getNotation()==ntInfix && lexer.isTermOperator(token) ) { //arranjar a coisa para o infixo e fica feito
                    aux2=TTree::newNode(token);
                    aux2->left=aux;
                    token=lexer.nextToken();
                    aux2->right=term(e,l,c,error);
                    aux=aux2;
        }
        
        return aux;
    } //caso dos prefixos ------------------------------------------------------------------------------------------------------------------
    
   
    
    //parentisis-----------------------------------------------------------------------------------------------------------------------------
    if(token.getType()==ttOpenPar) {
        contaPar++;
        token=lexer.nextToken();
        aux=term(e,l,c,error); //este termo não pode ser root no caso dos infixos, portanto temos de lhe por precedencia mínima
        
        if (token.getType()==ttClosePar) {
            contaPar--; 
            token=lexer.nextToken(); 
            //aqui ainda pode vir um postfixoperator
            while (token.getNotation()==ntPostfix && lexer.isTermOperator(token) && !error) { //um unário postfixo depois de uma variável
                aux=postFixOperatorTerm(true,aux,e,l,c,error);    //sabe-se que aqui a ariedade tem de ser 1 e que o postfixToken já foi encontrado
            } 

        if(token.getNotation()==ntInfix && lexer.isTermOperator(token) ) { //arranjar a coisa para o infixo e fica feito
                functions.insertNew(token);
                aux2=TTree::newNode(token);
                aux2->left=aux;
                token=lexer.nextToken();
                aux2->right=term(e,l,c,error);
                aux=aux2;
		
            }
            
         return aux; //aqui temos a root do parentesis, temos de 
        }
       
      
        //carlos sousa 10-02-2023
        //isto cheira-me a parentisis a envolver toda a atomicformula...portanto vamos retornar e depois a atomic formula que resolva
        if(token.getType()!=ttClosePar && contaPar>0 ) { 
        
            return aux;
        }
        //carlos sousa 10-02-2023
      
      
       //ou pode vir uma vírgula
        if (token.getType()==ttComma) {
            aux3= postFixOperatorTerm(false,aux,e,l,c,error);  //só falta aqui.....depois de um postfix operator pode vir outro postfix operator ou um infix operator
            while (token.getNotation()==ntPostfix && lexer.isTermOperator(token) && !error ) 
                    aux3=postFixOperatorTerm(true, aux3,e,l,c,error);
            
            if(token.getNotation()==ntInfix && lexer.isTermOperator(token) ) { //arranjar a coisa para o infixo e fica feito
                    functions.insertNew(token);
                    aux4=TTree::newNode(token);
                    aux4->left=aux3;
                    token=lexer.nextToken();
                    aux4->right=term(e,l,c,error);
                    aux3=aux4;
            }
            return aux3;
            
        }
        else {
                e="Term expected";
                l=token.getLine();
                c=token.getColumn();
                error=true;
                return NULL;
        }
    
    }
    //parentisis---------------------------------------------------------------------------------------------------------------------------------------
    
    
//variáveis ou constantes -----------------------------------------------------------------------------------------------------------------------------
    if(token.getType()==ttVariable || token.getType()==ttConstant || token.getType()==ttTrue || token.getType()==ttFalse || (token.getType()==ttInteger && !lexer.getPrologStyle() ))  { //pode ser uma variável ou uma constante .... não esquecer que uma constante podem ser um inteiro em !prologStyle
        token.setArity(0); //as variáveis e as constantes têm ariedade 0
	token.setPrecedence(0); //as variáveis e as constantes têm precedência 0 pois estão o mais perto do ground possível
        if(token.getType()==ttVariable) {

                //se a variável ainda não apareceu num quantificador desta fórmula, então é universal por defeito
                //se a variável já apareceu num quantificador desta fórmula, então assume o vtype desse quantificador
                listNode *aux;
                switch (mode) {
                    case ASSUMPTIONS_MODE:  aux=AssumptionsQuantifiers[fACount].getById(token.getId());
                                            if(!aux) token.setVType(vtUniversal);                            
                                            else token.setVType(aux->data.getVType());
                                            break;
                                            
                    case SOS_MODE:          aux=SosQuantifiers[fSCount].getById(token.getId());
                                            if(!aux) token.setVType(vtUniversal);                            
                                            else token.setVType(aux->data.getVType());
                                            break;
            
                    case GOALS_MODE:        aux=GoalsQuantifiers[fGCount].getById(token.getId());
                                            if(!aux) token.setVType(vtUniversal);                            
                                            else token.setVType(aux->data.getVType());
                                            break;
                }
            
                variables.insertNew(token);
                
                switch (mode) {
                    case ASSUMPTIONS_MODE:  AssumptionsQuantifiers[fACount].insertNew(token);
                                            break;
                    case SOS_MODE:          SosQuantifiers[fSCount].insertNew(token);
                                            break;
                    case GOALS_MODE:        
                                            GoalsQuantifiers[fGCount].insertNew(token);
                                            break;
                }
        }
        else constants.insertNew(token);
     
     //criar um nó para a variável ou constante
     aux=TTree::newNode(token);
     token=lexer.nextToken();   
     //se depois de uma variável vier um operador infixo (e tem de ser um builtin operator)
     //assumir infixos com ariedade 2 e associativos
     //tratar aqui do caso do v
     if(  (token.getNotation()==ntInfix && lexer.isTermOperator(token))  || ( token.getId()=="v" && !variables.getById("v") ) ) {
        //infelizmente temos de fazer isto no sintático
         if(token.getId()=="v") {
           token.setArity(2);
           token.setNotation(ntInfix);
           token.setType(ttV);
           token.setOperatorType(otOperator);
           token.setVType(vtNone);
           token.setPrecedence(500); //como o * ou o +
           lexer.insertNewBuiltIn(token);
        }
       //vamos criar um nó para este operador
       functions.insertNew(token);
       aux1=TTree::newNode(token);
       aux1->left=aux;
       aux=aux1;
       token=lexer.nextToken(); 
       aux->right=term(e,l,c,error);
    } 
     
     else 
         if (token.getNotation()==ntPostfix && lexer.isTermOperator(token)) { //um unário postfixo depois de uma variável
            aux=postFixOperatorTerm(true,aux,e,l,c,error);    //sabe-se que aqui a ariedade tem de ser 1 e que o postfixToken já foi encontrado
            //depois de um postfix só pode vir mais um postfix ou infix
            //se for postfix
            while (token.getNotation()==ntPostfix && lexer.isTermOperator(token) ) 
                    aux=postFixOperatorTerm(true, aux,e,l,c,error);
          
	    
	    if(token.getNotation()==ntInfix && lexer.isTermOperator(token) ) { //aqui vem um infix
                aux2=TTree::newNode(token);
                aux2->left=aux;
                token=lexer.nextToken();
                aux2->right=term(e,l,c,error);
                aux=aux2;
            }
            //se for infix
        }
     return aux;
    }//variáveis ou constantes incluindo postfix operator ------------------------------------------------------------------------------------------------
 
 
 //carlos sousa 10-02-2023
 //aqui vai retrornar null, ou seja, não foi encontrado termo
 e="Term expected";
 l=token.getLine();
 c=token.getColumn();
 error=true;
 //carlos sousa 10-02-2023
 
 return NULL;   
}






treeNode * TSyntatic::atomicFormula(string &e, long &l, long &c, bool & error) {
     treeNode *aux,*aux1,*aux2, *aux3, *aux4;

     //comecemos com o caso de funções predicadas prefixas
        //funções predicados
     
     
    
     
     
     if (token.getType()==ttFunction) {
            aux= functionPredicate(e,l,c,error); //A função aqui vem como predicado
            //se o token seguinte for uma relação
            if(!aux) {
                 e="This symbol: " + token.getId()+ " cannot be used as a function";
                    l=token.getLine();
                    c=token.getColumn();
                    
                    error=true;
                    return NULL;
            }
            
            if(token.getOperatorType()==otPredicate ||  token.getType()==ttEqual) {
                //tem de passar a ser operador
                //então testAndSetOperator
                functions.insertNew(token);
                if (!testAndSetOperator(aux->data)) {
                    e="This symbol: " + aux->data.getId() + " cannot be used as relation and function";
                    l=aux->data.getLine();
                    c=aux->data.getColumn();
                    error=true;
                    return NULL;
                }
                aux1=TTree::newNode(token);
                aux1->left=aux;
                token=lexer.nextToken();
                aux1->right=term(e,l,c,error); //vem aí um termo 
                aux=aux1;
            } 
            else { //senão devirá vir um connectivo ou o fim....logo passamos a ter um predicado
                    if (!testAndSetPredicate(aux->data)) {
                    e="This symbol: " + aux->data.getId() + " cannot be used as relation and as a function";
                    l=aux->data.getLine();
                    c=aux->data.getColumn();
                    error=true;
                    return NULL;
                 }
                
            if(aux->data.getArity() != checkArity(functions, aux->data)) {
                    e="Arity error";
                    l=aux->data.getLine();
                    c=aux->data.getColumn();
                    error=true;
                    return NULL;
               }
                
            }
            return aux;
    } //caso dos prefixos ------------------------------------------------------------------------------------------------------------------
  
    //podemos ter um termo relação termo
    aux1=term(e,l,c,error);
   
    //se o termo for uma variável ou constante (ariedade 0) e o operador não for o igual então não é um atomico formula
    
       
    if(!aux1) { //pode vir a null
        e="Term expected";
        l=token.getLine();
        c=token.getColumn();
        error=true;
        return NULL;
    }
    
    
    
    if(token.getType()==ttClosePar) {
        
        token=lexer.nextToken();
        if (token.getOperatorType()==otOperator && token.getNotation()==ntInfix) {
            functions.insertNew(token);
            aux3=TTree::newNode(token);
            token=lexer.nextToken();
            aux3->left=aux1;
            aux3->right=term(e,l,c,error);
            aux1=aux3;
            contaPar--;
     } 
        
    }
    
  
    
    if(aux1->data.getArity()==0 && token.getType()!=ttEqual && token.getOperatorType()!=otPredicate) {
        e="Variables or constants cannot be atomic formulas";
        l=aux1->data.getLine();
        c=aux1->data.getColumn();
        error=true;
        return NULL;
    }    
    
 
   //no caso de operadores infixos na atomic formula
   if (token.getType()==ttEqual || (token.getOperatorType()==otPredicate && token.getNotation()==ntInfix) )  {
        functions.insertNew(token);
        aux2=TTree::newNode(token);
        token=lexer.nextToken();
        aux2->left=aux1;
        aux2->right=term(e,l,c,error);
        return aux2;
     } 
     return aux1;
}





bool TSyntatic::insertQuantifier(void) {
                listNode *aux;
		
                switch (mode) {
                    case ASSUMPTIONS_MODE:  aux=AssumptionsQuantifiers[fACount].getById(token.getId());
                                            break;
                                            
                    case SOS_MODE:          aux=SosQuantifiers[fSCount].getById(token.getId());
                                            break;
            
                    case GOALS_MODE:        aux=GoalsQuantifiers[fGCount].getById(token.getId());
                                            break;
                }
                
                if(aux && aux->data.getVType()!=token.getVType()) return false;
            
                switch (mode) {
                    case ASSUMPTIONS_MODE:  AssumptionsQuantifiers[fACount].insertNew(token);
                                            break;
                                            
                    case SOS_MODE:          SosQuantifiers[fSCount].insertNew(token);
                                            break;
            
                    case GOALS_MODE:        GoalsQuantifiers[fGCount].insertNew(token);
                                            break;
                }
    
                return true;
    
}

treeNode *TSyntatic::applyPrecedence(treeNode *r) {
treeNode *aux;
    
      if(!r) return NULL;  
      if(r->right->data.getPrecedence()<r->data.getPrecedence()) {
	      aux=r->right->left; //guardar 
	      r->right->left=r;
	      r=r->right;
	      r->left->right=aux;
      }
      return r;
}

treeNode * TSyntatic::getFormula(bool equ, string &e, long &l, long &c, bool &error) {
    treeNode *aux,*aux1,*aux2, *aux3, *aux4=NULL;
   
        //partes comuns a fórmulas com igualdade e fol
        
        //os quantificadores
    
         if(token.getType()==ttAll) {
            token=lexer.nextToken();
            if(token.getType()==ttVariable) {
                token.setVType(vtUniversal);
                if (!insertQuantifier()) {
                    e="A variable cannot be existencial and universal in the same formula";
                    l=token.getLine();
                    c=token.getColumn();
                    error=true;
                    return NULL;
                    
                }
                token=lexer.nextToken();
                return getFormula(equ,e,l,c,error);
            } 
            else {
                e="Variable expected";
                l=token.getLine();
                c=token.getColumn();
                error=true;
                return NULL;
            }
        }

        if(token.getType()==ttExists) {
            token=lexer.nextToken();
            if(token.getType()==ttVariable || token.getType()==ttConstant) {
                token.setVType(vtExistencial);
                if (!insertQuantifier()) {
                    e="A variable/constant cannot be existencial and universal in the same formula";
                    l=token.getLine();
                    c=token.getColumn();
                    error=true;
                    return NULL;
                    
                }  
                token=lexer.nextToken();
                return getFormula(equ,e,l,c,error);
            } 
            else {
                e="Variable or constant expected";
                l=token.getLine();
                c=token.getColumn();
                error=true;
                return NULL;
            }  
        }
        
        //Fim dos quantificadores
	
        //Parentesis
        
        
        if(token.getType()==ttOpenPar && parentisisBeforeEqual>0 && parentisisAfterEqual<0 && equ) { //Parentesis a abranger toda a fórmula, no caso das igualdades
            contaPar++;
    	    parentisisBeforeEqual--;
            token=lexer.nextToken();
           
            aux1= getFormula(equ,e,l,c,error);
            if(token.getType()!=ttClosePar) {
                e="Close parentheses expected";
                l=token.getLine();	    
                c=token.getColumn();
                error=true;
                return NULL;
            }
          
            parentisisAfterEqual++;
            contaPar--;
            token=lexer.nextToken();
            return aux1;
       }
        
       
        
    //A PARTE DE FIRST ORDER FORMULA-----------------------------------------------------------------------------------
    if (!equ){
        //uma fórmula pode ser um operador de negação seguido de uma fórmula -- um conectivo de negação seguido de fórmula
        if(token.getType()==ttNegation && token.getOperatorType()==otConnective && token.getNotation()==ntPrefix) {
            functions.insertNew(token);
            aux=TTree::newNode(token);
            token=lexer.nextToken();
            aux->left=getFormula(equ,e,l,c,error);
            return aux;
        }
        
        
      
      
        //senão será uma fórmula connective fórmula
        aux=atomicFormula(e,l,c,error);
       
        
        //carlos sousa 10-02-2023
        //se o token for close par e o contarPar>0
        if(token.getType()==ttClosePar && contaPar>0 && !equ) { //carlos sousa 10-02-2023
            contaPar--;
            token=lexer.nextToken(); //temos a história da atomic formula ser abrangida por parentisis e isso é detectado no termo
        }
        
        //carlos sousa 10-02-2023
        
        
        //ver bem a questão das precedências....as implicações e equivalências têm mais precedência que os ands ors, etc
        //atomic formula connectiva infixa formula
        if( (token.getOperatorType()==otConnective) && (token.getNotation()==ntInfix || token.getNotation()==ntInfix_right || token.getNotation()==ntInfix_left) ) {
            
            aux2=TTree::newNode(token);
            token=lexer.nextToken();
            aux2->left=aux;
            aux2->right=getFormula(equ,e,l,c,error);   
            aux=aux2;
            aux=applyPrecedence(aux); //caso necessário trocar a root consoante a precedência
            
        } 
      
       return aux;
	
    } 
    // FIM DA FIRST ORDER FORMULA
    //A PARTE DA IGUALDADE ---- UMA FÓRMULA PODE SER TERMO=TERMO ou TERMO !=TERMO, no entanto de for TERMOS !=TERMO não serve para waldmeister.....
    else {
            //uma fórmula pode ser basicTerm=basicTerm, logo uma equality formula-boa para o waldmeister
            
            
            aux=term(e,l,c,error);
            if(token.getType()==ttEqual || token.getType()==ttNotEqual) { //a igualdade não se traduz....nem é uma função....
                                            aux1=TTree::newNode(token);
                                            token=lexer.nextToken();
                                            aux1->left=aux;
                                            aux1->right=term(e,l,c,error);
                                            aux=aux1;
            }
            
            
            
            
        return aux;
    }    
    //FIM DA IGUALDADE------------------------------------------------------------------------------------------------
}




TList & TSyntatic::getFunctionsList(void)  {
        return functions;
}

TList & TSyntatic::getConstantsList(void)  {
        return constants;
}


TList & TSyntatic::getVariablesList(void) {
    return variables;
}



TList & TSyntatic::getAssumptionsQuantifiers(uint i)  {
return AssumptionsQuantifiers[i];
}


TList & TSyntatic::getSosQuantifiers(uint i)  {
return SosQuantifiers[i];
}


TList & TSyntatic::getGoalsQuantifiers(uint i)  {
return GoalsQuantifiers[i];
}


void TSyntatic::resetComments(void) {
listNode *aux=comments.getHead();
while(aux) {
        aux->data.setPrinted(false);
        aux=aux->next;
    }
    
}

uint TSyntatic::getFacount(void) const {
return fACount;
}

uint  TSyntatic::getGoalsCount(void) const {
        return fGCount;
}

TTree & TSyntatic::getFormulasAssumptions(ulong index)  { //devolve a aŕvore de uma fórmula cujo index se passa
    return formulasAssumptions[index];
}

TTree & TSyntatic::getFormulasGoals(ulong index)  { //devolve a aŕvore de uma fórmula cujo index se passa
    return formulasGoals[index];
}

//aproveita-se para contar os parentesis antes e depos do igual
bool TSyntatic::isEqualityFormula(int &pbe, int &pae) {
    bool equal=false; 
    bool conectives=false;
    streampos oldPos=lexer.getFilePos();
    int dim = token.getId().length();
    pbe=pae=0;
    do {
       
       if(token.getType()==ttOpenPar && !equal) pbe++;
       if(token.getType()==ttClosePar && !equal) pbe--;
       
       if(token.getType()==ttOpenPar && equal) pae++;
       if(token.getType()==ttClosePar && equal) pae--;
       
       if(token.getOperatorType()==otConnective) conectives=true; 
       if(token.getType()==ttEqual || token.getType()==ttNotEqual) equal=true;
       token=lexer.nextToken();
    }while(token.getType()!=ttPoint && !lexer.eof());
   
    if(token.getType()==ttPoint) dim++;
    oldPos-=dim;
    lexer.goBack(oldPos);  
    token=lexer.nextToken();
    return equal && !conectives;
    
}


void TSyntatic::getAttribute(void) {
   
    while(token.getType()!=ttPoint && !lexer.eof() && !lexer.getCRet() ) {
       token=lexer.nextToken();
    }
}


TList & TSyntatic::getwaldGoalsConst(void) {
    return waldGoalsConst;
}

void TSyntatic::updateWaldGoalsConst(treeNode* r) {
    TP9Token aux;
    if (r==NULL) return;
    updateWaldGoalsConst(r->left);
  //  cout<<r->data.getId()<<endl;
    
    if(r->data.getType()==ttVariable) {
        aux=r->data;
        aux.setType(ttConstant);
        waldGoalsConst.insertNew(aux); 
    }   
    
    if(r->parameters) {
        treeNode *p=r->parameters;
        while (p) {   
                    updateWaldGoalsConst(p);
                    p=p->nextParameter;
        }
    }
    updateWaldGoalsConst(r->right);
}


void TSyntatic::updateWaldGoalsConst(TTree& g) {
    
        updateWaldGoalsConst(g.getRoot());
    
}


bool TSyntatic::run(string &e, long &l, long &c) {
    bool stop;
    bool sos=false, goals=false, assumptions=false, error=false;
    bool equality=false;
    
    //correr o sintático
      stop=false;
      fACount=0;
      fGCount=0;
      fSCount=0;
    
      
      
      token=lexer.nextToken(); //gets first token
      if (!getIgnoreTokens(e,l,c)) return false;
     
        do {
                if ( (mode=getFormulasDeclaration(e,l,c))==NO_MODE) return false;
            
                //enter in formulas declaration
                do{
                
                    if (!getIgnoreTokens(e,l,c)) return false;
                    
                    if (getEndOfListDeclaration(e,l,c)) break;
             
                    equality=isEqualityFormula(parentisisBeforeEqual, parentisisAfterEqual); //verifica se é uma fórmula de igualdade ou !equal
                
                    //testar as história dos parentesis...o resto dos testes fazemos dentro do getformula
                    if(parentisisBeforeEqual+parentisisAfterEqual!=0)  {
                        e="parentesis Error";
                        l=token.getLine();
                        c=0;
                        return false;
                    }
		
		
                    switch(mode) {
                   
                        //hints é para ignorar
                        case HINTS_MODE:  
                            
                                            do {
                                            
                                                token=lexer.nextToken();
                                                
                                            } while (token.getType()!=ttEndOfList && token.getType()!=ttEOF);
                                            
                                            
                                            if(token.getType()==ttEOF) {
                                                e="end_of_list expected";
                                                l=token.getLine();
                                                c=token.getColumn();
                                                return false;
                                                
                                            }
                                            
                                            
                                            
                                       
                                        
                                            break;
                                            
                                            
                        case ASSUMPTIONS_MODE: 
                                            formulasAssumptions[fACount].setRoot(getFormula(equality,e,l,c,error)); 
                                            formulasAssumptions[fACount].setEquality(equality);
                                            if(formulasAssumptions[fACount].isEmpty()) return false;
                                            if(error) return false;
        //                                      if(debug)  cout <<"Formulas Assumptions"<<endl<<formulasAssumptions[fACount]<<endl;
                                            
                                            assumptions=true;
                                            fACount++;
                                            break;
                        
                        case SOS_MODE:         
                                            formulasAssumptions[fACount].setRoot(getFormula(equality,e,l,c,error)); 
                                            formulasAssumptions[fACount].setEquality(equality);
                                            if(formulasAssumptions[fACount].isEmpty()) return false;
                                            if(error) return false;
        //                                    if(debug)   cout <<"Formulas Sos"<<endl<<formulasSos[fSCount]<<endl;
                                            sos=true;
                                            fACount++;
                                            break;                        
                    
                        case GOALS_MODE:      
                                            formulasGoals[fGCount].setRoot(getFormula(equality,e,l,c,error));
                                            formulasGoals[fGCount].setEquality(equality);
                                            if(formulasGoals[fGCount].isEmpty()) return false;
                                            if(error) return false;
                                           // cout <<"Formulas Goals"<<endl<<formulasGoals[fGCount]<<endl;
                                            //temos aqui uma fórmula em goals....há que inserir no waldGoalsConst as variáveis da fórmula
                                            updateWaldGoalsConst(formulasGoals[fGCount]);
                                            
                                        
                                            goals=true;
                                            fGCount++;
                                            break;

                    }  
            
            
            
                    if(mode!=HINTS_MODE) {
            
            
                        //contapars
                        if(contaPar>0) {
                            e="Close parentheses expected";
                            l=token.getLine();
                            c=token.getColumn();
                            return false;
                        }
                        
                        if(contaPar<0) {
                            e="Open parentheses expected";
                            l=token.getLine();
                            c=token.getColumn();
                            return false;
                        }
                        //se vier label
                        
                        if(token.getType()==ttAtribute) {
                            getAttribute();
                        }
                        
                        
                        //tratar do ponto
                        if(token.getType()!=ttPoint) {
                            e="Point expected";
                            l=token.getLine();
                            c=token.getColumn();
                            return false;
                        }
                        
                        
                        token=lexer.nextToken();
                    }
                    
                    
                    else { //hints mode
                    
                                
                        
                    }
                    
                
            
          } while ( !getEndOfListDeclaration(e,l,c));
          
          if (!getIgnoreTokens(e,l,c)) return false;
          
      } while (!lexer.eof());  
     
      if(debug) {
//                    cout<<"Funções encontradas"<<endl<<functions<<endl;
 //                   cout<<"Variáveis encontradas"<<endl<<variables<<endl;
  //                  cout<<"Constantes encontradas"<<endl<<constants<<endl;
   //                 cout<<"BuiltIns depois de alterados"<<endl<<lexer.getBuiltIn()<<endl;
        }
      
      if( !sos && !assumptions) {
        e="Sos or Assumptions expected";
        l=token.getLine();
        c=token.getColumn();
        return false;
      }
      
      if(!goals) {
        e="Goals formulas expected";
        l=token.getLine();
        c=token.getColumn();
        return false;
          
      }
      
    /*
    //Testar o léxico
    do {
          token=lexer.nextToken();
          cout << token<<endl;
      } while(!lexer.eof());
    */
    
      
    
    
        
    
    
    
    return true;
}
