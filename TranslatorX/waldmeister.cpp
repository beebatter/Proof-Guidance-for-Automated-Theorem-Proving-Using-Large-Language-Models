#ifndef _WALD_CPP
#define _WALD_CPP

#include "waldmeister.h"
#include "tree.h"
#include "list.h"


void TWaldmeister::init(char * nameFile, bool d) {
  debug=d; 
  
  outFile.open(nameFile);
   if(outFile.is_open()) {
		fileOK=true;
		fname="p9f0";
		fnumber=0;
		write("NAME          Prover9_Translated_File"); newLine();
		write("MODE          PROOF"); newLine();
		write("              %COMPLETION");newLine();
		write("SORTS         ANY");newLine();
		write("              %ANY1 ANY2");newLine();
   }
 else fileOK=false;
 tradError=false; 
 lastPrintedComment=0;
}



TWaldmeister:: TWaldmeister()
{
  fileOK=false;
  tradError=false;
  lastPrintedComment=0;
  
}

void TWaldmeister::clearFile(char * nameFile) {
    outFile.close();
    outFile.open(nameFile, std::ofstream::out | std::ofstream::trunc);
    outFile.close();
}


void TWaldmeister::tradGoalsVarstoConsts(TList& vars) {
    listNode *aux=vars.getHead();
    char n[10];
    uint index=0;
    string s;
    
    while(aux) {
        sprintf(n, "%d", index);    
        s.append("skvar");
        s.append(n);
        aux->data.setTrad(s);
        aux=aux->next;
        index++;
        s.clear();
        
    }
}


bool TWaldmeister::getTradError(void) const{
  return tradError;
}

string TWaldmeister::incFname(void) {
 char n[12];
  fnumber++;  
  sprintf(n, "%d", fnumber);
  fname="";
  fname.append("p9f");
  fname.append(n);
  return fname;
}



TWaldmeister::TWaldmeister(char * nameFile, bool d)
{
    init(nameFile, d);
    
}


TWaldmeister::~TWaldmeister() {
  outFile.close();
}


void TWaldmeister::write(string s) {
	if (!fileOK) return;
	
	outFile <<s;
	if (debug) cout << s;
}

void TWaldmeister::newLine(void) {
	if(!fileOK) return;
	outFile<<endl;
	if(debug) cout<<endl;
}



void TWaldmeister::orignalNames(const TList &t, const bool &f_c) {
  listNode *aux=t.getHead();
  if (f_c)  write("% Function Translation");
  else write("% Constant Translation"); 
  newLine();
  while(aux) {
    write("% Original:"); write(aux->data.getId()); write("     Translated:"); write(aux->data.getTrad());
    newLine();
    aux=aux->next;
  }
  newLine();
}

void TWaldmeister::signature(const TList &f, const TList &c, const TList &gc) {
  listNode *aux=f.getHead();
  listNode *auxgc=gc.getHead();
  if(!fileOK) return;
  write("SIGNATURE     ");
  signature(aux);
  aux=c.getHead();
  signature(aux);
  signatureGoalsVarsToConst(auxgc);
  newLine();
  orignalNames(f,true);
  orignalNames(c,false);
  orignalNames(gc,false);
}
  

void TWaldmeister::signatureGoalsVarsToConst(listNode *r) {
    int i;
    if(!r) return;
    write(r->data.getTrad());
    write(":");
    for (i=0;i< r->data.getArity(); i++) write(" ANY ");
    write("->ANY ");
    newLine();
    write("              ");
    signatureGoalsVarsToConst(r->next);
    
    
}

void TWaldmeister::signature(listNode *r) {
  string aux;
  int i=0;
  if(!r) return;
  if( (r->data.getType()==ttFunction || r->data.getNotation()==ntPrefix || r->data.getNotation()==ntInfix || r->data.getNotation()==ntPostfix) && (r->data.getType()!=ttTrue && r->data.getType()!=ttFalse) ) { //para as funçoes
	  r->data.setTrad(fname);
	  write(r->data.getTrad());
	  fname=incFname(); //
  }
  
  if(r->data.getType()==ttConstant || r->data.getType()==ttInteger || r->data.getType()==ttTrue || r->data.getType()==ttFalse) { //para constantes
    //como será o true e o false no waldmeister?????  
    //para já vamos meter true e false
    if(r->data.getType()==ttTrue) r->data.setTrad(_TRUE);
    else if (r->data.getType()==ttFalse) r->data.setTrad(_FALSE);                                              
    else  r->data.setTrad(r->data.getId()); //as constantes podem mater a designação ???? temos de ver bem depois mas para já sim
    write(r->data.getTrad());
  } 

  write(":");
  for (i=0;i< r->data.getArity(); i++) write(" ANY ");
  write("->ANY ");
  newLine();
  write("              ");
  signature(r->next);
}





void TWaldmeister::ordering(const TList &f, const TList &c, const TList &gc) {
 listNode *aux;
  if(!fileOK) return;
  write("ORDERING      LPO");newLine();
  
  //se apenas houver um somatório de uma função e uma constante
  //abortar
  
  if (f.length() + c.length() ==1) return;  
  
  //primeiro as funções
  write("              ");
  aux=f.getHead();
  ordering(aux);      
  if(c.length()>0) write(">");
  aux=c.getHead(); //depois as constantes
  ordering(aux);
  
  if(gc.length()>0) write(">");
  aux=gc.getHead(); //depois as variáveis dos goals traduzidas
  ordering(aux);
  
  newLine();
}

void TWaldmeister::ordering(listNode* r) {
  if(!r) return;
  outFile <<r->data.getTrad();
  if (r->next!=NULL) outFile<<">";
  
  if (debug) {
      cout <<r->data.getTrad();
      if (r->next!=NULL) cout<<">";
  }
  ordering(r->next);
}

//retorna true se existirem variáveis existenciais
bool TWaldmeister::variables(const TList &t) {
  listNode *aux=t.getHead();
  bool existential=false;
  
  if(!fileOK) return true;//error
  while (aux!=NULL && !existential) {
      if(aux->data.getVType()==vtExistencial) existential=true;
      else aux=aux->next;
  }
  if(existential) return true; //error
  write("VARIABLES     ");
  aux=t.getHead();
  variables(aux);
  write("  :ANY");newLine();
  return false;//no error, only universal variables
}

void TWaldmeister::variables(listNode* r) {
  if(!r) return;
  write(r->data.getId());
  if (r->next!=NULL) write(",");
  variables(r->next);
}


void TWaldmeister::incLevel() {
	write("              ");
}

void TWaldmeister::equationTile() {
  write("EQUATIONS     ");
}

void TWaldmeister::conclusionTile() {
  write("CONCLUSION    ");
}


void TWaldmeister::equation(const TTree &t, const TList &f, const TList &c){
 equationError=false;
 return equation(t.getRoot(),f,c); 
 newLine();
}

void TWaldmeister::equationGoals(const TTree &t,const TList &f, const TList &c, const TList &gc) {
    equationError=false;
    return equationGoals(t.getRoot(),f,c, gc); 
    newLine();
    
}

void TWaldmeister::equal(void)
{
 write("=");
}

bool TWaldmeister::getEquationError(void) const {
 return equationError;
}


void TWaldmeister::equationGoals(treeNode *r, const TList &f, const TList &c, const TList &gc) {
 //sem tradução
 treeNode *aux;
 if (r==NULL) return;
 
 if(r->data.getType()==ttNotEqual) {//para waldmeister não podem haver certos sýmbolos tal como o notEqual
        equationError=true;
        return;
 }
 write(r->data.getTrad());
 
 if(r->data.getType()==ttEqual) { //é o igual
    equationGoals(r->left,f,c, gc);
    equal();
    equationGoals(r->right,f,c,gc);
 }
 
 else 
     if( (   (r->data.getType()==ttFunction || r->data.getNotation()==ntPrefix || r->data.getNotation()==ntPostfix) && r->data.getOperatorType()!=otConnective) && r->data.getType()!=ttTrue && r->data.getType()!=ttFalse  ) { //as funções são sempre prefixas
        if (f.findTradById(r->data.getId())=="") tradError=true;
        write(f.findTradById(r->data.getId())+"(");   
        aux=r->parameters->nextParameter;
        while(aux) {
                    equationGoals(aux,f,c,gc);
                    if (aux->nextParameter) write(",");
                    aux=aux->nextParameter;
        }
        write(")");
    }

 else if(r->data.getNotation()==ntInfix) {
            if (f.findTradById(r->data.getId())=="") tradError=true;
            write(f.findTradById(r->data.getId())+"(");
            equationGoals(r->left,f,c,gc);
            if (r->right!=NULL) {
                write(",");
                equationGoals(r->right,f,c,gc);
            }
            write(")"); 
    }

 else if(r->data.getType()==ttNegation && r->data.getOperatorType()==otConnective && r->data.getNotation()==ntPrefix) { //connective negação
        if (f.findTradById(r->data.getId())=="") tradError=true;
        write(f.findTradById(r->data.getId())+"(");   
        equationGoals(r->left,f,c,gc);
        write(")");
      }
 else 
        {
            //podemos ter de diferenciar algumas constantes...neste momento não sei como é do true e do false
            if(r->data.getType()==ttTrue || r->data.getType()==ttFalse) write(c.findTradById(r->data.getId()));
            else 
                if(r->data.getType()==ttVariable) {
                //ir buscar a vairável pelo id e escrever a tradução
                string trad;
                trad=gc.findTradById(r->data.getId());
                write(trad); //variáveis, temos de ir buscar as traduções para escrever nos goals 
                }
            else write(r->data.getId()); 
            
        }
}

void TWaldmeister::equation(treeNode *r, const TList &f, const TList &c) {
 //sem tradução
 treeNode *aux;
 if (r==NULL) return;
 
 if(r->data.getType()==ttNotEqual) {//para waldmeister não podem haver certos sýmbolos tal como o notEqual
        equationError=true;
        return;
 }
 write(r->data.getTrad());
 if(r->data.getType()==ttEqual) { //é o igual
    equation(r->left,f,c);
    equal();
    equation(r->right,f,c);
 }
 
 else 
     if( (   (r->data.getType()==ttFunction || r->data.getNotation()==ntPrefix || r->data.getNotation()==ntPostfix) && r->data.getOperatorType()!=otConnective) && r->data.getType()!=ttTrue && r->data.getType()!=ttFalse  ) { //as funções são sempre prefixas
        if (f.findTradById(r->data.getId())=="") tradError=true;
        write(f.findTradById(r->data.getId())+"(");   
        aux=r->parameters->nextParameter;
        while(aux) {
                    equation(aux,f,c);
                    if (aux->nextParameter) write(",");
                    aux=aux->nextParameter;
        }
        write(")");
    }

 else if(r->data.getNotation()==ntInfix) {
            if (f.findTradById(r->data.getId())=="") tradError=true;
            write(f.findTradById(r->data.getId())+"(");
            equation(r->left,f,c);
            if (r->right!=NULL) {
                write(",");
                equation(r->right,f,c);
            }
            write(")"); 
    }

 else if(r->data.getType()==ttNegation && r->data.getOperatorType()==otConnective && r->data.getNotation()==ntPrefix) { //connective negação
        if (f.findTradById(r->data.getId())=="") tradError=true;
        write(f.findTradById(r->data.getId())+"(");   
        equation(r->left,f,c);
        write(")");
      }
 else 
        {
            //podemos ter de diferenciar algumas constantes...neste momento não sei como é do true e do false
            if(r->data.getType()==ttTrue || r->data.getType()==ttFalse) write(c.findTradById(r->data.getId()));
            else write(r->data.getId()); //variáveis 
        }
     
}


void TWaldmeister::printComments(TList &c, ulong from, ulong until) {
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

 
uint TWaldmeister::getLastPrintedComment(void) const {
    return lastPrintedComment;
}
  
  



#endif	    
