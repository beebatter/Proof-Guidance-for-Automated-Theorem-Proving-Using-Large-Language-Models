#ifndef _sintatic_h
#define _sintatic_h
#include "global.h"
#include "P9Token.h"
#include "list.h"
#include "tree.h"
#include "lexer.h"

#define INPUT_FILE_ERROR    4
#define ERROR_FILE_ERROR    1
#define IN_FILE_ERROR       2
#define NO_ERRORS           3


#define ASSUMPTIONS_MODE    1
#define GOALS_MODE          2
#define SOS_MODE            3
#define HINTS_MODE          4
#define NO_MODE             0

#define MAX_FORMULA 50      // máximo de fórmula permitidas por secção





class TSyntatic {

    
    private:    
                long contaPar;      //para validar a abertura e fecho de parentisis
                TP9Token token;
                ofstream errorFile;  //ficheiro de erro
                uint fACount,fSCount,fGCount;        //contador de funções
                bool debug;         //se estiver ou não em modo debug    
                uint mode;           //axiomas ou conslusões (0,1)
                ulong assmuptionsLine;
                ulong goalsLine;
                ulong sosLine;
                ulong hintsLine;
                int parentisisBeforeEqual, parentisisAfterEqual;
		
                
                
                
                TLexer lexer;  //lexer de prover9
                
                TTree formulasAssumptions [MAX_FORMULA*2];   //array para guardar as árvores das fórmulas de axiomas
                TTree formulasGoals[MAX_FORMULA]; //para guardar a arvore das fórmulas de conclusões
              //  TTree formulasSos[MAX_FORMULA]; //para guardar a arvore das formulas sos
                TList AssumptionsQuantifiers[MAX_FORMULA]; //quantificadores.....para assumptions
                TList SosQuantifiers[MAX_FORMULA]; //quantificadores.....para assumptions
                TList GoalsQuantifiers[MAX_FORMULA]; //quantificadores.....para assumptions
               
                TList functions;               //lista de funções usadas 
                TList constants;               //lista de constantes usadas 
                TList variables;  //lista de variáveis usadas...por causa dos quantificadores temos de ter uma lsita de variáveis por fórmula
                TList comments;                //lista com comentários 
                TList formula;                 //uma lista com todos os tokens de uma formula 
                
                TList waldGoalsConst;          //lista para termos todas as variáveris dos goals no waldmeister...isto poruqe o waldmeister só aceita contantes nos goals
                                               //e temos de converter as variáveis dos golas para constantes     
              
                
                void getComments(void); //trata de consumir os comentários e colocá-los na respectiva lista
                uint getFormulasDeclaration(string &, long &, long &); //valida formulas(sos) ou formulas(assumptions) ou formulas(goals).
                bool getEndOfListDeclaration(string &, long &, long &); //valida o end_of_list
                
                bool getAssign(string &, long &, long &); 
                bool getSet(string &, long &, long &,bool); 
                bool getClear(string &, long &, long &); 
                bool getIgnoreTokens(string &, long &, long &);
                bool assignConstant(const TP9Token &); 
                bool getRedeclare(string &e, long &l, long &c);
                bool isRedeclarable(const TP9Token &) const;
                bool isPrecedence(const TP9Token &t) const;
                bool getOp(string &, long &, long &); 
                bool getIf(string &, long &, long &) ;
                bool getEndIf(string &, long &, long &) ;
                bool getFunctionOrder(string &, long &, long &);
                
                
                treeNode * atomicFormula(string &, long &, long &, bool &);
                treeNode * term(string &, long &, long &, bool &);
                
                treeNode * functionTerm(TP9Token & ,  string &, long &, long &, bool &);
                treeNode * functionPredicate(string &, long &, long &, bool &);
                
                treeNode * prefixOperatorTerm(string & , long & , long & , bool &);
                treeNode * prefixOperatorPredicate(string &, long &, long &, bool &);
                
                treeNode * postFixOperatorTerm(bool , treeNode *, string &, long &, long &, bool &);
                
                uint checkArity(const TList &, const TP9Token &);
                
                bool testAndSetOperator(TP9Token &);
                bool testAndSetPredicate(TP9Token &);
                treeNode * getFormula(bool , string &s, long &l, long &c,bool &);
                bool isEqualityFormula(int &, int &);
                void getAttribute(void);
                bool insertQuantifier(void);
                treeNode *applyPrecedence(treeNode *); //aplica a precedência na árvore
                void updateWaldGoalsConst(TTree &);
                void updateWaldGoalsConst(treeNode *);
                
              
    public:
                TSyntatic(const char *, const char *,const bool) ; //infile for lexer, errorfile, debug, outputtype
                ~TSyntatic();
      
                void setErrorFile(const char *);
                
               
                
                void errorMessage(const string, const ulong , const ulong);
                _byte status(void) ;   //retorna 0 se tudo ok, 1 se não abriu o infile, 2 se não abriu o errofile
                bool run(string &, long &, long &); //executa a análise....devolve uma string com eventual erro e a linha e coluna desse erro
                TList & getCommentsList(void) ; //retorna para o exeterior a lista de comentários
              
                TList & getFunctionsList(void) ;
                TList & getConstantsList(void) ;
                TList & getVariablesList(void) ;
                TList & getwaldGoalsConst(void);
               
                TTree & getFormulasAssumptions(ulong); //devolve a aŕvore de uma fórmula cujo index se passa
                TTree & getFormulasGoals(ulong);
                
                void  resetComments(void) ; //os comentários podem ser imprimidos novamente
                ulong getAssumptionsLine(void) const;
                ulong getGoalsLine(void) const;
                ulong getSosLine(void) const;
                ulong getHintsLine(void) const;
                uint  getFacount(void)const;
                uint  getGoalsCount(void) const;
                TList &getAssumptionsQuantifiers(uint);
                TList &getSosQuantifiers(uint);
                TList &getGoalsQuantifiers(uint);
                
              
};



#endif
