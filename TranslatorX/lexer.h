#ifndef _lexer_h
#define _lexer_h

#include <fstream>
#include "P9Token.h"
#include "list.h"


typedef unsigned long ulong;



#define COMMENTS '%'
#define null() (lastChar=='\0' || lastChar=='\t' || lastChar=='\r' || lastChar=='\n' || lastChar==0) //null tokens
#define end_of_file() (_eof) //eofile
#define space() (lastChar==' ') //space troken
#define point() (lastChar=='.') //point token
#define openPar() (lastChar=='(') //openPar token
#define closePar() (lastChar==')') //closePar token
#define comma() (lastChar==',') //comma token
#define comments() (lastChar=='%') //comments token
#define lowerCase() (lastChar>='a' && lastChar<='z') 
#define upperCase() (lastChar >='A' && lastChar<='Z')
#define digit() (lastChar>='0' && lastChar <='9')
#define underScore() (lastChar=='_')
#define iscotes() (lastChar=='"')
#define isOrdinary() ( (lastChar>='a' && lastChar<='z') || (lastChar>='A' && lastChar<='Z') || (lastChar>='0' && lastChar<='9') || (lastChar=='_') || (lastChar=='$'))

                     
#define TRUE "$T"
#define FALSE "$F"



class TLexer {

    private:
                TList builtIn; //A list with buildin parsing declarations
        
                ulong line, column; //current line and current column
                ifstream iniFile;
                TP9Token currentToken;
                bool _fok;
                bool _eof;
                bool _eol;
                bool _cret;
                char lastChar;
                bool prologStyle;
  
                void buildBuiltIn(void);
                char getChar(void);
                string getComments(void);
                void getSpaces(void);
                bool isSpecialSymbol(void) const;
                
                string getSpecialSymbol(void);
                string getQuotedSymbol(void);
                
                bool isStartVariable(void)const;
                bool isStartVariable(char c) const;
                string getVariable(void);
                bool isStartConstant(void) const;
                bool isStartConstant(char c) const;
                string getConstant(void);
                string getOrdinarySymbol(void);
                
                bool isLowerCase(void) const;
                bool isUnderScore(void) const;
                bool isDigit(void) const;
            
                void setProperties(TP9Token &); //if it's a builtin token set's its properties with the buildin properties
                TP9Token _nextToken(void);
                bool isSpecial(void) const; 
                
                bool reserverWord(TP9Token &);
                bool integer(TP9Token &)const; 
                bool _operator(TP9Token &)const;
                
                
                
                
    public:           
                TLexer(const char *);
                TLexer(void);
                ~TLexer();
                
                bool eof(void)const ;
                bool eol(void)const;
                bool fok(void)const;
                bool isBuiltIn(const TP9Token &) const;
                bool isBuiltIn(const string &) const ;
                void setProlog(void);
                void clearProlog(void);
              
                TP9Token  nextToken(void);
                void Init(const char *); 
                void redeclare(TP9TokenType &, string &);
                void redefine(const TP9TokenType &, const TP9NotationType &, const uint &);
                void changeBuiltInProperties(const string &, const TP9NotationType &, const uint &);
                void changeBuiltInOperatorType(const string &,  const TP9OperatorType &);
                void changeBuiltInArity(const string &, uint );
                bool isTermOperator(const TP9Token &) const; //é um operador que pode pertencer a um termo? ... não pode ser conectivo, e tem de ser builtin
                bool isPredicateOperator(const TP9Token &) const; //é um operador que pode ser usado num predicado? --tem de ser builtin e tem de ser marcado como otPredicate
                TList getBuiltIn(void) const;
                bool getPrologStyle(void) const;
                streampos getFilePos(void) ;
                void goBack(streampos);
                bool getCRet(void) const;
                void insertNewBuiltIn(TP9Token &);
                
};



#endif
