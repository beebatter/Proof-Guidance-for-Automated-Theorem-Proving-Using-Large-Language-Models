#ifndef _P9Token_h
#define _P9Token_h

#include <string>

#include <ostream>

using namespace std;

typedef unsigned long ulong;
typedef unsigned int uint;

#define MAX_NOTATIONS 8

enum TP9TokenType {ttUnknown,ttSpecialSymbol, ttQuotedSymbol,ttOrdinarySymbol,ttComments,ttNull,ttEOF,ttSpace,ttPoint, ttOpenPar, ttClosePar, ttComma,ttOperator,
    ttEquivalence, ttImplication, ttBackImplication,ttAtribute,ttConjunction, ttDisjunction,ttEqual,ttNotEqual,ttIsEqual,ttLessOrEqual, ttGreaterOrEqual,
    ttLess, ttGreater,ttPlus, ttMult, ttAt,ttSlash, ttDivide, ttPower, ttNegation, ttLine,ttV,ttVariable, ttConstant,ttTrue, ttFalse,ttFormulas, ttSos, 
    ttEndOfList, ttGoals,ttAssumptions,ttFunction,ttAssign,ttMaxSeconds,ttNewConstants,ttMaxWeight, ttInteger,ttSet,ttRestricDenials,ttLabel, ttAnswer,ttAction,ttBusbHintWt,ttClear,
    ttStartSize, ttEndSize, ttIncrement, ttDomainSize, ttIterate, ttMaxSecondsPer, ttMaxModels,ttMaxMegs,
    ttInfixName,ttInfixLeftName,ttInfixRightName, ttPrefixName, ttPrefixParenName, ttPostfixName, ttPostfixParenName, ttOrdinaryName,ttOp, 
    ttPrologStyleVariables,ttRedeclare,ttTrueName,ttFalseName,ttNegationName,ttDisjunctionName,ttConjunctionName,
    ttImplicationName,ttBackwardImplicationName,ttEquivalenceName,ttUniversalQuantificationName,ttExistentialQuantificationName,
    ttEqualityName,ttNegatedEqualityName,ttAttributeName,ttAll,ttExists, ttIf,ttEndIf, ttFalse_Part, ttTrue_Part, ttOrder, ttEq_defs,ttAge_part, ttWeight_part, ttLiteral_selection,
    ttBacksub_check,ttSos_limit,ttFold_denial_max,ttSk_constant_weight,ttProp_atom_weight,ttSkolem_penalty, ttNest_penalty, ttStats, ttEval_limit, ttFunction_order, ttHints
   
}; //prover9 token types

const char* TP9TokenName[] = {"ttUnknown", "ttSpecialSymbol","ttQuotedSymbol","ttOrdinarySymbol","ttComments","ttNull","ttEOF","ttSpace","ttPoint", "ttOpenPar", "ttClosePar","ttComma","ttOperator",
"ttEquivalence","ttImplication", "ttBackImplication","ttAtribute","ttConjunction", "ttDisjunction","ttEqual","ttNotEqual","ttIsEqual","ttLessOrEqual", "ttGreaterOrEqual",
"ttLess", "ttGreater","ttPlus", "ttMult", "ttAt","ttSlash", "ttDivide", "ttPower", "ttNegation", "ttLine","ttV","ttVariable","ttConstant","ttTrue", "ttFalse","ttFormulas", "ttSos", 
"ttEndOfList", "ttGoals","ttAssumptions","ttFunction","ttAssign","ttMaxSeconds","ttNewConstants","ttMaxWeight","ttInteger","ttSet","ttRestricDenials","ttLabel", "ttAnswer","ttAction","ttBusbHintWt","ttClear",
"ttStartSize", "ttEndSize", "ttIncrement", "ttDomainSize", "ttIterate", "ttMaxSecondsPer", "ttMaxModels","ttMaxMegs",
"ttInfixName","ttInfixLeftName","ttInfixRightName", "ttPrefixName", "ttPrefixParenName", "ttPostfixName", "ttPostfixParenName", "ttOrdinaryName","ttOp", 
"ttPrologStyleVariables","ttRedeclare","ttTrueName","ttFalseName","ttNegationName","ttDisjunctionName","ttConjunctionName",
"ttImplicationName","ttBackwardImplicationName","ttEquivalenceName","ttUniversalQuantificationName","ttExistentialQuantificationName",
"ttEqualityName","ttNegatedEqualityName","ttAttributeName","ttAll","ttExists", "ttIf","ttEnd_if", "ttFalse_part", "ttTrue_part", "ttOrder", "ttEq_defs","ttAge_part","ttWeight_part","ttLiteral_selection",
"ttBacksub_check","ttSos_limt","ttFold_denial_max","ttSk_constant_weight","ttProp_atom_weight","ttSkolem_penalty", "ttNest_penalty", "ttStats", "ttEval_limit","ttFunction_order","ttHints"
    
}; //prover9 token names


enum TP9NotationType {ntInfix, ntInfix_left, ntInfix_right, ntPrefix, ntPrefix_paren, ntPostfix, ntPostfix_paren, ntOrdinary}; //token notation
  
  
  
const char* TP9NotationName[MAX_NOTATIONS] = {"infix", "infix_left", "infix_right", "prefix", "prefix_paren", "postfix", "postfix_paren", "ordinary"
}; //prover9 token names
  
enum TVariableType {vtUniversal, vtExistencial,vtNone};

const char* TP9QuantifierName[]= {"Universal", "Existencial","None"};


enum TP9OperatorType {otPredicate, otConnective, otOperator, otDefault}; //se é um predicado, um connective, um operator ou outra coisa (default)

class TP9Token {

		private:
				 ulong line,column; //linha e coluna do token
				 string id; //identificador 
				 string trad; //tradução
				 uint arity; //ariedade
                 uint precedence; //precedência
                 bool printed;
                 TP9NotationType notation; //notação (infixo, etc)
				 TP9TokenType type;  //tipo de token ()
                 TVariableType vtype; //quantificador (s'para variáveis)
                 TP9OperatorType operatorType; //se é um predicado, se é um operador, se é um conectivo
                 
                 
                 
                 
                 std::ostream&  print(std::ostream & out) const;
				 
		public:
					TP9Token(const ulong &,const ulong &,const string &,const TP9TokenType &);
					TP9Token(void); //default constructor*/
					void setLine(const ulong &);
					void setColumn(const ulong&);
					void setId(const string&);
					void setType(const TP9TokenType &);
                    void setNotation(const TP9NotationType &);
                    void setPrecedence(const uint &); 
                    void setTrad(const string &);
					void setArity(const int &);
                    void setVType(const TVariableType &);
                    void setOperatorType(const TP9OperatorType &);
                    
                   

					TP9OperatorType getOperatorType(void) const;
					ulong getLine(void) const;
					ulong getColumn(void) const;
					string getId(void) const;
					string getTrad(void) const;
					TP9TokenType getType(void) const;
					string getTypeName(void) const;
                    string getNotationName(void) const;
					int getArity(void) const;
                    TP9NotationType getNotation(void) const;            //get notation of this token
                    TP9NotationType getNotation(const string &s) const; //get notation type notation name
                    uint getPrecedence(void) const;
                    friend ostream& operator <<(ostream& osObject, const TP9Token& t);
                    void setPrinted(const bool );
                    bool getPrinted(void) const ;
                    TVariableType getVType(void) const;

};
#endif
