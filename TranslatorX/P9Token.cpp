#include "P9Token.h"

//Construtor por defeito
TP9Token::TP9Token(void) {
	line=1;
	column=1;
	id="";
	arity=1;
	type=ttUnknown;
    printed=false;

}

//Construtor passando linha, colunam id e tipo
TP9Token::TP9Token(const ulong &l,const ulong &c,const string &i,const TP9TokenType &t) {
	line=l;
	column=c;
	id=i;
	arity=1;
	type=t;
    printed=false;
}

void TP9Token::setOperatorType(const TP9OperatorType &ot) {
   operatorType= ot;
}
                   

TP9OperatorType TP9Token::getOperatorType() const {
    return operatorType;
}

TVariableType TP9Token::getVType(void) const {
    return vtype;
}

 void TP9Token::setVType(const TVariableType &vt) {
        vtype=vt;
     
}

void TP9Token::setLine(const ulong &l) {
	line=l;
}

void TP9Token::setColumn(const ulong &c) {
	column=c;
}

void TP9Token::setId(const string &i) {
	id=i;
}

void TP9Token::setType(const TP9TokenType &t) {
	type=t;
}


void TP9Token::setTrad(const string &s){
   trad=s;
}

void TP9Token::setArity(const int &a) {
	arity=a;
}

void TP9Token::setNotation(const TP9NotationType &t) {
    notation=t;
}

void TP9Token::setPrecedence(const uint &p) {
        precedence=p;
}


void TP9Token::setPrinted(const bool p) {
    printed=p;
}

bool TP9Token::getPrinted(void) const  {
    return printed;
}


ulong TP9Token::getLine(void) const{
	return line;
}
ulong TP9Token::getColumn(void) const {
	return column;
}

string TP9Token::getId(void) const{
	return id;
}

TP9TokenType TP9Token::getType(void) const {
	return type;
}					

string TP9Token::getTrad(void) const {
  return trad;
}


int TP9Token::getArity(void) const {
 return arity;
}

TP9NotationType TP9Token::getNotation(void) const{
    return notation;
}

TP9NotationType TP9Token::getNotation(const string &s) const {
    int i=0;
    bool found=false;
    while (i<MAX_NOTATIONS && !found) 
        if(s==TP9NotationName[i]) found=true;
        else i++;
    if (found) return TP9NotationType(i);
    else return TP9NotationType::ntInfix;
}


uint TP9Token::getPrecedence(void) const{
    return precedence;
}

string TP9Token::getTypeName(void) const {
    return TP9TokenName[type] ;
}


string TP9Token::getNotationName() const{
    return TP9NotationName[notation];
}

std::ostream&  TP9Token::print(std::ostream & out) const {
	out<<"Id: " << id << " Type: " << TP9TokenName[type] << " Precedence:" <<precedence << " Notation:" << TP9NotationName[notation]<<" Line:" <<line<< " Column:"<<column<<" Arity:"<<arity<<" Quantifier:"<<TP9QuantifierName[vtype];
    return out;
}


ostream& operator << (ostream& osObject, const TP9Token & t)
{
    return t.print(osObject); //just one line!
}
