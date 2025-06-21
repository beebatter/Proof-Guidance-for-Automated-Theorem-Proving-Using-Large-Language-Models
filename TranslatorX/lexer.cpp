#include "lexer.h"
#include "string.h"



TLexer::TLexer(const char *s) {
    iniFile.open(s);
    if(iniFile) {
        _fok-true;
        column=1;
        line=1;
        _eof=false;
        _eol=false;
        _cret=false;
        prologStyle=false;
        buildBuiltIn();    
        currentToken.setLine(0);
        currentToken.setColumn(0);                
        currentToken.setId("");
        currentToken.setOperatorType(otDefault);
        getChar();
    } else 
        _fok=false;
    
}


TLexer::TLexer::TLexer(void)
{
    
}


void TLexer::Init(const char *f) {
    iniFile.open(f);
    if(iniFile) {
        _fok=true;
        column=1;
        line=1;
        _eof=false;
        _eol=false;
        _cret=false;
        prologStyle=false;
        buildBuiltIn();    
        currentToken.setLine(0);
        currentToken.setColumn(0);                
        currentToken.setId("");
        currentToken.setOperatorType(otDefault);
        getChar();
    } else 
        _fok=false;    
}

streampos TLexer::getFilePos(void) {
return iniFile.tellg();
}

void TLexer::goBack(streampos p) {
    iniFile.seekg(p);
    lastChar=getChar();
}

//builtIn List 
void TLexer::buildBuiltIn(void) {
	currentToken.setVType(vtNone);
	//arity 2, preecedence 810, infix_right
   	currentToken.setArity(2); 
        currentToken.setPrecedence(810);
        currentToken.setNotation(ntInfix_right);
        currentToken.setOperatorType(otDefault);
        currentToken.setId("#");
        currentToken.setType(ttAtribute);
        builtIn+=currentToken;
        
    
    
        //arity 2, preecedence 800, infix
        currentToken.setArity(2);
        currentToken.setPrecedence(800);
        currentToken.setNotation(ntInfix);

        
        currentToken.setId("<->");
        currentToken.setType(ttEquivalence);
        currentToken.setOperatorType(otConnective); 
        builtIn+=currentToken;
        
        currentToken.setId("->");
        currentToken.setType(ttImplication);
        currentToken.setOperatorType(otConnective);                
        builtIn+=currentToken;
        
        currentToken.setId("<-");
        currentToken.setType(ttBackImplication);
        currentToken.setOperatorType(otConnective);        
        builtIn+=currentToken;
        
        //or and and infix_right
        currentToken.setArity(2);
        currentToken.setPrecedence(790);
        currentToken.setNotation(ntInfix_right);
        
        currentToken.setId("|");
        currentToken.setType(ttDisjunction);
        currentToken.setOperatorType(otConnective);
        builtIn+=currentToken;
        
        currentToken.setPrecedence(790);
        currentToken.setId("&");
        currentToken.setType(ttConjunction);
        currentToken.setOperatorType(otConnective);
        builtIn+=currentToken;
        
        //arity 2, preecedence 700, infix
        currentToken.setArity(2);
        currentToken.setPrecedence(700);
        currentToken.setNotation(ntInfix);
        
        currentToken.setId("=");
        currentToken.setType(ttEqual);
        currentToken.setOperatorType(otPredicate);
        builtIn+=currentToken;
        
        currentToken.setId("!=");
        currentToken.setType(ttNotEqual);
        currentToken.setOperatorType(otPredicate);
        builtIn+=currentToken;
        
        
	currentToken.setId("==");
        currentToken.setType(ttIsEqual);
        currentToken.setOperatorType(otPredicate);
        
        builtIn+=currentToken;
        
        currentToken.setId("<=");
        currentToken.setType(ttLessOrEqual);
        currentToken.setOperatorType(otPredicate);
        
        builtIn+=currentToken;
        
        currentToken.setId(">=");
        currentToken.setType(ttGreaterOrEqual);
        currentToken.setOperatorType(otPredicate);
        builtIn+=currentToken;
        
        currentToken.setId(">");
        currentToken.setType(ttGreater);
        currentToken.setOperatorType(otPredicate); 
        builtIn+=currentToken;
        
        currentToken.setId("<");
        currentToken.setType(ttLess);
        currentToken.setOperatorType(otPredicate);  
        builtIn+=currentToken;
        
        
        //arity 2, preecedence 500, infix
        currentToken.setArity(2);
        currentToken.setPrecedence(500);
        currentToken.setNotation(ntInfix);
        
        currentToken.setId("+");
        currentToken.setType(ttPlus);
        currentToken.setOperatorType(otOperator);
        builtIn+=currentToken;
        
        currentToken.setId("*");
        currentToken.setType(ttMult);
        currentToken.setOperatorType(otOperator);        
        builtIn+=currentToken;
        
        currentToken.setId("@");
        currentToken.setType(ttAt);
        currentToken.setOperatorType(otOperator);        
        builtIn+=currentToken;
        
 
        currentToken.setId("\\");
        currentToken.setType(ttSlash);
        currentToken.setOperatorType(otOperator);
        builtIn+=currentToken;
 
        currentToken.setId("/");
        currentToken.setType(ttDivide);
        currentToken.setOperatorType(otOperator);
        builtIn+=currentToken;
 
        currentToken.setId("^");
        currentToken.setType(ttPower);
        currentToken.setOperatorType(otOperator);        
        builtIn+=currentToken;
 

        //arity 1, preecedence 350, prefix 
        currentToken.setArity(1);
        currentToken.setPrecedence(350);
        currentToken.setNotation(ntPrefix);
        
        currentToken.setId("-");
        currentToken.setType(ttNegation);
        currentToken.setOperatorType(otConnective);
        builtIn+=currentToken;
        
        
        //arity 1, precedence 300, postfix
        currentToken.setArity(1);
        currentToken.setPrecedence(300);
        currentToken.setNotation(ntPostfix);
        currentToken.setOperatorType(otOperator);        
        currentToken.setId("'");
        currentToken.setType(ttLine);
        builtIn+=currentToken;
        
        
        currentToken.setOperatorType(otDefault);
        currentToken.setId("$T");
        currentToken.setType(ttTrue);
        builtIn+=currentToken;

        currentToken.setOperatorType(otDefault);        
        currentToken.setId("$F");
        currentToken.setType(ttFalse);
        builtIn+=currentToken;

        currentToken.setId("all");
        currentToken.setType(ttAll);
        currentToken.setOperatorType(otDefault);        
        builtIn+=currentToken;
        
        currentToken.setId("exists");
        currentToken.setType(ttExists);
        currentToken.setOperatorType(otDefault);        
        builtIn+=currentToken;
	
}


void TLexer::insertNewBuiltIn(TP9Token &t) {
    builtIn.insertNew(t);
}

bool TLexer::isTermOperator(const TP9Token &t) const {//é um operador que pode pertencer a um termo? ... não pode ser conectivo, e tem de ser builtin e não pode ser predicado
 return (isBuiltIn(t)) && (t.getOperatorType()!=otConnective && t.getType()!=ttEqual && t.getOperatorType()!=otPredicate ) ;
}




bool TLexer::isPredicateOperator(const TP9Token &t) const {//é um operador que pode ser usado num predicado? --tem de ser builtin e não pode ser usado como conective
return isBuiltIn(t) && (t.getOperatorType()!=otConnective && t.getType()!=ttEqual);
}

bool TLexer::isBuiltIn(const TP9Token &t) const{
    return builtIn.findById(t.getId());
}
 
bool TLexer::isBuiltIn(const string &s) const {
    return builtIn.findById(s);
}

void TLexer::setProperties(TP9Token &t) {
listNode *aux=builtIn.getHead(); //find in built in list token t.
bool found=false;
    
    while(!found && aux!=NULL)
       if(aux->data.getId()==t.getId()) found=true;
       else aux=aux->next;
       
    if (found) {  //we will never know //if found then it takes the build in properties
        t.setArity(aux->data.getArity());
        t.setNotation(aux->data.getNotation());
        t.setType(aux->data.getType());
        t.setPrecedence(aux->data.getPrecedence());
        t.setOperatorType(aux->data.getOperatorType());
    }
}
  
  
void TLexer::changeBuiltInOperatorType(const string &id,  const TP9OperatorType & ot) {
    listNode *aux=builtIn.getHead();
    bool found=false;
    while(!found && aux) 
        if(aux->data.getId()==id) found=true;
        else aux=aux->next;
  
    if(found) 
                aux->data.setOperatorType(ot);
}
  
void TLexer::changeBuiltInArity(const string & id, uint a) {
    listNode *aux=builtIn.getHead();
    bool found=false;
    while(!found && aux) 
        if(aux->data.getId()==id) found=true;
        else aux=aux->next;
    if(found) 
                aux->data.setArity(a);
}
  
void TLexer::changeBuiltInProperties(const string &id, const TP9NotationType &nt, const uint &p) {
    listNode *aux=builtIn.getHead();
    bool found=false;
    while(!found && aux) 
        if(aux->data.getId()==id) found=true;
        else aux=aux->next;
  
    if(found && aux->data.getType()!=ttEqual) {
                aux->data.setNotation(nt);
                aux->data.setPrecedence(p);
    }
}
  
void TLexer::setProlog(void) {
        prologStyle=true;
}

void TLexer::clearProlog(void) {
        prologStyle=false;
}
                
  
  
char TLexer::getChar(void) {
_eof=false;
_eol=false;
_cret=false;
lastChar=iniFile.get();
column++;    
if(lastChar=='\n') {
        _eol=true;
        line++;
        column=1;
    }

if(lastChar==EOF) 
    _eof=true;    

if (lastChar=='\r') _cret=true;
return lastChar;
}


 bool TLexer::getCRet(void) const {
  return _cret;
}

TP9Token TLexer::nextToken(void) {
 currentToken=_nextToken();
 while (currentToken.getType()==ttSpace || currentToken.getType()==ttNull) currentToken=_nextToken();
     
 //verify if current token is a built in token
if (isBuiltIn(currentToken)) 
    setProperties(currentToken); 
 currentToken.setPrinted(false);
 return currentToken;
}




TP9Token TLexer::_nextToken(void) {
  string s;
  s=s+lastChar;
  currentToken.setLine(line) ;
  currentToken.setColumn(column);
  currentToken.setType(ttUnknown);
  currentToken.setArity(1);
  currentToken.setId(s);
  currentToken.setNotation(ntOrdinary);
  currentToken.setPrecedence(900);
  currentToken.setVType(vtUniversal); //todos os tokens são quantificados por universal por defeito
  currentToken.setOperatorType(otDefault);
  
  
  
  if (null()) {
      
        currentToken.setType(ttNull);
        getChar();
  }
  
  else if (end_of_file()) {
         currentToken.setType(ttEOF);
  }
  else if (space()) {
         currentToken.setType(ttSpace);
         getSpaces();
  } 

  else if (point()) {
        currentToken.setType(ttPoint);
        getChar();
    }
  else if (openPar()) {
            currentToken.setType(ttOpenPar);
            getChar();
      
  }
  else if (closePar()) {
            currentToken.setType(ttClosePar);
            getChar();
      
  }
  else if (comma()){
        currentToken.setType(ttComma);
        getChar();
  }
  else if(comments()) {
      currentToken.setType(ttComments);
      currentToken.setPrecedence(900);
      currentToken.setLine(line);
      currentToken.setColumn(column);
      currentToken.setId(getComments());
  }
  else if(iscotes()) {
            currentToken.setType(ttQuotedSymbol);
            currentToken.setId(getQuotedSymbol());
            getChar();
}
 

else if (isOrdinary()) {
            currentToken.setType(ttOrdinarySymbol);
            currentToken.setId(getOrdinarySymbol()); //pode ser uma variável, uma constante, um inteiro, uma palavra reservada ou ser o que é, apenas um símbolo
            if (reserverWord(currentToken));
            else if(integer(currentToken)); //pode ser um inteiro
            else if(_operator(currentToken)); //pode ser um operador
            else if (isStartVariable(currentToken.getId().at(0))) currentToken.setType(ttVariable); //pode ser uma variável
            else if (isStartConstant(currentToken.getId().at(0))) currentToken.setType(ttConstant);
            else currentToken.setType(ttFunction); //é uma função
}

else if(isSpecialSymbol()) {
            currentToken.setType(ttSpecialSymbol); //daqui pode sair um operador ou nada
            currentToken.setId(getSpecialSymbol());
            _operator(currentToken);//pode ser um operador
        
}
else getChar();
return currentToken; 
}


bool TLexer::_operator(TP9Token &t) const {
    listNode *aux;
    aux=builtIn.getTokenById(t.getId());
    if (aux) {t.setType(aux->data.getType());return true;}
    return false;
}    
    
    


bool TLexer::integer(TP9Token &t) const{
   uint i;
   bool x=true;
   for (i=0; i<t.getId().length() && x ;i++)
       if ( !isdigit(t.getId().at(i)) ) x=false;
   if(x) {t.setType(ttInteger); return true; }
   return false;
}


bool TLexer::reserverWord(TP9Token &t)  {
    t.setArity(1); //by default
    //Palavras reservadas
    if (t.getId()=="formulas") {currentToken.setType(ttFormulas); return true;}
    if (t.getId()=="sos") {currentToken.setType(ttSos);return true;}
    if (t.getId()=="goals") {currentToken.setType(ttGoals);return true;}
    if (t.getId()=="assumptions") {currentToken.setType(ttAssumptions);return true;}
    if (t.getId()=="hints") {currentToken.setType(ttHints);return true;}
    
    if (t.getId()=="assign") { currentToken.setType(ttAssign); currentToken.setArity(2); return true;}
    if (t.getId()=="max_seconds") {currentToken.setType(ttMaxSeconds);return true;}
    if (t.getId()=="new_constants") {currentToken.setType(ttNewConstants);return true;}
    if (t.getId()=="max_weight") {currentToken.setType(ttMaxWeight);return true;}
    if (t.getId()=="set") {currentToken.setType(ttSet);return true;}
    if (t.getId()=="clear") {currentToken.setType(ttClear);return true;}
    if (t.getId()=="restrict_denials") {currentToken.setType(ttRestricDenials);return true;}
    if (t.getId()=="label") {currentToken.setType(ttLabel);return true;}
    if (t.getId()=="answer") {currentToken.setType(ttAnswer);return true;}
    if (t.getId()=="action") {currentToken.setType(ttAction);return true;}
    if (t.getId()=="busb_hint_wt") {currentToken.setType(ttBusbHintWt);return true;}
    if (t.getId()=="end_of_list") {currentToken.setType(ttEndOfList);return true;}
    if (t.getId()=="start_size") {currentToken.setType(ttStartSize);return true;}
    if (t.getId()=="end_size") {currentToken.setType(ttEndSize);return true;}
    if (t.getId()=="increment") {currentToken.setType(ttIncrement);return true;}
    if (t.getId()=="iterate") {currentToken.setType(ttIterate);return true;}
    if (t.getId()=="max_models") {currentToken.setType(ttMaxModels);return true;}
    if (t.getId()=="max_seconds_per") {currentToken.setType(ttMaxSecondsPer);return true;}
    if (t.getId()=="domain_size") {currentToken.setType(ttDomainSize);return true;}
    if (t.getId()=="max_megs") {currentToken.setType(ttMaxMegs);return true;}
    if (t.getId()=="infix") 	{currentToken.setType(ttInfixName);return true;}
    if (t.getId()=="infix_left") {currentToken.setType(ttInfixLeftName);return true;}
    if (t.getId()=="infix_right") {currentToken.setType(ttInfixRightName);return true;}
    if (t.getId()=="prefix")	{currentToken.setType(ttPrefixName);return true;}
    if (t.getId()=="prefix_paren")	{currentToken.setType(ttPrefixParenName);return true;}
    if (t.getId()=="postfix")	{currentToken.setType(ttPostfixName);return true;}
    if (t.getId()=="postfix_paren") {currentToken.setType(ttPostfixParenName);return true;}
    if (t.getId()=="ordinary")	{currentToken.setType(ttOrdinaryName);return true;}
    if (t.getId()=="op") {currentToken.setType(ttOp);return true;}
    if (t.getId()=="prolog_style_variables") {currentToken.setType(ttPrologStyleVariables);return true;}
    if (t.getId()=="redeclare") {currentToken.setType(ttRedeclare);return true;}
    if (t.getId()=="true")	{currentToken.setType(ttTrueName);return true;}
    if (t.getId()=="false") {currentToken.setType(ttFalseName);return true;}
    if (t.getId()=="negation")	{currentToken.setType(ttNegationName);return true;}
    if (t.getId()=="disjunction") {currentToken.setType(ttDisjunctionName);return true;}
    if (t.getId()=="conjunction") {currentToken.setType(ttConjunctionName);return true;}
    if (t.getId()=="implication") {currentToken.setType(ttImplicationName);	return true;}
    if (t.getId()=="backward_implication")	{currentToken.setType(ttBackwardImplicationName);return true;}
    if (t.getId()=="equivalence")	{currentToken.setType(ttEquivalenceName);return true;}
    if (t.getId()=="universal_quantification")  	{currentToken.setType(ttUniversalQuantificationName);	return true;}
    if (t.getId()=="existential_quantification") 	{currentToken.setType(ttExistentialQuantificationName);return true;}	
    if (t.getId()=="equality")		{currentToken.setType(ttEqualityName);return true;}
    if (t.getId()=="negated_equality")	{currentToken.setType(ttNegatedEqualityName);	return true;}
    if (t.getId()=="attribute")		{currentToken.setType(ttAttributeName);return true;}
    if (t.getId()=="false_part")		{currentToken.setType(ttFalse_Part);return true;}
    if (t.getId()=="true_part")		{currentToken.setType(ttTrue_Part);return true;}
    if (t.getId()=="order")		    {currentToken.setType(ttOrder);return true;}     
    if (t.getId()=="eq_defs")		{currentToken.setType(ttEq_defs);return true;}   
    if (t.getId() == "if" )         {currentToken.setType(ttIf); return true;}
    if (t.getId() == "end_if" )         {currentToken.setType(ttEndIf); return true;}
    if (t.getId() == "age_part" )         {currentToken.setType(ttAge_part); return true;}
    if (t.getId() == "weight_part" )         {currentToken.setType(ttWeight_part); return true;}
    if (t.getId() == "literal_selection" )         {currentToken.setType(ttLiteral_selection); return true;}
    if (t.getId() == "backsub_check" )         {currentToken.setType(ttBacksub_check); return true;}
    if (t.getId() == "sos_limit" )         {currentToken.setType(ttSos_limit); return true;}
    if (t.getId() == "fold_denial_max" )         {currentToken.setType(ttFold_denial_max); return true;}
    if (t.getId() == "sk_constant_weight" )         {currentToken.setType(ttSk_constant_weight); return true;}
    if (t.getId() == "prop_atom_weight" )         {currentToken.setType(ttProp_atom_weight); return true;}
    if (t.getId() == "skolem_penalty" )         {currentToken.setType(ttSkolem_penalty); return true;}
    if (t.getId() == "nest_penalty" )         {currentToken.setType(ttNest_penalty); return true;}
    if (t.getId() == "stats" )         {currentToken.setType(ttStats); return true;}
    if (t.getId() == "eval_limit")    {currentToken.setType(ttEval_limit); return true;}
    if (t.getId() == "function_order") { currentToken.setType(ttFunction_order); return true;}
    
    
    
    return false;
}
   

bool TLexer::isSpecialSymbol(void) const {
    
    return (    lastChar=='-'  ||
                lastChar=='|'  ||
                lastChar=='&'  ||
                lastChar=='<'  ||
                lastChar=='>'  ||
                lastChar=='+'  ||
                lastChar=='='  ||
                lastChar=='*'  ||
                lastChar=='@'  ||
                lastChar=='^'  ||
                lastChar=='\'' || //apóstrofo
                lastChar=='!'  ||
                lastChar=='\\' ||
                lastChar=='/'  ||
                lastChar=='#' 
        );


}

string TLexer::getQuotedSymbol(void) {
 string idlex;
 do {
      if (!iscotes()) idlex=idlex+lastChar;
      getChar();
    } while (!eof() && !eol() && !iscotes());
    return idlex;
}


string TLexer::getOrdinarySymbol(void) {
    string idlex;
    do {
          idlex=idlex+lastChar;
          getChar();
        
    } while(!eof() && !eol() && isOrdinary());
   return idlex; 
}

string TLexer::getSpecialSymbol(void) {
string idlex;
bool stop=false;
do {
    idlex=idlex + lastChar;
    getChar();
    if(idlex=="==") stop=true;
    if(idlex=="!=") stop=true;
    if(idlex=="->") stop=true;
    if(idlex=="<-" && lastChar!='>') stop=true;
    if(idlex=="<->") stop=true;
    if(idlex=="-" && lastChar!='>') stop=true;
    if(idlex=="|") stop=true;
    if(idlex=="'") stop=true;
    if(idlex=="+") stop=true;
    if(idlex=="*") stop=true;            
    if(idlex=="#") stop=true;
    if(idlex=="&") stop=true;
    if(idlex=="@") stop=true;
    if(idlex=="\\") stop=true;
 } while( !stop && !eol() && !eof() && isSpecialSymbol() );
  
 return idlex;   
    
}

bool TLexer::isStartVariable(void) const {
if (!prologStyle) {
        return (lastChar>='u' && lastChar<='z') || underScore() ;
    }
 else return (lastChar>='A' && lastChar<='Z');   
}


bool TLexer::isStartVariable(char c) const {
if (!prologStyle) {
        return (c>='u' && c<='z') || c=='_' ;
    }
else return (c>='A' && c<='Z');
}




bool TLexer::isStartConstant(char c) const {
    if(!prologStyle)
        return  ( c=='$' || c=='0' || c=='1' || (c>='a' && c<='e') || c=='H' || c=='K' || c=='M');
    else return (c>='a' && c<='z');
}





string TLexer::getVariable(void) {
    string idlex;
    if ( eof() || eol() || (!lowerCase()) ) return idlex ;
    if (!prologStyle) {
            do {
                idlex=idlex + lastChar;
                getChar();
        }while (!eof() && !eol() && (lowerCase()||underScore()) );
    }
   return idlex;
}


bool TLexer::getPrologStyle(void) const {
        return prologStyle;
}


bool TLexer::isStartConstant(void) const {
    if (!prologStyle) return  ( lastChar=='$' || lastChar=='0' || lastChar=='1' || (lastChar>='a' && lastChar<='e') || lastChar=='H' || lastChar=='K'|| lastChar=='M' );
    else return (lastChar>='a' && lastChar<='z');
}

bool TLexer::isLowerCase(void) const {
    return (lastChar>='a' && lastChar<='z');
}

bool TLexer::isDigit(void) const {
    return (lastChar>='0' && lastChar<='9');
    
}

bool TLexer::isUnderScore(void) const {
    return lastChar=='_';
    
}

string TLexer::getConstant(void) {
string idlex;
bool stop=false;
if ( eof() || eol() || (!lowerCase()) ) return idlex ;
do {
     idlex=idlex + lastChar;
     if (idlex==TRUE || idlex==FALSE) stop=true;
     getChar();
 }while (!stop && !eof() && !eol() && ( upperCase() || lowerCase() || digit() ) );

return idlex;
    
}

void TLexer::getSpaces(void) {
    do {
            getChar();
        
    } while (space());
}


string TLexer::getComments(void) {
 string idlex;
 do {
     idlex=idlex + lastChar;
     getChar();
 } while (!_eol && !_eof && !_cret);
 return idlex;
}


void TLexer::redeclare(TP9TokenType &t, string &s) {
listNode *aux;
    if ( (aux=builtIn.getByType(t))!=NULL) 
     if(aux->data.getType()!=ttEqual)
            aux->data.setId(s);
}
   
void TLexer::redefine(const TP9TokenType &t, const TP9NotationType &nt, const uint &p) {
    listNode *aux;
    if( (aux=builtIn.getByType(t))!=NULL ) {
        aux->data.setNotation(nt);
        aux->data.setPrecedence(p);
    }
}


TList  TLexer::getBuiltIn(void) const {
    return builtIn;
}

bool TLexer::eof(void) const{
    return _eof;
}

bool TLexer::eol(void) const{
        return _eol;
}

bool TLexer::fok(void)const {
        return _fok;
}


TLexer::~TLexer() {
    iniFile.close();
}



