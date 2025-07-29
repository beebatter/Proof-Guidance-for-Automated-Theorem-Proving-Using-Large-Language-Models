#ifndef _waldmeister_h
#define _waldmeister_h

#include <iostream>

#define LPO 1

#define _TRUE "true"
#define _FALSE "false"

class TWaldmeister {

  
private:
	    ofstream outFile; 	//output file for waldemeiter to prove
	    bool debug;			//if true, output to screen as well
	    bool fileOK;		//if system could open the output file
	    string fname;			//initial function name
	    int fnumber;
	    bool tradError;
        ulong lastPrintedComment;
	    bool equationError;
	    
	    
        
        
	    void variables(listNode *); 	//list with all variables found in prover9 file
	    void ordering(listNode *);		//to output the ordering declaration in waldmeister file	
	    void signature(listNode *);		//tou output the signature declaration in wlademeister file
        void signatureGoalsVarsToConst(listNode *); //a signatura das variávies dos goals que foram traduzidas
	    void equation(treeNode *,const TList &, const TList &); //to outupt the equations declaration in waldemeister file
        void equationGoals(treeNode *r, const TList &f, const TList &c, const TList &);
	    void orignalNames(const TList &, const bool &);
	    string incFname(void);
  
public:
	    
	  
  
	    TWaldmeister ();
	    TWaldmeister (char *, bool); //constructor with output file name and debug option
	    ~TWaldmeister();
	    void init(char *, bool); //para iniciar depois
	    
	    void signature (const TList &, const TList &, const TList &); //creates signature on output file. The inuput is a list of functions
	    
	    void ordering(const TList &, const TList &, const TList &);	//creates the ordering signature on output file. The input is a list of functions
	    
	    bool variables(const TList &); //creates the variables declaration on the output File. The input is the list of variables
	    void incLevel();
	    void equationTile() ;
	    void conclusionTile() ;
	    void equation(const TTree &,const TList &, const TList &); //Creates the equations declaration on the output file. The input is a tree wih the equation and a list of functions to translate
        void equationGoals(const TTree &,const TList &, const TList &, const TList &); //cria as euqções de conjecturas....temos de entrar com as variáveis dos goals
	    void write(string );
	    void newLine(void);
	    void equal(void);
	    
	    bool getTradError(void)const ;
	    void printComments(TList &c, ulong,ulong);
        uint getLastPrintedComment(void) const;
        bool getEquationError(void) const;
        void tradGoalsVarstoConsts(TList &);
        void clearFile(char *);
   
	
	    
};





#endif
