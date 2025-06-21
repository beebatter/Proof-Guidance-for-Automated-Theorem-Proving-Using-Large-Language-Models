#ifndef _TPTP_H
#define _TPTP_H



class Tptp {
                   
        private:
                    ofstream outFile;
                    bool debug;
                    bool fileOK;
                    bool tradError;
                    uint lastPrintedComment;
                    
                    string axiomName;
                    uint axiomNumber;
                    
                    string conjectureName;
                    uint conjectureNumber;
                    
                    string functionName;
                    uint functionNumber;
                    
                    string variableName;
                    uint variableNumber;
                    string constName;
                    uint constNumber;
                    
                    
                    
                    void init(const char *, bool);
                    
                    void equation(treeNode *, const TList &, const TList &, const TList &); 
                    void equation(const TTree &,const TList &, const TList &,const TList &); 
                    void writeQuantifiers(const TList &);
                    
                    string setVarTrad(string);
                    string setConstTrad(void);
                    string incAxiomName(void);
                    string incConjectureName(void);
                    string setFunctionTrad(const string );
                    string tradConnectives(TP9Token);
                   
                    
        
        public:
                    Tptp(void);
                    Tptp(const char *, const bool);
                    ~Tptp(void);
                    void newLine(void);
                    
                    bool getTradError(void) const;
                    void write(const string &);
                    void printComments(TList &c, ulong, ulong);
                    uint getLastPrintedComment(void) const;
                    void axioms(const TTree &, const TList &, const TList &, const TList &, const TList &);
                    void conjectures(const TTree &, const TList &, const TList &, const TList &, const TList &);
                   
                    void tradFunctions(TList &);
                    void tradVariables(TList &);
                    void tradConstants(TList &);
                    
};


#endif
