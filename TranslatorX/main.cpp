
//Ficheiro main.cpp
#include <iostream>
#include "P9Token.cpp"
#include "list.cpp"
#include "lexer.cpp"
#include "tree.cpp"
#include "sintatic.cpp"
#include "config.cpp"
#include "lancher.cpp"
#include "waldmeister.cpp"
#include "tptp.cpp"
#include "global.h"



#include <stdlib.h> 


#include "dirent.h"

#include <string.h>

using namespace std;

TLauncher launcher; //o launcher 

string error1;
 string command;



void print_banner(void) {
    mycout("******************************************************************************\n");
    mycout("*********Universidade Aberta - Doutoramento em Álgebra Computacional**********\n");
    mycout("*****************************TranslatorX**************************************\n");
    mycout("*******************************");
    mycout(VERSAO);
    mycout(" *****************************\n");
    mycout("******************************************************************************\n\n\n");
}


void general_error() {

    cout<<"General error"<<endl<<endl<<endl;
    
}

void killAll() {
      launcher.killAll();
}


void sig_handler(int sn) {
  
    if(sn==SIGTERM) {
        killAll();
        exit(0);
    }
    
    if(sn==SIGSEGV) {
        general_error();
        cout << command << " " << error1<<endl;
        exit(-1);
    }
    
}



void testar_directoria(string d) {
DIR *dir;
struct dirent *ent;


string directorie=d;


long line1 , column1;
 int contador=0;   
 

if ((dir = opendir (directorie.c_str())) != NULL) {

 ent=readdir(dir);   
 ent=readdir(dir);   
 
  while ((ent = readdir (dir)) != NULL) {
     
     
      string par;
      
      for (int i=0; i< strlen(ent->d_name); i++)
          par+= ent->d_name[i];
      
      
      command=directorie + par;
      
      TSyntatic s(command.c_str(), "error.txt",true); //creating syntatic --- passing inputfile, errorFile, and debug option
      
      if (!s.run(error1, line1,column1)) {
           
          
           cout << "ERRO NO FICHEIRO " << command << " " <<  error1 << endl;
           exit(0);
      }
      else {
      
                cout << command << " OK " << endl;
      }
        contador++;
    }
  
  
  
  cout << "Foram processados " << contador << " ficheiros" << endl;
  closedir (dir);
  exit(0);
} 
    
    
    
}


int main(int argc, char **argv) {
    
    enum TWaldmeisterError {Waldmeister_OK=0, Waldemeister_NOT_EQUALITY=1, Waldmeister_TOO_MANY_GOALS=2, Waldemeister_EXISTENTIAL_VARIABLES=3, Waldemeister_EQUATION_ERROR=4};
    
    
    TWaldmeisterError waldmeisterError=Waldmeister_OK;
    bool tptpError=false;
    //bool minionError=false;
    uint fcount;

    
    signal(SIGTERM, sig_handler);
    signal(SIGSEGV, sig_handler);
  
    



    
    
    if(argc<1) {
        cout<<"Usage : TranslatorX <config_file>"<<endl;
        cout<<"Or"<<endl;
        cout<<"Usage : TranslatorX -t <prover9_file>"<<endl;
        exit(0);
    }
    
    else if(argc==3) {
      
        if(strcmp(argv[1],"-t")) {
            cout<<"Usage : TranslatorX -t <prover9_file>"<<endl;
            exit(0);
        } 
        
        
        
        /********************************************TRADUTOR APENAS******************************************************************/
        else { //tradutor apenas
        
            gdebug=true; //no tradutor apenas temos sempre debug
            
            print_banner();
            
            
            
            TSyntatic s(argv[2], "error.txt",true); //creating syntatic --- passing inputfile, errorFile, and debug option
            _byte status=s.status();
            
            if (status==IN_FILE_ERROR) {
               cout<<"Input file not defined"<<endl;
               exit(badUsage);   
            }
            
            string in_file(argv[2]);
            
            //Primeiro passo - iniciar a análise sintatica
            mycout("Starting syntactic analysis\n");
            //Começar por identificar formulas(assumptions)
            long line, column;
            string error;
            
            
            if (s.run(error, line,column)) { //se sair sem erros, s contem todas as estruturas para os tradutores
                mycout("Stoping syntactic analysis with success\n"); 
            
                
            } else  {
                
                s.errorMessage(error,line,column);
                exit(TTranslationOnlyTermination::Translation_General_Error);
                
            }
            
            s.getFunctionsList().sort(); //pode-se começar por ordenar por ariedade a lista de funções
            
            
            mycout("-------------------------------Waldmeister Translation-------------------");
            
            
            string wald_file=in_file;
            wald_file.append(".wald");
            
            waldmeisterError=TWaldmeisterError::Waldmeister_OK;
            
            TWaldmeister waldmeister((char *) wald_file.c_str() ,true);
           
            waldmeister.tradGoalsVarstoConsts(s.getwaldGoalsConst());
            s.resetComments(); //os comentários podem-se impirmir novamente
            waldmeister.signature(s.getFunctionsList(), s.getConstantsList(), s.getwaldGoalsConst()); //e usar tb a lista waldConstGoals para escrever a signature
            waldmeister.ordering(s.getFunctionsList(), s.getConstantsList(), s.getwaldGoalsConst());
            
            //waldmeisterError=waldmeister.variables(s.getVariablesList());       //se houverem variáveis existenciais dá erro...pode ser
            
            if(waldmeister.variables(s.getVariablesList())) waldmeisterError=TWaldmeisterError::Waldemeister_EXISTENTIAL_VARIABLES;
            
            waldmeister.equationTile();
     
            for (fcount=0; fcount<s.getFacount() && waldmeisterError==Waldmeister_OK ; fcount++) {
            
            
             if (s.getFormulasAssumptions(fcount).isEquality()) {
                 waldmeister.equation(s.getFormulasAssumptions(fcount), s.getFunctionsList(), s.getConstantsList());
                 if (waldmeister.getEquationError()) waldmeisterError=TWaldmeisterError::Waldemeister_EQUATION_ERROR;
                 waldmeister.printComments(s.getCommentsList(),waldmeister.getLastPrintedComment(), s.getFormulasAssumptions(fcount).getRoot()->data.getLine());
                 waldmeister.newLine();
                 waldmeister.incLevel(); 
 
             }
             else waldmeisterError=TWaldmeisterError::Waldemeister_NOT_EQUALITY;
            }

            if(waldmeisterError==TWaldmeisterError::Waldmeister_OK) {
             waldmeister.printComments(s.getCommentsList(), waldmeister.getLastPrintedComment(), s.getGoalsLine());
             waldmeister.newLine();
            
             waldmeister.conclusionTile();
             for (fcount=0; fcount<s.getGoalsCount() && waldmeisterError==Waldmeister_OK; fcount++) {
            
                if (s.getFormulasGoals(fcount).isEquality()  ) {
                    waldmeister.equationGoals(s.getFormulasGoals(fcount), s.getFunctionsList(),s.getConstantsList(), s.getwaldGoalsConst());
                    if (waldmeister.getEquationError()) waldmeisterError=TWaldmeisterError::Waldemeister_EQUATION_ERROR;
                    
                    waldmeister.printComments(s.getCommentsList(),waldmeister.getLastPrintedComment(), s.getFormulasGoals(fcount).getRoot()->data.getLine());
                    
                    waldmeister.newLine();
                    waldmeister.incLevel();
                }
                else waldmeisterError=TWaldmeisterError::Waldemeister_NOT_EQUALITY;
            }
            waldmeister.printComments(s.getCommentsList(),waldmeister.getLastPrintedComment(), -1); 
           } //fim do Waldemeister
          
          
          if(waldmeisterError!=TWaldmeisterError::Waldmeister_OK) {
              mycout("Cannot translate to Waldmeister....");
              
           
              
              switch ((int) waldmeisterError) {
              
                  case Waldemeister_NOT_EQUALITY:
                       
                                                    mycout("Waldemeister equations must be equality ones!\n\n");
                                                    break;
                                                    
                  case Waldmeister_TOO_MANY_GOALS:
                       
                                                    mycout("Waldemeister only allow one goal!\n\n");
                                                    break;
                       
                  case Waldemeister_EXISTENTIAL_VARIABLES:
                       
                                                    mycout("Waldemeister does not allow existential variables!\n\n");
                                                    break;     
                  
                   case Waldemeister_EQUATION_ERROR:
                       
                                                    mycout("Waldemeister does not support this type opf equation!\n\n");
                                                    break;     
            }
              
              
              
              waldmeister.clearFile((char *) wald_file.c_str());
            }
          
          
          
          
          cout<<endl<<endl; 
           
          mycout("-------------------------------TPTP Translation-------------------"); 
          //para tptp 
          string tptp_file=in_file;
          tptp_file.append(".tptp");
          Tptp tptp((char *) tptp_file.c_str(), true);
          s.resetComments();
          tptp.printComments(s.getCommentsList(),1, s.getAssumptionsLine() );
          tptp.tradFunctions(s.getFunctionsList()); 
          tptp.tradVariables(s.getVariablesList());
          tptp.tradConstants(s.getConstantsList());
       
          if(tptp.getTradError()) {
            tptpError=true;
          }
          else 
          { //go axioms and conjectures
                for (fcount=0; fcount<s.getFacount() && !tptpError; fcount++) {
                        tptp.axioms(s.getFormulasAssumptions(fcount),s.getFunctionsList(), s.getVariablesList(), s.getConstantsList(),s.getAssumptionsQuantifiers(fcount));
                        tptp.printComments(s.getCommentsList(), tptp.getLastPrintedComment(), s.getFormulasAssumptions(fcount).getRoot()->data.getLine());
                        tptp.newLine();
                }
                
                for (fcount=0; fcount<s.getGoalsCount() && !tptpError; fcount++){
                        tptp.conjectures(s.getFormulasGoals(fcount),s.getFunctionsList(), s.getVariablesList(), s.getConstantsList(), s.getGoalsQuantifiers(fcount));
                        tptp.printComments(s.getCommentsList(), tptp.getLastPrintedComment(), s.getFormulasGoals(fcount).getRoot()->data.getLine());
                        tptp.newLine();
                }
                tptp.printComments(s.getCommentsList(), tptp.getLastPrintedComment(), -1);
         }

         
         //correu tudo bem
         exit(TTranslationOnlyTermination::Translation_OK);
        }
        /*********************************************Tradutor Apenas*****************************************************************/
        
        
        
        
    }    
    
    TConfig cfg(argv[1]); //read configuration file with debug enabled or not
    gdebug=cfg.isDebugEnabled();
    
   
    print_banner();


    TSyntatic s(cfg.getInFilePath().c_str(), cfg.getErrorFilePath().c_str(),cfg.isDebugEnabled()); //creating syntatic --- passing inputfile, errorFile, and debug option
    
    _byte status=s.status();
    
     if(status==ERROR_FILE_ERROR) {
                                    cout <<"Error file not defined, error.txt created"<<endl; 
                                    s.setErrorFile("error.txt");
                                    }
                                   
                                    
      else if (status==IN_FILE_ERROR) {
               cout<<"Input file not defined"<<endl;
               exit(badUsage);   
      }
    
    

    
    //Primeiro passo - iniciar a análise sintatica
    mycout("Starting syntactic analysis\n");
    //Começar por identificar formulas(assumptions)
    long line, column;
    string error;
    if (!s.run(error, line,column)) { //se sair sem erros, s contem todas as estruturas para os tradutores
        //try to make with prover9
        cout<<"Syntactic analysis with errors....try only prover 9 and mace 4!"<<endl;
        cfg.setOnlyProver9_Mace4(); //if there is an error on translatoion try only with prover9
    }
    else mycout("Stoping syntactic analysis with success\n"); 
   
    
    s.getFunctionsList().sort(); //pode-se começar por ordenar por ariedade a lista de funções

    
    
    //waldmeister
    
    waldmeisterError=TWaldmeisterError::Waldmeister_OK;
    if(cfg.isWaldmeisterEnabled() && !cfg.onlyProver9_Mace4()) {
    TWaldmeister waldmeister((char *) cfg.getInFilePath().append(".wald").c_str() , cfg.isDebugEnabled());
        
        //ao que parece o waldmeister apenas aceita constantes nos getFormulasGoals
        //logo, já se implementou uma lista de variáveis dos goals no sintatico - waldGoalsConst
        //agora peagmos nessa lista que sai do sintatico e aplicamos a tradução
        waldmeister.tradGoalsVarstoConsts(s.getwaldGoalsConst());
    
    
        s.resetComments(); //os comentários podem-se impirmir novamente
        
        waldmeister.signature(s.getFunctionsList(), s.getConstantsList(), s.getwaldGoalsConst()); //e usar tb a lista waldConstGoals para escrever a signature
        
        
        waldmeister.printComments(s.getCommentsList(),1,s.getAssumptionsLine());//imprimir os comentários até aqui...antes dos formulas assumptions
        waldmeister.ordering(s.getFunctionsList(), s.getConstantsList(), s.getwaldGoalsConst());
        
        //waldmeisterError=waldmeister.variables(s.getVariablesList());       //se houverem variáveis existenciais dá erro...pode ser
        
        if(waldmeister.variables(s.getVariablesList())) waldmeisterError=TWaldmeisterError::Waldemeister_EXISTENTIAL_VARIABLES;
        
        waldmeister.equationTile();
     
        for (fcount=0; fcount<s.getFacount() && waldmeisterError==Waldmeister_OK ; fcount++) {
            
            
            if (s.getFormulasAssumptions(fcount).isEquality()) {
                 waldmeister.equation(s.getFormulasAssumptions(fcount), s.getFunctionsList(), s.getConstantsList());
                 if (waldmeister.getEquationError()) waldmeisterError=TWaldmeisterError::Waldemeister_EQUATION_ERROR;
                 waldmeister.printComments(s.getCommentsList(),waldmeister.getLastPrintedComment(), s.getFormulasAssumptions(fcount).getRoot()->data.getLine());
                 waldmeister.newLine();
                 waldmeister.incLevel(); 
 
            }
            else waldmeisterError=waldmeisterError=TWaldmeisterError::Waldemeister_NOT_EQUALITY;
        }

        
        
        if( waldmeisterError==Waldmeister_OK) {
            waldmeister.printComments(s.getCommentsList(), waldmeister.getLastPrintedComment(), s.getGoalsLine());
            waldmeister.newLine();
            
            waldmeister.conclusionTile();
            for (fcount=0; fcount<s.getGoalsCount() && waldmeisterError==Waldmeister_OK ; fcount++) {
            
                if (s.getFormulasGoals(fcount).isEquality()  ) {
                    waldmeister.equationGoals(s.getFormulasGoals(fcount), s.getFunctionsList(),s.getConstantsList(), s.getwaldGoalsConst());
                    if (waldmeister.getEquationError()) waldmeisterError=TWaldmeisterError::Waldemeister_EQUATION_ERROR;
                    waldmeister.printComments(s.getCommentsList(),waldmeister.getLastPrintedComment(), s.getFormulasGoals(fcount).getRoot()->data.getLine());
                    waldmeister.newLine();
                    waldmeister.incLevel();
                }
                else waldmeisterError=waldmeisterError=TWaldmeisterError::Waldemeister_NOT_EQUALITY;
            }
        
        
            waldmeister.printComments(s.getCommentsList(),waldmeister.getLastPrintedComment(), -1); 
        }
        
       
       
                
          if(waldmeisterError!=TWaldmeisterError::Waldmeister_OK) {
              mycout("Cannot translate to Waldmeister....");
              
           
              
              switch ((int) waldmeisterError) {
              
                  case Waldemeister_NOT_EQUALITY:
                       
                                                    mycout("Waldemeister equations must be equality ones!\n\n");
                                                    break;
                                                    
                  case Waldmeister_TOO_MANY_GOALS:
                       
                                                    mycout("Waldemeister only allow one goal!\n\n");
                                                    break;
                       
                  case Waldemeister_EXISTENTIAL_VARIABLES:
                       
                                                    mycout("Waldemeister does not allow existential variables!\n\n");
                                                    break;     
                  
                   case Waldemeister_EQUATION_ERROR:
                       
                                                    mycout("Waldemeister does not support this type opf equation!\n\n");
                                                    break;     
            }
              
              
              
             
            }

    }
    
     
    
    //para tptp ---------------------------------------------------------------------------------
    if ((cfg.isEProverEnabled() || cfg.isVampireEnabled()) && !cfg.onlyProver9_Mace4()) {
        Tptp tptp((char *) cfg.getInFilePath().append(".tptp").c_str(), cfg.isDebugEnabled());
        s.resetComments();
        tptp.printComments(s.getCommentsList(),1, s.getAssumptionsLine() );
        tptp.tradFunctions(s.getFunctionsList()); 
        tptp.tradVariables(s.getVariablesList());
        tptp.tradConstants(s.getConstantsList());
       
        if(tptp.getTradError()) {
           tptpError=true;
        }
        else 
          { //go axioms and conjectures
                for (fcount=0; fcount<s.getFacount() && !tptpError; fcount++) {
                        tptp.axioms(s.getFormulasAssumptions(fcount),s.getFunctionsList(), s.getVariablesList(), s.getConstantsList(),s.getAssumptionsQuantifiers(fcount));
                        tptp.printComments(s.getCommentsList(), tptp.getLastPrintedComment(), s.getFormulasAssumptions(fcount).getRoot()->data.getLine());
                        tptp.newLine();
                }
                
                for (fcount=0; fcount<s.getGoalsCount() && !tptpError; fcount++){
                        tptp.conjectures(s.getFormulasGoals(fcount),s.getFunctionsList(), s.getVariablesList(), s.getConstantsList(), s.getGoalsQuantifiers(fcount));
                        tptp.printComments(s.getCommentsList(), tptp.getLastPrintedComment(), s.getFormulasGoals(fcount).getRoot()->data.getLine());
                        tptp.newLine();
                }
                tptp.printComments(s.getCommentsList(), tptp.getLastPrintedComment(), -1);
        }
    }// para tptp
   
   
    //Launch provers 
    
    
    if(cfg.onlyProver9_Mace4()) {
    
    if (cfg.isProver9Enabled() ) 
      if (!launcher.setCommand(cfg.getProver9Path() , cfg.getInFilePath(), cfg.getInFilePath()+".p9.out",0,0,0, prover9))
        mycout("Prover " + cfg.getProver9Path() +" not found\n");   
    
       if (cfg.isMace4Enabled()) 
         if(!launcher.setCommand(cfg.getMace4Path(), cfg.getInFilePath(), cfg.getInFilePath()+".mace4.out",0,0,0,mace4))
          mycout("Prover " + cfg.getMace4Path() +" not found\n");   
        
    
      if (cfg.isPXEnabled() ) 
      if (!launcher.setCommand(cfg.getPXPath() , cfg.getInFilePath(), cfg.getInFilePath()+".PX.out",0,0,0, PX))
        mycout("Prover " + cfg.getPXPath() +" not found\n");   
    
       if (cfg.isMXEnabled()) 
         if(!launcher.setCommand(cfg.getMXPath(), cfg.getInFilePath(), cfg.getInFilePath()+".MX.out",0,0,0,MX))
          mycout("Prover " + cfg.getMXPath() +" not found\n");    
    } 
    
    else {
   
    
    if (cfg.isProver9Enabled() ) 
      if (!launcher.setCommand(cfg.getProver9Path() , cfg.getInFilePath(), cfg.getInFilePath()+".p9.out",0,0,0, prover9))
        mycout("Prover " + cfg.getProver9Path() +" not found\n");   
    
  
    if (cfg.isWaldmeisterEnabled() && ! waldmeisterError)
      if(!launcher.setCommand(cfg.getWaldmeisterPath(), cfg.getInFilePath().append(".wald"), cfg.getInFilePath() + ".wald.out",0,0,0, waldmeister))
        mycout("Prover " + cfg.getWaldmeisterPath() +" not found\n");      
           
    
    if(s.getGoalsCount()==1) { //o vampire suporta apenas uma conjectura
    if (cfg.isVampireEnabled() && !tptpError) 
            if(!launcher.setCommand(cfg.getVampirePath(), cfg.getInFilePath().append(".tptp"), cfg.getInFilePath() + ".vamp.out",1,0,0,vampire))
               mycout("Prover " + cfg.getVampirePath() +" not found\n"); 
    } 
    else mycout("Vampire only supports one conjecture\n");

    if (cfg.isEProverEnabled() && !tptpError) 
           if(!launcher.setCommand(cfg.getEProverPath(),cfg.getInFilePath().append(".tptp"), cfg.getInFilePath() + ".e.out",0,0,0,eprover))
               mycout("Prover " + cfg.getEProverPath() +" not found\n");
    
    if (cfg.isMace4Enabled()) 
         if(!launcher.setCommand(cfg.getMace4Path(), cfg.getInFilePath(), cfg.getInFilePath()+".mace4.out",0,0,0,mace4))
          mycout("Prover " + cfg.getMace4Path() +" not found\n");
    
    if (cfg.isPXEnabled() ) 
      if (!launcher.setCommand(cfg.getPXPath() , cfg.getInFilePath(), cfg.getInFilePath()+".PX.out",0,0,0, PX))
        mycout("Prover " + cfg.getPXPath() +" not found\n");   
    
       if (cfg.isMXEnabled()) 
         if(!launcher.setCommand(cfg.getMXPath(), cfg.getInFilePath(), cfg.getInFilePath()+".MX.out",0,0,0,MX))
          mycout("Prover " + cfg.getMXPath() +" not found\n");    
         
         
    }     
  
   launcher.runAndWait(FIRST);

    switch(launcher.getTerminationType()) {
            case sintaticError: break;
            case badUsage:break;
            case eproverProved: mycout("Eprover proved the theorem");break;
            case prover9Proved: mycout("Prover9 proved the theorem");break;
            case vampireProved: mycout("Vampire proved the theorem");break;
            case waldmeisterProved: mycout("Waldmeister proved the theorem");break;
            case mace4Refused: mycout("Mace4 has found a counter exemple for the theorem");break;
            case noProved:mycout("The theorem has not been proved nor refused"); break;
            case PXProved:mycout("PX proved the theorem");break;
            case MXRefused:mycout("MX has found a counter exemple for the theorem");break;
        }
    killAll();
    mycout("\n\n\n");
    mycout("******************************************************************************\n");
    mycout("********Thanks for using. Please report bugs or doubts to Carlos Sousa********\n");
    mycout("*********************EMAIL: 801829@estudante.uab.pt***************************\n");
    mycout("******************************************************************************\n");
    _byte tt=launcher.getTerminationType(); //para ver o resultado na shell, após executar o comando fazer echo$?
    mycout("Exit status as int: "+my_to_string(tt));
    exit(tt);
    return tt;
}
