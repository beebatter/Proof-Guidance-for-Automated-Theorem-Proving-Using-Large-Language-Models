
#ifndef _launcher_h
#define _launcher_h

#include <unistd.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <string.h>
#include <pthread.h>
#include <string>

#define MAX_COMMANDS 10

#define FIRST 0
#define ALL 1

using namespace std;

enum TTerminationType {badUsage,sintaticError,prover9Proved, eproverProved, vampireProved, waldmeisterProved, mace4Refused, noProved, PXProved, MXRefused}; 


enum TTranslationOnlyTermination {Translation_OK=0,  Translation_General_Error=-1};


enum TProverType  {prover9, mace4, waldmeister, eprover, vampire,PX,MX};

typedef struct {
                   
                    int resultCode;
                    int wantedCode;
                    bool active;
                    uint time;
                    uint memory;
                   
                    
                    
                    
                    //nova versão   
                    string executavel; //executável
                 //   string parameter; //-f por exemplo, ou vazio
                    string inFile;
                    string outFile;
                    pid_t pid;
                    TProverType proverType;
            
                    
                   
                
    
}TComando;


class TLauncher {
                    private:
                                TComando comandos[MAX_COMMANDS];
                                pthread_t tid[MAX_COMMANDS];
                                string mode; //modo all ou first...termina quando todos terminarem ou termina quando o primeiro terminar sem erro
                                int indexComando; //próximo comando a lançar
                                pthread_t effectsThread;
                                TTerminationType terminationType;
                                bool fileExists(const string);    
                                static void *screenEffect(void *);
                                
                                bool alguemTerminou(pid_t, TComando , int );
                    
                    public:
                                TLauncher(void);
                                ~TLauncher();
                               
                               
                                void killAll();
                                bool setCommand(const string , const string , const string, const int, const uint, const uint, const TProverType);
                                
                                void runAndWait(const uint);
                                pthread_t getEffectsThread(void) const;
                                TTerminationType getTerminationType(void) const;
                             
};



#endif
