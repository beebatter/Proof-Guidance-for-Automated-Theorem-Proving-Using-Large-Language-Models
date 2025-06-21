#ifndef _LAUNCHER_CPP
#define _LAUNCHER_CPP

#include "lancher.h"
#include <string.h>
#include <iostream>
#include <fcntl.h>
#include <unistd.h>
#include <pthread.h>
#include "global.h"

using namespace std;

// YVES: replaced getFileName by getFileName

const char *getFileName(const char *path)
{
    const char *base = strrchr(path, '/');
    return base ? base+1 : path;
}

string vr="-\\|/-";
int vrpos=0;
bool thRun=0;
                                
void * TLauncher::screenEffect(void *ptr) {
 
    while(true) { 
     if(thRun) {   
            if(vrpos==vr.length()) vrpos=0;
            mycout(vr.at(vrpos));
            vrpos++;
            mycout("\b");
     }
  }
}


TLauncher::TLauncher(void) {
 indexComando=0;
 vrpos=0;
 terminationType=noProved;
 for(int i=0; i<MAX_COMMANDS; i++) comandos[i].pid=-1;
 
}


void TLauncher::killAll() {

    for (int i=0; i<MAX_COMMANDS; i++) 
        if(comandos[i].pid!=-1) {
            kill(comandos[i].pid, SIGKILL);
            comandos[i].pid=-1;
        }
}


TLauncher::~TLauncher(){
    killAll();
}


bool TLauncher::fileExists(const string name) {
 return access(name.c_str(), 0 ) == 0;
}

bool TLauncher::setCommand(const string exe, const string in, const string out, const int wc, const uint t, const uint m, const TProverType pt){
    if (indexComando<MAX_COMMANDS) {
        if (!fileExists(exe)) 
            return false;//se não achou o provador então azar
        
        comandos[indexComando].executavel=exe;
        comandos[indexComando].inFile=in;
        comandos[indexComando].outFile=out;
        comandos[indexComando].wantedCode=wc;
        comandos[indexComando].active=false;
        comandos[indexComando].time=t;
        comandos[indexComando].memory=m;
        comandos[indexComando].proverType=pt;
        indexComando++;
        return true;
    } else return false;
    
}

pthread_t TLauncher::getEffectsThread(void) const {
    return effectsThread;
}


TTerminationType TLauncher::getTerminationType(void) const {
    return terminationType;
}



bool TLauncher::alguemTerminou(pid_t retpid, TComando comando, int status) {

     if  (
         retpid==comando.pid && (comando.wantedCode==(status)  || //isto termina se o retpid for o do comando que estamos a analizar e se o wantedcode do comando for o status
         (status==0 && !strcmp(getFileName(comando.executavel.c_str()),"vampire"))  ) //ou no caso do vampire pode terminar por ser o wanted code ou por ser o 0 ou outros exit codes que ainda não sei
     ) return true; 
                    
     else return 0;
}

void TLauncher::runAndWait(const uint mode) {
pid_t pid;
int status;
bool stop=false;
pthread_t thread;

//criação da thread de efeitos
mycout("\x1B[32m");
// effectsThread=pthread_create(&thread, NULL, screenEffect, (void *) NULL);
// YVES:
effectsThread=pthread_t(pthread_create(&thread, NULL, screenEffect, (void *) NULL));

if (mode==FIRST) mycout("\nRUNNING PROVERS IN FIRST MODE\n");
if (mode==ALL) mycout("\nRUNNING PROVERS IN ALL MODE\n");



for (int n=0;n<indexComando;n++) {
        if((pid=fork())==0) {
            //LANÇAMENTO DOS PROVADORES
            
            mycout("LAUNCHING ");
            mycout(getFileName(comandos[n].executavel.c_str()));
            mycout(" ");
            mycout(comandos[n].inFile); mycout(" > ");
            mycout(comandos[n].outFile); mycout(" Pid: ");
            mycout(my_to_string(getpid())); mycout("\n");
            comandos[n].pid=getpid();
            int fd=open(comandos[n].outFile.c_str() ,O_RDWR|O_CREAT,0666);
            dup2(fd,STDOUT_FILENO);
            close(fd);
           
   
            
            //não esquecer que o ficheiro de output está redireccionado anteriormente
            switch(comandos[n].proverType) {
            
                case prover9:
                case mace4: 
                case PX:
                case MX:    
                                    execl(comandos[n].executavel.c_str(), getFileName(comandos[n].executavel.c_str()), "-f" , comandos[n].inFile.c_str(), NULL);
                                    break;
                    
                    
                case waldmeister:   //sem parâmetros
                                    execl(comandos[n].executavel.c_str(), getFileName(comandos[n].executavel.c_str()),comandos[n].inFile.c_str() , NULL); 
                                    break;  
                
                case eprover:       //com parâmetros fixos
                                    execl(comandos[n].executavel.c_str(), getFileName(comandos[n].executavel.c_str()) ,"--memory-limit=Auto", comandos[n].inFile.c_str() , NULL); 
                                    break;
                                
                case vampire:       //com parâmetros fixos e apenas com o tempo variável
                                    execl(comandos[n].executavel.c_str(), getFileName(comandos[n].executavel.c_str()),"-t", my_to_string(comandos[n].time).c_str(),"-m",my_to_string(comandos[n].memory).c_str(),comandos[n].inFile.c_str(),NULL); 
                                    break;
                
            }
            
            exit(0);
        }
        else {      //processos pais
                    comandos[n].pid=pid; //os processos pais guardam na estrutura os pids dos filhos
                    comandos[n].active=true; //e marcam a estrutura dos processos como estando activos, running
              }
    }



   if (mode==FIRST) { //ESPERAR PELO PRIMEIRO QUE TERMINE COM CÓDIGO BOM
            while (!stop) { //ESPERAR PELO PRIMEIRO QUE TERMINE COM EXIST STATUS BOM
                 
                thRun=true;
                 for(int n=0;n<indexComando;n++) {
                    pid_t retpid=waitpid(comandos[n].pid,&status,WNOHANG);
                    if(retpid==comandos[n].pid ) {  //o chato do prover9 pode sair com 7 com sucesso
                        if (comandos[n].active) 
                            thRun=false;
                            mycout("END ");
                            mycout(getFileName(comandos[n].executavel.c_str()));
                            mycout(" ");
                            mycout(comandos[n].inFile);
                            mycout(" > ");
                            mycout(comandos[n].outFile);
                            mycout( "Pid: ");
                            mycout(my_to_string(comandos[n].pid));
                            mycout(" with status: ");
                            mycout(my_to_string(status));
                            mycout("\n");
                            thRun=true;
                        comandos[n].active=false;  //MARCA-O COMO MORTO
                     }
                     
                     
                     
                     if (alguemTerminou(retpid, comandos[n], status)) {
                     
                    
                                //mete o pid respectivo =0 porque terminou
                                for (int i=0;i<indexComando;i++) { //MATA OS OUTROS TODOS
                                   if(comandos[i].pid!=retpid) { 
                                     thRun=false;
                                     mycout("KILL ");
                                     mycout(getFileName(comandos[i].executavel.c_str()));
                                     mycout(" ");
                                     mycout(comandos[i].inFile);
                                     mycout(" > ");
                                     mycout(comandos[i].outFile);
                                     mycout(" Pid: ");
                                     mycout(my_to_string(comandos[i].pid));
                                     mycout("\n");
                                     thRun=true;
                                     kill(comandos[i].pid,SIGKILL);
                                     comandos[i].active=false;
                                     //e mete o pid respectivo =0 porque foram mortos
                                   } 
                                }
                               
                                //ver quem terminou correctamente para se devolver o estado no terminationType
                                if (!strcmp(getFileName(comandos[n].executavel.c_str()),"prover9")) terminationType=prover9Proved;  
                                if (!strcmp(getFileName(comandos[n].executavel.c_str()),"eprover")) terminationType=eproverProved;
                                if (!strcmp(getFileName(comandos[n].executavel.c_str()),"waldmeister")) terminationType=waldmeisterProved;
                                if (!strcmp(getFileName(comandos[n].executavel.c_str()),"vampire")) terminationType=vampireProved;
                                if (!strcmp(getFileName(comandos[n].executavel.c_str()),"mace4")) terminationType=mace4Refused;
                                if (!strcmp(getFileName(comandos[n].executavel.c_str()),"PX")) terminationType=PXProved;
                                if (!strcmp(getFileName(comandos[n].executavel.c_str()),"MX")) terminationType=MXRefused;
                                
                                
                                mycout("\nEND PROVERS WORK IN FIRST MODE BECAUSE ");
                                mycout(getFileName(comandos[n].executavel.c_str()));
                                mycout(" ENDED CORRECTLY: ");
                                mycout("Please check ");
                                mycout(comandos[n].outFile);
                                mycout("\n");
                                if (getEffectsThread()) pthread_cancel(getEffectsThread());
                                stop=true;
                             
                    }
             
                  }
                  //OU ESPERAR QUE TODOS TEMRINEM INDEPENDENTEMENTE DO EXIT STATUS BOM OU NÃO
                  if(!stop) {
                    int contaMortos=0;
                    for (int j=0; j<indexComando; j++) 
                        if(!comandos[j].active) contaMortos++;
                    if (contaMortos==indexComando) {
                        thRun=false;
                        mycout("\nEND PROVERS WORK IN FIRST MODE BECAUSE ALL PROVERS ENDED INCORRECTLY\n");
                        terminationType=noProved;
                        stop=true;
                    }
                  }
          
            } // FIM DO STOP
      thRun=false; 
      if (getEffectsThread()) pthread_cancel(getEffectsThread());
          
    } //fim do modo FIRST

  
  
    if (mode==ALL) {
      thRun=true;
      for(int n=0;n<indexComando;n++) { //cada pai fica à espera do seu filho
          waitpid(-1, &status, 0); 
      }
      if (getEffectsThread()) pthread_cancel(getEffectsThread());
      mycout("END PROVERS WORK IN ALL MODE BECAUSE ALL PROVERS ENDED\n");
    }

    mycout("\x1B[0m");
   
}









#endif
