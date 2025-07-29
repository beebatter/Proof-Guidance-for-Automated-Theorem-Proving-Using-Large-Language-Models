#ifndef _config_h
#define _config_h


class TConfig {
					private:
								string configFilePath;  //path para o ficheiro de configuração
								string inFilePath;		//path para o ficheiro de origem
								string errorFilePath;	//path para o ficheiro de erros
								
								string eProverPath;		 //path do executável E
								string waldmeisterPath;  //path do executável waldmeisterPath
								string prover9Path;		 //path do executável do prover9	
								string vampirePath;		 //path do executável do vampire
								string minionPath;		 //path do executável do minion	
								string mace4Path;        //path do executável do mace4   
								string PXPath;
                                string MXPath;

								
								ifstream configFile;	//o ficheiro de configuração	
								
								//variáveis que têm o estado da configuração
								bool _isDebugEnabled;	
								bool _isWaldMeisterEnabled;
								bool _isEProverEnabled;
								bool _isProver9Enabled;
								bool _isVampireEnabled;
								bool _isMinionEnabled;
                                bool _isMace4Enabled;
                                bool _isPXEnabled;
                                bool _isMXEnabled;
                                
                                bool _onlyProver9_Mace4;
                                
                                
                                
								
                                string getTag(string,string);
                                
					public:
								TConfig(char *);//cria a configuração a partir do ficheiro aqui passado
								~TConfig(void); //destroy a classe Tconfig
								
								string getInFilePath(void) 	 	 const;
								string getErrorFilePath(void) 	 const;
								string getEProverPath(void) 	 const;
								string getWaldmeisterPath(void)  const;
								string getProver9Path(void) 	 const;
								string getVampirePath(void) 	 const;
								string getMinionPath(void) 		 const;
                                string getMace4Path(void) 		 const;
                                string getPXPath(void) 		 const;
                                string getMXPath(void) 		 const;
								
								bool isDebugEnabled(void)		 const;
								bool isEProverEnabled(void)	 	 const;
								bool isWaldmeisterEnabled(void)  const;
								bool isProver9Enabled(void)	 	 const;
								bool isVampireEnabled(void)	 	 const;
								bool isMinionEnabled(void)		 const;
								bool isMace4Enabled(void)		 const;
								bool isPXEnabled(void)           const;
                                bool isMXEnabled(void)           const;
                                
                                
                                void setOnlyProver9_Mace4(void);
                                bool onlyProver9_Mace4(void);
	
};



#endif
