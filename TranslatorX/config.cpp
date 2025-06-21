#include "config.h"

#include <fstream>
#include <iostream>
#include <stdio.h>
#include <unistd.h>

using namespace std;

TConfig::TConfig(char *cf) {
	string s,aux;
    char buf[100];
    std::size_t found;
    
    
    _isDebugEnabled=false;
	_isEProverEnabled=false;
	_isMinionEnabled=false;
	_isProver9Enabled=false;
	_isVampireEnabled=false;
	_isWaldMeisterEnabled=false;
    _isMace4Enabled=false;
	_onlyProver9_Mace4=false;
    _isPXEnabled=false;
    _isMXEnabled=false;
    
    
	errorFilePath.clear();
	inFilePath.clear();
	eProverPath.clear();
	waldmeisterPath.clear();
	prover9Path.clear();
	vampirePath.clear();
	minionPath.clear();
    PXPath.clear();
    MXPath.clear();    


	
	configFile.open(cf);

	if (!configFile) {
            cout << "Configuration file not found"<<endl;
            exit(0);
        
    }
	
	while (!configFile.eof()) {
            configFile>>s;
            //testar as várias linhas
  
           aux=getTag(s,"debug:");
           if (!aux.empty() && aux=="true") _isDebugEnabled=true;
          
           
           aux=getTag(s,"infile:");
           if(!aux.empty()) inFilePath=aux;
           
           aux=getTag(s,"errorfile:");
           if(!aux.empty()) errorFilePath=aux;
           
           aux=getTag(s,"waldmeisterpath:");
           if(!aux.empty()) {
               waldmeisterPath=aux;
               _isWaldMeisterEnabled=true;
            }
           


           aux=getTag(s,"eproverpath:");
           if(!aux.empty()) {
               eProverPath=aux;
               _isEProverEnabled=true;
            }
           
           aux=getTag(s,"vampirepath:");
           if(!aux.empty()) {
               vampirePath=aux;
               _isVampireEnabled=true;
            }
            
           aux=getTag(s,"prover9path:");
           if(!aux.empty()) {
               prover9Path=aux;
               _isProver9Enabled=true;
             
            }
            
            
            
           aux=getTag(s,"minionpath:");
           if(!aux.empty()) {
                minionPath=aux;
                _isMinionEnabled=true;
           }
    
           aux=getTag(s,"mace4path:");
           if(!aux.empty()) {
                mace4Path=aux;
                _isMace4Enabled=true;
             
           }
           
           aux=getTag(s,"PXpath:");
           if(!aux.empty()) {
                PXPath=aux;
                _isPXEnabled=true;
             
           }
           
           aux=getTag(s,"MXpath:");
           if(!aux.empty()) {
                MXPath=aux;
                _isMXEnabled=true;
             
           }
        
    }

    configFile.close();
}


string TConfig::getTag(string origem, string tag) {
    char buf[100];
    std::size_t found;
    if(origem.at(0)==';' || origem.at(0)=='%') return "";
    found=origem.find(tag);
    if (found != std::string::npos) {
        found=origem.copy(buf,90,tag.length());
        buf[found]='\0';
        return string(buf);
    }    
    else return "";  
}


TConfig::~TConfig(void) {
	
}
								
string TConfig::getInFilePath(void) 	 const {
	return inFilePath;
}
string TConfig::getErrorFilePath(void) 	 const {
	return errorFilePath;
}
string TConfig::getEProverPath(void) 	 const {
		return eProverPath;
}
string TConfig::getWaldmeisterPath(void)  const{
	return waldmeisterPath;
}
string TConfig::getProver9Path(void) 	 const{
	return prover9Path;
}
string TConfig::getVampirePath(void) 	 const{
	return vampirePath;
}
string TConfig::getMinionPath(void) 		 const{
	return minionPath;
}

string TConfig::getMace4Path(void) const {
    return mace4Path;
}


string TConfig::getMXPath(void) const {
    return MXPath;
}


string TConfig::getPXPath(void) const {
    return PXPath;
}


bool TConfig::isDebugEnabled(void)		 const{
	return _isDebugEnabled;
}
bool TConfig::isEProverEnabled(void)	 	 const{
	return _isEProverEnabled;
}
bool TConfig::isWaldmeisterEnabled(void)  const{
	return _isWaldMeisterEnabled;
}
bool TConfig::isProver9Enabled(void)	 	 const{
	return _isProver9Enabled;
}
bool TConfig::isVampireEnabled(void)	 	 const {
	return _isVampireEnabled;
}
bool TConfig::isMinionEnabled(void)		 const {
	return _isMinionEnabled;
}

bool TConfig::isMace4Enabled(void)	const {
    return _isMace4Enabled;
}

bool TConfig::isPXEnabled(void)	const {
    return _isPXEnabled;
}

bool TConfig::isMXEnabled(void)	const {
    return _isMXEnabled;
}


void TConfig::setOnlyProver9_Mace4(void) {
    _onlyProver9_Mace4=true;
    
}

bool TConfig::onlyProver9_Mace4() {
    return _onlyProver9_Mace4;
}




