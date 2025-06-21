#ifndef _global_h
#define _global_h
#include <string.h>
#include <iostream>
#include <stdlib.h>

//Definições gerais

#define VERSAO "V1.2 - 22-01-2023"





using namespace std;

typedef unsigned char _byte;
typedef unsigned int  uint;


bool gdebug;

void mycout(string s) {
    if(gdebug) cout<<s;
}

void mycout(char c) {
if(gdebug) cout<<c;
}


void removeQuotes(string &s) {
   string aux="";
   for(int i=0; i<s.length(); i++)
       if (s.at(i)!='"') aux+=s.at(i);
   s=aux;    
}


string my_to_string(int val) {
    char buffer[10];
    sprintf(buffer,"%d",val);
    return string(buffer);
}

int my_stoi(string s) {
    return atoi(s.c_str());
    
}

#endif
