#ifndef _list_h
#define _list_h

#include "P9Token.h"

using namespace std;

struct listNode {
                    TP9Token data;
                    listNode *next;
};


class TList {

        public:
                TList(); //construtor
                ~TList(); //destrutor
                bool findById(const string &) const; //o id já existe na lista?
                
                listNode * getTokenById(const string &) const;
                listNode * getByType(const TP9TokenType &)const;
                listNode * getById(const string &)const;
                
                string findTradById(const string &) const; //devolve a tradução do id passado
                TList & operator+=(const TP9Token &); //insere novo
                friend ostream& operator <<(ostream& osObject, const TList& l); //imprime os membros da lista
                TList & insert(const TP9Token &);   
                listNode *getHead(void)const;
                TList & insertNew(const TP9Token &t);
                uint length(void) const ;
                void sort(void) ;  //sort list with arity order type
                
        private:
                    listNode *head;
                   
                    std::ostream&  print(std::ostream &) const ; //imprime a lista                    
                    void destroy(listNode *);
    
};



#endif
