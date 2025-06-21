#include "list.h"




TList::TList() {
    head=NULL;    
}

listNode* TList::getHead(void)const {
return head;
    
}

//insere se não existir
TList & TList::operator+=(const TP9Token &t){
    return insert(t);
    //return insertNew(t);
}



TList & TList::insertNew(const TP9Token &t) {
listNode *n,*a;
   if (!findById(t.getId())) { 
        n=new(listNode);
        n->data=t;
        n->next=NULL;
        if (!head) head=n;
        else 
            { //if not found then insert
                a=head;
                while(a->next) a=a->next;
                a->next=n;
        }
   }
    return *this;
}

TList & TList::insert(const TP9Token &t) {
listNode *n,*a;        
        n=new(listNode);
        n->data=t;
        n->next=NULL;
        if (!head) head=n;
        else 
            { //if not found then insert
                a=head;
                while(a->next) a=a->next;
                a->next=n;
        }
        return *this;
}


uint TList::length(void) const {
uint dim=0;
listNode *aux=getHead();
while(aux) {
            dim++;
            aux=aux->next;
    }

 return dim;   
}

bool TList::findById(const string &s) const {
    listNode *aux=head;
    bool found=false;
    while(!found && aux!=NULL) 
       if(aux->data.getId()==s) found=true;
       else aux=aux->next;
    return found;
}




string TList::findTradById(const string &s) const {
    listNode *aux=head;
    bool found=false;
    while(!found && aux!=NULL)
       if(aux->data.getId()==s) found=true;
       else aux=aux->next;
    if(found) return aux->data.getTrad();
    else return "";
}


listNode * TList::getTokenById(const string &id) const {
    listNode *aux=head;
    bool found=false;
    while (!found && aux)
        if(aux->data.getId()==id) found=true;
        else aux=aux->next;
    if(found) return aux;
    else return NULL;
}

listNode * TList::getByType(const TP9TokenType &ty) const {
    listNode *aux=head;
    bool found=false;
    while(!found && aux)
       if(aux->data.getType()==ty) found=true;
       else aux=aux->next;
    if(found) return aux;
    else return NULL;
}

listNode * TList::getById(const string &s)const {
    listNode *aux=head;
    bool found=false;
    while(!found && aux)
       if(aux->data.getId()==s) found=true;
       else aux=aux->next;
    if(found) return aux;
    else return NULL;
}


void TList::sort(void){   //sort list with arity order type
    listNode *aux=getHead();
    bool swap;
    while(aux) {
        listNode *t=aux->next;
        while(t) {
              swap=false;
              if(aux->data.getArity()>t->data.getArity() ) swap=true; //if the tow of them are function and the first has greater arity, then swap the two
	          if (swap) {
                            TP9Token auxt;
                            auxt=aux->data;
                            aux->data=t->data;
                            t->data=auxt;
                        }
              t=t->next;
        }
    aux=aux->next;
  }
  
}
  
    

TList::~TList() {
    destroy(head);
    head=NULL;
}

void TList::destroy(listNode *h) {
   if(!h) return;
   if(!h->next) delete(h);
   else delete(h->next);
}



std::ostream&  TList::print(std::ostream & out) const {
        listNode *h=head;
        while (h) {
        	out<<h->data<<endl;
            h=h->next;
        }
    return out; //return so that op<< can be just one line!
}


ostream& operator <<(ostream& osObject, const TList& l)
{
    return l.print(osObject); //just one line!
}
