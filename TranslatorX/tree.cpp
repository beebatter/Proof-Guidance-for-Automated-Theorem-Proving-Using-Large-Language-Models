#include "tree.h"



TTree::TTree(){
    root=NULL;
   
}


TTree ::~TTree() {
    destroy(root);
    root=NULL;
}


bool TTree::isEmpty(void) const {
    if(this->root==NULL) return true;
    else return false;
}

treeNode * TTree::newNode(const TP9Token &t) {
 treeNode * novo;
 novo=new(treeNode);
 novo->data=t;
 novo->left=novo->right=novo->nextParameter=novo->parameters=NULL;
 return novo;
}

treeNode * TTree::getRoot() const {
    return root;
}

void TTree::setRoot(treeNode *t) {
    root=t;
}


void TTree::insertLeft(const TP9Token &t) {
    treeNode *aux,*aux1;
    aux=newNode(t);
    if(!root) root=aux;
    else {
            aux1=root;
            while(aux1->left) aux1=aux1->left;
            aux1->left=aux;
    }
    
}

void TTree::insertRight(const TP9Token &t) {
    treeNode *aux, *aux1;    



    aux=newNode(t);
    if (!root) root=aux;
    else {
            aux1=root;
            while(aux1->right) aux1=aux1->right;
            aux1->right=aux;
    }
    
}


void TTree::destroyPar(treeNode *p) {
  if(!p) return;
  if(!p->nextParameter) delete(p);
  destroy(p->nextParameter);
}


void TTree::destroy(treeNode *t) {
    if(!t) return;
    destroy(t->left);
    destroy(t->right);
    destroyNextParameter(t->nextParameter);
    destroyPar(t->parameters);
    delete(t);
}


void TTree::destroyNextParameter(treeNode *t) {
    if(!t) return;
    if(!t->nextParameter) delete(t);
    else destroy(t->nextParameter);
}



string TTree::findTrad (const string s) const {
    return findTrad(s,root);
}


string TTree::findTrad(const string s, const treeNode* t) const {
    if(!t) return "";
    if (t->data.getId()==s) return t->data.getTrad();
    return findTrad(s,t->left);
    return findTrad(s,t->right);
}



std::ostream& TTree:: printParameters(std::ostream & out, treeNode *t) const {
    treeNode *aux;

    out<< t->data.getId()<<endl;
    aux=t->nextParameter;
    while (aux) {   
                    print(out,aux);
                    aux=aux->nextParameter;
    }
    out<<"End of parameters"<<endl;
    return out;
}

std::ostream& TTree:: print(std::ostream & out, treeNode *t) const {
    treeNode *aux,*aux1;
    if(!t) return out;    
    print(out,t->left);
    out << t->data << endl;
    //print parameters
    if (t->parameters) 
        printParameters(out,t->parameters);
    print(out,t->right);
   return out;
}

void TTree::setEquality(bool e) {
    equality=e;
}

bool TTree::isEquality(void)  {
    return equality;
}




std::ostream&  TTree::print(std::ostream & out) const {
    if (equality) out<< "Equality formula"<<endl;
    else out << "Non equality forumla"<<endl;
    print(out, root);
    return out;
}


ostream& operator <<(ostream& osObject, const TTree &t){
    return t.print(osObject); //just one line!
}

