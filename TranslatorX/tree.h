#ifndef _tree_h
#define _tree_h

#define COUNT 5

#include "P9Token.h"

using namespace std;

struct treeNode {
                    TP9Token data;
                    //pointers: left pointe, right pointer, parameters pointer and next parameter pointer
                    treeNode *left, *right,*parameters, *nextParameter;
                    
};


class TTree {
    
          
                        

                        public :    TTree();    //initializes root noda as NULL
                                    ~TTree();  //destroy a lista
                                    static treeNode * newNode(const TP9Token &); //creates a new node
                                    treeNode * getRoot(void) const; //returns tree root
                                    friend ostream& operator <<(ostream& osObject, const TTree& l); //print tree nodes
                                    
                                    void insertLeft(const TP9Token &);
                                    void insertRight(const TP9Token &);
                                    string findTrad (const string) const;
                                    bool isEmpty(void) const;
                                    void setRoot(treeNode *);
                                    void setEquality(bool);
                                    bool isEquality(void) ;
                                  
                                    
                                
                                    
                        
                        private:    
                                    treeNode *root;
                                    std::ostream&  print(std::ostream & out, treeNode *t) const ;
                                    std::ostream&  print(std::ostream & out) const;
                                    std::ostream&  printParameters(std::ostream & out, treeNode *t) const;
                                    void destroy(treeNode *);
                                    void destroyNextParameter(treeNode *);
                                    void destroyPar(treeNode *); 
                                    string findTrad(const string, const treeNode *t) const;
                                    bool equality;
                                   
};



#endif
