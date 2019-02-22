#include <iostream>
#include <stdlib.h>
#include <algorithm>
#include <vector>
using namespace std;

#define UNDEF -1
#define TRUE 1
#define FALSE 0

uint numVars;
uint numClauses;
vector<vector<int> > clauses;
vector<int> model;
vector<int> modelStack;
uint indexOfNextLitToPropagate;
uint decisionLevel;
vector<vector<int> > taulaAux;
vector<pair<int,int>> heuristic;

int calculaPos(int lit){
    int resultat = 0;
    if(lit < 0){
        resultat = 2*((-lit)-1)+1;
    }
    else
        resultat = 2*(lit-1);
    
    return resultat;
}

//inversa de calculaPos
int getPos(int i){
    int lit = ((i/2)+1);;
    if(i%2){
        return lit;        
    }
    else{
        return -lit;
    }
}




void readClauses( ){
    
  // Skip comments
  char c = cin.get();
  while (c == 'c') {
    while (c != '\n') c = cin.get();
    c = cin.get();
  }  
  // Read "cnf numVars numClauses"
  string aux;
  cin >> aux >> numVars >> numClauses;
  clauses.resize(numClauses);  
  // Read clauses
  for (uint i = 0; i < numClauses; ++i) {

    int lit;
    while (cin >> lit and lit != 0) {
        clauses[i].push_back(lit);
        taulaAux[calculaPos(lit)].push_back(lit);
    }
  }
  
}



int currentValueInModel(int lit){
  if (lit >= 0) return model[lit];
  else {
    if (model[-lit] == UNDEF) return UNDEF;
    else return 1 - model[-lit];
  }
}


void setLiteralToTrue(int lit){
  modelStack.push_back(lit);
  if (lit > 0) model[lit] = TRUE;
  else model[-lit] = FALSE;		
}

// COMENTARI PROFE vector<int>& clausestovisit = lit<0?posList[-lit]:negList[lit];

bool propagateGivesConflict ( ) {
  while ( indexOfNextLitToPropagate < modelStack.size() ) {
    int pos = calculaPos(indexOfNextLitToPropagate);
    for (uint i = 0; i < taulaAux[pos].size(); ++i) {
      bool someLitTrue = false;
      int numUndefs = 0;
      int lastLitUndef = 0;
      
      for (uint k = 0; not someLitTrue and k < clauses[taulaAux[pos][i]].size(); ++k){
        int val = currentValueInModel(clauses[taulaAux[pos][i]][k]);
        if (val == TRUE) someLitTrue = true;
        else if (val == UNDEF){ ++numUndefs; lastLitUndef = clauses[taulaAux[pos][i]][k]; }
        }
        if (not someLitTrue and numUndefs == 0) return true; // conflict! all lits false
        else if (not someLitTrue and numUndefs == 1) setLiteralToTrue(lastLitUndef);	
    }    
  
      ++indexOfNextLitToPropagate; 
    }
  return false;
}

/*Tècnica VSIDS
 * Heuristica: ordenes segons el nombre d'aparicions, sumes puntuació a les que han tingut conflicte
 * i cada x iteracions ho divideixes per donar-li importància als conflictes més recents
*/

void backtrack(){
  uint i = modelStack.size() -1;
  int lit = 0;
  while (modelStack[i] != 0){ // 0 is the DL mark
    lit = modelStack[i];
    model[abs(lit)] = UNDEF;
    modelStack.pop_back();
    --i;
  }
  // at this point, lit is the last decision
  modelStack.pop_back(); // remove the DL mark
  --decisionLevel;
  indexOfNextLitToPropagate = modelStack.size();
  setLiteralToTrue(-lit);  // reverse last decision
}

// Heuristic for finding the next decision literal:
int getNextDecisionLiteral(){
    heuristic.resize(numVars);
    for(int i = 0; i < taulaAux.size(); ++i){
        heuristic.push_back(make_pair(getPos(i),taulaAux[i].size()));        
    }
    heuristic.sort();
  for (uint i = 1; i <= heuristic.size(); ++i) // stupid heuristic:
    if (model[heuristic[i]] == UNDEF) return i;  // returns first UNDEF var, positively
  return 0; // reurns 0 when all literals are defined
}

void checkmodel(){
  for (uint i = 0; i < numClauses; ++i){
    bool someTrue = false;
    for (uint j = 0; not someTrue and j < clauses[i].size(); ++j)
      someTrue = (currentValueInModel(clauses[i][j]) == TRUE);
    if (not someTrue) {
      cout << "Error in model, clause is not satisfied:";
      for (uint j = 0; j < clauses[i].size(); ++j) cout << clauses[i][j] << " ";
      cout << endl;
      exit(1);
    }
  }  
}

int main(){ 
  readClauses(); // reads numVars, numClauses and clauses
  model.resize(numVars+1,UNDEF);
  indexOfNextLitToPropagate = 0;  
  decisionLevel = 0;
  
  // Take care of initial unit clauses, if any
  for (uint i = 0; i < numClauses; ++i)
    if (clauses[i].size() == 1) {
      int lit = clauses[i][0];
      int val = currentValueInModel(lit);
      if (val == FALSE) {cout << "UNSATISFIABLE" << endl; return 10;}
      else if (val == UNDEF) setLiteralToTrue(lit);
    }
  
  // DPLL algorithm
  while (true) {
    while ( propagateGivesConflict() ) {
      if ( decisionLevel == 0) { cout << "UNSATISFIABLE" << endl; return 10; }
      backtrack();
    }
    int decisionLit = getNextDecisionLiteral();
    if (decisionLit == 0) { checkmodel(); cout << "SATISFIABLE" << endl; return 20; }
    // start new decision level:
    modelStack.push_back(0);  // push mark indicating new DL
    ++indexOfNextLitToPropagate;
    ++decisionLevel; 
    setLiteralToTrue(decisionLit);    // now push decisionLit on top of the mark
  }
}  
//un cop ja no tinc cap conflicte doncs he d'agafar del model stack quines son indefinides fent servir la heurística
