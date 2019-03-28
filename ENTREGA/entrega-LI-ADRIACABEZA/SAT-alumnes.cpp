#include <iostream>
#include <stdlib.h>
#include <algorithm>
#include <vector>
using namespace std;

#define UNDEF -1
#define TRUE 1
#define FALSE 0




/*Tècnica VSIDS
 * Heuristica: ordenes segons el nombre d'aparicions, sumes puntuació a les que han tingut conflicte
 * i cada x iteracions ho divideixes per donar-li importància als conflictes més recents
*/


const double MULT = 0.75;     
const int TIME_UPDATE = 1000;
const int INCREMENT = 2;

uint numVars;
uint numClauses;
vector<vector<int> > clauses;
vector<int> model;
vector<int> modelStack;
uint indexOfNextLitToPropagate;
uint conflictCnt;
//int propagationCnt;  
uint decisionCnt;
uint decisionLevel;
vector<vector<int> > taulaAux;
vector<int> heuristic;
    

inline void rescale() {
  for (uint i=1; i<=numVars; ++i) heuristic[i] *= MULT;
}

inline void updateLiteralHeuristics(int index){
  vector<int> literalClause = clauses[index];
  int size = literalClause.size();

  for(int i = 0; i < size; ++i){
    int lit = literalClause[i];
    if(lit<0) heuristic[-lit] += INCREMENT;
    else heuristic[lit] += INCREMENT; 
  }
}


inline void readClauses( ){
    
  // Skip comments
  char c = cin.get();
  while (c == 'c') {
    while (c != '\n') c = cin.get();
    c = cin.get();
  }  
  // Read "cnf numVars numClauses"
  string aux;
  cin >> aux >> numVars >> numClauses;
  taulaAux.resize(numVars*2+1);
  heuristic.resize(numVars+1,0.0);
  clauses.resize(numClauses);  
  // Read clauses
  for (uint i = 0; i < numClauses; ++i) {
    int lit;
    while (cin >> lit and lit != 0) {
        clauses[i].push_back(lit);
        if(lit < 0) {
          taulaAux[-lit+numVars].push_back(i);
          heuristic[-lit] += INCREMENT;
        }
        else {
          taulaAux[lit].push_back(i);
          heuristic[lit] += INCREMENT;
        }
    }
  }
}



inline int currentValueInModel(int lit){
  if (lit >= 0) return model[lit];
  else {
    if (model[-lit] == UNDEF) return UNDEF;
    else return 1 - model[-lit];
  }
}


inline void setLiteralToTrue(int lit){
  modelStack.push_back(lit);
  if (lit > 0) model[lit] = TRUE;
  else model[-lit] = FALSE;		
}

bool propagateGivesConflict ( ) {
  while ( indexOfNextLitToPropagate < modelStack.size() ) {
         
    int lit = modelStack[indexOfNextLitToPropagate];
    if (lit > 0) lit += numVars;
    else lit = -lit;
    
    ++indexOfNextLitToPropagate; 
    for (uint i = 0; i < taulaAux[lit].size(); ++i) {
        int literal_aux = taulaAux[lit][i]; 
      bool someLitTrue = false;
      int numUndefs = 0;
      int lastLitUndef = 0;
     
      for (uint k = 0; not someLitTrue and k < clauses[literal_aux].size(); ++k){
        int val = currentValueInModel(clauses[literal_aux][k]);
        if (val == TRUE) someLitTrue = true;
        else if (val == UNDEF){ 
          ++numUndefs; 
          lastLitUndef = clauses[literal_aux][k]; 
        }
      }


        if (not someLitTrue and numUndefs == 0) {
             ++conflictCnt;
             if(conflictCnt%TIME_UPDATE == 0) rescale();
             updateLiteralHeuristics(literal_aux);
            return 1; // conflict! all lits false
        }
        else if (not someLitTrue and numUndefs == 1) setLiteralToTrue(lastLitUndef);	
    }    
    // ++propagationCnt;
    }
  return 0;
}


inline void backtrack(){
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
  inline int getNextDecisionLiteral(){
    int max_score = 0;
    int index_max = 0;
    for (uint i = 1; i <= numVars; ++i) {
      if (model[i] == UNDEF) {
          if(heuristic[i] > max_score){
            max_score = heuristic[i];
            index_max = i;
          }
          
      }
    }
    ++decisionCnt;
   return index_max; // returns 0 when all literals are defined
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
inline void print_info(){
  cout << decisionCnt << " decisions"<< endl;
  // cout << "Propagations: " << propagationCnt << endl;
  // cout << "Conflicts:    " << conflictCnt << endl;
}

int main(){ 
  readClauses(); // reads numVars, numClauses and clauses
  model.resize(numVars+1,UNDEF);
  indexOfNextLitToPropagate = 0;  
  decisionLevel = 0;
  conflictCnt = 0; 
  //propagationCnt = 0;
  decisionCnt = 0;
  
  // Take care of initial unit clauses, if any
  for (uint i = 0; i < numClauses; ++i)
    if (clauses[i].size() == 1) {
      int lit = clauses[i][0];
      int val = currentValueInModel(lit);
      if (val == FALSE) {cout << "UNSATISFIABLE" << endl; print_info(); return 10;}
      else if (val == UNDEF) setLiteralToTrue(lit);
    }
  
  // DPLL algorithm
  while (true) {
    while ( propagateGivesConflict() ) {
      if ( decisionLevel == 0) { cout << "UNSATISFIABLE" << endl; print_info(); return 10; }
      backtrack();
    }
    int decisionLit = getNextDecisionLiteral();
    if (decisionLit == 0) { checkmodel(); cout << "SATISFIABLE" << endl; print_info();  return 20; }
    // start new decision level:
    modelStack.push_back(0);  // push mark indicating new DL
    ++indexOfNextLitToPropagate;
    ++decisionLevel; 
    setLiteralToTrue(decisionLit);    // now push decisionLit on top of the mark
  }
}
