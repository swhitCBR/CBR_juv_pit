  #include <TMB.hpp>
  template<class Type>
  Type objective_function<Type>::operator() (){

//////////////////
// LOADING DATA //
//////////////////

  DATA_VECTOR( cell_vals );

  ////////////////////////////////////////////////////
  // INITIALIZING PARAMETERS (Must be real numbers) //
  ////////////////////////////////////////////////////

  PARAMETER (p1); 
  PARAMETER (s1); 
  PARAMETER (lambda); 

  ////////////////////////////
  // INTERMEDIATE VARIABLES //
  ////////////////////////////

	int n_cells = cell_vals.size();	
	vector<Type> cell_probs(n_cells);
    Type nll = 0;

	cell_probs[0] = s1*p1*lambda;
	cell_probs[1] = s1*p1*(1-lambda);
	cell_probs[2] = s1*(1-p1)*lambda;
	cell_probs[3] = (1-s1) + s1*(1-p1)*(1-lambda);

  //////////////////
  /// LIKELIHOOD ///
  //////////////////
 
  nll =  -dmultinom(cell_vals,cell_probs,1);

  return nll;
  
  };
