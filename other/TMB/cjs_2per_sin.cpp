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

  PARAMETER (P_lo); 
  PARAMETER (S_lo); 
  PARAMETER (lambda_lo); 

  ////////////////////////////////////////////////////
  // INITIALIZING PARAMETERS (Must be real numbers) //
  ////////////////////////////////////////////////////


	int n_cells = cell_vals.size();	
	vector<Type> cell_probs(n_cells);

    Type nll = 0;						// joint log likelihood (objective_function)
	// Type P_prob = (1/(1+exp(-1*(P_lo)))); 
	// Type S_prob = (1/(1+exp(-1*(S_lo)))); 
	// Type lambda = (1/(1+exp(-1*(lambda_lo)))); 

	Type P_prob = (sin(P_lo)+1)/2; 
	Type S_prob = (sin(S_lo)+1)/2; 
	Type lambda = (sin(lambda_lo)+1)/2; 


	cell_probs[0] = S_prob*P_prob*lambda;
	cell_probs[1] = S_prob*P_prob*(1-lambda);
	cell_probs[2] = S_prob*(1-P_prob)*lambda;
	cell_probs[3] = (1-S_prob) + S_prob*(1-P_prob)*(1-lambda);

  //////////////////
  /// LIKELIHOOD ///
  //////////////////
 
  // DERIVED PARAMETERS (no SE ests)
  REPORT(nll);
  REPORT(P_prob);
  REPORT(S_prob);
  REPORT(lambda);

  // DERIVED PARAMETER (with SE ests)
  ADREPORT(P_prob);
  ADREPORT(S_prob);
  ADREPORT(lambda);
  
  // JOINT LIKELIHOOD SUMMATION
  nll =  -dmultinom(cell_vals,cell_probs,1);

  return nll;
  
  };
