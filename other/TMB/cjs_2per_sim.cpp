  #include <TMB.hpp>
  template<class Type>
  Type objective_function<Type>::operator() (){

//////////////////
// LOADING DATA //
//////////////////

  // DATA_VECTOR( cell_vals );
  DATA_SCALAR( R );
  DATA_VECTOR( x );

  ////////////////////////////////////////////////////
  // INITIALIZING PARAMETERS (Must be real numbers) //
  ////////////////////////////////////////////////////

  PARAMETER (P_lo); 
  PARAMETER (S_lo); 
  PARAMETER (lambda_lo); 

  ////////////////////////////////////////////////////
  // INITIALIZING PARAMETERS (Must be real numbers) //
  ////////////////////////////////////////////////////


	// int n_cells = cell_vals.size();	
	int n_cells = 4;	
	
	vector<Type> cell_probs(n_cells);
	// Type R = cell_vals.sum();

	// cell_vals[0] = R;
	// cell_vals[1] = 0'
	// cell_vals[2] = 0'
	// cell_vals[3] = 0'
	
	Type nll = 0;						// joint log likelihood (objective_function)
	Type P_prob = P_lo; 
	Type S_prob = S_lo; 
	Type lambda = lambda_lo; 

	cell_probs[0] = S_prob*P_prob*lambda;
	cell_probs[1] = S_prob*P_prob*(1-lambda);
	cell_probs[2] = S_prob*(1-P_prob)*lambda;
	cell_probs[3] = (1-S_prob) + S_prob*(1-P_prob)*(1-lambda);

  nll =  -dmultinom(x,cell_probs,1);



  SIMULATE {
	
   x[0]=rbinom(R,cell_probs[0]) ;
   x[1]=rbinom(R-x[0],cell_probs[1]/(1-cell_probs[0])) ;
   x[2]=rbinom(R-x[0]-x[1],cell_probs[2]/(1-(cell_probs[0] + cell_probs[1]))) ;
   x[3]=rbinom(R-x[0]-x[1]-x[2],cell_probs[3]/(1-(cell_probs[0] + cell_probs[1] + cell_probs[2]))) ;

    REPORT(x);          // Report the simulation
	
  }


  //////////////////
  /// LIKELIHOOD ///
  //////////////////
 
  // // DERIVED PARAMETERS (no SE ests)
  // REPORT(nll);
  // REPORT(P_prob);
  // REPORT(S_prob);
  // REPORT(lambda);

  // // DERIVED PARAMETER (with SE ests)
  // ADREPORT(P_prob);
  // ADREPORT(S_prob);
  // ADREPORT(lambda);
  
  // JOINT LIKELIHOOD SUMMATION

  return nll;
  
  };





