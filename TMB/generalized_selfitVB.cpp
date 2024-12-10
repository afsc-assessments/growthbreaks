// https://kaskr.github.io/adcomp/register_atomic_8cpp-example.html#a3
// Similar to example 'adaptive_integration' using CppAD Romberg integration. REGISTER_ATOMIC is used to reduce tape size.

#include <TMB.hpp>
template <class Type> Type selex(Type x){return 1/(1+exp( 52.976 - x)) ;} // I hardcoded the L50; replace with yours

// this generates the function which will be integrated
template<class Type>
struct univariate {
  Type Linf, k, Age, t0, Sigma; // variables in integrand,  
  Type operator() (Type u) {
    Type yfit = 0;
    Type selPred = 0;
    Type ans = 0;
    yfit = Linf*(1-exp(-k*(Age - t0))); // generate predicted length
    selPred = selex(yfit);
    // ans = selPred * exp(-(yfit - u)/(2*pow(Sigma*Age,2.0))); // u is what is plugged a and b
    ans = selPred * dnorm(yfit,u,Sigma,false); // just hardcode the individuals
    return ans;
  }
};

// the function mydenom will integrate the above function from 0 to 200; bounds including -INFINITY and INFINITY 
// are OK, but I needed to reduce runtime. The maximum observed size in my dataset was 160cm.
template<class Type>
vector<Type> mydenom(vector<Type> input) {
  Type Linf  = input[0], k  = input[1];         // Data & parameters
  Type Age = input[2],  t0 = input[3]; Type Sigma = input[4];       
  univariate<Type> f = {Linf, k, Age, t0, Sigma};
  Type a = 0, b = 200; // 
  vector<Type> res(1); // a place to store this output
  res[0] = romberg::integrate(f, a, b);
  return res;
}
REGISTER_ATOMIC(mydenom)
  template<class Type>
  Type objective_function<Type>::operator() ()
  {
    
    DATA_VECTOR(Length_cm); // observed lengths
    DATA_VECTOR(Age); // observed ages
    DATA_VECTOR(Sel); // selectivity at observed length
    DATA_IVECTOR(selType); // turns selectivity on/off for DFO data
    DATA_INTEGER(a2); // fixed, for reporing L1/L2
    
    PARAMETER_VECTOR(t0); // t0 can be negative
    
    PARAMETER(log_Sigma);
    Type Sigma = exp(log_Sigma); // use exponentiated here
    
    PARAMETER_VECTOR(log_Linf);
    vector<Type> Linf(nStrata); //nstrata
    Linf = exp(log_Linf);
    
    PARAMETER_VECTOR(log_k);
    vector<Type> k(nStrata);
    k = exp(log_k);  
    
    // things to calculate
    vector<Type> ypreds(Age.rows());
    Type selObs = 0.0;
    Type yfit = 0;
    Type ans = 0;
    
    Type numerator = 0.0; 
    Type denominator = 0.0; 
    
    
    for(int i=0; i < Age.rows(); i++) { // loop through rows of age data
      yfit = Linf(i)*(1-exp(-k(i)*(Age(i) - t0(i)))); 
      
      selObs = selex(Length_cm(i)); // selex of OBSERVED value
      numerator = selObs*dnorm(Length_cm(i),yfit,Sigma,true); // typical dnorm, sel obs is 1
      
      vector<Type> input(5);
      input << Linf(i), k(i), Age(i), t0(i), Sigma; // bundle the current estimates
      denominator = mydenom(input)[0]; // calc denominator based on curr estimates
      
      ans -= numerator-denominator;
      ypreds(i) = yfit; // store estimated length 
      
    } // end rows
    
    REPORT(ypreds);
    ADREPORT(t0);
    ADREPORT(k);
    ADREPORT(Linf);
    ADREPORT(Sigma);
    return(ans);
  }
  