#define TMB_LIB_INIT R_init_growthbreaks
// https://kaskr.github.io/adcomp/register_atomic_8cpp-example.html#a3
// Similar to example 'adaptive_integration' using CppAD Romberg integration. REGISTER_ATOMIC is used to reduce tape size.
// #include <TMB.hpp> // per https://github.com/mlysy/TMBtools this shouldn't be here
template <class Type> Type selex(Type x){return 1/(1+exp( 52.976 - x)) ;}


template<class Type>
struct univariate {
  Type Linf, k, Age, t0, Sigma; // variables in integrand,
  Type operator() (Type u) {
    Type yfit = 0;
    Type selPred = 0;
    Type ans = 0;
    yfit = Linf*(1-exp(-k*(Age - t0))); // generate predicted length
    selPred = selex(yfit); //1.0 for testing
    // ans = selPred * exp(-(yfit - u)/(2*pow(Sigma*Age,2.0))); // u is what is plugged a and b
    ans = selPred * dnorm(yfit,u,Sigma,false); // just hardcode the individuals
    return ans;
  }
};

template<class Type>
vector<Type> mydenom(vector<Type> input) {
  Type Linf  = input[0], k  = input[1];         // Data
  Type Age = input[2],  t0 = input[3]; Type Sigma = input[4];         // Parameters
  univariate<Type> f = {Linf, k, Age, t0, Sigma};
  Type a = 0, b = 200; // max obs was 160
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
    DATA_IVECTOR(DES); // to organize data
    DATA_INTEGER(nStrata);
    DATA_INTEGER(a2); // fixed, for reporing L1/L2

    PARAMETER_VECTOR(t0); // t0 can be negative

    PARAMETER_VECTOR(log_Sigma);
    vector<Type> Sigma(nStrata); //nstrata
    Sigma = exp(log_Sigma);
    // Type Sigma = exp(log_Sigma); // use exponentiated here

    PARAMETER_VECTOR(log_Linf);
    vector<Type> Linf(nStrata); //nstrata
    Linf = exp(log_Linf);

    PARAMETER_VECTOR(log_k);
    vector<Type> k(nStrata);
    k = exp(log_k);

    // things to calculate
    vector<Type> ypreds(Age.rows());
    // Type selPred = 0.0;
    Type selObs = 0.0;
    Type yfit = 0;
    Type ans = 0;
    // Type tiny = 0.0; // Set to 1e-12 for robustness

    Type numerator = 0.0; // will be the same for everyone
    Type denominator = 0.0;

    vector<Type> L1(nStrata);
    vector<Type> L2(nStrata);

    for(int i=0; i < Age.rows(); i++) {
      yfit = Linf(DES(i))*(1-exp(-k(DES(i))*(Age(i) - t0(DES(i)))));


      if(selType(i) != 2){
        selObs = 1.0; // coerce to 1.0 for non DFO regions
        numerator = selObs*dnorm(Length_cm(i),yfit,Sigma(DES(i)),false) ; // typical dnorm, sel obs is 1
        denominator = 1.0;
      }
      if(selType(i) == 2){
        selObs = selex(Length_cm(i)); // selex of OBSERVED value
        numerator = selObs*dnorm(Length_cm(i),yfit,Sigma(DES(i)),false); // typical dnorm, sel obs is 1

        vector<Type> input(5);
        input << Linf(DES(i)), k(DES(i)), Age(DES(i)), t0(DES(i)), Sigma(DES(i)); // bundle the current estimates
        denominator = mydenom(input)[0]; // calc denominator based on curr estimates

        // ans -= log( mydenom(input)[0] + tiny );
      }
      ans -= log(numerator + 1e-5)-log(denominator + 1e-5); // will just be numerator for denom = 1
      ypreds(i) = yfit; // store estimated length


      L1(DES(i)) =  Linf(DES(i))*(1-exp(-k(DES(i))*(4.0 - t0(DES(i))))); // change 0, 0.5
      L2(DES(i))  =  Linf(DES(i))*(1-exp(-k(DES(i))*(a2 - t0(DES(i)))));


    } // end rows

    REPORT(ypreds);
    ADREPORT(ypreds);
    REPORT(denominator)
      ADREPORT(t0);
    ADREPORT(k);
    ADREPORT(Linf);
    ADREPORT(L1);
    ADREPORT(L2);
    ADREPORT(Sigma);
    return(ans);
  }
