
library ("Rcpp")

cumsum.bounded <- cppFunction(
    'NumericVector cumsum_bounded ( NumericVector x, 
                                    const double start, 
                                    NumericVector lower, 
                                    NumericVector upper) {
        
        double acc = start;
        NumericVector result(x.size());
        
        for(int i = 0; i < x.size(); i++) {
            acc += x[i];

            if (acc < lower[i]) acc = lower[i];
            if (acc > upper[i]) acc = upper[i];

            result[i] = acc;
        }
    
        return result;
    }')

