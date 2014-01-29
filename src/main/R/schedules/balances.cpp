#include <iostream>
#include <Rcpp.h>

using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
List balances ( const double  start,
                NumericVector early_demand,
                NumericVector supply,
                NumericVector late_demand,
                NumericVector capacity ) {

    int days = supply.size();

    // maintain the min, max, and ending balance each day
    NumericVector daily_balance(days);
    NumericVector excess_demand(days);
    NumericVector excess_supply(days);
    IntegerVector faults(days);

    double balance = start;
    
    // iterate through each day
    for(int i = 0; i < days; i++) {

        // what is the minimum balance from "early" customer demand?
        balance += early_demand[i];
        if (balance < 0.0) { 
            
            // demand exceeded supply - not enough cash for customers
            excess_demand[i] = balance;
            balance = 0.0;
            faults[i] = 1;            
        }
        
        // what is the max balance based on a service run?
        balance += supply[i];
        if (balance > capacity[i]) {
         
            // adding cash with cash bin not empty - occurs normally, but should be minimized
            excess_supply[i] = balance - capacity[i];
            balance = capacity[i];
        }
        
        // what is the ending balanced based on "late" customer demand?
        balance += late_demand[i];
        if (balance < 0.0) { 
            
            // demand exceeded supply - not enough cash for customers            
            excess_demand[i] += balance;
            balance = 0.0;
            faults[i] = 1;
        }
        
        daily_balance[i] = balance;
    }
    
    return List::create(_["balance"]       = daily_balance, 
                        _["faults"]        = faults,
                        _["excess_demand"] = excess_demand,
                        _["excess_supply"] = excess_supply);
}

