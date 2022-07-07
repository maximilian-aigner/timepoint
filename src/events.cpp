#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
class EventObservations {
private:
  NumericVector bounds;
  DatetimeVector observations_datetime;
  NumericVector time_from_origin;
  
protected:
public:
  NumericVector observations;
  
  NumericVector get_bounds() {
    return(bounds);
  }
  
};