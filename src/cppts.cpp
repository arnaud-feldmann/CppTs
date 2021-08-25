#include "CppTs_types.h"
#include <Rcpp.h>

using namespace Rcpp;

TimeSerie::TimeSerie(SEXP data):
  m_data(data) {
  if (! Rf_inherits(data,"ts")) stop("Not a ts object");
}

TimeSerie::operator SEXP const &(){
  return m_data;
}

namespace Rcpp {
template <> SEXP wrap(const TimeSerie &x) {
  return (const SEXP&)x;
}
}
