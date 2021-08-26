#include "CppTs_types.h"
#include <Rcpp.h>

using namespace Rcpp;

TimeSerie::TimeSerie(SEXP data):
  m_data(data) {
  if (! Rf_inherits(data,"ts")) stop("Not a ts object");
}

bool TimeSerie::isMatrix(){
  return Rf_isMatrix(m_data);
}

bool TimeSerie::isNumeric(){
  return Rf_isNumeric(m_data);
}

TimeSerie::operator SEXP(){
  return m_data;
}

namespace Rcpp {
template <> SEXP wrap(const TimeSerie& x) {
  return const_cast<TimeSerie&>(x);
}
template <> SEXP wrap(const TimeSerieNumeric& x) {
  return wrap((const TimeSerie&)x);
}
template <> SEXP wrap(const TimeSerieNumericMatrix& x) {
  return wrap((const TimeSerieNumeric&)x);
}
}

TimeSerieNumeric::TimeSerieNumeric(SEXP data):
  TimeSerie(data) {
  if (! Rf_isNumeric(data)) stop("Not a numeric");
}

TimeSerieNumericMatrix::TimeSerieNumericMatrix(SEXP data):
  TimeSerieNumeric(data) {
  if (! Rf_isMatrix(data)) stop("Not a matrix");
}
