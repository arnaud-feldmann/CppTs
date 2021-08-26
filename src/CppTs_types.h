#ifndef RCPPTS_H
#define RCPPTS_H

#include<RcppCommon.h>

class TimeSerie {
private:
  SEXP m_data;
public:
  TimeSerie(SEXP);
  operator SEXP();
  bool isMatrix();
  bool isNumeric();
};

class TimeSerieNumeric : public TimeSerie {
public:
  TimeSerieNumeric(SEXP);
};

class TimeSerieNumericMatrix : public TimeSerieNumeric {
public:
  TimeSerieNumericMatrix(SEXP);
};

namespace Rcpp {
template <> SEXP wrap(const TimeSerie&);
template <> SEXP wrap(const TimeSerieNumeric&);
template <> SEXP wrap(const TimeSerieNumericMatrix&);
}

#endif
