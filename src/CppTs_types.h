#ifndef RCPPTS_H
#define RCPPTS_H

#include<RcppCommon.h>

class TimeSerie {
private:
  SEXP m_data;
public:
  TimeSerie(SEXP);
  operator const SEXP&();
};

namespace Rcpp {
template <> SEXP wrap(const TimeSerie&);
}

#endif
