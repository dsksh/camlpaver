
#include <iostream>
#include <sstream>

#include <kv/interval.hpp>

#include "wrapper.h"

using namespace std;
using namespace kv;

#define DEF_BIN_OP(NAME, OP) \
  cInterval NAME(const double inf1, const double sup1, \
                 const double inf2, const double sup2) { \
    interval<double> intv1(inf1, sup1); \
    interval<double> intv2(inf2, sup2); \
    interval<double> intvR( intv1 OP intv2 ); \
    cInterval res = {intvR.lower(), intvR.upper()}; \
    return res; \
  }

DEF_BIN_OP(kv_add, +)
DEF_BIN_OP(kv_sub, -)
DEF_BIN_OP(kv_mul, *)
DEF_BIN_OP(kv_div, /)

#define DEF_UN_OP(NAME, FUN) \
  cInterval NAME(const double inf, const double sup) { \
    interval<double> intv(inf, sup); \
    interval<double> intvR( FUN(intv) ); \
    cInterval res = {intvR.lower(), intvR.upper()}; \
    return res; \
  }

DEF_UN_OP(kv_sqrt, sqrt)
DEF_UN_OP(kv_exp, exp)
DEF_UN_OP(kv_log, log)
DEF_UN_OP(kv_sin, sin)
DEF_UN_OP(kv_cos, cos)
DEF_UN_OP(kv_tan, tan)
DEF_UN_OP(kv_asin, asin)
DEF_UN_OP(kv_acos, acos)
DEF_UN_OP(kv_atan, atan)
DEF_UN_OP(kv_abs, abs)

cInterval kv_pow(const double inf, const double sup, const int e) {
  interval<double> intv(inf, sup);
  interval<double> intvR( pow(intv, e) ); 
  cInterval res = {intvR.lower(), intvR.upper()};
  return res;
}

#define DEF_ATTR(NAME, FUN) \
  double NAME(const double inf, const double sup) { \
    interval<double> intv(inf, sup); \
    double res( FUN(intv) ); \
    return res; \
  }

DEF_ATTR(kv_width, width)
DEF_ATTR(kv_rad, rad)
DEF_ATTR(kv_mid, mid)
DEF_ATTR(kv_norm, norm)

char *kv_str_of_intv(const double inf, const double sup) {
  interval<double> intv(inf, sup);
  stringstream ss;
  ss << intv;
  char *cs = new char[ss.str().length()+1];
  std::strcpy (cs, ss.str().c_str());
  return cs;
}

