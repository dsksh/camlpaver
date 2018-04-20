#include <stdio.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>

#include "wrapper.h"

#define DEF_BIN_OP(NAME, IMPL) \
  value NAME(value inf1, value sup1, value inf2, value sup2) { \
    CAMLparam4(inf1, sup1, inf2, sup2); \
    CAMLlocal1(intv); \
    \
    cInterval res = IMPL(Double_val(inf1), Double_val(sup1), \
                         Double_val(inf2), Double_val(sup2)); \
    \
    intv = caml_alloc(2, 0); \
    Store_field(intv, 0, caml_copy_double(res.inf)); \
    Store_field(intv, 1, caml_copy_double(res.sup)); \
    CAMLreturn(intv); \
  }

DEF_BIN_OP(intv_add, kv_add)
DEF_BIN_OP(intv_sub, kv_sub)
DEF_BIN_OP(intv_mul, kv_mul)
DEF_BIN_OP(intv_div, kv_div)

#define DEF_UN_OP(NAME, IMPL) \
  value NAME(value inf, value sup) { \
    CAMLparam2(inf, sup); \
    CAMLlocal1(intv); \
    \
    cInterval res = IMPL(Double_val(inf), Double_val(sup)); \
    \
    intv = caml_alloc(2, 0); \
    Store_field(intv, 0, caml_copy_double(res.inf)); \
    Store_field(intv, 1, caml_copy_double(res.sup)); \
    CAMLreturn(intv); \
  }

DEF_UN_OP(intv_sqrt, kv_sqrt)
DEF_UN_OP(intv_exp, kv_exp)
DEF_UN_OP(intv_log, kv_log)
DEF_UN_OP(intv_sin, kv_sin)
DEF_UN_OP(intv_cos, kv_cos)
DEF_UN_OP(intv_tan, kv_tan)
DEF_UN_OP(intv_asin, kv_asin)
DEF_UN_OP(intv_acos, kv_acos)
DEF_UN_OP(intv_atan, kv_atan)
 
value intv_pow(value inf, value sup, value e) {
  CAMLparam3(inf, sup, e);
  CAMLlocal1(intv);

  cInterval res = kv_pow(Double_val(inf), Double_val(sup), Int_val(e));

  intv = caml_alloc(2, 0);
  Store_field(intv, 0, caml_copy_double(res.inf));
  Store_field(intv, 1, caml_copy_double(res.sup));
  CAMLreturn(intv);
}

value intv_str_of(value inf, value sup) {
  CAMLparam2(inf, sup);
  CAMLlocal1(str);
  char *cs = kv_str_of_intv(Double_val(inf), Double_val(sup));
  if (cs != NULL) {
    str = caml_copy_string(cs);
    free(cs);
  } else
    str = caml_copy_string("");
  CAMLreturn(str);
}

