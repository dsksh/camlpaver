
#ifdef __cplusplus
extern "C" {
#endif

typedef struct cInterval {
    double inf;
    double sup;
} cInterval;

cInterval kv_add(const double, const double, const double, const double);
cInterval kv_sub(const double, const double, const double, const double);
cInterval kv_mul(const double, const double, const double, const double);
cInterval kv_div(const double, const double, const double, const double);
cInterval kv_sqrt(const double, const double);
cInterval kv_pow(const double, const double, const int);
cInterval kv_exp(const double, const double);
cInterval kv_log(const double, const double);
cInterval kv_sin(const double, const double);
cInterval kv_cos(const double, const double);
cInterval kv_tan(const double, const double);
cInterval kv_asin(const double, const double);
cInterval kv_acos(const double, const double);
cInterval kv_atan(const double, const double);
cInterval kv_abs(const double, const double);
double kv_width(const double, const double);
double kv_rad(const double, const double);
double kv_mid(const double, const double);
double kv_norm(const double, const double);
char *kv_str_of_intv(const double, const double);

#ifdef __cplusplus
}
#endif
