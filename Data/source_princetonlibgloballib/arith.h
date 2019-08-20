#ifndef F2C_ARITH_H
#define F2C_ARITH_H

#include "config.h"
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#define asl cur_ASL
#include <errno.h>
#include <setjmp.h>

#ifdef _MSC_VER
#define isnan _isnan
#define isinf(x) (!_finite(x))
#endif

#ifndef isnan
# define isnan(x)						 \
  (sizeof (x) == sizeof (long double) ? isnan_ld (x)		 \
   : sizeof (x) == sizeof (double) ? isnan_d (x)		 \
   : isnan_f (x))
static inline int isnan_f  (float       x) { return x != x; }
static inline int isnan_d  (double      x) { return x != x; }
static inline int isnan_ld (long double x) { return x != x; }
#endif
          
#ifndef isinf
# define isinf(x)						 \
  (sizeof (x) == sizeof (long double) ? isinf_ld (x)		 \
   : sizeof (x) == sizeof (double) ? isinf_d (x)		 \
   : isinf_f (x))
static inline int isinf_f  (float       x)
{ return !isnan (x) && isnan (x - x); }
static inline int isinf_d  (double      x)
{ return !isnan (x) && isnan (x - x); }
static inline int isinf_ld (long double x)
{ return !isnan (x) && isnan (x - x); }
#endif

#ifndef signbit
#define signbit(x) (((x) < 0)? 1 : 0)
#endif

#endif


#ifndef real
#define real double
#endif

 void
#ifdef KR_headers
in_trouble(who, a) char *who; real a;
#else
in_trouble(char *who, real a)
#endif
{
	char buf[64];
	// if (err_jmp)
	// 	longjmp(err_jmp->jb, errno ? errno : 1);
	sprintf(buf, "can't evaluate %s(%g)", who, a);
	perror(buf);
	exit(1);
	}

 void
#ifdef KR_headers
in_trouble2(who, a, b) char *who; real a, b;
#else
in_trouble2(char *who, real a, real b)
#endif
{
	char buf[64];
	// if (err_jmp)
	// 	longjmp(err_jmp->jb, errno ? errno : 1);
	sprintf(buf, "can't evaluate %s(%g,%g)", who, a, b);
	perror(buf);
	exit(1);
	}

// Ref: http://www.netlib.no/netlib/ampl/solvers/nlc/nlc.c
// static char *zerdiv_fmt = "\tzerdiv_(&%s);"; 


 void
// #ifdef KR_headers
// zerdiv_(s)
// 	char *s;
// #else
// zerdiv_(char *s)
// #endif
// { printf(zerdiv_, s); }

#ifdef KR_headers
zerdiv(s)
	char *s;
#else
zerdiv(char *s)
#endif
{ printf(zerdiv, s); }