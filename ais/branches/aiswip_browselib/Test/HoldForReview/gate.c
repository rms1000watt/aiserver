#include "fsmtbase.h"

void main_init(void);
TVAL main_procedure(INT argc,TVAL argv[]);

void main_init()
{
FSmartbase_RegisterCProcedure((LpCHAR)"main",(LpFUNC)&main_procedure);
}


TVAL main_procedure(INT argc,TVAL argv[])
{
register INT  _finish;
register INT  _clock = 0;

INT           w_count = 0;
INT           w_clock = 0;
INT           w_f = 0;
INT           w_af = 0;
INT           w_R = 0;
INT           w_L = 0;
INT           w_A = 0;
INT           w_RV = 0;
INT           w_BV = 0;
INT           s_S1 = 0;
INT           s_S2 = 0;
INT           s_monitor = 0;
INT           s_count = 0;
INT           s_clock = 0;
INT           __tmp9;
INT           __tmp11;
INT           __tmp8;
INT           __tmp10;
INT           __tmp7;

if ((argc != 1) || (argv[0].Tag != TYINT)) return(TERROR("!arglist!"));
_finish = argv[0].u.Int;

for (_clock = 0;_clock <= _finish; _clock += 1)
   {
   __tmp7 = (_clock % 2);
   __tmp8 = (s_S1 >> 24);
   __tmp9 = (_clock - (s_S1 & 16777215));
   __tmp10 = (s_S2 >> 24);
   __tmp11 = (_clock - (s_S2 & 16777215));
   if (_clock == 0) {w_clock = 0;}
   else if (__tmp7 == 0) {w_clock = !w_clock;}
   if ((s_clock - w_clock) == 1) {w_count = ((w_count + 1) % 16);}
   w_f =  ((w_count & 15) == 15 ? 1 : 0);
   w_af =  ((w_count & 15) == 15 ? 1 : 0);
   if (((__tmp8 == 0) && (__tmp9 >= 1)) && (w_R == 1)) {s_S1 = (16777216 + _clock);}
   else if (((__tmp8 == 1) && (__tmp9 >= 5)) && (w_L <= 0)) {s_S1 = (33554432 + _clock);}
   else if (((__tmp8 == 2) && (__tmp9 >= 1)) && (w_R == 0)) {s_S1 = _clock;}
   if (((__tmp10 == 0) && (__tmp11 >= 1)) && (w_A == 0)) {s_S2 = (16777216 + _clock);}
   else if (((__tmp10 == 1) && (__tmp11 >= 12)) && (w_A == 1)) {s_S2 = _clock;}
   s_count = w_count;
   s_clock = w_clock;
   }

_clock = _finish;
FSmartbase_Eval(TGVALUE("writeln"),7,TINT(_clock),TSTRING(",,,count="),TINT(w_count),TSTRING(" ,f="),TINT(w_f),TSTRING(",af="),TINT(w_af));


return(argv[0]);
}


