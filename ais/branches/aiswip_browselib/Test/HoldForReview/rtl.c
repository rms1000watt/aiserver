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

INT           w_R = 0;
INT           w_A = 0;
INT           w_BV = 0;
INT           w_L = 0;
INT           w_RV = 0;
INT           s_S1 = 0;
INT           s_S2 = 0;
INT           s_monitor = 0;
INT           __tmp2;
INT           __tmp1;
INT           __tmp4;
INT           __tmp6;
INT           __tmp3;
INT           __tmp5;

if ((argc != 1) || (argv[0].Tag != TYINT)) return(TERROR("!arglist!"));
_finish = argv[0].u.Int;

for (_clock = 0;_clock <= _finish; _clock += 1)
   {
   __tmp1 = (_clock - s_S2);
   __tmp2 = (_clock - s_S1);
   __tmp3 = (s_S1 >> 24);
   __tmp4 = (_clock - (s_S1 & 16777215));
   __tmp5 = (s_S2 >> 24);
   __tmp6 = (_clock - (s_S2 & 16777215));
   if (__tmp1 == 1) {w_R = 0;}
   else if (__tmp1 == -16777204) {w_R = 1;}
   if (__tmp2 == 1) {w_A = 0;}
   else if (__tmp2 == -33554431) {w_A = 1;}
   if (__tmp1 == -16777205) {w_BV =  ((w_BV >= 99) ? 0 : w_BV+1);}
   if (__tmp1 == -16777205) {w_RV = w_BV;}
   if (__tmp2 == -16777211) {w_L = w_RV;}
   else if (((__tmp3 == 1) && (__tmp4 >= 5)) && (w_L > 0)) {w_L = w_L-1;}
   if (((__tmp3 == 0) && (__tmp4 >= 1)) && (w_R == 1)) {s_S1 = (16777216 + _clock);}
   else if (((__tmp3 == 1) && (__tmp4 >= 5)) && (w_L <= 0)) {s_S1 = (33554432 + _clock);}
   else if (((__tmp3 == 2) && (__tmp4 >= 1)) && (w_R == 0)) {s_S1 = _clock;}
   if (((__tmp5 == 0) && (__tmp6 >= 1)) && (w_A == 0)) {s_S2 = (16777216 + _clock);}
   else if (((__tmp5 == 1) && (__tmp6 >= 12)) && (w_A == 1)) {s_S2 = _clock;}
   }

_clock = _finish;
FSmartbase_Eval(TGVALUE("writeln"),20,TSTRING("Tm="),TINT(_clock),TSTRING(",A="),TINT(w_A),TSTRING(",L="),TINT(w_L),TSTRING(",R="),TINT(w_R),TSTRING(",RV="),TINT(w_RV),TSTRING(",BV="),TINT(w_BV),TSTRING(",S1="),TINT((s_S1 >> 24)),TSTRING(",t_S1="),TINT((s_S1 & 16777215)),TSTRING(",S2="),TINT((s_S2 >> 24)),TSTRING(",t_S2="),TINT((s_S2 & 16777215)));


return(argv[0]);
}


