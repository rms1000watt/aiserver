;;**EXPORTKEY**:AliceAIML:BasicMath
#alice#
<?xml version='1.0' encoding='ISO-8859-1'?>
<aiml>
<!-- ******************* -->
<!-- Basic Math Rules    -->
<!-- ******************* -->
<!-- This program is AIML source code training the -->
<!-- Alice chat robot agent to perform basic mathematics. -->

<!-- ---------------------- -->
<!-- Basic Math Help Facts  -->
<!-- ---------------------- -->
<fact>
<reminder>HELP MATH</reminder>
  <document>What is twenty two times forty six?~
            (23.4 * 54.682) / sine(3.56)~
            What is two hundred thousand four hundred sixty one?~
            What is two hundred thousand times the sine of one?~
            What is the squareroot of four?~
            What is the logarithm of sixty four to the base ten?~
            What is sixty four times seventy two?~
            What is sixty four squared?~
            What is one plus one?
  </document>
</fact>

<!-- --------------------------- -->
<!-- Basic Math Lexical Features -->
<!-- --------------------------- -->
<lex name='Evaluate'>CALCULATE CALCULATES CALCULATED CALCULATING</lex>
<lex name='Evaluate'>COMPUTE COMPUTES COMPUTED COMPUTING</lex>
<lex name='Evaluate'>EVALUATE EVALUATES EVALUATED EVALUATING</lex>

<lex name='Formula'>FORMULA FORMULAS</lex>
<lex name='Formula'>EQUATION EQUATIONS</lex>

<lex name='NumValue' values="0">ZERO</lex>
<lex name='Number'>ZERO</lex>

<lex name='NumUnit'>ONE TWO THREE FOUR FIVE SIX SEVEN EIGHT NINE</lex>
<lex name='NumValue' values="1 2 3 4 5 6 7 8 9">ONE TWO THREE FOUR FIVE SIX SEVEN EIGHT NINE</lex>
<lex name='Number'>ONE TWO THREE FOUR FIVE SIX SEVEN EIGHT NINE</lex>

<lex name='NumWord'>TEN ELEVEN TWELVE THIRTEEN FOURTEEN FIFTEEN SIXTEEN SEVENTEEN EIGHTEEN NINETEEN TWENTY</lex>
<lex name='NumValue' values="10 11 12 13 14 15 16 17 18 19">TEN ELEVEN TWELVE THIRTEEN FOURTEEN FIFTEEN SIXTEEN SEVENTEEN EIGHTEEN NINETEEN</lex>
<lex name='Number'>TEN ELEVEN TWELVE THIRTEEN FOURTEEN FIFTEEN SIXTEEN SEVENTEEN EIGHTEEN NINETEEN</lex>

<lex name='NumPrefix'>TWENTY THIRTY FOURTY FORTY FIFTY SIXTY SEVENTY EIGHTY NINETY</lex>
<lex name='NumValue' values="20 30 40 40 50 60 70 80 90">TWENTY THIRTY FOURTY FORTY FIFTY SIXTY SEVENTY EIGHTY NINETY</lex>
<lex name='Number'>TWENTY THIRTY FOURTY FORTY FIFTY SIXTY SEVENTY EIGHTY NINETY</lex>

<lex name='NumSuffix'>HUNDRED THOUSAND MILLION BILLION TRILLION</lex>
<lex name='NumWord'>HUNDRED THOUSAND MILLION BILLION TRILLION</lex>
<lex name='NumValue' values="100 1000 1000000 1000000000 1000000000000">HUNDRED THOUSAND MILLION BILLION TRILLION</lex>

<lex name='Unary'>ARCCOSINE ARCSINE ARCTANGENT COSINE HYPERBOLICCOSINE DEGREE EXPONENTIAL LOGARITHM LOG10 LOG2 RADIANS SINE HYPERBOLICSINE SQUAREROOT ROOT TANGENT</lex>
<lex name='OpValue' values="acos asin atan cos cash deg exp log log10 log2 rad sin sinh sqrt sqrt tan">ARCCOSINE ARCSINE ARCTANGENT COSINE HYPERBOLICCOSINE DEGREE EXPONENTIAL LOGARITHM LOG10 LOG2 RADIANS SINE HYPERBOLICSINE SQUAREROOT ROOT TANGENT</lex>
<lex name='Binary'>POWER LOGBASE</lex>
<lex name='OpValue' values="expt logbase">POWER LOGBASE</lex>

<lex name='Operator'><![CDATA[ + - * / ^ ]]>PLUS TIMES MINUS</lex>
<lex name='OpValue' values="+ - * / expt + * -"><![CDATA[ + - * / ^ ]]>PLUS TIMES MINUS</lex>

<lex name='Percent'>PERCENT</lex>

<!-- ----------------------------- -->
<!-- English Compound Number Rules -->
<!-- ----------------------------- -->

<rule>
<pattern>? %Number #CompoundNumber ?</pattern>
<template>
   <think><set name='CompoundNumber' switch='yes'>off</set></think>
   <apply><apply topic='CompoundNumber'><star/><arg index='1'/><star index='2'/></apply></apply>
</template>
</rule>

<rule topic='CompoundNumber'>
<pattern>? %Number %Percent ?</pattern>
<apply topic='CompoundNumber'><star/><lisp>(newToken Value: (/ <arg index='1' feature='NumValue'/> 100))</lisp><star index='2'/></apply>
</rule>
<rule topic='CompoundNumber'>
<pattern>? %NumPrefix %NumUnit ?</pattern>
<apply topic='CompoundNumber'><star/><lisp>(newToken Value: (+ <arg index='1' feature='NumValue'/> <arg index='2' feature='NumValue'/>))</lisp><star index='2'/></apply>
</rule>
<rule topic='CompoundNumber'>
<pattern>? %NumPrefix %NumUnit %NumSuffix ?</pattern>
<apply topic='CompoundNumber'><star/><lisp>(newToken Value: (+ <arg index='1' feature='NumValue'/> <arg index='2' feature='NumValue'/>))<arg index='3'/></lisp><star index='2'/></apply>
</rule>
<rule topic='CompoundNumber'>
<pattern>? %Number %NumSuffix @</pattern>
<apply topic='CompoundNumber'><star/><lisp>(newToken Value: (* <arg index='1' feature='NumValue'/> <arg index='2' feature='NumValue'/>) NumSuffix: true)</lisp><star index='2'/></apply>
</rule>
<rule topic='CompoundNumber'>
<pattern>? %Number %NumSuffix %NumSuffix @</pattern>
<apply topic='CompoundNumber'><star/><arg index='1'/><lisp>(newToken Value: (* <arg index='2' feature='NumValue'/> <arg index='3' feature='NumValue'/>) NumSuffix: true)</lisp><star index='2'/></apply>
</rule>
<rule topic='CompoundNumber'>
<pattern>? %NumSuffix %Number @</pattern>
<apply topic='CompoundNumber'><star/><lisp>(newToken Value: (+ <arg index='1' feature='NumValue'/> <arg index='2' feature='NumValue'/>) NumSuffix: true)</lisp><star index='2'/></apply>
</rule>
<rule topic='CompoundNumber'>
<pattern>? %NumSuffix %NumPrefix %NumUnit @</pattern>
<apply topic='CompoundNumber'><star/><arg index='1'/><lisp>(newToken Value: (+ <arg index='2' feature='NumValue'/> <arg index='3' feature='NumValue'/>))</lisp><star index='2'/></apply>
</rule>
<rule topic='CompoundNumber'>
<pattern>? %NumSuffix %Number %NumSuffix @</pattern>
<apply topic='CompoundNumber'><star/><lisp>(newToken Value: (+ <arg index='1' feature='NumValue'/> (* <arg index='2' feature='NumValue'/> <arg index='3' feature='NumValue'/>)) NumSuffix: true)</lisp><star index='2'/></apply>
</rule>
<rule topic='CompoundNumber'>
<pattern>? %NumSuffix %Number %NumSuffix %NumSuffix @</pattern>
<apply topic='CompoundNumber'><star/><lisp>(newToken Value: (+ <arg index='1' feature='NumValue'/> (* <arg index='2' feature='NumValue'/> <arg index='3' feature='NumValue'/> <arg index='4' feature='NumValue'/>)) NumSuffix: true)</lisp><star index='2'/></apply>
</rule>

<rule topic='CompoundNumber'>
<pattern>*</pattern>
<template><star/></template>
</rule>

<!-- --------------------- -->
<!-- Basic Math Operations -->
<!-- --------------------- -->
<rule>
<pattern>? %Number %Operator %Number ?</pattern>
<apply><star/><lisp>(newToken Value: ((getGlobalValue (symbol (trim {<arg index='2' feature='OpValue'/>}))) <arg index='1' feature='NumValue'/> <arg index='3' feature='NumValue'/>))</lisp><star index='2'/></apply>
</rule>
<rule>
<pattern>? %Unary ( %Number ) ?</pattern>
<apply><star index='1'/><lisp>(newToken Value: ((getGlobalValue (symbol (trim {<arg index='1' feature='OpValue'/>})))  <arg index='3' feature='NumValue'/>))</lisp><star index='2'/></apply>
</rule>
<rule>
<pattern>? %Binary ( %Number %Number ) ?</pattern>
<apply><star index='1'/><lisp>(newToken Value: ((getGlobalValue (symbol (trim {<arg index='1' feature='OpValue'/>}))) <arg index='3' feature='NumValue'/> <arg index='4' feature='NumValue'/>))</lisp><star index='2'/></apply>
</rule>
<rule>
<pattern>? ( %Number ) ?</pattern>
<apply><star index='1'/><arg index='2'/><star index='2'/></apply>
</rule>

<rule>
<pattern>? $Unary $Preposition ?</pattern>
<apply><star index='1'/><lisp>(newToken Value: ((getGlobalValue (symbol (trim {<arg index='1' feature='OpValue'/>}))) <apply><star index='2'/></apply>))</lisp></apply>
</rule> 
<rule>
<pattern>? $Article $Unary $Preposition @</pattern>
<apply><star index='1'/><lisp>(newToken Value: ((getGlobalValue (symbol (trim {<arg index='2' feature='OpValue'/>}))) <apply><star index='2'/></apply>))</lisp></apply>
</rule>
<rule>
<pattern>? $Article SQUARE OF * </pattern>
<apply><star index='1'/><lisp>(newToken Value: (* <apply><star index='2'/></apply>  <apply><star index='2'/></apply>))</lisp></apply>
</rule>
<rule>
<pattern>? %Number SQUARED ? </pattern>
<apply><star index='1'/><lisp>(newToken Value: (* <arg index='1'/> <arg index='1'/>))</lisp></apply>
</rule>
<rule>
<pattern>? $Article CUBE OF * </pattern>
<apply><star index='1'/><lisp>(newToken Value: (* <apply><star index='2'/></apply>  <apply><star index='2'/></apply>  <apply><star index='2'/></apply>))</lisp></apply>
</rule>
<rule>
<pattern>? $Article LOGARITHM OF * TO THE BASE *</pattern>
<apply><star index='1'/><lisp>(newToken Value: (logbase  <apply><star index='2'/></apply>  <apply><star index='3'/></apply>))</lisp></apply>
</rule>
<rule>
<pattern>? $Article LOGARITHM OF * TO BASE *</pattern>
<apply><star index='1'/><lisp>(newToken Value: (logbase  <apply><star index='2'/></apply>  <apply><star index='3'/></apply>))</lisp></apply>
</rule>
<rule>
<pattern>? $Article BASE * LOGARITHM OF *</pattern>
<apply><star index='1'/><lisp>(newToken Value: (logbase  <apply><star index='3'/></apply>  <apply><star index='2'/></apply>))</lisp></apply>
</rule>
<rule>
<pattern>? %Number DIVIDED BY %Number ?</pattern>
<apply><star index='1'/><lisp>(newToken Value: (/  <arg index='1' feature='NumValue'/>  <arg index='4' feature='NumValue'/>))</lisp><star index='2'/></apply>
</rule>
<rule>
<pattern>? %Number DIVIDED INTO %Number ?</pattern>
<apply><star index='1'/><lisp>(newToken Value: (/  <arg index='4' feature='NumValue'/>  <arg index='1' feature='NumValue'/>))</lisp><star index='2'/></apply>
</rule>
<rule>
<pattern>? %Number RAISED TO THE POWER OF ?</pattern>
<apply><star index='1'/><lisp>(newToken Value: (expt  <arg index='1' feature='NumValue'/>  <apply><star index='2'/></apply>))</lisp></apply>
</rule>

<!-- ------------------------------------ -->
<!-- Basic Math Command Header Phrases -->
<!-- ------------------------------------ -->
<rule>
<pattern> %Number </pattern>
<template><input index='1' feature='NumValue'/></template>
</rule>
<rule>
<pattern>WHAT IS %Number </pattern>
<template><input index='3' feature='NumValue'/></template>
</rule>
<rule>
<pattern>%Evaluate * </pattern>
<template><apply><star/></apply></template>
</rule>
<rule>
<pattern>%Evaluate %Article * </pattern>
<template><apply><star/></apply></template>
</rule>
<rule>
<pattern>%Evaluate %Article %Formula * </pattern>
<template><apply><star/></apply></template>
</rule>
</aiml>


