;;**EXPORTKEY**:AliceAIML:SelfTest
#alice#
<?xml version='1.0' encoding='ISO-8859-1'?>
<aiml>
<!-- ************************ -->
<!-- Alice Self Test Rules    -->
<!-- ************************ -->
<!-- This program is AIML source code training the -->
<!-- Alice chat robot agent to perform the self test. -->

<rule topic='selfTest'>
<pattern>ZZZTEST #SelfTest TEST SWITCH</pattern>
<template>Please <arg index='2'/> my <arg index='3'/>.
</template>
</rule>
<rule topic='selfTest'>
<pattern>ZZZTEST !SelfTest TEST SWITCH</pattern>
<template>What?
</template>
</rule>
<rule topic='selfTest'>
<pattern>ZZZTEST ? TEST SWITCH</pattern>
<template>Your wild card is <star/>.
</template>
</rule>
<rule topic='selfTest'>
<pattern>ZZZTEST TEST ME</pattern>
<template>Why should I?
</template>
</rule>
<rule topic='selfTest'>
<pattern>ZZZTEST @ <lisp>true</lisp> BLACK @ WATCH @</pattern>
<template>The second star is <string enclose='dquote'><star index='2'/></string>.
</template>
</rule>
<rule topic='selfTest'>
<pattern>ZZZTEST ? %Verb %Article _ %Preposition %Pronoun ?</pattern>
<template>The preposition is <string enclose='dquote'><arg index='4'/></string>.
</template>
</rule>

</aiml>

