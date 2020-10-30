;;*************************************
;;*************************************
;; Exported Agent File Cabinet Document
;;*************************************
;;*************************************

;;**EXPORTKEY**:AliceAIML:IndexRefGuide

#alice#
<?xml version='1.0' encoding='ISO-8859-1'?>
<aiml>
<!-- Free software (c) 2001 ALICE A.I. Foundation.   -->
<!-- This program is open source code released under -->
<!-- the terms of the GNU General Public License     -->
<!-- as published by the Free Software Foundation.   -->
<!-- Complies with AIML 1.0 Tag Set Specification -->
<!-- as adopted by the ALICE A.I. Foundation.  -->
<!-- A.L.I.C.E. Build Sun Jul 29 02:34:12 PDT 2001   -->
<!-- 39615 total categories.  -->

<!-- ------------------------------ -->
<!-- Alice IndexRefGuide Lexical Rules   -->
<!-- ------------------------------ -->
<lex name='Internet'>REFGUIDE AIS</lex>

<!-- --------------------------------------- -->
<!-- Alice AIS RefGuide Search Hints in HTML     -->
<!-- --------------------------------------- -->
<rule>
<pattern>XFIND *</pattern>
<template> 
  <random>
    <li>May I consult our documentation?</li>
    <li>Would you like to know more?</li>
    <li>Should I try searching relavant AIS documents?</li>
  </random>
  <think>
    <lisp>
      <![CDATA[
      vars:(htmlPage)
      (setq htmlPage (append "<hr><br>"
                             "<input type='button' value='Search AIS Documentation for" 
                             (appendFeatures star[0] Original:) 
                             "' onClick='loadDetails(AliceQuery.value = this.value);'>"
                             "<br><br>"
                             ))
      (setq myHtmlResponseResults htmlPage)      ]]>
    </lisp>
  </think>
<think><set name='it'><set name='subject'><person/></set></set></think>
</template></rule>

<!-- -------------------------------- -->
<!-- Alice Ask Jeeves Search Command  -->
<!-- -------------------------------- -->
<rule>
<pattern>SEARCH AIS DOCUMENTATION %Preposition *</pattern>
<template>
  <random>
    <li>I will give you a list of relevant pages.</li>
    <li>The AIS RefGuide Documentation is a great tool for me, it helps me so much.</li>
    <li>I am almost sure the answer can be found in our AIS Documentation.</li>
  </random>
  <think>
    <lisp>
      <![CDATA[
      vars:(searchPhrase htmlPage 
           
           ) ; end temporary variables
      ;; Make sure the web is online before we waste time searching.
     
      ;; Create the Internet search command using the Yahoo search page.
      (setq searchPhrase star[0])
      (if (isVector searchPhrase)
	      (setq searchPhrase (appendFeatures searchPhrase Original:))
	      (setq searchPhrase "")
          ) ; end
      (setq command searchPhrase)
      (setq htmlPage (IndexRefGuide.queryHtml command))
      (if (and (<> htmlPage #void) (> (length htmlPage) 0)) 
          (setq myHtmlResponseResults htmlPage)
          (setq myHtmlResponseHeading "<hr><p>I tried searching our online documentation but I got no results.</p>")
          ) ; end if
      ]]>
    </lisp>
  </think>
</template>
</rule>

 

<!-- ------------------------------------------ -->
<!-- Alice Internet Parameter Command Phrases   -->
<!-- ------------------------------------------ -->
<rule>
<pattern>SET %Internet TIMEOUT %Preposition %Number</pattern>
<template>
  Internet timeout set to <arg index='5'/>
  <think>
    <lisp>
      (setq myWebTimeOut (integer arg[4].NumValue))
    </lisp>
  </think>
</template>
</rule>
<rule>
<pattern>SET %Article %Internet TIMEOUT %Preposition %Number</pattern>
<template>Set Internet timeout to <arg index='4'/></template>
</rule>

<rule>
<pattern>SET %Internet OFFLINE</pattern>
<template>
  Internet set offline
  <think>
    <lisp>
      (setq webOnline false)
    </lisp>
  </think>
</template>
</rule>
<rule>
<pattern>SET %Article %Internet OFFLINE</pattern>
<template>Set Internet offline</template>
</rule>

<rule>
<pattern>SET %Internet ONLINE</pattern>
<template>
  Internet set online
  <think>
    <lisp>
      (setq webOnline true)
    </lisp>
  </think>
</template>
</rule>
<rule>
<pattern>SET %Article %Internet ONLINE</pattern>
<template>Set Internet online</template>
</rule>


</aiml>




