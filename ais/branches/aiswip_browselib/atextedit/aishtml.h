/**********************************************************************************
    Copyright (C) 2008 Investment Science Corp.

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

***********************************************************************************/

#ifndef AISHTML_H
#define AISHTML_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/atextedit/aishtml.h
													HTML Syntax Definitions
Aishtml.h holds the comment delimiters and keywords for composing HTML.

CHANGE HISTORY
Version	Date		Who		Change
2.0004	 2/17/2007	tlw		Add quote characters and placeholder for escape char.
1.0057	 3/18/2005	tlw		Update documentation
												--------------- ------------------
NOTES
 1. See atextstaticdefs.h for more info on language definitions.

The AisEd  highlighter uses a fast, but not very smart, syntax analyzer.  The highlighter just recognizes quoted strings,
comments, and keywords.  It does not recognize other syntax such as HTML tags, so it highlights keywords whenever they are
found.  For this reason, tag names that are common English words, such as title, are not highlighted.
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
	
//	---------------------------------------------------- DEFINITIONS ----------------------------------------------------------
ALangDef scHtmlDef = { "html,htm,docbook", "\"", "", ",,<!,->","<(div[^>]*)>",
"abbr,ABBR,acronym,ACRONYM,applet,APPLET,area,AREA,b,B,"
"base,BASE,basefont,BASEFONT,bdo,BDO,blockquote,BLOCKQUOTE,bgsound,BGSOUND,"
"body,BODY,br,BR,caption,CAPTION,center,CENTER,cite,CITE,"
"col,COL,colgroup,COLGROUP,dd,DD,dfn,DFN,dir,DIR,"
"div,DIV,dl,DL,dt,DT,em,EM,embed,EMBED,"
"fieldset,FIELDSET,frame,FRAME,frameset,h1,H1,h2,H2,"
"h3,H3,h4,H4,h5,H5,h6,H6,head,HEAD,"
"html,HTML,hr,HR,iframe,IFRAME,ilayer,ILAYER,img,IMG,"
"isindex,ISINDEX,keygen,KEYGEN,li,LI,marquee,MARQUEE,multicol,MULTICOL,"
"nobr,NOBR,noembed,NOEMBED,noframes,NOFRAMES,noscript,NOSCRIPT,ol,OL,"
"optgroup,OPTGROUP,p,P,param,PARAM,pre,PRE,q,Q,"
"s,S,samp,SAMP,sub,SUB,sup,SUP,tbody,TBODY,"
"td,TD,textarea,TEXTAREA,tfoot,TFOOT,th,TH,thead,THEAD,"
"tr,TR,tt,TT,u,U,ul,UL,wbr,WBR,xmp,XMP,"

"accesskey,ACCESSKEY,alt,ALT,bgcolor,BGCOLOR,bordercolor,BORDERCOLOR,cellpadding,CELLPADDING,"
"cellspacing,CELLSPACING,charoff,CHAROFF,colspan,COLSPAN,coords,COORDS,dir,DIR,"
"enctype,ENCTYPE,frameborder,FRAMEBORDER,framespacing,FRAMESPACING,href,HREF,hspace,HSPACE,"
"id,ID,lang,LANG,leftmargin,LEFTMARGIN,longdesc,LONGDESC,marginheight,MARGINHEIGHT,"
"marginwidth,MARGINWIDTH,maxlength,MAXLENGTH,noresize,NORESIZE,noshade,NOSHADE,notab,NOTAB,"
"nowrap,NOWRAP,onchange,ONCHANGE,onclick,ONCLICK,ondblclick,ONDBLCLICK,onfocus,ONFOCUS,"
"onkeydown,ONKEYDOWN,onkeypress,ONKEYPRESS,onkeyup,ONKEYUP,onmousedown,ONMOUSEDOWN,onmousemove,ONMOUSEMOVE,"
"onmouseout,ONMOUSEOUT,onmouseover,ONMOUSEOVER,onmouseup,ONMOUSEUP,onreset,ONRESET,onselect,ONSELECT,"
"onsubmit,ONSUBMIT,onunload,ONUNLOAD,pluginspage,PLUGINSPAGE,readonly,READONLY,rel,REL,"
"rowspan,ROWSPAN,scrollamount,SCROLLAMOUNT,scrolldelay,SCROLLDELAY,scrolling,SCROLLING,tabindex,TABINDEX,"
"taborder,TABORDER,topmargin,TOPMARGIN,usemap,USEMAP,valign,VALIGN,vlink,VLINK,vspace,VSPACE"
};
#endif	// AISHTML_H
