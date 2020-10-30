
function showAll() {
	var frmElements = document.forms.control.elements;
	for (var i=1; i < frmElements.length; i++) {
		frmElements[i].selectedIndex = 0;
		}
	document.all.showview.innerHTML = "";
	document.all.showview.innerHTML = document.divs["printview"].innerHTML;
	return true;
	}

function showSection(currentCombo) {
	// Set the other selection combos to blank
	var sectionName = currentCombo.options[currentCombo.selectedIndex].text;
	var frmElements = document.forms.control.elements;
	for (var i=1; i < frmElements.length; i++) {
		if (frmElements[i] != currentCombo) {
			frmElements[i].selectedIndex = 0;
			}
		}

	document.all.showview.innerHTML = "";
	if (sectionName == "") return true;
	
	document.all.showview.innerHTML = document.divs[sectionName].innerHTML;
	return true;
	}

function buildIndex() {
	var s;
	document.divs = window.frames[0].document.all.tags("div");

	s = '<FORM ID="control"><TABLE CELLPADDING=0 CELLSPACING=5>';
	s += '<TR ><TD ROWSPAN=7 VALIGN=TOP WIDTH=150><SPAN STYLE="color: blue; font-size: large">' + document.title + '</SPAN><BR>';
	s += '<INPUT TYPE="BUTTON" VALUE="Show All" ONCLICK="showAll();"><BR></TD></TR>';
	
	// Create combo boxes for each h1section
	var i=0;
	while (i < document.divs.length) {
		if (document.divs[i].className == "h1section") {
			s += '<TR><TD>' + document.divs[i].id + '</TD><TD><SELECT  ONCHANGE="showSection(this)"  NAME="' + document.divs[i].id + '">';
			s += '<OPTION>'
			i++;
			while ( i < document.divs.length && document.divs[i].className != "h1section") {
				// Add h2sections and options in the select
				if (document.divs[i].className == "h2section") {
					s += '<OPTION>' + document.divs[i].id;
					}
				i++;
				}
			s += '</SELECT></TD></TR>';
			if (i < document.divs.length && document.divs[i].className == "h1section") continue; 
			} // end of handling h1section 
			i++;
		}
		
	// Now add a full alphabetic index of h2sections
	s += '<TR><TD>Alphabetic Index</TD><TD><SELECT ONCHANGE="showSection(this)"  NAME="Alphabetic Index">';
	s += '<OPTION>'
	var j = 0;
	var h2sections = new Array();
	for (i=0; i < document.divs.length; i++) {
		if (document.divs[i].className == "h2section")
			h2sections[j++] = document.divs[i].id;		
		}
	h2sections.sort();

	for (i=0; i < h2sections.length; i++) {
		s += "<OPTION>" + h2sections[i];
		}
	s += '</SELECT></TD></TR>';

	s += '</TABLE><HR ID="bottomofindex"></FORM>';

    document.all.indexsection.innerHTML = s;
	document.forms.control.elements[1].selectedIndex = 1;
	showSection(document.forms.control.elements[1]);
	document.all.showview.style.top = document.all.bottomofindex.offsetTop + 10;
	document.frames[0].printview.style.top = document.all.bottomofindex.offsetTop + 10;
	return true;
	}

function loadDoc() {
	buildIndex();	
	}	
