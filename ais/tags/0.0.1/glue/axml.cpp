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

#include "axml.h"
#include <stdio.h>
/*!
    The order of events in this interface is very important, and
    mirrors the order of information in the document itself. For
    example, all of an element's content (character data, processing
    instructions, and sub-elements) appears, in order, between the
    startElement() event and the corresponding endElement() event.

    The class QXmlDefaultHandler provides a default implementation for
    this inteHrface; subclassing from the QXmlDefaultHandler class is
    very convenient if you only want to be informed of some parsing
    events.

    The startDocument() function is called at the start of the
    document, and endDocument() is called at the end. Before parsing
    begins setDocumentLocator() is called. For each element
    startElement() is called, with endElement() being called at the
    end of each element. The characters() function is called with
    chunks of character data; ignorableWhitespace() is called with
    chunks of whitespace and processingInstruction() is called with
    processing instructions. If an entity is skipped skippedEntity()
    is called. At the beginning of prefix-URI scopes
    startPrefixMapping() is called.

*/


AXml::AXml(LpXCONTEXT aCP, LpTHREAD aTP, TVAL aXmlLambda) : gCP(aCP), gTP(aTP), mXmlLambda(aXmlLambda)
{
	// Grab a few important references from mXmlLambda. Note, we set references to these
	// items this way to insure they don't get garbage collected later. It allows us
	// use the TVAL's directly in our event handlers.
	StartFrame
	DeclareTVAL(Pv);
	DeclareTVAL(member);
	DeclareTVAL(getValue);
	EndFrame
	*getValue = TINT(1);
	*Pv = FSmartbase_Ref(gCP,gTP,2,aXmlLambda,TSYMBOL("Pv"));

	//new command
	*member = FSmartbase_Eval(gCP,gTP,TGVALUE("member"),2,TSYMBOL("cmdNew"),*Pv);
	if (member->Tag == TYNUM) {
		mCmdNew = FSmartbase_Ref(gCP,gTP,3,*Pv,*member,*getValue);
		}
	//Vector symbol
	*member = FSmartbase_Eval(gCP,gTP,TGVALUE("member"),2,TSYMBOL("symVector"),*Pv);
	if (member->Tag == TYNUM) {
		mSymVector = FSmartbase_Ref(gCP,gTP,3,*Pv,*member,*getValue);
		}

	//makeStructure command
	*member = FSmartbase_Eval(gCP,gTP,TGVALUE("member"),2,TSYMBOL("cmdMakeStructure"),*Pv);
	if (member->Tag == TYNUM) {
		mCmdMakeStructure = FSmartbase_Ref(gCP,gTP,3,*Pv,*member,*getValue);
		}

	//ColumnNumber symbol
	*member = FSmartbase_Eval(gCP,gTP,TGVALUE("member"),2,TSYMBOL("symColumnNumber"),*Pv);
	if (member->Tag == TYNUM) {
		mSymColumnNumber = FSmartbase_Ref(gCP,gTP,3,*Pv,*member,*getValue);
		}

	//LineNumber symbol
	*member = FSmartbase_Eval(gCP,gTP,TGVALUE("member"),2,TSYMBOL("symLineNumber"),*Pv);
	if (member->Tag == TYNUM) {
		mSymLineNumber = FSmartbase_Ref(gCP,gTP,3,*Pv,*member,*getValue);
		}

	//Message symbol
	*member = FSmartbase_Eval(gCP,gTP,TGVALUE("member"),2,TSYMBOL("symMessage"),*Pv);
	if (member->Tag == TYNUM) {
		mSymMessage = FSmartbase_Ref(gCP,gTP,3,*Pv,*member,*getValue);
		}

	//SystemID symbol
	*member = FSmartbase_Eval(gCP,gTP,TGVALUE("member"),2,TSYMBOL("symSystemID"),*Pv);
	if (member->Tag == TYNUM) {
		mSymSystemID = FSmartbase_Ref(gCP,gTP,3,*Pv,*member,*getValue);
		}

		//PublicID symbol
	*member = FSmartbase_Eval(gCP,gTP,TGVALUE("member"),2,TSYMBOL("symPublicID"),*Pv);
	if (member->Tag == TYNUM) {
		mSymPublicID = FSmartbase_Ref(gCP,gTP,3,*Pv,*member,*getValue);
		}
}

void AXml::setFeature ( const QString & name, bool value )
{	mFeatures[name] = value;
}

void AXml::clearFeatures()
{	mFeatures.clear();
}

// Pass through the pvars of aHanderLambda and 
// register any event handlers (child Lambdas) we find.
bool AXml::setHandlerLambda(TVAL aHandlerLambda)
{	
	// aHandlerLambda is a AIS Lambda that has one or more
	// child Lambdas named the same as the SAX handlers.
	// This routine finds those handlers and registers
	// them for callbacks.
	StartFrame
	DeclareTVAL(Pv);
	DeclareTVAL(member);
	DeclareTVAL(getValue);
	EndFrame
	*getValue = TINT(1);
	*Pv = FSmartbase_Ref(gCP,gTP,2,aHandlerLambda,TSYMBOL("Pv"));

	//StartDocument
	*member = FSmartbase_Eval(gCP,gTP,TGVALUE("member"),2,TSYMBOL("startDocument"),*Pv);
	if (member->Tag == TYNUM)
		mStartDocument = FSmartbase_Ref(gCP,gTP,3,*Pv,*member,*getValue);
	else mStartDocument = TVOID;

	//EndDocument
	*member = FSmartbase_Eval(gCP,gTP,TGVALUE("member"),2,TSYMBOL("endDocument"),*Pv);
	if (member->Tag == TYNUM) 
		mEndDocument = FSmartbase_Ref(gCP,gTP,3,*Pv,*member,*getValue);
	else mEndDocument = TVOID;

	//StartPrefixMapping
	*member = FSmartbase_Eval(gCP,gTP,TGVALUE("member"),2,TSYMBOL("startPrefixMapping"),*Pv);
	if (member->Tag == TYNUM) 
		mStartPrefixMapping = FSmartbase_Ref(gCP,gTP,3,*Pv,*member,*getValue);
	else mStartPrefixMapping = TVOID;

	//EndPrefixMapping
	*member = FSmartbase_Eval(gCP,gTP,TGVALUE("member"),2,TSYMBOL("endPrefixMapping"),*Pv);
	if (member->Tag == TYNUM) 
		mEndPrefixMapping = FSmartbase_Ref(gCP,gTP,3,*Pv,*member,*getValue);
	else mEndPrefixMapping = TVOID;

	//StartElement
	*member = FSmartbase_Eval(gCP,gTP,TGVALUE("member"),2,TSYMBOL("startElement"),*Pv);
	if (member->Tag == TYNUM) 
		mStartElement = FSmartbase_Ref(gCP,gTP,3,*Pv,*member,*getValue);
	else mStartElement = TVOID;

	//EndElement
	*member = FSmartbase_Eval(gCP,gTP,TGVALUE("member"),2,TSYMBOL("endElement"),*Pv);
	if (member->Tag == TYNUM) 
		mEndElement = FSmartbase_Ref(gCP,gTP,3,*Pv,*member,*getValue);
	else mEndElement = TVOID;

	//Characters
	*member = FSmartbase_Eval(gCP,gTP,TGVALUE("member"),2,TSYMBOL("characters"),*Pv);
	if (member->Tag == TYNUM) 
		mCharacters = FSmartbase_Ref(gCP,gTP,3,*Pv,*member,*getValue);
	else mCharacters = TVOID;

	//IgnoreableWhitespace
	*member = FSmartbase_Eval(gCP,gTP,TGVALUE("member"),2,TSYMBOL("ignorableWhitespace"),*Pv);
	if (member->Tag == TYNUM) 
		mIgnorableWhitespace = FSmartbase_Ref(gCP,gTP,3,*Pv,*member,*getValue);
	else mIgnorableWhitespace = TVOID;

	//ProcessingInstruction
	*member = FSmartbase_Eval(gCP,gTP,TGVALUE("member"),2,TSYMBOL("processingInstruction"),*Pv);
	if (member->Tag == TYNUM) 
		mProcessingInstruction = FSmartbase_Ref(gCP,gTP,3,*Pv,*member,*getValue);
	else mProcessingInstruction = TVOID;

	//SkippedEntity
	*member = FSmartbase_Eval(gCP,gTP,TGVALUE("member"),2,TSYMBOL("skippedEntity"),*Pv);
	if (member->Tag == TYNUM) 
		mSkippedEntity = FSmartbase_Ref(gCP,gTP,3,*Pv,*member,*getValue);
	else mSkippedEntity = TVOID;

	//Warning
	*member = FSmartbase_Eval(gCP,gTP,TGVALUE("member"),2,TSYMBOL("warning"),*Pv);
	if (member->Tag == TYNUM) 
		mWarning = FSmartbase_Ref(gCP,gTP,3,*Pv,*member,*getValue);
	else mWarning = TVOID;

	//Error
	*member = FSmartbase_Eval(gCP,gTP,TGVALUE("member"),2,TSYMBOL("error"),*Pv);
	if (member->Tag == TYNUM) 
		mError = FSmartbase_Ref(gCP,gTP,3,*Pv,*member,*getValue);
	else mError = TVOID;

	//FatalError
	*member = FSmartbase_Eval(gCP,gTP,TGVALUE("member"),2,TSYMBOL("fatalError"),*Pv);
	if (member->Tag == TYNUM) 
		mFatalError = FSmartbase_Ref(gCP,gTP,3,*Pv,*member,*getValue);
	else mFatalError = TVOID;

	//NotationDecl
	*member = FSmartbase_Eval(gCP,gTP,TGVALUE("member"),2,TSYMBOL("notationDecl"),*Pv);
	if (member->Tag == TYNUM) 
		mNotationDecl = FSmartbase_Ref(gCP,gTP,3,*Pv,*member,*getValue);
	else mNotationDecl = TVOID;

	//UnparsedEntity
	*member = FSmartbase_Eval(gCP,gTP,TGVALUE("member"),2,TSYMBOL("unparsedEntity"),*Pv);
	if (member->Tag == TYNUM) 
		mUnparsedEntity = FSmartbase_Ref(gCP,gTP,3,*Pv,*member,*getValue);
	else mUnparsedEntity = TVOID;

	//ResolveEntity
	*member = FSmartbase_Eval(gCP,gTP,TGVALUE("member"),2,TSYMBOL("resolveEntity"),*Pv);
	if (member->Tag == TYNUM) 
		mResolveEntity = FSmartbase_Ref(gCP,gTP,3,*Pv,*member,*getValue);
	else mResolveEntity = TVOID;

	//StartDTD
	*member = FSmartbase_Eval(gCP,gTP,TGVALUE("member"),2,TSYMBOL("startDTD"),*Pv);
	if (member->Tag == TYNUM) 
		mStartDTD = FSmartbase_Ref(gCP,gTP,3,*Pv,*member,*getValue);
	else mStartDTD = TVOID;

	//EndDTD
	*member = FSmartbase_Eval(gCP,gTP,TGVALUE("member"),2,TSYMBOL("endDTD"),*Pv);
	if (member->Tag == TYNUM) 
		mEndDTD = FSmartbase_Ref(gCP,gTP,3,*Pv,*member,*getValue);
	else mEndDTD = TVOID;

	//StartEntity
	*member = FSmartbase_Eval(gCP,gTP,TGVALUE("member"),2,TSYMBOL("startEntity"),*Pv);
	if (member->Tag == TYNUM) 
		mStartEntity = FSmartbase_Ref(gCP,gTP,3,*Pv,*member,*getValue);
	else mStartEntity = TVOID;

	//EndEntity
	*member = FSmartbase_Eval(gCP,gTP,TGVALUE("member"),2,TSYMBOL("endEntity"),*Pv);
	if (member->Tag == TYNUM) 
		mEndEntity = FSmartbase_Ref(gCP,gTP,3,*Pv,*member,*getValue);
	else mEndEntity = TVOID;

	//StartCDATA
	*member = FSmartbase_Eval(gCP,gTP,TGVALUE("member"),2,TSYMBOL("startCDATA"),*Pv);
	if (member->Tag == TYNUM) 
		mStartCDATA = FSmartbase_Ref(gCP,gTP,3,*Pv,*member,*getValue);
	else mStartCDATA = TVOID;

	//EndCDATA
	*member = FSmartbase_Eval(gCP,gTP,TGVALUE("member"),2,TSYMBOL("endCDATA"),*Pv);
	if (member->Tag == TYNUM)
		mEndCDATA = FSmartbase_Ref(gCP,gTP,3,*Pv,*member,*getValue);
	else mEndCDATA = TVOID;

	//Comment
	*member = FSmartbase_Eval(gCP,gTP,TGVALUE("member"),2,TSYMBOL("comment"),*Pv);
	if (member->Tag == TYNUM) 
		mComment = FSmartbase_Ref(gCP,gTP,3,*Pv,*member,*getValue);
	else mComment = TVOID;

	//AttributeDecl
	*member = FSmartbase_Eval(gCP,gTP,TGVALUE("member"),2,TSYMBOL("attributeDecl"),*Pv);
	if (member->Tag == TYNUM) 
		mAttributeDecl = FSmartbase_Ref(gCP,gTP,3,*Pv,*member,*getValue);
	else mAttributeDecl = TVOID;

	//InternalEntityDecl
	*member = FSmartbase_Eval(gCP,gTP,TGVALUE("member"),2,TSYMBOL("internalEntityDecl"),*Pv);
	if (member->Tag == TYNUM)
		mInternalEntityDecl = FSmartbase_Ref(gCP,gTP,3,*Pv,*member,*getValue);
	else mInternalEntityDecl = TVOID;

	//ExternalEntityDecl
	*member = FSmartbase_Eval(gCP,gTP,TGVALUE("member"),2,TSYMBOL("externalEntityDecl"),*Pv);
	if (member->Tag == TYNUM)
		mExternalEntityDecl = FSmartbase_Ref(gCP,gTP,3,*Pv,*member,*getValue);
	else mExternalEntityDecl = TVOID;

return true;
}

// Parse the file specified by aFilename
bool AXml::parseFile(const QString& aFilename)
{
	QXmlSimpleReader reader;

	// Set features
	FeatureMap::Iterator it;
    for ( it = mFeatures.begin(); it != mFeatures.end(); ++it ) {
		reader.setFeature(it.key(),*it);
    }
	reader.setContentHandler(this);
	reader.setDeclHandler(this);
	reader.setDTDHandler (this);
	reader.setEntityResolver(this);
	reader.setErrorHandler(this);
	reader.setLexicalHandler(this);
	QFile xmlFile( aFilename );
	QXmlInputSource source( &xmlFile );
	reader.parse( source );
	return true;
}

// Parse a null terminated memory buffer
bool AXml::parseBuffer(const char* ipBuffer)
{
	QXmlSimpleReader reader;
	FeatureMap::Iterator it;
    for ( it = mFeatures.begin(); it != mFeatures.end(); ++it ) {
		reader.setFeature(it.key(),*it);
    }
	reader.setContentHandler(this);
	reader.setDeclHandler(this);
	reader.setDTDHandler(this);
	reader.setEntityResolver(this);
	reader.setErrorHandler(this);
	reader.setLexicalHandler(this);
	QXmlInputSource source;
	source.setData(QByteArray(ipBuffer));
	reader.parse( source );
	return true;
}

/*!
    \fn void AXml::setDocumentLocator( QXmlLocator* locator )

    The reader calls this function before it starts parsing the
    document. The argument \a locator is a pointer to a QXmlLocator
    which allows the application to get the parsing position within
    the document.

    Do not destroy the \a locator; it is destroyed when the reader is
    destroyed. (Do not use the \a locator after the reader is
    destroyed).
*/

void AXml::setDocumentLocator( QXmlLocator* )
{

}
/*!
    \fn bool AXml::startDocument()

    The reader calls this function when it starts parsing the
    document. The reader calls this function just once, after the call
    to setDocumentLocator(), and before any other functions in this
    class or in the AXml class are called.

    If this function returns FALSE the reader stops parsing and
    reports an error. The reader uses the function errorString() to
    get the error message.

    \sa endDocument()
*/
bool AXml::startDocument()
{
	if (mStartDocument.Tag == TYLAMBDA)
		FSmartbase_Eval(gCP,gTP,mStartDocument,0);
	mIndent = "";
    return TRUE;
}

/*!
    \fn bool AXml::endDocument()

    The reader calls this function after it has finished parsing. It
    is called just once, and is the last handler function called. It
    is called after the reader has read all input or has abandoned
    parsing because of a fatal error.

    If this function returns FALSE the reader stops parsing and
    reports an error. The reader uses the function errorString() to
    get the error message.

    \sa startDocument()
*/
bool AXml::endDocument()
{
	StartFrame
	DeclareTVAL(result);
	EndFrame
	if(mEndDocument.Tag == TYLAMBDA) {
		*result = FSmartbase_Eval(gCP,gTP,mEndDocument,0);
		FrameExit(asBool(result))
	}
    FrameExit(true)
}

/*!
    \fn bool AXml::startPrefixMapping( const QString& prefix, const QString& uri )

    The reader calls this function to signal the begin of a prefix-URI
    namespace mapping scope. This information is not necessary for
    normal namespace processing since the reader automatically
    replaces prefixes for element and attribute names.

    Note that startPrefixMapping() and endPrefixMapping() calls are
    not guaranteed to be properly nested relative to each other: all
    startPrefixMapping() events occur before the corresponding
    startElement() event, and all endPrefixMapping() events occur
    after the corresponding endElement() event, but their order is not
    otherwise guaranteed.

    The argument \a prefix is the namespace prefix being declared and
    the argument \a uri is the namespace URI the prefix is mapped to.

    If this function returns FALSE the reader stops parsing and
    reports an error. The reader uses the function errorString() to
    get the error message.

    See also the \link xml.html#sax2Namespaces namespace description\endlink.

    \sa endPrefixMapping()
*/
bool AXml::startPrefixMapping( const QString& prefix, const QString& uri )
{
	StartFrame
	DeclareTVAL(result);
	EndFrame
	if (mStartPrefixMapping.Tag == TYLAMBDA) {
		*result = FSmartbase_Eval(gCP,gTP,mStartPrefixMapping,2,
		TSTRING(prefix.toLatin1().data()),
		TSTRING(uri.toLatin1().data()));
		FrameExit(asBool(result))
	}
    FrameExit(true)
}

/*!
    \fn bool AXml::endPrefixMapping( const QString& prefix )

    The reader calls this function to signal the end of a prefix
    mapping for the prefix \a prefix.

    If this function returns FALSE the reader stops parsing and
    reports an error. The reader uses the function errorString() to
    get the error message.

    See also the \link xml.html#sax2Namespaces namespace description\endlink.

    \sa startPrefixMapping()
*/
bool AXml::endPrefixMapping( const QString& prefix )
{
	StartFrame
	DeclareTVAL(result);
	EndFrame
	if(mEndPrefixMapping.Tag == TYLAMBDA) {
		*result = FSmartbase_Eval(gCP,gTP,mEndPrefixMapping,1,TSTRING(prefix.toLatin1().data()));
		FrameExit(asBool(result))
	}
    FrameExit(true)
}

/*!
    \fn bool AXml::startElement( const QString& namespaceURI, const QString& localName, const QString& qName, const QXmlAttributes& atts )

    The reader calls this function when it has parsed a start element
    tag.

    There is a corresponding endElement() call when the corresponding
    end element tag is read. The startElement() and endElement() calls
    are always nested correctly. Empty element tags (e.g. \c{<x/>})
    cause a startElement() call to be immediately followed by an
    endElement() call.

    The attribute list provided only contains attributes with explicit
    values. The attribute list contains attributes used for namespace
    declaration (i.e. attributes starting with xmlns) only if the
    namespace-prefix property of the reader is TRUE.

    The argument \a namespaceURI is the namespace URI, or
    QString::null if the element has no namespace URI or if no
    namespace processing is done. \a localName is the local name
    (without prefix), or QString::null if no namespace processing is
    done, \a qName is the qualified name (with prefix) and \a atts are
    the attributes attached to the element. If there are no
    attributes, \a atts is an empty attributes object.

    If this function returns FALSE the reader stops parsing and
    reports an error. The reader uses the function errorString() to
    get the error message.

    See also the \link xml.html#sax2Namespaces namespace description\endlink.

    \sa endElement()
*/
bool AXml::startElement(const QString& namespaceURI, 
						const QString& localName, 
                        const QString& qName, 
                        const QXmlAttributes& aAttrs )
{
	StartFrame
	DeclareTVAL(result);
	DeclareTVAL(sAttrs);
	DeclareTVAL(attrStruct);
	EndFrame

	// Build sAttrs object vector to hold
	// structure for each attribute.
	long c = aAttrs.count();
	*sAttrs = FSmartbase_Eval(gCP,gTP,mCmdNew,2,mSymVector,TINT(c));
	for(long i=0; i < aAttrs.count(); ++i)
	{	
		*attrStruct = FSmartbase_Eval(gCP,gTP,mCmdNew,7,mSymVector,TINT(5),
			TSTRING(aAttrs.localName(i).toLatin1().data()),
			TSTRING(aAttrs.qName(i).toLatin1().data()),
			TSTRING(aAttrs.uri(i).toLatin1().data()),
			TSTRING(aAttrs.type(i).toLatin1().data()),
			TSTRING(aAttrs.value(i).toLatin1().data()));
		*result = FSmartbase_Set(gCP,gTP,3,*sAttrs,TINT(i),*attrStruct);
	}

	if(mStartElement.Tag == TYLAMBDA) {
		*result = FSmartbase_Eval(gCP,gTP,mStartElement,4,
			TSTRING(namespaceURI.toLatin1().data()),
			TSTRING(localName.toLatin1().data()),
			TSTRING(qName.toLatin1().data()), 
			*sAttrs);
		FrameExit(asBool(result))
	}
    FrameExit(true)
}

/*!
    \fn bool AXml::endElement( const QString& namespaceURI, const QString& localName, const QString& qName )

    The reader calls this function when it has parsed an end element
    tag with the qualified name \a qName, the local name \a localName
    and the namespace URI \a namespaceURI.

    If this function returns FALSE the reader stops parsing and
    reports an error. The reader uses the function errorString() to
    get the error message.

    See also the \link xml.html#sax2Namespaces namespace description\endlink.

    \sa startElement()
*/
bool AXml::endElement( const QString& namespaceURI, const QString& localName, const QString& qName )
{
	StartFrame
	DeclareTVAL(result);
	EndFrame

	if(mEndElement.Tag == TYLAMBDA) {
		*result = FSmartbase_Eval(gCP,gTP,mEndElement,3,
			TSTRING(namespaceURI.toLatin1().data()),
			TSTRING(localName.toLatin1().data()),
			TSTRING(qName.toLatin1().data()));
		FrameExit(asBool(result))
	}

    FrameExit(true);
}

/*!
    \fn bool AXml::characters( const QString& ch )

    The reader calls this function when it has parsed a chunk of
    character data (either normal character data or character data
    inside a CDATA section; if you need to distinguish between those
    two types you must use AXml::startCDATA() and
    AXml::endCDATA()). The character data is reported in
    \a ch.

    Some readers report whitespace in element content using the
    ignorableWhitespace() function rather than using this one.

    A reader may report the character data of an element in more than
    one chunk; e.g. a reader might want to report "a\<b" in three
    characters() events ("a ", "\<" and " b").

    If this function returns FALSE the reader stops parsing and
    reports an error. The reader uses the function errorString() to
    get the error message.
*/

bool AXml::characters( const QString& ch)
{
	StartFrame
	DeclareTVAL(result);
	EndFrame
	if(mCharacters.Tag == TYLAMBDA) {
		*result = FSmartbase_Eval(gCP,gTP,mCharacters,1,TSTRING(ch.toLatin1().data()));
		FrameExit(asBool(result))
	}
    FrameExit(true);
}

/*!
    \fn bool AXml::ignorableWhitespace( const QString& ch )

    Some readers may use this function to report each chunk of
    whitespace in element content. The whitespace is reported in \a ch.

    If this function returns FALSE the reader stops parsing and
    reports an error. The reader uses the function errorString() to
    get the error message.
*/

bool AXml::ignorableWhitespace( const QString& ch)
{
	StartFrame
	DeclareTVAL(result);
	EndFrame
	if(mIgnorableWhitespace.Tag == TYLAMBDA) {
		*result = FSmartbase_Eval(gCP,gTP,mIgnorableWhitespace,1,
			TSTRING(ch.toLatin1().data()));
		FrameExit(asBool(result))
	}
    FrameExit(true);
}

/*!
    \fn bool AXml::processingInstruction( const QString& target, const QString& data )

    The reader calls this function when it has parsed a processing
    instruction.

    \a target is the target name of the processing instruction and \a
    data is the data in the processing instruction.

    If this function returns FALSE the reader stops parsing and
    reports an error. The reader uses the function errorString() to
    get the error message.
*/
bool AXml::processingInstruction( const QString& target,
	const QString& data)
{
	StartFrame
	DeclareTVAL(result);
	EndFrame
	if(mProcessingInstruction.Tag == TYLAMBDA) {
		*result = FSmartbase_Eval(gCP,gTP,mProcessingInstruction,2,
			TSTRING(target.toLatin1().data()),
			TSTRING(data.toLatin1().data()));
		FrameExit(asBool(result))
	}
    FrameExit(true);

}
/*!
    \fn bool AXml::skippedEntity( const QString& name )

    Some readers may skip entities if they have not seen the
    declarations (e.g. because they are in an external DTD). If they
    do so they report that they skipped the entity called \a name by
    calling this function.

    If this function returns FALSE the reader stops parsing and
    reports an error. The reader uses the function errorString() to
    get the error message.
*/

bool AXml::skippedEntity( const QString& name)
{
	StartFrame
	DeclareTVAL(result);
	EndFrame
	if(mSkippedEntity.Tag == TYLAMBDA) {
		*result = FSmartbase_Eval(gCP,gTP,mSkippedEntity,1,
			TSTRING(name.toLatin1().data()));
		FrameExit(asBool(result))
	}
    FrameExit(true);

}

/*!
    If you want your application to report errors to the user or to
    perform customized error handling, you should subclass this class.

    You can set the error handler with QXmlReader::setErrorHandler().

    Errors can be reported using warning(), error() and fatalError(),
    with the error text being reported with errorString().

*/

/*!
    \fn bool AXml::warning( const QXmlParseException& exception )

    A reader might use this function to report a warning. Warnings are
    conditions that are not errors or fatal errors as defined by the
    XML 1.0 specification. Details of the warning are stored in \a
    exception.

    If this function returns FALSE the reader stops parsing and
    reports an error. The reader uses the function errorString() to
    get the error message.
*/

bool AXml::warning( const QXmlParseException& exception)
{
	StartFrame
	DeclareTVAL(result);
	DeclareTVAL(sException);
	EndFrame

	*sException = FSmartbase_Eval(gCP,gTP,mCmdMakeStructure,0);	
	*result = FSmartbase_Set(gCP,gTP,3,*sException,mSymColumnNumber,TINT(exception.columnNumber()));
	*result = FSmartbase_Set(gCP,gTP,3,*sException,mSymLineNumber,TINT(exception.lineNumber()));
	*result = FSmartbase_Set(gCP,gTP,3,*sException,mSymMessage,TSTRING(exception.message().toLatin1().data()));
	*result = FSmartbase_Set(gCP,gTP,3,*sException,mSymPublicID,TSTRING(exception.publicId().toLatin1().data()));
	*result = FSmartbase_Set(gCP,gTP,3,*sException,mSymSystemID,TSTRING(exception.systemId().toLatin1().data()));
		
	if(mWarning.Tag == TYLAMBDA) {
		*result = FSmartbase_Eval(gCP,gTP,mWarning,1,*sException);
		FrameExit(asBool(result))
	}
    FrameExit(true)

}

/*!
    \fn bool AXml::error( const QXmlParseException& exception )

    A reader might use this function to report a recoverable error. A
    recoverable error corresponds to the definiton of "error" in
    section 1.2 of the XML 1.0 specification. Details of the error are
    stored in \a exception.

    The reader must continue to provide normal parsing events after
    invoking this function.

    If this function returns FALSE the reader stops parsing and
    reports an error. The reader uses the function errorString() to
    get the error message.
*/

bool AXml::error( const QXmlParseException& exception)
{
	StartFrame
	DeclareTVAL(result);
	DeclareTVAL(sException);
	EndFrame

	*sException = FSmartbase_Eval(gCP,gTP,mCmdMakeStructure,0);	
	*result = FSmartbase_Set(gCP,gTP,3,*sException,mSymColumnNumber,TINT(exception.columnNumber()));
	*result = FSmartbase_Set(gCP,gTP,3,*sException,mSymLineNumber,TINT(exception.lineNumber()));
	*result = FSmartbase_Set(gCP,gTP,3,*sException,mSymMessage,TSTRING(exception.message().toLatin1().data()));
	*result = FSmartbase_Set(gCP,gTP,3,*sException,mSymPublicID,TSTRING(exception.publicId().toLatin1().data()));
	*result = FSmartbase_Set(gCP,gTP,3,*sException,mSymSystemID,TSTRING(exception.systemId().toLatin1().data()));
		
	if(mError.Tag == TYLAMBDA) {
		*result = FSmartbase_Eval(gCP,gTP,mError,1,*sException);
		FrameExit(asBool(result))
	}
    FrameExit(true)

}

/*!
    \fn bool AXml::fatalError( const QXmlParseException& exception )

    A reader must use this function to report a non-recoverable error.
    Details of the error are stored in \a exception.

    If this function returns TRUE the reader might try to go on
    parsing and reporting further errors; but no regular parsing
    events are reported.
*/

bool AXml::fatalError( const QXmlParseException& exception)
{
	StartFrame
	DeclareTVAL(result);
	DeclareTVAL(sException);
	EndFrame

	*sException = FSmartbase_Eval(gCP,gTP,mCmdMakeStructure,0);	
	*result = FSmartbase_Set(gCP,gTP,3,*sException,mSymColumnNumber,TINT(exception.columnNumber()));
	*result = FSmartbase_Set(gCP,gTP,3,*sException,mSymLineNumber,TINT(exception.lineNumber()));
	*result = FSmartbase_Set(gCP,gTP,3,*sException,mSymMessage,TSTRING(exception.message().toLatin1().data()));
	*result = FSmartbase_Set(gCP,gTP,3,*sException,mSymPublicID,TSTRING(exception.publicId().toLatin1().data()));
	*result = FSmartbase_Set(gCP,gTP,3,*sException,mSymSystemID,TSTRING(exception.systemId().toLatin1().data()));
		
	if(mFatalError.Tag == TYLAMBDA) {
		*result = FSmartbase_Eval(gCP,gTP,mFatalError,1,*sException);
		FrameExit(asBool(result))
	}
    FrameExit(true)

}


/*!
    If an application needs information about notations and unparsed
    entities, it can implement this interface and register an instance
    with QXmlReader::setDTDHandler().

    Note that this interface includes only those DTD events that the
    XML recommendation requires processors to report, i.e. notation
    and unparsed entity declarations using notationDecl() and
    unparsedEntityDecl() respectively.

    See also the \link xml.html#sax2Intro Introduction to SAX2\endlink.

    \sa AXml AXml AXml AXml
    AXml
*/

/*!
    \fn bool AXml::notationDecl( const QString& name, const QString& publicId, const QString& systemId )

    The reader calls this function when it has parsed a notation
    declaration.

    The argument \a name is the notation name, \a publicId is the
    notation's public identifier and \a systemId is the notation's
    system identifier.

    If this function returns FALSE the reader stops parsing and
    reports an error. The reader uses the function errorString() to
    get the error message.
*/

bool AXml::notationDecl( const QString& name, const QString& publicId,const QString& systemId )
{
	StartFrame
	DeclareTVAL(result);
	EndFrame	
	if(mNotationDecl.Tag == TYLAMBDA) {
		*result = FSmartbase_Eval(gCP,gTP,mNotationDecl,3,
			TSTRING(name.toLatin1().data()),
			TSTRING(publicId.toLatin1().data()),
			TSTRING(systemId.toLatin1().data()));
		FrameExit(asBool(result))
	}
    FrameExit(true)
}

/*!
    \fn bool AXml::unparsedEntityDecl( const QString& name, const QString& publicId, const QString& systemId, const QString& notationName )

    The reader calls this function when it finds an unparsed entity
    declaration.

    The argument \a name is the unparsed entity's name, \a publicId is
    the entity's public identifier, \a systemId is the entity's system
    identifier and \a notationName is the name of the associated
    notation.

    If this function returns FALSE the reader stops parsing and
    reports an error. The reader uses the function errorString() to
    get the error message.
*/

bool AXml::unparsedEntityDecl( const QString& name, const QString& publicID,	const QString& systemID, const QString& notationName)
{
	StartFrame
	DeclareTVAL(result);
	EndFrame	
	if(mNotationDecl.Tag == TYLAMBDA) {
		*result = FSmartbase_Eval(gCP,gTP,mNotationDecl,4,
			TSTRING(name.toLatin1().data()),
			TSTRING(publicID.toLatin1().data()),
			TSTRING(systemID.toLatin1().data()),
			TSTRING(notationName.toLatin1().data()));
		FrameExit(asBool(result))
	}
    FrameExit(true)

}
/*!
    If an application needs to implement customized handling for
    external entities, it must implement this interface, i.e.
    resolveEntity(), and register it with
    QXmlReader::setEntityResolver().

*/

/*!
    \fn bool AXml::resolveEntity( const QString &publicId, const QString &systemId, QXmlInputSource *&ret )

    The reader calls this function before it opens any external
    entity, except the top-level document entity. The application may
    request the reader to resolve the entity itself (\a ret is 0) or
    to use an entirely different input source (\a ret points to the
    input source).

    The reader deletes the input source \a ret when it no longer needs
    it, so you should allocate it on the heap with \c new.

    The argument \a publicId is the public identifier of the external
    entity, \a systemId is the system identifier of the external
    entity and \a ret is the return value of this function. If \a ret
    is 0 the reader should resolve the entity itself, if it is
    non-zero it must point to an input source which the reader uses
    instead.

    If this function returns FALSE the reader stops parsing and
    reports an error. The reader uses the function errorString() to
    get the error message.
*/
bool AXml::resolveEntity( const QString &publicId, const QString &systemId, QXmlInputSource *&ret )
{
    Q_UNUSED(publicId);
    Q_UNUSED(systemId);

	// I currently belive that we should handle this at this level. However, we can wait to implement this until
	// later. TM
    ret = 0;
    return TRUE;
}

/*!
    \fn QString AXml::errorString()

    The reader calls this function to get an error string, e.g. if any
    of the handler functions returns FALSE.
*/

QString AXml::errorString()
{
    return QString( XMLERR_ERRORBYCONSUMER );
}

/*!
    The events in the lexical handler apply to the entire document,
    not just to the document element, and all lexical handler events
    appear between the content handler's startDocument and endDocument
    events.

    You can set the lexical handler with
    QXmlReader::setLexicalHandler().

    This interface's design is based on the the SAX2 extension
    LexicalHandler.

    The interface provides the startDTD(), endDTD(), startEntity(),
    endEntity(), startCDATA(), endCDATA() and comment() functions.

*/

/*!
    \fn bool AXml::startDTD( const QString& name, const QString& publicId, const QString& systemId )

    The reader calls this function to report the start of a DTD
    declaration, if any. It reports the name of the document type in
    \a name, the public identifier in \a publicId and the system
    identifier in \a systemId.

    If the public identifier is missing, \a publicId is set to
    QString::null. If the system identifier is missing, \a systemId is
    set to QString::null. Note that it is not valid XML to have a
    public identifier but no system identifier; in such cases a parse
    error will occur.

    All declarations reported through AXml or
    AXml appear between the startDTD() and endDTD() calls.

    If this function returns FALSE the reader stops parsing and
    reports an error. The reader uses the function errorString() to
    get the error message.

    \sa endDTD()
*/

bool AXml::startDTD( const QString& name, const QString& publicId, const QString& systemId )
{
	StartFrame
	DeclareTVAL(result);
	EndFrame	
	if(mStartDTD.Tag == TYLAMBDA) {
		*result = FSmartbase_Eval(gCP,gTP,mStartDTD,3,
			TSTRING(name.toLatin1().data()),
			TSTRING(publicId.toLatin1().data()),
			TSTRING(systemId.toLatin1().data()));
		FrameExit(asBool(result))
	}
    FrameExit(true)
}

/*!
    \fn bool AXml::endDTD()

    The reader calls this function to report the end of a DTD
    declaration, if any.

    If this function returns FALSE the reader stops parsing and
    reports an error. The reader uses the function errorString() to
    get the error message.

    \sa startDTD()
*/

bool AXml::endDTD()
{
	StartFrame
	DeclareTVAL(result);
	EndFrame	
	if(mNotationDecl.Tag == TYLAMBDA) {
		*result = FSmartbase_Eval(gCP,gTP,mEndDTD,0);
		FrameExit(asBool(result))
	}
    FrameExit(true)

}

/*!
    \fn bool AXml::startEntity( const QString& name )

    The reader calls this function to report the start of an entity
    called \a name.

    Note that if the entity is unknown, the reader reports it through
    AXml::skippedEntity() and not through this
    function.

    If this function returns FALSE the reader stops parsing and
    reports an error. The reader uses the function errorString() to
    get the error message.

    \sa endEntity() QXmlSimpleReader::setFeature()
*/
bool AXml::startEntity( const QString& name)
{
	StartFrame
	DeclareTVAL(result);
	EndFrame	
	if(mStartEntity.Tag == TYLAMBDA) {
		*result = FSmartbase_Eval(gCP,gTP,mStartEntity,1,
			TSTRING(name.toLatin1().data()));
		FrameExit(asBool(result))
	}
    FrameExit(true)
}

/*!
    \fn bool AXml::endEntity( const QString& name )

    The reader calls this function to report the end of an entity
    called \a name.

    For every startEntity() call, there is a corresponding endEntity()
    call. The calls to startEntity() and endEntity() are properly
    nested.

    If this function returns FALSE the reader stops parsing and
    reports an error. The reader uses the function errorString() to
    get the error message.

    \sa startEntity() AXml::skippedEntity() QXmlSimpleReader::setFeature()
*/
bool AXml::endEntity( const QString& name)
{
	StartFrame
	DeclareTVAL(result);
	EndFrame	
	if(mEndEntity.Tag == TYLAMBDA) {
		*result = FSmartbase_Eval(gCP,gTP,mEndEntity,1,
			TSTRING(name.toLatin1().data()));
		FrameExit(asBool(result))
	}
    FrameExit(true)

}

/*!
    \fn bool AXml::startCDATA()

    The reader calls this function to report the start of a CDATA
    section. The content of the CDATA section is reported through the
    AXml::characters() function. This function is
    intended only to report the boundary.

    If this function returns FALSE the reader stops parsing and
    reports an error. The reader uses the function errorString() to
    get the error message.

    \sa endCDATA()
*/
bool AXml::startCDATA()
{
	StartFrame
	DeclareTVAL(result);
	EndFrame	
	if(mStartCDATA.Tag == TYLAMBDA) {
		*result = FSmartbase_Eval(gCP,gTP,mStartCDATA,0);
		FrameExit(asBool(result))
	}
    FrameExit(true)

}

/*!
    \fn bool AXml::endCDATA()

    The reader calls this function to report the end of a CDATA
    section.

    If this function returns FALSE the reader stops parsing and reports
    an error. The reader uses the function errorString() to get the error
    message.

    \sa startCDATA() AXml::characters()
*/
bool AXml::endCDATA()
{
	StartFrame
	DeclareTVAL(result);
	EndFrame	
	if(mEndCDATA.Tag == TYLAMBDA) {
		*result = FSmartbase_Eval(gCP,gTP,mEndCDATA,0);
		FrameExit(asBool(result))
	}
    FrameExit(true)

}

/*!
    \fn bool AXml::comment( const QString& ch )

    The reader calls this function to report an XML comment anywhere
    in the document. It reports the text of the comment in \a ch.

    If this function returns FALSE the reader stops parsing and
    reports an error. The reader uses the function errorString() to
    get the error message.
*/
bool AXml::comment( const QString& ch)
{
	StartFrame
	DeclareTVAL(result);
	EndFrame	
	if(mComment.Tag == TYLAMBDA) {
		*result = FSmartbase_Eval(gCP,gTP,mComment,1,
			TSTRING(ch.toLatin1().data()));
		FrameExit(asBool(result))
	}
    FrameExit(true)

}

/*!
    You can set the declaration handler with
    QXmlReader::setDeclHandler().

    This interface is based on the SAX2 extension DeclHandler.

    The interface provides attributeDecl(), internalEntityDecl() and
    externalEntityDecl() functions.

*/

/*!
    \fn bool AXml::attributeDecl( const QString& eName, const QString& aName, const QString& type, 
	const QString& valueDefault, const QString& value )

    The reader calls this function to report an attribute type
    declaration. Only the effective (first) declaration for an
    attribute is reported.

    The reader passes the name of the associated element in \a eName
    and the name of the attribute in \a aName. It passes a string that
    represents the attribute type in \a type and a string that
    represents the attribute default in \a valueDefault. This string
    is one of "#IMPLIED", "#REQUIRED", "#FIXED" or QString::null (if
    none of the others applies). The reader passes the attribute's
    default value in \a value. If no default value is specified in the
    XML file, \a value is QString::null.

    If this function returns FALSE the reader stops parsing and
    reports an error. The reader uses the function errorString() to
    get the error message.
*/
bool AXml::attributeDecl( const QString& eName, const QString& aName, const QString& type, 
						 const QString& valueDefault, const QString& value )
{
	StartFrame
	DeclareTVAL(result);
	EndFrame	
	if(mAttributeDecl.Tag == TYLAMBDA) {
		*result = FSmartbase_Eval(gCP,gTP,mAttributeDecl,5,
			TSTRING(eName.toLatin1().data()),
			TSTRING(aName.toLatin1().data()),
			TSTRING(type.toLatin1().data()),
			TSTRING(valueDefault.toLatin1().data()),
			TSTRING(value.toLatin1().data()));
		FrameExit(asBool(result))
	}
    FrameExit(true)

}

/*!
    \fn bool AXml::internalEntityDecl( const QString& name, const QString& value )

    The reader calls this function to report an internal entity
    declaration. Only the effective (first) declaration is reported.

    The reader passes the name of the entity in \a name and the value
    of the entity in \a value.

    If this function returns FALSE the reader stops parsing and
    reports an error. The reader uses the function errorString() to
    get the error message.
*/

bool AXml::internalEntityDecl( const QString& name, const QString& value)
{
	StartFrame
	DeclareTVAL(result);
	EndFrame	
	if(mInternalEntityDecl.Tag == TYLAMBDA) {
		*result = FSmartbase_Eval(gCP,gTP,mInternalEntityDecl,2,
			TSTRING(name.toLatin1().data()),
			TSTRING(value.toLatin1().data()));
		FrameExit(asBool(result))
	}
    FrameExit(true)

}

/*!
    \fn bool AXml::externalEntityDecl( const QString& name, const QString& publicId, const QString& systemId )

    The reader calls this function to report a parsed external entity
    declaration. Only the effective (first) declaration for each
    entity is reported.

    The reader passes the name of the entity in \a name, the public
    identifier in \a publicId and the system identifier in \a
    systemId. If there is no public identifier specified, it passes
    QString::null in \a publicId.

    If this function returns FALSE the reader stops parsing and
    reports an error. The reader uses the function errorString() to
    get the error message.
*/
bool AXml::externalEntityDecl( const QString& name, const QString& publicId, const QString& systemId)
{
	StartFrame
	DeclareTVAL(result);
	EndFrame	
	if(mExternalEntityDecl.Tag == TYLAMBDA) {
		*result = FSmartbase_Eval(gCP,gTP,mExternalEntityDecl,3,
			TSTRING(name.toLatin1().data()),
			TSTRING(publicId.toLatin1().data()),
			TSTRING(systemId.toLatin1().data()));
		FrameExit(asBool(result))
	}
    FrameExit(true)
}

