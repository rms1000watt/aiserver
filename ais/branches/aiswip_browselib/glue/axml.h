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

#ifndef XMLPARSER_H
#define XMLPARSER_H   
#include <QtCore/QMap>
#include <QtXml/qxml.h>
#include <QtCore/QFile>

// needed for QT_TRANSLATE_NOOP:
#include <QtCore/QObject>
#include <QtCore/QString>

extern "C" { // includes for modules written in C
	#include "fsmtbase.h" // SmartBase engine declarations
}

typedef QMap<QString, bool> FeatureMap;

// Error strings for the XML reader - note that these are duplicated from qxml.cpp.
#define XMLERR_OK                         QT_TRANSLATE_NOOP( "QXml", "no error occurred" )
#define XMLERR_ERRORBYCONSUMER            QT_TRANSLATE_NOOP( "QXml", "error triggered by consumer" )
#define XMLERR_UNEXPECTEDEOF              QT_TRANSLATE_NOOP( "QXml", "unexpected end of file" )
#define XMLERR_MORETHANONEDOCTYPE         QT_TRANSLATE_NOOP( "QXml", "more than one document type definition" )
#define XMLERR_ERRORPARSINGELEMENT        QT_TRANSLATE_NOOP( "QXml", "error occurred while parsing element" )
#define XMLERR_TAGMISMATCH                QT_TRANSLATE_NOOP( "QXml", "tag mismatch" )
#define XMLERR_ERRORPARSINGCONTENT        QT_TRANSLATE_NOOP( "QXml", "error occurred while parsing content" )
#define XMLERR_UNEXPECTEDCHARACTER        QT_TRANSLATE_NOOP( "QXml", "unexpected character" )
#define XMLERR_INVALIDNAMEFORPI           QT_TRANSLATE_NOOP( "QXml", "invalid name for processing instruction" )
#define XMLERR_VERSIONEXPECTED            QT_TRANSLATE_NOOP( "QXml", "version expected while reading the XML declaration" )
#define XMLERR_WRONGVALUEFORSDECL         QT_TRANSLATE_NOOP( "QXml", "wrong value for standalone declaration" )
#define XMLERR_EDECLORSDDECLEXPECTED      QT_TRANSLATE_NOOP( "QXml", "encoding declaration or standalone declaration expected while reading the XML declaration" )
#define XMLERR_SDDECLEXPECTED             QT_TRANSLATE_NOOP( "QXml", "standalone declaration expected while reading the XML declaration" )
#define XMLERR_ERRORPARSINGDOCTYPE        QT_TRANSLATE_NOOP( "QXml", "error occurred while parsing document type definition" )
#define XMLERR_LETTEREXPECTED             QT_TRANSLATE_NOOP( "QXml", "letter is expected" )
#define XMLERR_ERRORPARSINGCOMMENT        QT_TRANSLATE_NOOP( "QXml", "error occurred while parsing comment" )
#define XMLERR_ERRORPARSINGREFERENCE      QT_TRANSLATE_NOOP( "QXml", "error occurred while parsing reference" )
#define XMLERR_INTERNALGENERALENTITYINDTD QT_TRANSLATE_NOOP( "QXml", "internal general entity reference not allowed in DTD" )
#define XMLERR_EXTERNALGENERALENTITYINAV  QT_TRANSLATE_NOOP( "QXml", "external parsed general entity reference not allowed in attribute value" )
#define XMLERR_EXTERNALGENERALENTITYINDTD QT_TRANSLATE_NOOP( "QXml", "external parsed general entity reference not allowed in DTD" )
#define XMLERR_UNPARSEDENTITYREFERENCE    QT_TRANSLATE_NOOP( "QXml", "unparsed entity reference in wrong context" )
#define XMLERR_RECURSIVEENTITIES          QT_TRANSLATE_NOOP( "QXml", "recursive entities" )
#define XMLERR_ERRORINTEXTDECL            QT_TRANSLATE_NOOP( "QXml", "error in the text declaration of an external entity" )

class QString;

// This is the handler class that knows how to generate the AIS structures from the parse process
// This class contains an instance of the QXmlSimpleParser. This class inherits the QXmlDefaultHandler
// inteface so it can redirect the SAX events to the AIS Lambda instance that owns the AXml object instance.
class AXml : public QXmlDefaultHandler
{
public:
	AXml(LpXCONTEXT aCP, LpTHREAD aTP, TVAL aXmlLambda);

	// AIS specific member functions
	bool setHandlerLambda(TVAL aHandlerLambda);
	bool parseFile(const QString& aFilename);
	bool parseBuffer(const char* aBuffer);
	void setFeature (const QString & name, bool value );
	void clearFeatures();

	// The remaining member functions are those defined in QXmlDefaultHander
	// These functions are called when the the XML source is parsed. Note that
	// the implementation of these functions includes a call to the similarily
	// named functions in the lisp Lambda instance that owns this AXml instance.

	// QXmlContentHandler Implementation
    void setDocumentLocator( QXmlLocator* locator );
    bool startDocument();
    bool endDocument();
    bool startPrefixMapping( const QString& prefix, const QString& uri );
    bool endPrefixMapping( const QString& prefix );
    bool startElement( const QString& namespaceURI, const QString& localName, const QString& qName, const QXmlAttributes& atts );
    bool endElement( const QString& namespaceURI, const QString& localName, const QString& qName );
    bool characters( const QString& ch );
    bool ignorableWhitespace( const QString& ch );
    bool processingInstruction( const QString& target, const QString& data );
    bool skippedEntity( const QString& name );

	// QXmlErrorHander Implementation
    bool warning( const QXmlParseException& exception );
    bool error( const QXmlParseException& exception );
    bool fatalError( const QXmlParseException& exception );

	// QXmlDTDHandler Implementation
    bool notationDecl( const QString& name, const QString& publicId, const QString& systemId );
    bool unparsedEntityDecl( const QString& name, const QString& publicId, const QString& systemId, const QString& notationName );

	// QXmlEntityResolver Implementation
    bool resolveEntity( const QString& publicId, const QString& systemId, QXmlInputSource*& ret );

	// QXmlLexicalHandler Implementation
    bool startDTD( const QString& name, const QString& publicId, const QString& systemId );
    bool endDTD();
    bool startEntity( const QString& name );
    bool endEntity( const QString& name );
    bool startCDATA();
    bool endCDATA();
    bool comment( const QString& ch );
	
	// QXmlDeclHandler
    bool attributeDecl( const QString& eName, const QString& aName, const QString& type, const QString& valueDefault, const QString& value );
    bool internalEntityDecl( const QString& name, const QString& value );
    bool externalEntityDecl( const QString& name, const QString& publicId, const QString& systemId );

    QString errorString();
	

private:
// Note that gCP and gTP don't follow our mValue naming convention
// for member variables. This is necessary becuase they participate
// in the macros defined in fsmartbase.h.
LpXCONTEXT			gCP;			// AIS context pointer
LpTHREAD			gTP;			// AIS context thread pointer

TVAL				mXmlLambda;
TVAL				mHandlerLambda;
TVAL				mStartDocument;
TVAL				mEndDocument;
TVAL				mStartPrefixMapping;
TVAL				mEndPrefixMapping;
TVAL				mStartElement;
TVAL				mEndElement;
TVAL				mCharacters;
TVAL				mIgnorableWhitespace;
TVAL				mProcessingInstruction;
TVAL				mSkippedEntity;
TVAL				mWarning;
TVAL				mError;
TVAL				mFatalError;
TVAL				mNotationDecl;
TVAL				mUnparsedEntity;
TVAL				mResolveEntity;
TVAL				mStartDTD;
TVAL				mEndDTD;
TVAL				mStartEntity;
TVAL				mEndEntity;
TVAL				mStartCDATA;
TVAL				mEndCDATA;
TVAL				mComment;
TVAL				mAttributeDecl;
TVAL				mInternalEntityDecl;
TVAL				mExternalEntityDecl;

// TVAL references that are duplicated in the AXml Lambda - these
// references must be duplilcated in the AXml Lambda so that the
// values referenced do not get garbage collected (or moved)!
TVAL				mCmdNew;
TVAL				mSymVector;
TVAL				mCmdMakeStructure;
TVAL				mSymColumnNumber;
TVAL				mSymLineNumber;
TVAL				mSymPublicID;
TVAL				mSymSystemID;
TVAL				mSymMessage;

QString mIndent;
FeatureMap			mFeatures;	// Value based map of SAX2 features

};            

#endif 
