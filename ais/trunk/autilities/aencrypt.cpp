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

/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/autilities/aencrypt.cpp
															Encryption

Encryption algorithms for keeping selected sequences confidential.

CHANGE HISTORY
Version	Date		Who		Change
1.0113	11/7/2006	tlw		Omit unused constructor, destructor.
1.0067	 7/31/2005	tlw		Add embedded documentation
1.0057	 3/18/2005	tlw		Update documentation
												--------------- ---------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include	<time.h>				// time
#include	<QtCore/QObject>
#include	<QtCore/QStringList>
#include	<QtCore/QUuid>

#include	"aglobals.h"			// gAis
#include	"alogmgr.h"				// ALogMgr

//	-------------------------------------------------- DEFINED CONSTANTS ------------------------------------------------------
// Base-32 character maps - must be a collating sequence.
static char  cAlpha[] = "0123456789BCDFGHJKLMNPQRSTUVWXYZ";
static int cInvAlpha[] = {0,1,2,3,4,5,6,7,8,9,32,32,32,32,32,32,32,32,10,11,12,32,13,14,15,32,16,17,18,19,20,32,21,22,23,24,25,26,27,28,29,30,31};
// Number of significant bits in binary value.
static int cBits[] = {0,1,2,2,3,3,3,3,4,4,5,5,5,5,5,5,5,5,4,4,4,5,4,4,4,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5};

//	-------------------------------------------------- LOCAL FUNCTIONS --------------------------------------------------------

//	--------------------------------------------------- CLASS METHODS ---------------------------------------------------------
/*!
base32ToBinary - Convert string from base32 chars to word array
Args:
\param irValue Sequence of chars. Limited to chars in cAlpha array.
\param orResult An array of unsigned words holding binary value of sequence.
\sa	shiftLeft5
\par Notes:
 -# See decryptNum for an example.
 -#	First char is represents the 5 most-significant bits in result.
 -#	Size of word array must be large enough to hold all the converted bits.
 -# Numbers are stored in the word array little-endian (LSW is in lowest memory location)
 */
AWordArray& AEncrypt::base32ToBinary(const QString& irValue, AWordArray& orResult)
{
	// Strip off leading 0s from input
	long aLgth = irValue.length();	// Number of chars to convert
	long aMsc = 0;						// Index to next most-sig. input char
	while (irValue[(int)aMsc] == '0' && aMsc < aLgth)
		++aMsc;
	char aC = irValue[(int)aMsc].toAscii();	// Next most-sig. input char
	long aZero = 5 - cBits[aC - '0'];			// Number of leading zero bits

	// Set sizes and indexes into word array
	uint aSize = ((aLgth - aMsc) * 5 + 31 - aZero) / 32;
	orResult.fill(0, aSize);
	long  aMsw = 0;					// Index to next (most sig.) word being filled

	// Fill word array from 5 most significant bits to 5 least significant bits
	while (aMsc < aLgth)
	{	AWord a5;					// Next 5 bits
		// Convert the next base-32 char to binary
		aC = irValue[(int)aMsc++].toAscii();
		if (aC < '0' || aC > 'Z' || (a5 = cInvAlpha[aC - '0']) >= 32)
		{	qDebug("AEncrypt::base32ToBinary, Invalid value in input:%s", irValue.toAscii().data());
			break;
		}
		// Shift word array left 5 bits and fold in next 5 bits.
		shiftLeft5(orResult, &aMsw);
		orResult[0] |= a5;
	}
	return orResult;
}

/*	---------------------------------------------------------------------------------------------------------------------------
binaryToBase32 - Convert string from base32 chars to word array
Args:
	irValue		An array of unsigned 32-bit words holding binary value of sequence.
	orResult	Sequence of chars. Limited to chars in cAlpha array (above).
Returns:
	orResult	Reference to returned string.
Notes:
 1.	Five least-significant bits of number end up at the beginning of string.
 2. Numbers are stored in the word array little-endian (LSB is in lowest memory location)
	------------------------------------------------------------------------------------------------------------------------ */
QString& AEncrypt::binaryToBase32(const AWordArray& irValue, QString& orResult)
{
	// Compute number of chars in result
	uint aSize = irValue.size();
	long aMsw = aSize - 1;				// Index to next most-sig. input word
	while (irValue[aMsw] == 0 && aMsw > 0)
		--aMsw;
	AWord aWd = irValue[aMsw];			// Most-sig. non-zero input word
	long aZero = 0;						// Number of leading zero bits
	while ((aWd & 0x80000000) == 0 && aZero < 32)
		++aZero, aWd <<= 1;
	uint aLgth = (aSize * 32 + 4 - aZero) / 5;	// Num of chars in result

	// Convert next least sig 5 bits of array into a base-32 char
	AWordArray aValue = irValue;
	for (long i = aLgth - 1; i >= 0 ; --i)
	{	orResult[(int)i] = cAlpha[aValue[0] & 0x1f];
		shiftRight5(aValue, &aMsw);
	}
	return orResult;
}

/*	---------------------------------------------------------------------------------------------------------------------------
decryptNum - Convert encrypted string to word array
Args:
	irValue		Encrypted string of base-32 characters (MSB first).
	irKey		N-word key used to scramble value
	orResult	N-word array with converted values
Returns:
	orResult	Reference to decrypted word array.
Notes:
 1. Numbers are stored in the word array little-endian (LSB is in lowest memory location)
 2. The size of the Result array must be big enough to hold the converted bits from irValue.
 3. The size of the resulting word array must be same size as Key array
	------------------------------------------------------------------------------------------------------------------------ */
AWordArray& AEncrypt::decryptNum(const QString& irValue, const AWordArray& irKey, AWordArray& orRslt)
{
	// Convert input from base32 to binary and then decrypt
	base32ToBinary(irValue, orRslt);
	long aSz = orRslt.size();
	long aKeySz = irKey.size();
	if (aSz > aKeySz)
	{	qDebug("AEncrypt::decryptNum, Warning aSz:%ld > aKeySz:%ld", aSz, aKeySz);
		aSz = aKeySz;
	}
	for (long i = 0; i < aSz; ++i)
		orRslt[i] ^= irKey.at(i);
	return orRslt;
}

/*	---------------------------------------------------------------------------------------------------------------------------
encryptNum - Convert encrypted string to word array
Args:
	irValue		N-word array holding number to be encrypted
	irKey		N-word key used to scramble value
	orResult	Encrypted string of base-32 characters (MSB first).
Returns:
	orResult	Reference to encrypted string.
Notes:
 1. Numbers are stored in the word array little-endian (LSB is in lowest memory location)
 2. The size of the resulting word array must be same size as Key array
	------------------------------------------------------------------------------------------------------------------------ */
QString& AEncrypt::encryptNum(const AWordArray& irValue, const AWordArray& irKey, QString& orResult)
{
	long aSz = irValue.size();
	long aKeySz = irKey.size();
	if (aSz > aKeySz)
	{	qDebug("AEncrypt::encryptNum, Warning aSz:%ld > aKeySz:%ld", aSz, aKeySz);
		aSz = aKeySz;
	}
	// Encrypt irValue using irKey and then convert to base32
	// This encryption will be made more elaborate after other components are tested.
	AWordArray aValue(aSz);
	for (long i = 0; i < aSz; ++ i)
		aValue[i] = irValue[i] ^ irKey[i];

	binaryToBase32(aValue, orResult);
	return orResult;
}

/*	---------------------------------------------------------------------------------------------------------------------------
newKey - Generate a quasi-random key
Args:
	irLgth		Number of words in key.
	orKey		Generated N-word key
Returns:
	orResult	Reference to generated Key.

Notes:
 1. A Guid is pretty hard to reproduce (but not impossible).
 2. AWord is quint32 (same as unsigned int)
 3.  Guid structure:
	uint    data1;
    ushort  data2;
    ushort  data3;
    uchar   data4[8];
	------------------------------------------------------------------------------------------------------------------------ */
AWordArray& AEncrypt::newKey(int iLgth, AWordArray& orKey)
{
	AWord aGuid[4];
	if (orKey.size() < iLgth) orKey.resize(iLgth);

	for (int i = 0; i < iLgth; ++i)
	{	if (i % 4 == 0)
		{	QUuid aUuid(QUuid::createUuid());
			aGuid[3] = aUuid.data1;
			aGuid[2] = aUuid.data3 << 16 | aUuid.data3;
			aGuid[1] = aUuid.data4[0] << 24 | aUuid.data4[1] << 16 | aUuid.data4[2] << 8 | aUuid.data4[3];
			aGuid[0] = aUuid.data4[4] << 24 | aUuid.data4[5] << 16 | aUuid.data4[6] << 8 | aUuid.data4[7];
		}
		orKey[i] = aGuid[i];
	}
	return orKey;
}

/*	---------------------------------------------------------------------------------------------------------------------------
packCookie - Convert the integers in a cookie structure into an encrypted string.
Args:
	irCookie	Structure holding integers
	irKey		Key used to scramble the integers
	orCookie	Encrypted string
Returns:
	orCookie	Reference to encrypted string.
Notes:
 1. Just for use by AHttpSvr.
	------------------------------------------------------------------------------------------------------------------------ */
QString& AEncrypt::packCookie(const ACookieSt& irCookie, const AWordArray& irKey, QString& orCookie)
{
	// Transfer contents into a word array and encrypt.
	AWordArray aNum(4);
	aNum[0] = irCookie.mClientIp;
	aNum[1] = irCookie.mReserved;
	aNum[2] = irCookie.mCookieId;
	aNum[3] = irCookie.mCreateTime;

	encryptNum(aNum, irKey, orCookie);
	return orCookie;
}

/*	---------------------------------------------------------------------------------------------------------------------------
shiftLeft5 - Shift a large number stored in an N-word array left 5 bits.
Args:
	iorValue	Reference to array to be shifted
	iopMsw		-> index into MSW of the array
Returns:
	nothing
Notes:
 1. *iopMsw is set to the index of the most-significant non-zero word of array.
 2.	If upper 5 bits of word at *ipMsw are non-zero, result may be shifted into ipMsw+1
 3. So, it is advisable to check to make sure that upper 5 bits of iorValue are zero to avoid
	a word overflow (especially if ipMsw points to last word in the array!).
 4.	Lowest 5 bits of iopBeg are set to 0.
	------------------------------------------------------------------------------------------------------------------------ */
void AEncrypt::shiftLeft5(AWordArray& iorValue, long* iopMsw)
{
	// Skip leading empty words.
	long aMsw = *iopMsw;
	if ((iorValue[aMsw] & 0xf8000000) != 0)
		*iopMsw = ++aMsw;

	// Shift word array left 5 bits
	for (long i = aMsw; i >= 0; --i)
	{	AWord aWord = iorValue[i];
		// Shove upper 5 bits into 5 lsbs of next most-sig word
		if (i != aMsw)
			iorValue[i+1] |= (aWord & 0xf8000000) >> 27;
		// Shift next less sig word left
		iorValue[i] <<= 5;
	}                      
}

/*	---------------------------------------------------------------------------------------------------------------------------
shiftRight5 - Shift a large number stored in an N-word array right 5 bits.
Args:
	iorValue	Reference to array to be shifted
	iopMsw		-> index into MSW of the array
Returns:
	orCookie	Reference to encrypted string.
Notes:
 1. *iopMsw is set to the index of the most-significant non-zero word of array.
 2.	Least signficant 5 bits of *iopBeg are discarded.
 3. Upper 5 bits of *ipEnd are set to 0.
	------------------------------------------------------------------------------------------------------------------------ */
void AEncrypt::shiftRight5(AWordArray& iorValue, long* iopMsw)
{
	long aMsw = *iopMsw;
	iorValue[0] >>= 5;
	for (long i = 0; i < aMsw;)
	{	AWord aWd = iorValue[i + 1];
		// Shove 5 lsbs from next most sig. word into 5 msbs of this word
		iorValue[i++] |= (aWd & 0x1f) << 27;
		iorValue[i] >>= 5;
	}
	// No use shifting zeros
	if (iorValue[aMsw] == 0 && aMsw > 0)
		--*iopMsw;
}

/*	---------------------------------------------------------------------------------------------------------------------------
unpackCookie - Decrypt a string of base-32 chars to recover the contents of a cookie structure.
Args:
	irXCookie	Encrypted string.
	irKey		Key used to unscramble the integers
	orCookie	Receives converted values.
Returns:
	orCookie	Reference to cookie structure
Notes:
 1. Just for use by AHttpSvr.
	------------------------------------------------------------------------------------------------------------------------ */
ACookieSt& AEncrypt::unpackCookie(const QString& irCookie, const AWordArray& irKey, ACookieSt& orCookie)
{
	AWordArray aNum(4);
	decryptNum(irCookie, irKey, aNum);
	
	// Unpack the result.
	orCookie.mClientIp = aNum[0];
	orCookie.mReserved = aNum[1];
	orCookie.mCookieId = aNum[2];
	orCookie.mCreateTime = aNum[3];
	return orCookie;
}
// end
