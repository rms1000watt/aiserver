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

#ifndef AENCRYPT_H
#define AENCRYPT_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/include/aencrypt.h
															Encryption

Encryption algorithms for keeping selected sequences confidential.

CHANGE HISTORY
Version	Date		Who		Change
1.0113	11/7/2006	tlw		Omit unused constructor, destructor.
1.0067	 7/31/2005	tlw		Add embedded documentation
1.0057	 3/18/2005	tlw		Update documentation
												---------------------------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
//	------------------------------------------------------ IMPORTS ------------------------------------------------------------

//	------------------------------------------------------ TYPEDEFS -----------------------------------------------------------
typedef struct
{	AWord		mClientIp;			// IP address for the client (or its proxy)
	AWord		mReserved;			// Server Create Time for now
	AWord		mCookieId;			// Unique identifier for this connection
	AWord		mCreateTime;		// Seconds since 1/1/2000. Time cookie created
}
ACookieSt;
//	---------------------------------------------------- DEFINITIONS ----------------------------------------------------------

//	------------------------------------------------- CLASS DECLARATION -------------------------------------------------------
/*!
AEncrypt provides a facility to encrypt/decrypt N-word values, a Value, where a word is a 32-bit unsigned integer (N > 0).
encryptNum converts a Value into a string of alphanumeric characters. decryptNum reverses the process.
\par Base-32 characters.
The alphanum characters are referred to as Base-32 characters.  There are 32 Base-32 chars, each one representing a
specific 5-bit value.  The mapping of the characters to a 5-bit number is found in cAlpha in aencrypt.cpp. The first
character in the string represents the 5 most significant 5 bits of the N-word value, the second character the next
5 bits and so on.  That way, a display of the string shows the number in the conventional order.
\par Encryption.
Before the input word is converted into a base-32 string, it is encrypted using an N-word encryption key.  Decryption of
the resulting string reverses the process.  Note that is is very difficult to decrypt the encrypted string without the key.

Just one instance of this class should be defined in each application. The calling routine is responsible for supplying
encryptNum and decryptNum with an appropriate key.

\par Notes:
 -# encryptNum and decryptNum are the two main encryption/decryption routines that provide a fairly high level of
security.  It is important to make sure that the same key is used for decryption as was used for the encryption even if
the server is restarted after a crash.
 -# shiftLeft5 and shiftRight5 are low-level routines for shifting multi-word number arrays by 5 bits.
 -#	base32ToBinary and binaryToBase32 are helper routines that convert Base-32 strings to N-Word numbers and vice-versa.
 -# packCookie and unpackCookie are special-purpose 4-Word encryption routines used by our HTTP protocol to encrypt
cookies.
 -# newCookie is a utility that constructs a fairly random N-word key.
\par Example
The packCookie and unpackCookie routines provide examples of encrypting and decrypting a 4-word binary value.
*/
class AEncrypt
{
public:
	static AWordArray& base32ToBinary(const QString& irValue, AWordArray& orResult);
	static QString& binaryToBase32(const AWordArray& irValue, QString& orResult);
	static AWordArray& decryptNum(const QString& irValue, const AWordArray& irKey, AWordArray& orResult);
	static QString& encryptNum(const AWordArray& irValue, const AWordArray& irKey, QString& orResult);
	static AWordArray& newKey(int iLgth, AWordArray& orKey);
	static QString& packCookie(const ACookieSt& irCookie, const AWordArray& irKey, QString& orCookie);
	static void shiftLeft5(AWordArray& iorValue, long* iopMsw);
	static void shiftRight5(AWordArray& iorValue, long* iopMsw);
	static ACookieSt& unpackCookie(const QString& irXCookie, const AWordArray& irKey, ACookieSt& orCookie);
};

#endif // AENCRYPT_H
