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

/* aisdev/include/asortvector.h

									Sorted Vector Template

Sorted Vector provides a fairly efficient sorted list of ints, floats, or small structs.
Insert does a binary chop on the sorted list, so it can find the insertion point quickly O(ln(N)).
However, insert(or delete) opens (or closes) gaps in the array, which is slow O(N).
Resize requires a malloc and may require a memcopy of the entire array to a new location (slow).
Iterators are provided to facilitate looping thru a list.

A balanced binary tree should be implemented for holding very large arrays.  So far, ASortVector
is plenty good enough.  Qt does not provide an efficient sorted vector class.  QMemArray comes
the closest, but it does nothing to help maintain a sorted list.   Subclassing QMemArray
provides only a small amount of help and a fairly large overhead because, well, it is a kludge
and it suffers the exact same limitations as described below.  And, the documentation does not
tell you what you need to know.

DATA TYPES
Currently ASortVector can only hold integers, floats or small structures.  Someday, it
should be enhanced to hold pointers to aggregate types, such as arrays, structures, or
classes.

SIZE VS LENGTH
The size of the array is the total number of elements currently allocated.  Length is the
total number of elements in-use.  Length must be less than, or equal to, size.

POINTERS
An Iterator is provided, but AIterator is not any different from a pointer to an element in
the array.  Begin() just returns the beginning address of the array and end() returns the
address one past the last element of the array.  End() is called on every iteration, so it
is more expensive than a precalculated pointer.

RESIZE
When expanding the size of an array, resize calls malloc to find another hunk of memory large
enough to hold the new array. Since all pointers may be invalid, it is very important to never
resize in the middle of a loop that uses pointers into the array.  Because resize is very
expensive, resize infrequently and leave plenty of spare room for expansion.  Do not reduce
the size of an array if there is a good chance that it will require expansion later on.

You are responsible for increasing the size of the array prior to inserting any elements that
that would increase the length past the current size.  The class itself cannot just increase
the size in the middle of an insert operation because the resize might invalidate pointers that
are in-use.  Use resize before setting any pointers.  If you must resize inside a loop, be sure
to recompute all the pointers currently in use.

INSERT/DELETE
For all the reasons described above, insert and delete do not resize.  Insert will move all values
past the insertion point in the vector to the right.  Delete will move all values past the delete
point to the left.  If inserting or deleting while in a loop, be sure to adjust the loop variables
accordingly.

USAGE
	ASortVector<int> aVec(7);
	aVec.insert(5);
	aVec.insert(3);		// Insert in front
	aVec.remove(1);		// Remove trailing element
	aVec.insert(5);		// Insert at end
	aVec.insert(0);
	aVec.insert(11);
	aVec.insert(13);
	aVec.insert(7);
	aVec.insert(9);		// aVec = [0,3,5,7,9,11,13]

	ASortVector<int>::AIterator apIt;		// -> array
	int aPos, aVal;
	for (apIt = aVec.begin(); apIt != aVec.end(); ++apIt)
		aVal = *apIt;

	int *apVec, *apEnd = aVec.data() + aVec.length();
	for (aPos = 0, apVec = aVec.data(); apVec  < apEnd; ++apVec, ++aPos)
	{	aVal = *apVec;
		// Delete at current position. Compensate by moving cursors back.
		if (aPos == 1 && aVec.remove(apVec))
			--apVec, --apEnd;

		// aVal goes in front of current position, so bump pointers up.
		if (aPos == 2 && aVec.insert(3))
			++apVec, ++apEnd;
	}
	aPos = aVec.bsearch(aVal);		// aVal is 13, aPos is 6

// ASortVector can also hold small structures if a compare function is provided.
typedef struct { int mA; int mB;} AInts;
static int comp(const void* ipS1, const void* ipS2)
{
	AInts *apV1 = (AInts*)ipS1, *apV2 = (AInts*)ipS2;
	return (apV1->mA < apV2->mA) ? -1 : (apV1->mA > apV2->mA) ? 1 : 0;
}

	AInts aE1 = {1, 2}, aE2 = {0, 4};
	ASortVector<AInts> aVec(2, comp);
	aVec.insert(aE1);
	aVec.insert(aE2);	// Insert in front
	aVec.remove(0);
	aVec.insert(aE2);

	AInts *apVec, *apEnd = aVec.data() + aVec.length();
	int aVal;
	for (apVec = aVec.data(); apVec  < apEnd; ++apVec)
		aVal = apVec->mA;

	int aPos = aVec.bsearch(aE2);		// aE2 is the first element.
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	----------------------------------------- IMPORTS ---------------------------------------------
#ifndef ASORTVECTOR_H
#define ASORTVECTOR_H


//	----------------------------------------- TYPEDEFS --------------------------------------------

//	------------------------------------------ CLASSES --------------------------------------------
template<class T>
class ASortVector
{
public:
	typedef T* AIterator;
	typedef const T* AConstIterator;
	// Default compare if no compare function provided.  Use just for comparing integers or floats.
	static int compare(const void* ipV1, const void* ipV2);

	ASortVector() : cpData(NULL), cLength(0), cSize(0) {}
	ASortVector(int iSize, int (*ipCmp)(const void*, const void*) = NULL) : cpData(NULL), cLength(0), cSize(0)
					{ resize(iSize); cpCompare = (ipCmp == NULL) ? compare : ipCmp;}
	~ASortVector()	{}
	int  bsearch(const T& irPattern);
	T*   data()		const	{ return cpData;}
	bool insert(const T& irVal);
	bool isEmpty()	const	{ return cLength == 0;}
	bool isNull()	const	{ return cpData == NULL;}
	int	 length()	const	{ return cLength;}
	bool remove(int aPos);
	bool remove(T* ipVal);
	bool resize(uint iSize);
	uint size()		const	{ return cSize;}
	bool truncate(int iLen)	{ bool aOk = false; if (cLength > iLen){ cLength = iLen; aOk = true;} return aOk;}

	// Iterators:
	AIterator begin() { return cpData;}
	AIterator end() { return cpData + cLength;}		// -> just past last element in-use
	AConstIterator begin() const { return cpData; }
	AConstIterator end() const { return cpData() + cLength;}

private:
	static int	csWidth;		// Size of one element [bytes] (not currently used).

private:
	int		(*cpCompare)(const void*, const void*);
	T*		cpData;
	int		cLength;
	uint	cSize;
};

//	------------------------------------- STATIC METHODS ------------------------------------------
template<class T>
int ASortVector<T>::compare(const void* ipV1, const void* ipV2)
{
	int aWidth = sizeof(T);
    return (ipV1 != NULL && ipV2 != NULL) ? memcmp(ipV1, ipV2, aWidth) : -1;
}

//	------------------------------------- CLASS METHODS -------------------------------------------
// Since the vector is in sorted order, bsearch provides a very fast search O(ln(N)).
template<class T>
inline int ASortVector<T>::bsearch(const T& irPattern)
{
   int aSizeof = sizeof(T), aPos;
   if (cLength <= 0)
	   aPos = -1;
	else
	{	T* apPos = (T*)::bsearch(&irPattern, cpData, cLength, aSizeof, (int (*)(const void*, const void*))cpCompare);
		aPos = (apPos == NULL) ? -1 : apPos - cpData;
	}
	return aPos;
}

// memmove is a fast assembly language program (part of the standard run-time library) that
// works just like memcpy except that it copies from the high end back towards the low end,
// if the destination lies inside the source area.  Well-suited to open up a hole.
// For now, insert does not allow duplicate elements.  This could be an option.
// Returns true iff element is not already in the vector and if enough room in array to
//	insert one more element.
template<class T>
inline bool ASortVector<T>::insert(const T& irVal)
{
	// For now just append. Later, we will maintain sorted order.
	bool aFits = false;
	if (cLength < cSize)
	{	// Find the insertion point (binary chop) to maintain sorted order.
		int aLo = 0, aHi = cLength, aMid, aCmp;
		if (aHi == 0)				// Just insert at front if vector is empty.
			aMid = 0, aFits = true;
		else
		{	// Chop range in half until Mid reduced to Lo
			while ((aMid = (aLo + aHi) / 2) > aLo)
			{	if ((aCmp = (*cpCompare)(&irVal, cpData + aMid)) == 0)
					break;				// Insert fails if irNew matches a result
				else if (aCmp < 0)		// Val < Vec[mid]
					aHi = aMid;
				else					// Val > Vec[mid]
					aLo = aMid;
			}
			// Make a final decision based upon the vector value at the insertion point
			if ((aCmp = (*cpCompare)(&irVal, cpData + aMid)) != 0)
			{	aFits = true;
				if (aCmp > 0)			// Val < Vec[mid]
					++aMid;
			}
		}
		if (aFits)
		{	// Open up a hole unless inserting on the end. Move elements at or past aMid right.
			T* apSrc = cpData + aMid;
			if (aMid < cLength)
			{	int aN = (cLength - aMid) * sizeof(T);
				memmove(apSrc + 1, apSrc, aN);
			}
			*apSrc = irVal, ++cLength;
		}
	}
	return aFits;
}

template<class T>
inline bool ASortVector<T>::remove(int aPos)
{
	return remove(cpData + aPos);
}

// remove - delete an element from the array. Close the gap.
// memcpy is a fast assembly language program (part of the standard run-time library) that
// copies from low end of apSrc towards the end.  Well-suited to close a gap.  Note: memcpy
// is fastest if the elements lie on word boundaries.  malloc guarantees that cpData starts
// on a word boundry.
template<class T>
inline bool ASortVector<T>::remove(T* ipVal)
{
	// Shift the remaining items over the top of this item.
	bool aInRange = false;
	T *apEnd = cpData + cLength - 1;
	if (ipVal >= cpData && ipVal <= apEnd)
	{	int aN = (apEnd - ipVal) * sizeof(T);
		--cLength, aInRange = true;
		if (aN > 0)
			memcpy(ipVal, ipVal + 1, aN);
	}
	return aInRange;
}

template<class T>
inline bool ASortVector<T>::resize(uint iSize)
{
	bool aAlloc = false;
	// Nothing to do
   if (iSize == cSize)
		aAlloc = true;
   // Delete the array
	else if (iSize <= 0)
	{	if (cpData != NULL)
			free((char*)cpData);
		cpData = NULL;
		cLength = cSize = 0;
		aAlloc = true;
	}
	// expand or contract existing space.
	else if (cpData != NULL)
	{	if ((cpData = (T*)realloc(cpData, iSize * sizeof(T))) != NULL)
			aAlloc = true, cSize = iSize;
	}
	// allocate new space
	else if ((cpData = (T*)malloc(iSize * sizeof(T))) != NULL)
		aAlloc = true, cSize = iSize;
	return aAlloc;
}

/* dead code
template<class T>
class ASortVector : public QMemArray<T>
{
public:
	typedef T* AIterator;
	typedef const T* AConstIterator;

	ASortVector() {}
	ASortVector(int iSize) : QMemArray<T>(iSize) {}
	ASortVector(const ASortVector<T>& irCopy) : QMemArray(irCopy) {}
	~ASortVector() {}
	ASortVector<T>& operator=(const ASortVector<T> & irRhs)
							{return (ASortVector<T>&)QMemArray::assign(irRhs);}
	T*   data()		const	{return QMemArray<T>::data();}
	uint nrefs()	const	{return QMemArray<T>::nrefs();}
	uint size()		const	{return QMemArray<T>::size();}
	uint count()	const	{return size();}
	bool isEmpty()	const	{return QMemArray<T>::size() == 0;}
	bool isNull()	const	{return QMemArray<T>::data() == NULL;}
	bool resize(uint iSize)	{return QMemArray<T>::resize(iSize);}
	bool truncate(uint iPos){return QMemArray<T>::resize(iPos);}
	bool fill(const T& irItem, int iSize = -1 )
							{ return QMemArray<T>::fill(irItem, iSize);}
	void detach()			{ASortVector::detach();}
	ASortVector<T> copy() const	{ ASortVector<T> aTmp; return aTmp.duplicate(*this);}
	ASortVector<T>& assign(const ASortVector<T>& irRhs)	{return (ASortVector<T>&)QMemArray<T>::assign(irRhs);}
	ASortVector<T>& assign(const T* ipRhs, uint iN)	{return (ASortVector<T>&)QMemArray<T>::assign(ipRhs, iN);}
	ASortVector<T>& duplicate(const ASortVector<T>& irRhs) {return (ASortVector<T>&)QMemArray<T>::duplicate(irRhs);}
	ASortVector<T>& duplicate(const T* ipRhs, uint iN){return (ASortVector<T>&)QMemArray<T>::duplicate(ipRhs,iN);}
	int  find(const T& irPattern, uint iN = 0) const {return QMemArray::find(irPattern, iN);}
	int  contains(const T& irPattern) const {return QMemArray<T>::contains(irPattern);}
	void sort() { QMemArray::sort();}
	int  bsearch(const T& irPattern) const { return QMemArray<T>::bsearch(irPattern);}

	T& operator[](int iPos) const {return QMemArray<T>::at(iPos);}
	T& at(uint i) const {return QMemArray<T>::at(iPos);}
	operator const T*() const {return data();}
	bool operator==(const ASortVector<T> &irRhs ) const {return  isEqual(irRhs);}
	bool operator!=( const ASortVector<T> &iRhs ) const {return !isEqual(irRhs);}
	AIterator begin() {return data();}
	AIterator end() { return data() + size(); }
	AConstIterator begin() const {return data(); }
	AConstIterator end() const {return data() + size();}
}; */


#endif // ASORTVECTOR_H
