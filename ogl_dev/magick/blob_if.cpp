
// File blob_if.cpp

#include "blob_if.h"

Magick::Blob* newBlob(const void* data,const size_t length)
{
  return newBlob(data, length);
}

void deleteBlob (Magick::Blob& theBlob)
{
  theBlob.~Blob();
}

// Obtain pointer to data.
const void* getData(Magick::Blob& theBlob)
{
return theBlob.data();
}

size_t length(Magick::Blob& theBlob)
{
  return theBlob.length();
}
