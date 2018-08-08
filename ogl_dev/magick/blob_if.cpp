
// File blob_if.cpp

#include "blob_if.h"

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
