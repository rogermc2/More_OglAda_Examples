
// File blob_if.h

#ifndef BLOB_IF_H
#define BLOB_IF_H

#include "Magick++.h"
#include "Blob.h"

// using namespace Magick;

extern "C"
{
  Magick::Blob* newBlob(const void* data,const size_t length);
  void deleteBlob (Magick::Blob&);
  // Obtain pointer to data.
  const void* data(Magick::Blob&);
  size_t length(Magick::Blob&);
}
#endif /* BLOB_IF_H */
