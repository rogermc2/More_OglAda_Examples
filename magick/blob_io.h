
// File blob_io.h

#ifndef BLOB_IO_H
#define BLOB_IO_H

#include "Magick++.h"       /* Needed for proper termination */
#include "Magick++/Image.h"

// using namespace Magick;

//  std::string has a member c_str() which returns a C-style array.
//  To "convert" a C array char* to a C++ std::string
//  simply use the latter's constructor or assignment operator.

extern "C"
{
void readFile (Magick::Image& anImage, char* fileName);
void writeFile (Magick::Image& anImage, char* fileName);
void writeBlob (Magick::Image& anImage, Magick::Blob* theBlob,
                char* fileName);
}

#endif /* BLOB_IO_H */
