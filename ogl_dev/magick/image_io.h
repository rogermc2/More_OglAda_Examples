
// File image_io.h

#ifndef IMAGE_IO_H
#define IMAGE_IO_H

#include "Magick++.h"
#include "Image.h"

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

#endif /* IMAGE_IO_H */
