
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
bool loadBlob (char* fileName, char* magickType);
void readFile (Magick::Image& anImage, char* fileName);
void writeFile (Magick::Image& anImage, char* fileName);
//void writeBlob (Magick::Image& anImage, char* Blob_Data,
//		  char* magickType);
//void writeBlob (char* Blob_Data, char* magickType);
}

#endif /* IMAGE_IO_H */
