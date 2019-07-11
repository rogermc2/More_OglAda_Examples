
// File blob_io.cpp

#include "blob_io.h"

//  std::string has a member c_str() which returns a C-style array.
//  To "convert" a C array char* to a C++ std::string
//  simply use the latter's constructor or assignment operator.

void readFile (Magick::Image& anImageRef, char* fileName)
  {
  const std::string cppString (fileName);

  //  Read single image frame into current object (ImageRef *_imgRef;)
  anImageRef.read (cppString);
  }

//  -----------------------------------------------------------------

void writeFile (Magick::Image& anImage, char* fileName)
  {
  const std::string cppString (fileName);

  anImage.write (cppString);
}

//  -----------------------------------------------------------------

void writeBlob (Magick::Image& anImage, Magick::Blob* theBlob,
		char* fileName)
{
  const std::string cppString (fileName);
  anImage.write (theBlob, cppString);
 }
