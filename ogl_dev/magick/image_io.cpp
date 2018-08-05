
// File image_io.cpp

#include "image_io.h"

//  std::string has a member c_str() which returns a C-style array.
//  To "convert" a C array char* to a C++ std::string
//  simply use the latter's constructor or assignment operator.

void readFile (Magick::Image& anImage, char* fileName)
  {
  std::string cppString (fileName);

  anImage.read (cppString);
  }

//  -----------------------------------------------------------------

void writeFile (Magick::Image& anImage, char* fileName)
  {
  std::string cppString (fileName);

  anImage.write (cppString);
}

//  -----------------------------------------------------------------

void writeBlob (Magick::Image& anImage, Magick::Blob* theBlob,
		char* fileName)
{
  std::string cppString (fileName);
  anImage.write (theBlob, cppString);
 }
