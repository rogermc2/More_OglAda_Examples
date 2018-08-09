
// File image_io.cpp

#include "image_io.h"

static Magick::Blob theBlob;
static Magick::Image theImage;

bool loadBlob (char* fileName, char* magickType)
{
  std::string cppString (magickType);
  try
  {
    theImage.read(fileName);
    theImage.write(&theBlob, cppString);
    return true;
  }
  catch (Magick::Error& Error)
  {
    printf ("Error loading texture %s: %s/n", fileName, Error.what());
    return false;
  }
}

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

//void writeBlob (Magick::Image& anImage, char* Blob_Data,
//		char* magickType)

/* void writeBlob (char* Blob_Data, char* magickType, size_t dataLength) */
/* { */
/*   std::string cppString (magickType); */
/*   theBlob.update (Blob_Data, dataLength); */
/*   theImage.write (theBlob, cppString); */
/* } */
