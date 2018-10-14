
// File image_io.cpp

#include "image_io.h"

static Magick::Blob theBlob;
static Magick::Image theImage;

// Obtain pointer to data.
const void* blobData()
{
return theBlob.data();
}

const void* imageData()
{
return theImage.image();
}

size_t blobLength()
{
  return theBlob.length();
}

bool loadBlob (char* fileName, char* magickType)
{
  std::string cppString (magickType);
  bool        result;
  try
  {
    printf ("image_io.loadBlob reading texture %s \n", fileName);
    theImage.read(fileName);
    printf ("image_io.loadBlob texture read %s \n", fileName);
    theImage.write(&theBlob, cppString);
    result = true;
  }
  catch (Magick::Error& Error)
  {
    printf ("\nColumns: %zd", theImage.columns());
    printf ("\nFileName: %s", theImage.fileName().c_str());
    printf ("\nFile size: %llu", theImage.fileSize());
    printf ("\nScene number: %zd", theImage.scene());
    if (theImage.isValid() == true)
      printf ("\nFormat: %s", theImage.format().c_str());
    else
      printf ("\nInvalid image");

    printf ("\nimage_io.loadBlob Error loading texture %s: ", fileName);
    printf ("\nError: %s", Error.what());
    result = false;
  }
  return result;
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
