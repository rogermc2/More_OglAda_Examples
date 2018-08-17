
// File importer_io.cpp

#include <string>
#include "importer_io.h"
#include "assimp/importer.hpp"

//  std::string has a member c_str() which returns a C-style array.
//  To "convert" a C array char* to a C++ std::string
//  use the latter's constructor or assignment operator.

//  Read_File returns a pointer to the imported data; NULL if the import failed.
static Assimp::Importer aiScene_Object;

const aiScene* aiReadFile(const char* fileName, unsigned int Flags)
  {
  const aiScene*   theScene_Ptr;
  std::string cppString (fileName);

  aiScene_Object.ReadFile(cppString, Flags);
  theScene_Ptr = aiScene_Object.GetScene();
  printf ("importer_IO.aiReadFile theScene_Ptr: %p\n", theScene_Ptr);
  return theScene_Ptr;
  }
