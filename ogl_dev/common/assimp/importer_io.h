
// File importer.h

#ifndef IMPORTER_IO_H
#define IMPORTER_IO_H

#include <scene.h>

extern "C"
{
const aiScene* Read_File(const char* fileName, unsigned int Flags);
}

#endif /* IMPORTER_IO_H */
