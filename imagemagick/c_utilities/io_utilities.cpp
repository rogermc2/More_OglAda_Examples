#include <iostream>
#include <sys/stat.h>
#include <string>

using namespace std;

int dirExists(const char *path)
{
  struct stat info;
  bool        found = false;

  if(stat( path, &info ) == 0 && info.st_mode & S_IFDIR)
  {
    cout << "Directory " << path << " found." << endl;
    found= true;
  }
  return found;
}

bool fileExists (const std::string& name)
  {
  struct stat buffer;
  return (stat (name.c_str(), &buffer) == 0);
  }

