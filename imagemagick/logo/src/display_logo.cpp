#include <Magick++.h>
#include <iostream>
#include <string>

using namespace std;
using namespace Magick;

int main(int argc, char **argv)
{
  string        Logo_Dir = "../images/";
  Magick::Image logo_image;

  Magick::InitializeMagick(*argv);
  // Construct the image object. Separating image construction from the
  // the read operation ensures that a failure to read the image file
  // doesn't render the image object useless.
  try
   {
    logo_image.read(Logo_Dir + "logo.jpg");
    // Crop the image to specified size (width, height, xOffset, yOffset)
    logo_image.crop(Magick::Geometry(100,100, 100, 100));
    logo_image.write("logo.png");
   }
  catch( Magick::Exception &error_)
   {
    cout << "Caught exception: " << error_.what() << endl;
    return 1;
   }
  return 0;
}
