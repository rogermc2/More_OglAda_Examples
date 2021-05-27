
#include <string.h>
#include <errno.h>

#include "fmod_common.h"
#include "fmod_errors.h"
#include "fmod.h"

int main(void)
{
  FMOD_RESULT result;
  FMOD_SYSTEM *system = NULL;
  FMOD_SOUND *sound = NULL;
  FMOD_CHANNELGROUP *channelGroup = NULL;
  FMOD_CHANNEL *channel = NULL;
  char In;

  // Create the main system object.
  result = FMOD_System_Create(&system);
  if (result != FMOD_OK)
    {
      printf ("FMOD error! %s\n\n", FMOD_ErrorString(result));
      exit(-1);
    }

  // Initialize FMOD
  result = init(system, 512, FMOD_INIT_NORMAL, 0);
  if (result != FMOD_OK)
    {
      printf ("FMOD init error! %s\n\n", FMOD_ErrorString(result));
      exit(-1);
    }

  // Create the sound.
  createSound(system, "../src/oil.wav", FMOD_DEFAULT, nullptr, &sound);
  if (result != FMOD_OK)
    {
      printf ("FMOD createSound error! %s\n\n", FMOD_ErrorString(result));;
      exit(-1);
    }

  // Play the sound.
  result = system->playSound(sound, nullptr, false, &channel);
  if (result != FMOD_OK)
    {
      cout << "FMOD playSound error! " << result << FMOD_ErrorString(result) << endl;
      cout << "FMOD playSound fopen failed, error " << strerror (errno) << endl;
      exit(-1);
    }
  cout << "Press return to quit" << endl;
  getline (cin, In);
  result = system->release ();
  cout << "FMOD tutorial done" << endl << endl;
  return 0;
}
