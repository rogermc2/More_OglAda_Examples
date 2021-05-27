
#include <cstdlib>
#include <string.h>
#include <iostream>
#include <errno.h>

using namespace std;

#include "fmod_common.h"
#include "fmod_errors.h"
#include "fmod.hpp"

int main(void)
{
  FMOD_RESULT result;
  FMOD::System *system = NULL;
  FMOD::Sound *sound = nullptr;
  FMOD::ChannelGroup *channelGroup = nullptr;
  FMOD::Channel *channel = nullptr;
  string In;

  // Create the main system object.
  result = FMOD::System_Create(&system);
  if (result != FMOD_OK)
    {
      cout << "FMOD error! " << result << FMOD_ErrorString(result) << endl << endl << endl;
      exit(-1);
    }

  // Initialize FMOD
  result = system->init(512, FMOD_INIT_NORMAL, 0);
  if (result != FMOD_OK)
    {
      cout << "FMOD init error! " << result << FMOD_ErrorString(result) << endl << endl;
      exit(-1);
    }

  // Create the sound.
  system->createSound("./src/oil.wav", FMOD_DEFAULT, nullptr, &sound);
  if (result != FMOD_OK)
    {
      cout << "FMOD createSound error! " << result << FMOD_ErrorString(result) << endl << endl;
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