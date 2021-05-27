
#include <stdio.h>
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
      return -1;
    }

  // Initialize FMOD
  result = FMOD_System_Init(system, 512, FMOD_INIT_NORMAL, 0);
  if (result != FMOD_OK)
    {
      printf ("FMOD init error! %s\n\n", FMOD_ErrorString(result));
      return -1;
    }

  // Create the sound.
  result = FMOD_System_CreateSound(system, "./src/oil.wav", FMOD_DEFAULT,
				   NULL, &sound);
  if (result != FMOD_OK)
    {
      printf ("FMOD createSound error! %s\n\n", FMOD_ErrorString(result));
      return -1;
    }

  // Play the sound.
  result = FMOD_System_PlaySound (system, sound, NULL, 0, &channel);
  if (result != FMOD_OK)
    {
      printf ("FMOD playSound error! %s\n\n", FMOD_ErrorString(result));
      printf ("FMOD playSound fopen failed, error %s\n\n",
	      FMOD_ErrorString(result));
      return -1;
    }
  printf ("Press return to quit\n");
  In = getchar ();
  result = FMOD_System_Release (system);
  printf ("FMOD tutorial done\n\n");
  return 0;
}
