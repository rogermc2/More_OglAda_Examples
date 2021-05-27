
#include <stdio.h>
#include <stdlib.h>
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
  FMOD_OPENSTATE openstate;
  unsigned percentbuffered;
  FMOD_BOOL starving;
  FMOD_BOOL diskbusy;
  char In;

  // Create the main system object.
  result = FMOD_System_Create(&system);
  if (result != FMOD_OK)
    {
      printf ("FMOD error! %s\n\n", FMOD_ErrorString(result));
      exit (-1);
    }

  // Initialize FMOD
  result = FMOD_System_Init(system, 512, FMOD_INIT_NORMAL, 0);
  if (result != FMOD_OK)
    {
      printf ("FMOD init error! %s\n\n", FMOD_ErrorString(result));
      exit (-1);
    }

  // Create the sound.
  result = FMOD_System_CreateSound(system, "./src/oil.wav", FMOD_DEFAULT,
				   NULL, &sound);
  if (result != FMOD_OK)
    {
      printf ("FMOD createSound error! %s\n\n", FMOD_ErrorString(result));
      exit (-1);
    }
  result = FMOD_Sound_GetOpenState (sound, &openstate, &percentbuffered,
				    &starving, &diskbusy);
  printf ("FMOD OpenState: %u\n", openstate);
  printf ("FMOD percentbuffered: %u\n", percentbuffered);
  printf ("FMOD starving: %u\n", starving);
  printf ("FMOD diskbusy: %u\n\n", diskbusy);
  // Play the sound.
  result = FMOD_System_PlaySound (system, sound, NULL, 0, &channel);
  if (result != FMOD_OK)
    {
      printf ("FMOD playSound error! %s\n\n", FMOD_ErrorString(result));
      printf ("FMOD playSound fopen failed, error %s\n\n",
	      FMOD_ErrorString(result));
      exit (-1);
    }
  printf ("Press return to quit\n");
  In = getchar ();
  result = FMOD_System_Release (system);
  printf ("FMOD tutorial done\n\n");
  exit (0);
}
