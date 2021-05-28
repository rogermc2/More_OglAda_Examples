
with GL.Types;

with Input_Callback;

package Text_Manager is

   procedure Draw_Text (Window  : in out Input_Callback.Callback_Window;
                        Text : String; X, Y, R, G, B : GL.Types.Single);
   procedure Initialize  (Window  : in out Input_Callback.Callback_Window);

end Text_Manager;
