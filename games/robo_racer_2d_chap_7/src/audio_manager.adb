
with System;

with Ada.Text_IO; use Ada.Text_IO;

with Fmod_Common;
with Fmod;

package body Audio_Manager is

    --  -------------------------------------------------------------------------

    procedure Close is
        use Fmod_Common;
        F_Result : Fmod_Result;
    begin
        F_Result := Fmod.Close_System;
        if F_Result = Fmod_Ok then
            Put_Line ("Audio_Manager.Init audio system closed");
        else
            raise Audio_Exception with
              "Audio_Manager.Init audio system closure failed"
              & " with failure code " & Fmod_Result'Image (F_Result);
        end if;

    end Close;

    --  -------------------------------------------------------------------------

    procedure Init is
        use Fmod_Common;
        F_Result : Fmod_Result;
    begin
        F_Result := Fmod.Create_System;
        if F_Result = Fmod_Ok then
            Put_Line ("Audio_Manager.Init audio system created");
            F_Result := Fmod.Init_System (50, Fmod_Init_Normal, System.Null_Address);
            if F_Result = Fmod_Ok then
                Put_Line ("Audio_Manager.Init audio system initialized");
            else
                raise Audio_Exception with
                  "Audio_Manager.Init audio system initialization failed"
                  & " with failure code " & Fmod_Result'Image (F_Result);
            end if;
        else
            raise Audio_Exception with
              "Audio_Manager.Init audio system creation failed"
              & " with failure code " & Fmod_Result'Image (F_Result);
        end if;

    end Init;

    --  -------------------------------------------------------------------------

end Audio_Manager;
