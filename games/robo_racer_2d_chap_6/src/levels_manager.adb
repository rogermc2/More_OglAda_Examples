
package body Levels_Manager is

    Pickups_Threshold       : constant Integer := 5;
    Pickup_Spawn_Adjustment : constant Float := 0.25;
    Enemies_Hit             : Integer := 0;
    Pickups_Received        : Integer := 0;
    Level_Max_Time          : Float := 30.0;

    procedure Next_Level (Game_State : in out Game_Status;
                          Level_Timer, Pickup_Spawn_Threshold : in out Float) is
    begin
        if Pickups_Received < Pickups_Threshold then
            Game_State := Game_Over;
        else
            Pickup_Spawn_Threshold := Pickup_Spawn_Threshold +
              Pickup_Spawn_Adjustment;
            Level_Timer := 0.0;
            Game_State := Game_Next_Level;
        end if;
    end Next_Level;

end Levels_Manager;
