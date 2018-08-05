
package Ogldev_Engine_Common is

type Texture_Unit_Index is (Colour_Texture_Unit_Index,
                            Shadow_Texture_Unit_Index,
                            Normal_Texture_Unit_Index,
                            Random_Texture_Unit_Index,
                            Displacement_Texture_Unit_Index,
                            Motion_Texture_Unit_Index,
                            Cascade_Shadow_Texture_Unit1_Index,
                            Cascade_Shadow_Texture_Unit2_Index);
private
   for Texture_Unit_Index use (Colour_Texture_Unit_Index          => 0,
                               Shadow_Texture_Unit_Index          => 1,
                               Normal_Texture_Unit_Index          => 2,
                               Random_Texture_Unit_Index          => 3,
                               Displacement_Texture_Unit_Index    => 4,
                               Motion_Texture_Unit_Index          => 5,
                               Cascade_Shadow_Texture_Unit1_Index => 6,
                               Cascade_Shadow_Texture_Unit2_Index => 7);

   function Cascade_Shadow_Texture_Unit0_Index return Texture_Unit_Index
     renames Shadow_Texture_Unit_Index;

end Ogldev_Engine_Common;
