
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GL.Objects.Textures;

package Maps_Manager is

    package Story_Lines_Package is new Ada.Containers.Vectors
      (Positive, Unbounded_String);
    type Story_Lines_List is new Story_Lines_Package.Vector with null record;

    type Map is private;

    procedure Load_Maps (Map_Path : String; theMap : out Map;
                        Tile_Diff_Tex, Tile_Spec_Tex, Ramp_Diff_Tex,
                        Ramp_Spec_Tex : in out GL.Objects.Textures.Texture);
    procedure Set_Title (aMap : in out Map; Title : Unbounded_String);
    procedure Set_Par_Time (aMap : in out Map; Time : Unbounded_String);
    procedure Set_Music_Track (aMap : in out Map; Track : Unbounded_String);
    procedure Set_Hammer_Music_Track (aMap : in out Map; Track : Unbounded_String);

private
    type Map is record
       Level_Title        : Unbounded_String := Null_Unbounded_String;
       Level_Par_Time     : Unbounded_String := Null_Unbounded_String;
       Story_Lines        : Story_Lines_List;
       Music_Track        : Unbounded_String := Null_Unbounded_String;
       Hammer_Music_Track : Unbounded_String := Null_Unbounded_String;
    end record;

end Maps_Manager;
