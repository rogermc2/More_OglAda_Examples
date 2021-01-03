
with Ada.Containers.Vectors;

with GL.Types; use GL.Types;

package Prop_Renderer.Render is

    package Indicies_Package is new Ada.Containers.Vectors (Positive, Positive);
    type Indicies_List is new Indicies_Package.Vector with null Record;

    Basic_Render_List             : Indicies_List;
    Skinned_Render_List           : Indicies_List;
    Jav_Stand_Render_List         : Indicies_List;
    Portal_Render_List            : Indicies_List;
    Treasure_Render_List          : Indicies_List;

    procedure Render_Basic (Prop_Dyn_Light_Pos_Wor, Prop_Dyn_Light_Diff,
                            Prop_Dyn_Light_Spec : Singles.Vector3;
                            Prop_Dyn_Light_Range : Single := 1.0;
                            Prop_Dyn_Light_Dirty : Boolean := True);
    procedure Render_Javelin_Standard;
    procedure Render_Portal;
    procedure Render_Skinned (Prop_Dyn_Light_Pos_Wor, Prop_Dyn_Light_Diff,
                              Prop_Dyn_Light_Spec : Singles.Vector3;
                              Prop_Dyn_Light_Range : Single := 1.0;
                              Prop_Dyn_Light_Dirty : Boolean := True);
    procedure Render_Treasure;

end Prop_Renderer.Render;
