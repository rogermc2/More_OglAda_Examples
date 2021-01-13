
with Ada.Containers.Vectors;

with GL.Types; use GL.Types;

with GL_Maths;

package Prop_Renderer.Render is

    Basic_Render_List             : GL_Maths.Indices_List;
    Skinned_Render_List           : GL_Maths.Indices_List;
    Jav_Stand_Render_List         : GL_Maths.Indices_List;
    Portal_Render_List            : GL_Maths.Indices_List;
    Treasure_Render_List          : GL_Maths.Indices_List;

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
