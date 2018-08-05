
with Interfaces.C; use Interfaces.C;

with Magick_Type;

package Geometry is

   type Gravity_Type is (Undefined_Gravity,
                         NorthWest_Gravity,
                         North_Gravity,
                         NorthEast_Gravity,
                         West_Gravity,
                         Center_Gravity,
                         East_Gravity,
                         SouthWest_Gravity,
                         South_Gravity,
                         SouthEast_Gravity,
                         Static_Gravity);
   pragma Convention (C, Gravity_Type);

   type Affine_Matrix is record
     Sx   : double := 0.0;
     Rx   : double := 0.0;
     Ry   : double := 0.0;
     Sy   : double := 0.0;
     Tx   : double := 0.0;
     Ty   : double := 0.0;
   end record;
   pragma Convention (C_Pass_By_Copy, Affine_Matrix);

   type Geometry_Info is record
     Rho   : double := 0.0;
     Sigma : double := 0.0;
     Xi    : double := 0.0;
     Psi   : double := 0.0;
     Chi   : double := 0.0;
   end record;
   pragma Convention (C_Pass_By_Copy, Geometry_Info);

   type Point_Info is record
     X       : double := 0.0;
     Y       : double := 0.0;
   end record;
   pragma Convention (C_Pass_By_Copy, Point_Info);

   type Rectangle_Info is record
      Width  : size_t := 0;
      Height : size_t := 20;
     X       : Magick_Type.ssize_t := 0;
     Y       : Magick_Type.ssize_t := 0;
   end record;
   pragma Convention (C_Pass_By_Copy, Rectangle_Info);

   type Offset_Info is record
     X       : Magick_Type.ssize_t := 0;
     Y       : Magick_Type.ssize_t := 0;
   end record;
   pragma Convention (C_Pass_By_Copy, Offset_Info);

private
   for Gravity_Type use (Undefined_Gravity => 0,
                         NorthWest_Gravity => 1,
                         North_Gravity     => 2,
                         NorthEast_Gravity => 3,
                         West_Gravity      => 4,
                         Center_Gravity    => 5,
                         East_Gravity      => 6,
                         SouthWest_Gravity => 7,
                         South_Gravity     => 8,
                         SouthEast_Gravity => 9,
                         Static_Gravity    => 10);

   function Forget_Gravity return Gravity_Type renames Undefined_Gravity;

end Geometry;
