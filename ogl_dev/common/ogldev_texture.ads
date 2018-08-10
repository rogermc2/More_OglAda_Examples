
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Strings.Unbounded;

with System;

with Core_Image;

with GL.Types; use GL.Types;
with GL.Low_Level.Enums;
with GL.Objects.Textures;

with Magick_Blob;
with Magick_Image.API;
with Ogldev_Engine_Common;

Package Ogldev_Texture is

   type Ogl_Texture is record
      File_Name      : Ada.Strings.Unbounded.Unbounded_String :=
        Ada.Strings.Unbounded.To_Unbounded_String ("");
      Texture_Target : GL.Low_Level.Enums.Texture_Kind;
      Texture_Object : GL.Objects.Textures.Texture;
      --  DON't Change Image_Ref or Blob_Data
      Image          : Core_Image.Image;
      Blob_Data      : Magick_Blob.Blob_Data;  --  Blob_Package.List
   end record;

   package Mesh_Texture_Package is new
     Ada.Containers.Indefinite_Ordered_Maps (UInt, Ogl_Texture);
   subtype Mesh_Texture_Map is Mesh_Texture_Package.Map;
   Texture_Exception : Exception;

   procedure Bind (theTexture : Ogl_Texture;
                   Texture_Unit : Ogldev_Engine_Common.Texture_Unit_Index);
   procedure Init_Texture (theTexture : in out Ogl_Texture;
                           Target_Type : GL.Low_Level.Enums.Texture_Kind;
                           File_Name  :  String);
   procedure Load (theTexture : in out Ogl_Texture);

end Ogldev_Texture;
