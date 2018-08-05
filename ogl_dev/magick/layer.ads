
package Layer is

   type Dispose_Type is (Unrecognized_Dispose,
                         None_Dispose,
                         Background_Dispose,
                         Previous_Dispose);
   pragma Convention (C, Dispose_Type);

   function Undefined_Dispose return Dispose_Type renames Unrecognized_Dispose;

private

   for Dispose_Type use (Unrecognized_Dispose => 0,
                         None_Dispose         => 1,
                         Background_Dispose   => 2,
                         Previous_Dispose     => 3);


end Layer;
