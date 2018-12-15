
with Interfaces.C.Pointers;

with GL.Types; use GL.Types;

with Utilities;

package Teacup_Maths is

--     type Vector1 is array (Size range <>) of aliased Single;
--     pragma Convention (C, Vector1);
--
--     package Vector1_Pointers is new Interfaces.C.Pointers
--       (GL.Types.Int, GL.Types.Single, Utilities.Singles_Array, GL.Types.Single'Last);

    function Bernstein_Polynomial (I, N : Int; U : Single) return Single;
    function Binomial_Coefficient (I, N : Int) return Single;
    function Factorial (N : Int) return Int;

end Teacup_Maths;
