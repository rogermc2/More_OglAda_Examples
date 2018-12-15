
with Interfaces.C.Pointers;

with GL.Types; use GL.Types;

with Utilities;

package Teacup_Maths is

    function Bernstein_Polynomial (I, N : Int; U : Single) return Single;
    function Binomial_Coefficient (I, N : Int) return Single;
    function Factorial (N : Int) return Int;

end Teacup_Maths;
