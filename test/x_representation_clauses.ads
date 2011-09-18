with System;
package X_Representation_Clauses is
   I : Integer;

   V1 : Integer;                      -- 'Address, global 'Address, rng_over, overlay
   for V1'Address use I'Address;

   package Inner is
      V2  : Integer;                  -- 'Address, global 'Address, rng_over, overlay
      for V2'Address use I'Address;
   end Inner;

end X_Representation_Clauses;
