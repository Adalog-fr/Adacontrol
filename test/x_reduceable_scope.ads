with Ada.Text_IO;
with Ada.Numerics;
with Ada.Exceptions;
use Ada.Numerics;                 -- Movable to body
use Ada.Exceptions;               -- Not used
use type Ada.Text_IO.Count;       -- Movable to Needs_Body
package X_Reduceable_Scope is

   procedure Needs_Body;
end X_Reduceable_Scope;
