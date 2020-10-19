----------------------------------------------------------------------
--  Framework.Ordering_Machine - Package specification              --
--                                                                  --
--  This software is (c) Alstom and Adalog 2004-2013.               --
--  The Ada Controller is  free software; you can  redistribute  it --
--  and/or modify it under  terms of the GNU General Public License --
--  as published by the Free Software Foundation; either version 2, --
--  or (at your option) any later version. This unit is distributed --
--  in the hope  that it will be useful,  but WITHOUT ANY WARRANTY; --
--  without even the implied warranty of MERCHANTABILITY or FITNESS --
--  FOR A  PARTICULAR PURPOSE.  See the GNU  General Public License --
--  for more details.   You should have received a  copy of the GNU --
--  General Public License distributed  with this program; see file --
--  COPYING.   If not, write  to the  Free Software  Foundation, 59 --
--  Temple Place - Suite 330, Boston, MA 02111-1307, USA.           --
--                                                                  --
--  As  a special  exception, if  other files  instantiate generics --
--  from the units  of this program, or if you  link this unit with --
--  other files  to produce  an executable, this  unit does  not by --
--  itself cause the resulting executable  to be covered by the GNU --
--  General  Public  License.   This  exception  does  not  however --
--  invalidate any  other reasons why the executable  file might be --
--  covered by the GNU Public License.                              --
----------------------------------------------------------------------

-- This is a state machine used to check that certain tokens are
-- given in an appropriate order.
--
-- The machine is initialized by giving successive states, each state
-- being a set of allowed tokens.
--
-- When a token is checked, the machine advances to the next state that
-- allows this token, or Is_Allowed returns False if none exists (and
-- the state does not change).

-- WARNING !!
-- If you instantiate this packages immediately inside a library package,
-- you must put a "pragma Elaborate (Framework.Ordering_Machine);", or
-- circularity will result.

generic
   Rule_Id : in Wide_String;
   type Token     is (<>);
   type Token_Set is array (Token) of Boolean;
package Framework.Ordering_Machine is
   type Instance is private;

   procedure Add_State (Machine : in out Instance; Allowed : Token_Set);
   -- Register a new state to the machine (after those already given)

   procedure Reset (Machine : out Instance);
   -- Removes all registered states

   procedure Set_Initial (Machine : in out Instance);
   -- Initializes the machine to the first state

   procedure Set_State (Machine : in out Instance; To : Token);
   -- Sets the machine to the next state (possibly the current one) that
   -- allows To, or to Not_Allowed if there is none.

   function Is_Allowed (Machine : Instance) return Boolean;
   -- Returns True if Machine is not in the Not_Allowed state.

   function Current_State (Machine : Instance) return Positive;

private
   type States_Index_Base is range 0 .. Max_Controls_Set;
   -- Absolute limit for the number of possible states
   -- Arbitrary, limited by Max_Controls_Set because it is assumed a
   -- reasonable limit for arrays of sets.
   -- If this limit is exceeded, Constraint_Error will be raised at elaboration time
   -- so there is no risk.


   subtype States_Index is States_Index_Base range 1 .. Token'Pos (Token'Last) + 1 + 1;
   -- Real number of states that can be specified for this instantiation.
   -- Quite arbitrary; here we allow one position for each Token, plus a possible extra
   -- guard value.

   type States_Array is array (States_Index) of Token_Set;

   type Instance is
      record
         States            : States_Array;
         Registered_States : States_Index_Base := 0;
         Current_State     : States_Index;
         Is_Allowed        : Boolean := False;
      end record;

end Framework.Ordering_Machine;
