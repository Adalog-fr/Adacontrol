----------------------------------------------------------------------
--  Framework.Ordering_Machine - Package body                       --
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

-- AdaCtl
with
  Framework.Language;

package body Framework.Ordering_Machine is

   ---------------
   -- Add_State --
   ---------------

   procedure Add_State (Machine : in out Instance; Allowed : Token_Set) is
      use Framework.Language;
   begin
      if Machine.Registered_States = States_Index'Last then
         Parameter_Error (Rule_Id, "Too many parameters");
      end if;

      Machine.Registered_States := Machine.Registered_States + 1;
      Machine.States (Machine.Registered_States) := Allowed;
   end Add_State;

   -----------
   -- Reset --
   -----------

   procedure Reset (Machine : out Instance) is
   begin
      Machine.Registered_States := 0;
      Machine.Is_Allowed        := False;
   end Reset;

   -----------------
   -- Set_Initial --
   -----------------

   procedure Set_Initial (Machine : in out Instance) is
   begin
      Machine.Current_State := 1;
      Machine.Is_Allowed    := False; -- Until a token is given
   end Set_Initial;

   ----------------
   -- Is_Allowed --
   ----------------

   function Is_Allowed (Machine : Instance) return Boolean is
   begin
      return Machine.Is_Allowed;
   end Is_Allowed;

   ---------------
   -- Set_State --
   ---------------

   procedure Set_State (Machine : in out Instance; To : Token) is
      New_State : States_Index := Machine.Current_State;
   begin
      while not Machine.States (New_State) (To) loop
         if New_State = Machine.Registered_States then
            -- Not found
            -- Current_State stays, to avoid multiple messages if there is only one token out of order
            Machine.Is_Allowed := False;
            return;
         end if;

         New_State := New_State + 1;
      end loop;

      Machine.Is_Allowed    := True;
      Machine.Current_State := New_State;
   end Set_State;

   -------------------
   -- Current_State --
   -------------------

   function Current_State (Machine : Instance) return Positive is
   begin
      return Integer (Machine.Current_State);
   end Current_State;

end Framework.Ordering_Machine;
