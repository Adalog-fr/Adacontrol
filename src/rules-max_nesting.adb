----------------------------------------------------------------------
--  Rules.Max_Nesting - Package body                                --
--                                                                  --
--  This software  is (c) The European Organisation  for the Safety --
--  of Air  Navigation (EUROCONTROL) and Adalog  2004-2005. The Ada --
--  Controller  is  free software;  you can redistribute  it and/or --
--  modify  it under  terms of  the GNU  General Public  License as --
--  published by the Free Software Foundation; either version 2, or --
--  (at your  option) any later version.  This  unit is distributed --
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
--                                                                  --
--  This  software is  distributed  in  the hope  that  it will  be --
--  useful,  but WITHOUT  ANY  WARRANTY; without  even the  implied --
--  warranty  of  MERCHANTABILITY   or  FITNESS  FOR  A  PARTICULAR --
--  PURPOSE.                                                        --
----------------------------------------------------------------------

-- Ada
with
  Ada.Strings.Wide_Unbounded;

-- Adalog
with
  Utilities;

-- Asis
with
  Asis.Elements;

-- Adactl
with
  Framework.Language,
  Framework.Reports,
  Framework.Rules_Manager,
  Framework.Scope_Manager;

package body Rules.Max_Nesting is
   use Framework, Framework.Scope_Manager;

   Rule_Used    : Boolean := False;
   Save_Used    : Boolean;
   Count_Depth  : Scope_Range := Scope_Range'Last;
   Search_Depth : Scope_Range := Scope_Range'Last;
   Check_Depth  : Scope_Range := Scope_Range'Last;
   Rule_Check_Label  : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
   Rule_Search_Label : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
   Rule_Count_Label  : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;

   Not_Counted : Scope_Range := 0;
   -- Number of active scopes not counted for the depth
   -- i.e.: for loops and accept statements

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Parameter 1: <maximum allowed nesting level>");
      User_Message ("Control scopes nested deeper than a given limit.");
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Ada.Strings.Wide_Unbounded;
      use Framework.Language;

      Max : Integer;
   begin
      if not Parameter_Exists then
         Parameter_Error (Rule_Id, "max nesting value expected");
      end if;

      Max := Get_Integer_Parameter (Min => 0);

      case Ctl_Kind is
         when Check =>
            if Check_Depth /= Scope_Range'Last then
               Parameter_Error (Rule_Id, "this rule can be specified only once for each of check, search and count");
            end if;
            Check_Depth      := Scope_Range (Max) + 1;
            Rule_Check_Label := To_Unbounded_Wide_String (Ctl_Label);
         when Search =>
            if Search_Depth /= Scope_Range'Last then
               Parameter_Error (Rule_Id, "this rule can be specified only once for each of check, search and count");
            end if;
            Search_Depth      := Scope_Range (Max) + 1;
            Rule_Search_Label := To_Unbounded_Wide_String (Ctl_Label);
         when Count =>
            if Count_Depth /= Scope_Range'Last then
               Parameter_Error (Rule_Id, "this rule can be specified only once for each of check, search and count");
            end if;
            Count_Depth       := Scope_Range (Max) + 1;
            Rule_Count_Label := To_Unbounded_Wide_String (Ctl_Label);
     end case;

     Rule_Used  := True;
   exception
      when Constraint_Error =>
         Parameter_Error (Rule_Id,
                          "specified nesting greater than allowed maximum of"
                          & Scope_Range'Wide_Image (Scope_Range'Last - 1));
   end Add_Control;

   -------------
   -- Command --
   -------------

   procedure Command (Action : Framework.Rules_Manager.Rule_Action) is
      use Ada.Strings.Wide_Unbounded, Framework.Rules_Manager;
   begin
      case Action is
         when Clear =>
            Rule_Used         := False;
            Count_Depth       := Scope_Range'Last;
            Search_Depth      := Scope_Range'Last;
            Check_Depth       := Scope_Range'Last;
            Rule_Check_Label  := Null_Unbounded_Wide_String;
            Rule_Search_Label := Null_Unbounded_Wide_String;
            Rule_Count_Label  := Null_Unbounded_Wide_String;
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := False;
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;

   -------------------------
   -- Process_Scope_Enter --
   -------------------------

   procedure Process_Scope_Enter (Scope : in Asis.Element) is
      use Framework.Reports, Ada.Strings.Wide_Unbounded, Asis, Asis.Elements;
   begin
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      -- Do not count exception handlers and statements other than blocks
      case Element_Kind (Scope) is
         when A_Statement =>
            if Statement_Kind (Scope) /= A_Block_Statement then
               Not_Counted := Not_Counted + 1;
               return;
            end if;
         when An_Exception_Handler =>
            Not_Counted := Not_Counted + 1;
            return;
         when others =>
            null;
     end case;

      -- We check only if it is equal to the first forbidden level.
      -- It is not useful to issue a message if there are even deeper levels.
      if Current_Depth - Not_Counted = Check_Depth then
         Report (Rule_Id,
                 To_Wide_String (Rule_Check_Label),
                 Check,
                 Get_Location (Scope),
                 "nesting deeper than" & Scope_Range'Wide_Image (Check_Depth-1));
      elsif Current_Depth - Not_Counted = Search_Depth then
         Report (Rule_Id,
                 To_Wide_String (Rule_Search_Label),
                 Search,
                 Get_Location (Scope),
                 "nesting deeper than" & Scope_Range'Wide_Image (Search_Depth-1));
      end if;

      -- But counting is independent
      if Current_Depth - Not_Counted = Count_Depth then
         Report (Rule_Id,
                 To_Wide_String (Rule_Count_Label),
                 Count,
                 Get_Location (Scope),
                 "nesting deeper than" & Scope_Range'Wide_Image (Count_Depth-1));
      end if;
  end Process_Scope_Enter;

   ------------------------
   -- Process_Scope_Exit --
   ------------------------

   procedure Process_Scope_Exit (Scope : in Asis.Element) is
      use Asis, Asis.Elements;
   begin
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      case Element_Kind (Scope) is
         when A_Statement =>
            if Statement_Kind (Scope) /= A_Block_Statement then
               Not_Counted := Not_Counted - 1;
            end if;
         when An_Exception_Handler =>
            Not_Counted := Not_Counted - 1;
         when others =>
            null;
      end case;
   end Process_Scope_Exit;

begin  -- Rules.Max_Nesting
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access);
end Rules.Max_Nesting;
