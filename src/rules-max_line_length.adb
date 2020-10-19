----------------------------------------------------------------------
--  Rules.Max_Line_Length - Package body                            --
--                                                                  --
--  This software  is (c) The European Organisation  for the Safety --
--  of Air Navigation (EUROCONTROL) and Adalog 2004-2005.           --
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

-- Ada
with
  Ada.Strings.Wide_Unbounded;

-- Adalog
with
  Utilities;

-- Adactl
with
  Framework.Language,
  Framework.Rules_Manager,
  Framework.Reports;

package body Rules.Max_Line_Length is
   use Framework;

   Rule_Used : Boolean := False;
   Save_Used : Boolean;

   Ctl_Labels : array (Control_Kinds) of Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
   Maximum    : array (Control_Kinds) of Asis.ASIS_Natural := (others => Asis.ASIS_Natural'Last);

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Control that no source line is longer than the indicated maximum");
      User_Message;
      User_Message ("Parameter: <Max allowed length>");
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Ada.Strings.Wide_Unbounded;
      use Framework.Language;

      use type Asis.ASIS_Integer;   -- Gela-ASIS compatibility
   begin
      if Maximum (Ctl_Kind) /= Asis.ASIS_Natural'Last then
         Parameter_Error (Rule_Id, "rule already specified");
      end if;

      if not Parameter_Exists then
         Parameter_Error (Rule_Id, "Maximum value required");
      end if;

      begin
         Maximum (Ctl_Kind) := Get_Integer_Parameter (Min => 10, Max => Asis.ASIS_Natural'Last - 1);
      exception
         when Constraint_Error =>
            Parameter_Error (Rule_Id, "maximum value negative or too big");
      end;

      if Maximum (Ctl_Kind) <= 10 then -- Must be at least as long as the longest KW...
         Parameter_Error (Rule_Id, "maximum value must be at least 10");
      end if;

      Ctl_Labels (Ctl_Kind) := To_Unbounded_Wide_String (Ctl_Label);
      Rule_Used := True;
   end Add_Control;

   -------------
   -- Command --
   -------------

   procedure Command (Action : Framework.Rules_Manager.Rule_Action) is
      use Framework.Rules_Manager;
   begin
      case Action is
         when Clear =>
            Rule_Used := False;
            Maximum := (others => Asis.ASIS_Natural'Last);
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := False;
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;

   ------------------
   -- Process_Line --
   ------------------

   procedure Process_Line (Line : in Asis.Program_Text; Loc : Locations.Location) is
      use Framework.Reports, Utilities, Ada.Strings.Wide_Unbounded;

      use type Asis.ASIS_Integer;   --## rule line off Unnecessary_Use_Clause Reduceable_Scope ## Gela compatibility
   begin
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      if Line'Length > Maximum (Check) then
         Report (Rule_Id, To_Wide_String (Ctl_Labels (Check)), Check, Loc,
                 "line too long (" & Integer_Img (Line'Length) & ')');
      elsif Line'Length > Maximum (Search) then
         Report (Rule_Id, To_Wide_String (Ctl_Labels (Search)), Search, Loc,
                 "line too long ("& Integer_Img (Line'Length) & ')');
      end if;

      if Line'Length > Maximum (Count) then
         Report (Rule_Id, To_Wide_String (Ctl_Labels (Count)), Count, Loc, "");
      end if;
  end Process_Line;

begin  -- Rules.Max_Line_Length
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Textual,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access);
end Rules.Max_Line_Length;
