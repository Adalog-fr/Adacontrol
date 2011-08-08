----------------------------------------------------------------------
--  Rules.When_Others_Null - Package body                           --
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

-- Asis
with
  Asis,
  Asis.Elements,
  Asis.Statements;

-- Adalog
with
  Utilities;

-- Adactl
with
  Framework.Language,
  Framework.Rules_Manager,
  Framework.Reports;

package body Rules.When_Others_Null is
   use Framework;

   type To_Check is (K_Case, K_Exception);
   type Usage is array (To_Check) of Boolean;

   Rule_Used  : Usage := (others => False);
   Save_Used  : Usage;
   Rule_Type  : array (To_Check) of Rule_Types;
   Rule_Label : array (To_Check) of Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;

   -----------
   -- Image --
   -----------

   function Image (Check : To_Check) return Wide_String is
      use Utilities;
      Img : constant Wide_String := To_Lower (To_Check'Wide_Image (Check));
   begin
      -- Remove "K_"
      return Img (3 .. Img'Last);
   end Image;

   -------------
   -- Command --
   -------------

   procedure Command (Action : Framework.Rules_Manager.Rule_Action) is
      use Framework.Rules_Manager;
   begin
      case Action is
         when Clear =>
            Rule_Used := (others => False);
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := (others => False);
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Parameter(s) : case | exception (optional, default=all)");
      User_Message ("Control occurrence of ""when others => null;""");
      User_Message ("in case statements and exception handlers.");
   end Help;

   -------------
   -- Add_Use --
   -------------

   procedure Add_Use (Label         : in Wide_String;
                      Rule_Use_Type : in Rule_Types) is
      use Ada.Strings.Wide_Unbounded, Framework.Language;

      Check : To_Check;
      function Get_Check_Parameter is new Get_Flag_Parameter (Flags     => To_Check,
                                                              Allow_Any => False,
                                                              Prefix    => "K_");

   begin
      if Parameter_Exists then
         while Parameter_Exists loop
            Check := Get_Check_Parameter;
            if Rule_Used (Check) then
               Parameter_Error ("Check already given for rule " & Rule_Id
                                & ": " & Image (Check));
            else
               Rule_Type  (Check) := Rule_Use_Type;
               Rule_Label (Check) := To_Unbounded_Wide_String (Label);
               Rule_Used  (Check) := True;
            end if;
         end loop;
      else
         Rule_Type  := (others => Rule_Use_Type);
         Rule_Label := (others => To_Unbounded_Wide_String (Label));
         Rule_Used  := (others => True);
     end if;
   end Add_Use;

   ----------------------------
   -- Process_Case_Statement --
   ----------------------------

   procedure Process_Case_Statement (Statement : in Asis.Statement) is
      use Ada.Strings.Wide_Unbounded, Asis, Asis.Elements, Asis.Statements, Framework.Reports;
      Paths        : constant Asis.Element_List := Statement_Paths (Statement);
      Last_Path    : Asis.Element renames Paths (Paths'LAST);
      First_Choice : constant Asis.Element      := Case_Statement_Alternative_Choices (Last_Path)(1);
   begin
      if not Rule_Used (K_Case) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      if Definition_Kind (First_Choice) = An_Others_Choice then
         declare
            Statements : constant Statement_List := Sequence_Of_Statements (Last_Path);
         begin
            for I in Statements'Range loop
               if Statement_Kind (Statements (I)) /= A_Null_Statement then
                  return;
               end if;
            end loop;
            Report (Rule_Id,
                    To_Wide_String (Rule_Label (K_Case)),
                    Rule_Type (K_Case),
                    Get_Location (First_Choice),
                    "null ""when others"" in case statement");
         end;
      end if;

   end Process_Case_Statement;

   -------------------------------
   -- Process_Exception_Handler --
   -------------------------------

   procedure Process_Exception_Handler (Handler : in Asis.Exception_Handler) is
      use Ada.Strings.Wide_Unbounded, Asis, Asis.Elements, Asis.Statements, Framework.Reports;
   begin
      if not Rule_Used (K_Exception) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      if Definition_Kind (Exception_Choices (Handler)(1)) = An_Others_Choice then
         declare
            Statements : constant Statement_List := Handler_Statements (Handler);
         begin
            for I in Statements'Range loop
               if Statement_Kind (Statements (I)) /= A_Null_Statement then
                  return;
               end if;
            end loop;
            Report (Rule_Id,
                    To_Wide_String (Rule_Label (K_Exception)),
                    Rule_Type (K_Exception),
                    Get_Location (Handler),
                    "null ""when others"" exception handler");

         end;
      end if;

   end Process_Exception_Handler;

begin
   Framework.Rules_Manager.Register (Rule_Id,
                                     Help    => Help'Access,
                                     Add_Use => Add_Use'Access,
                                     Command => Command'Access);
end Rules.When_Others_Null;
