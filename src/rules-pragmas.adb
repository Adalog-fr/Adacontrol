----------------------------------------------------------------------
--  Rules.Pragmas - Package body                                    --
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

-- ASIS
with
  Asis.Elements;

-- Adalog
with
  Utilities;

-- Adactl
with
  Framework.Language,
  Framework.Rules_Manager,
  Framework.Reports;

package body Rules.Pragmas is
  use Framework;

   Rule_Used : Boolean := False;
   Save_Used : Boolean;

   Rule_Uses : Context_Store;

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Parameter(s): all | nonstandard | <pragma names>");
      User_Message ("Control usage of specific pragmas");
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Framework.Language;

   begin
      if  not Parameter_Exists then
         Parameter_Error (Rule_Id, "At least one parameter required");
      end if;

      while Parameter_Exists loop
         declare
            Pragma_Name : constant Wide_String := Get_Name_Parameter;
         begin
            -- "Nonstandard" and "all" are handled just as if they were pragma names
            Associate (Rule_Uses,
                       Specification => Value (Pragma_Name),
                       Context       => Basic.New_Context (Ctl_Kind, Ctl_Label));
         exception
            when Already_In_Store =>
               Parameter_Error (Rule_Id, Pragma_Name & " already given");
         end;
      end loop;

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
            Clear (Rule_Uses);
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := False;
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;

   -------------
   -- Prepare --
   -------------

   procedure Prepare is
   begin
      Balance (Rule_Uses);
   end Prepare;

   --------------------
   -- Process_Pragma --
   --------------------

   procedure Process_Pragma (Pragma_Element : in Asis.Pragma_Element) is
      use Asis, Asis.Elements, Utilities, Framework.Reports;
      Found : Boolean := False;
   begin
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      -- Check specific pragma
      declare
         Current_Context : constant Root_Context'Class := Matching_Context (Rule_Uses, Pragma_Element);
      begin
         if Current_Context /= No_Matching_Context then
            Report (Rule_Id,
                    Current_Context,
                    Get_Location (Pragma_Element),
                    "use of pragma """ & To_Title (Last_Matching_Name (Rule_Uses)) & '"');
            Found := True;
         end if;
      end;

      -- Check nonstandard
      declare
         Current_Context : constant Root_Context'Class := Framework.Association (Rule_Uses, "NONSTANDARD");
      begin
         if Current_Context /= No_Matching_Context
            and then Pragma_Kind (Pragma_Element) in An_Implementation_Defined_Pragma .. An_Unknown_Pragma
         then
              Report (Rule_Id,
                      Current_Context,
                      Get_Location (Pragma_Element),
                      "use of non-standard pragma """ & Pragma_Name_Image (Pragma_Element) & '"',
                      Count_Only => Found);
              Found := True;
         end if;
      end;

      -- check all
      declare
         Current_Context : constant Root_Context'Class := Framework.Association (Rule_Uses, "ALL");
      begin
         if Current_Context /= No_Matching_Context then
            Report (Rule_Id,
                    Current_Context,
                    Get_Location (Pragma_Element),
                    "use of pragma """ & Pragma_Name_Image (Pragma_Element) & '"',
                    Count_Only => Found);
         end if;
      end;

   end Process_Pragma;

begin  -- Rules.Pragmas
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access,
                                     Prepare_CB     => Prepare'Access);
end Rules.Pragmas;
