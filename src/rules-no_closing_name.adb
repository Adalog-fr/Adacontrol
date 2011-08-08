----------------------------------------------------------------------
--  Rules.No_Closing_Name - Package body                            --
--                                                                  --
--  This software  is (c) The European Organisation  for the Safety --
--  of Air  Navigation (EUROCONTROL) and Adalog  2004-2005. The Ada --
--  Code Cheker  is free software;  you can redistribute  it and/or --
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
  Asis.Declarations,
  Asis.Text;

-- Adalog
with
  Utilities;

-- Adactl
with
  Framework.Language,
  Framework.Reports,
  Framework.Rules_Manager;

package body Rules.No_Closing_Name is
   use Framework, Utilities, Asis;

   Rule_Used         : Boolean := False;
   Search_Length     : ASIS_Integer := ASIS_Integer'Last;
   Check_Length      : ASIS_Integer := ASIS_Integer'Last;
   Rule_Check_Label  : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
   Rule_Search_Label : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;

   ----------
   -- Help --
   ----------

   procedure Help is
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Parameter(s): none");
      User_Message ("This rule can be used to check/search for constructs where repeating");
      User_Message ("the construct name at the end is optional and not provided.");
   end Help;

   -------------
   -- Add_Use --
   -------------

   procedure Add_Use (Label         : in Wide_String;
                      Rule_Use_Type : in Rule_Types) is
      use Ada.Strings.Wide_Unbounded;
      use Framework.Language;

      Max_Length : Integer;
   begin
      if  Parameter_Exists then
         Max_Length := Get_Integer_Parameter;
      else
         Max_Length := -1;
      end if;
      if  Parameter_Exists then
         Parameter_Error ("At most one parameter for rule " & Rule_Id);
      end if;

      case Rule_Use_Type is
         when Check =>
            if Check_Length /= ASIS_Integer'Last then
               Parameter_Error (Rule_Id &
                                  ": this rule can be specified only once for check " &
                                  "and once for search");
            end if;
            Check_Length     := ASIS_Integer (Max_Length);
            Rule_Check_Label := To_Unbounded_Wide_String (Label);
         when Search =>
            if Search_Length /= ASIS_Integer'Last then
               Parameter_Error (Rule_Id &
                                  ": this rule can be specified only once for check " &
                                  "and once for search");
            end if;
            Search_Length     := ASIS_Integer (Max_Length);
            Rule_Search_Label := To_Unbounded_Wide_String (Label);
      end case;
      Rule_Used := True;
   end Add_Use;

   -----------------------
   -- Process_Construct --
   -----------------------

   procedure Process_Construct (Construct : in Asis.Declaration) is
      use Framework.Reports, Asis.Declarations, Ada.Strings.Wide_Unbounded, Asis.Text;
      Length : Line_Number;
   begin
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      if not Is_Name_Repeated (Construct) then
         Length := Last_Line_Number (Construct) - First_Line_Number (Construct) + 1;
         if Length > Check_Length then
            Report (Rule_Id,
                    To_Wide_String (Rule_Check_Label),
                    Check,
                    Get_Location (Construct),
                    "name not repeated at the end");
         elsif Length > Search_Length then
            Report (Rule_Id,
                    To_Wide_String (Rule_Search_Label),
                    Search,
                    Get_Location (Construct),
                    "name not repeated at the end");
         end if;
     end if;
   end Process_Construct;

begin
   Framework.Rules_Manager.Register (Rule_Id,
                           Help    => Help'Access,
                           Prepare => null,
                           Add_Use => Add_Use'Access);
end Rules.No_Closing_Name;
