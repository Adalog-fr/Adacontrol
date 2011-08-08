----------------------------------------------------------------------
--  Rules.Declaration - Package body                                --
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

-- ASIS
with
  ASIS.Elements,
  ASIS.Declarations;

-- Adalog
with
  Utilities;

-- Adactl
with
  Framework.Language,
  Framework.Rules_Manager,
  Framework.Reports;

package body Rules.Declaration is
   use Framework;

   type Declaration_Names is (Decl_Access,    Decl_Access_Subprogram, Decl_Aliased,
                              Decl_Exception, Decl_Tagged,            Decl_Task);

   -----------
   -- Image --
   -----------

   function Image (Stmt : Declaration_Names) return Wide_String is
      use Utilities;
      Img : constant Wide_String := To_Lower (Declaration_Names'Wide_Image (Stmt));
   begin
      -- Remove "DECL_"
      return Img (6 .. Img'Last);
   end Image;

   type Usage_Flags is array (Declaration_Names) of Boolean;
   Rule_Used : Usage_Flags := (others => False);
   Save_Used : Usage_Flags;
   Usage     : array (Declaration_Names) of Simple_Context;

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Parameter(s): access | access_subprogram | aliased | exception | tagged | task");
      User_Message ("Control occurrences of Ada declarations");
   end Help;

   -------------
   -- Add_Use --
   -------------

   procedure Add_Use (Label     : in Wide_String;
                      Rule_Type : in Rule_Types) is
      use Ada.Strings.Wide_Unbounded;
      use Framework.Language;
      Decl    : Declaration_Names;

      function Get_Declaration_Parameter is new Get_Flag_Parameter (Flags     => Declaration_Names,
                                                                    Allow_Any => False,
                                                                    Prefix    => "DECL_");
   begin
      if not Parameter_Exists then
         Parameter_Error ("At least one parameter required for rule " & Rule_Id);
      end if;

      while Parameter_Exists loop
         Decl := Get_Declaration_Parameter;
         if Rule_Used (Decl) then
            Parameter_Error ("Declaration already given for rule " & Rule_Id
                             & ": " & Image (Decl));
         end if;

         Rule_Used (Decl) := True;
         Usage (Decl)     := (Rule_Type, To_Unbounded_Wide_String (Label));
      end loop;
   end Add_Use;

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

   -------------------------
   -- Process_Declaration --
   -------------------------

   procedure Process_Declaration (Element : in Asis.Declaration) is
      use Ada.Strings.Wide_Unbounded, Asis, Asis.Elements, Asis.Declarations, Framework.Reports;
      Decl : Declaration_Names;
   begin
      if Rule_Used = (Declaration_Names => False) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      case Declaration_Kind (Element) is
         when An_Ordinary_Type_Declaration =>
            case Type_Kind (Type_Declaration_View (Element)) is
               when An_Access_Type_Definition =>
                  case Access_Type_Kind (Type_Declaration_View (Element)) is
                     when Access_To_Subprogram_Definition =>
                        if Rule_Used (Decl_Access_Subprogram) then
                           Decl := Decl_Access_Subprogram;
                        else
                           Decl := Decl_Access;
                        end if;
                     when others =>
                        Decl := Decl_Access;
                  end case;

               when A_Tagged_Record_Type_Definition =>
                  Decl := Decl_Tagged;

               when others =>
                  return;
            end case;

         when A_Variable_Declaration
           | A_Constant_Declaration =>
            case Trait_Kind (Element) is
               when An_Aliased_Trait =>
                  Decl := Decl_Aliased;
               when others =>
                  return;
            end case;

         when A_Task_Type_Declaration =>
            Decl := Decl_Task;

         when A_Single_Task_Declaration =>
            Decl := Decl_Task;

         when An_Exception_Declaration =>
            Decl := Decl_Exception;

         when others =>
            return;
      end case;

      if not Rule_Used (Decl) then
         return;
      end if;

      Report (Rule_Id,
              To_Wide_String (Usage(Decl).Rule_Label),
              Usage (Decl).Rule_Type,
              Get_Location (Element),
              "use of declaration """ & Image (Decl) & '"');
   end Process_Declaration;

begin
   Framework.Rules_Manager.Register (Rule_Id,
                                     Help    => Help'Access,
                                     Add_Use => Add_Use'Access,
                                     Command => Command'Access);
end Rules.Declaration;
