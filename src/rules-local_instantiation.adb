----------------------------------------------------------------------
--  Rules.Local_Instantiation - Package body                        --
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
  Asis.Elements,
  Asis.Declarations;

-- Adalog
with
  Utilities;

-- Adactl
with
  Framework.Language,
  Framework.Rules_Manager,
  Framework.Reports,
  Framework.Scope_Manager;

package body Rules.Local_Instantiation  is
   use Framework;

   Rule_Used : Boolean := False;

   Rule_Uses : Context_Store;

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Parameter(s): <generic name list>");
      User_Message ("This rule can be used to check/search for the instantiations "
                      & "that are done in a local scope.");
   end Help;

   -------------
   -- Add_Use --
   -------------

   procedure Add_Use (Label     : in Wide_String;
                      Rule_Type : in Rule_Types) is
      use Ada.Strings.Wide_Unbounded;
      use Framework.Language;

   begin
       if  not Parameter_Exists then
         Parameter_Error ("At least one parameter required for rule " & Rule_Id);
      end if;

      while Parameter_Exists loop
         declare
            Entity : constant Entity_Specification := Get_Entity_Parameter;
         begin
            Associate (Rule_Uses,
                       Entity,
                       Simple_Context'(Rule_Type  => Rule_Type,
                                       Rule_Label => To_Unbounded_Wide_String (Label)));
         exception
            when Already_In_Store =>
               Parameter_Error (To_Wide_String (Entity.Specification)
                                  & " is already used in rule " & Rule_Id);
         end;
      end loop;

      Rule_Used := True;
   end Add_Use;

   -------------
   -- Prepare --
   -------------

   procedure Prepare is
   begin
      Balance (Rule_Uses);
   end Prepare;

   ---------------------------
   -- Process_Instantiation --
   ---------------------------

   procedure Process_Instantiation (Instantiation : in Asis.Declaration) is
      use Asis.Declarations;
      use Ada.Strings.Wide_Unbounded;
      use Framework.Reports;
   begin
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      declare
         use Framework.Scope_Manager, Asis, Asis.Elements, Utilities;
         Current_Context : Rule_Context'Class
           := Matching_Context (Rule_Uses, Generic_Unit_Name (Instantiation));
         Scopes          : constant Asis.Element_List := Active_Scopes;
      begin
         if Current_Context = No_Matching_Context then
            return;
         end if;

         for I in Scopes'Range loop
            -- Note that an instantiation cannot happen in a specification other than
            -- a package specification, nor (directly) in a protected body.
            -- Note also that generic bodies appear simply as regular bodies.
            -- If an instantiation is inside a generic package, we do not consider
            -- it local, since there may be instantiations of the enclosing package
            -- that do not violate the rule. This does not of course extend to
            -- generic subprograms.
            case Element_Kind (Scopes(I)) is
               when A_Declaration =>
                  case Declaration_Kind (Scopes (I)) is
                     when A_Procedure_Body_Declaration
                       | A_Function_Body_Declaration
                       | A_Task_Body_Declaration
                       | A_Protected_Body_Declaration
                       | An_Entry_Body_Declaration
                       =>
                        Report (Rule_Id,
                                To_Wide_String ( Simple_Context (Current_Context).Rule_Label),
                                Simple_Context (Current_Context).Rule_Type,
                                Get_Location (Instantiation),
                                "local instantiation of """ & To_Title (Last_Matching_Name (Rule_Uses)) & '"');
                     when others =>
                        -- This covers :
                        --   all specifications (no call can happen there)
                        --   things that are not declarations, i.e. statements and exception
                        --   handlers
                        null;
                  end case;
               when A_Statement =>
                  case Statement_Kind (Scopes (I)) is
                     when A_Block_Statement =>
                        Report (Rule_Id,
                                To_Wide_String ( Simple_Context (Current_Context).Rule_Label),
                                Simple_Context (Current_Context).Rule_Type,
                                Get_Location (Instantiation),
                                "local instantiation of """ & To_Title (Last_Matching_Name (Rule_Uses)) & '"');
                     when others =>
                        null;
                  end case;

               when others =>
                  null;
            end case;
         end loop;
      end;
   end Process_Instantiation;

begin
   Framework.Rules_Manager.Register (Rule_Id,
                         Help    => Help'Access,
                         Prepare => Prepare'Access,
                         Add_Use => Add_Use'Access);
end Rules.Local_Instantiation;
