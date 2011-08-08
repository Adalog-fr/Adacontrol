----------------------------------------------------------------------
--  Rules.Local_Hiding - Package body                               --
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
  Ada.Strings.Wide_Fixed,
  Ada.Strings.Wide_Unbounded;

-- Asis
with
  Asis.Declarations,
  Asis.Elements;

-- Adalog
with
  Thick_Queries,
  Utilities;

-- Adactl
with
  Framework.Language,
  Framework.Rules_Manager,
  Framework.Reports,
  Framework.Scope_Manager;

package body Rules.Local_Hiding is
   use Framework, Utilities;

   Rule_Used  : Boolean := False;
   Rule_Type  : Rule_Types;
   Rule_Label : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;

   type Identifier_Data (Length : Positive) is
      record
         Name : Wide_String (1..Length);
         Elem : Asis.Element;
      end record;
   package Visible_Identifiers is new Framework.Scope_Manager.Scoped_Store (Identifier_Data);

   ----------
   -- Help --
   ----------

   procedure Help is
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Parameter(s): none");
      User_Message ("This rule can be used to check/search for local identifiers"
                      & " that hide an outer identical name");
   end Help;

   -------------
   -- Add_Use --
   -------------

   procedure Add_Use (Label         : in Wide_String;
                      Rule_Use_Type : in Rule_Types) is
      use Ada.Strings.Wide_Unbounded;
      use Framework.Language;

   begin
      if  Parameter_Exists then
         Parameter_Error ("No parameter for rule " & Rule_Id);
      end if;

      if Rule_Used then
         Parameter_Error (Rule_Id & ": this rule can be specified only once");
      else
         Rule_Type  := Rule_Use_Type;
         Rule_Label := To_Unbounded_Wide_String (Label);
         Rule_Used  := True;
      end if;
   end Add_Use;

   ---------------------------
   -- Process_Defining_Name --
   ---------------------------

   procedure Process_Defining_Name (Name : in Asis.Defining_Name) is
      use Thick_Queries, Asis, Asis.Elements, Asis.Declarations;
      Scope : Asis.Element;
   begin
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      -- A Defining_Name is always inside a Declaration.
      -- The scope of this declaration is (at least) its enclosing element.
      -- NB: Scope is Nil_Element for a library unit, this is checked later
      Scope := Enclosing_Element (Name);
      if Defining_Name_Kind (Scope) = A_Defining_Expanded_Name then
         -- Special case: the defining name is that of a child unit
         -- We must go up one more time to account for the composite name
         Scope := Enclosing_Element (Scope);
      end if;
      Scope := Enclosing_Element (Scope);

      if Element_Kind (Scope) = A_Definition -- 1)
        or else Declaration_Kind (Scope) in A_Renaming_Declaration -- 2)
      then
         -- This identifier is either :
         -- 1) a part of a type definition (i.e. components of record of protected types,
         --    formal parameters of an access-to-subprogram or of a protected operation).
         -- 2) A formal parameter of the profile of a renaming declaration
         -- => Ignore.
         return;
      end if;

      declare
         use Framework.Reports, Framework.Scope_Manager, Asis.Elements;
         use Ada.Strings.Wide_Fixed, Ada.Strings.Wide_Unbounded;
         Short_Name    : constant Wide_String := To_Upper (Defining_Name_Image (Name));
         Full_Name     : constant Wide_String := Short_Name & Profile_Image (Name);
         Callable_Name : constant Boolean     := Is_Callable_Construct (Name);

         function Is_Same (Check : Identifier_Data) return Boolean is
            -- If both are callable entities, compare with profiles
            -- otherwise, compare without profile
         begin
            if Is_Callable_Construct (Check.Elem) then
               if Callable_Name then
                  return Check.Name = Full_Name;
               else
                  return Check.Name (Check.Name'First .. Index (Check.Name, "{")-1) = Short_Name;
               end if;
            else
               return Check.Name = Short_Name;
            end if;
         end Is_Same;
      begin
         if Is_Nil (Scope) then
            -- This is the name of a compilation unit
            -- => cannot hide anything (but can be hidden)
            Visible_Identifiers.Push ((Full_Name'Length, Full_Name, Name));
            return;
         end if;

         Visible_Identifiers.Reset (All_Scopes);
         while Visible_Identifiers.Data_Available loop
            if Is_Same (Visible_Identifiers.Get_Current_Data) then

               -- Discard the case where we find a name within the same scope as Name:
               -- this corresponds for example to a spec and corresponding body, incomplete
               -- type and corresponding full declarations...
               -- These must correspond to the same entity, otherwise it would not be allowed
               -- by the compiler.
               if not Is_Equal (Enclosing_Element
                                  (Enclosing_Element (Visible_Identifiers.Get_Current_Data.Elem)),
                                Scope)
               then
                  Report (Rule_Id,
                          To_Wide_String (Rule_Label),
                          Rule_Type,
                          Get_Location (Name),
                          '"' & To_Title (Full_Name)
                            & """ hides declaration at "
                            & Image (Get_Location (Visible_Identifiers.Get_Current_Data.Elem)));
                  exit;
               end if;
            end if;
            Visible_Identifiers.Next;
         end loop;
         Visible_Identifiers.Push ((Full_Name'Length, Full_Name, Name));
      end;
   end Process_Defining_Name;

begin
   Framework.Rules_Manager.Register (Rule_Id,
                           Help    => Help'Access,
                           Prepare => null,
                           Add_Use => Add_Use'Access);
end Rules.Local_Hiding;
