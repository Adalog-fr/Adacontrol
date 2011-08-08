----------------------------------------------------------------------
--  Framework.Queries - Package body                                --
--                                                                  --
--  This software  is (c) The European Organisation  for the Safety --
--  of Air  Navigation (EUROCONTROL) and Adalog  2004-2007. The Ada --
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

-- Asis
with
  Asis.Compilation_Units,
  Asis.Declarations,
  Asis.Elements,
  Asis.Expressions,
  Asis.Iterator,
  Asis.Statements;

-- Adalog
with
  A4G_Bugs,
  Binary_Map,
  Thick_Queries,
  Utilities;

-- Adacontrol
with
  Framework.Reports;

package body Framework.Queries is

   package Element_Map is new Binary_Map (Unbounded_Wide_String, Asis.Element);

   Symbol_Map : Element_Map.Map;

   ----------------------------
   -- Enclosing_Package_Name --
   ----------------------------

   function Enclosing_Package_Name (Rule_Id : Wide_String; N : in Asis.Name) return Wide_String is
      use Asis, Asis.Compilation_Units, Asis.Declarations, Asis.Elements, Asis.Expressions;
      use Framework.Reports, Thick_Queries, Utilities;

      E : Asis.Element := A4G_Bugs.Corresponding_Name_Declaration (N);
      C : Asis.Compilation_Unit;
   begin
      if Is_Nil (E) then
         -- This should be:
         -- 1) A dispatching call
         --    There is nothing we can do in this case
         -- 2) an implicitely defined operation for which the
         --    implementation does not build an artificial declaration
         --    We have no way to access the declaration, but this declaration is
         --    at the same place as the type it operates on
         --    => use the type of the parameter to determine where the operation
         --       is declared

         -- Go up to the function call, but beware that the name of the
         -- function may be composite.
         E := Enclosing_Element (N);

         while Expression_Kind (E) = A_Selected_Component loop
            E := Enclosing_Element (E);
         end loop;

         if Asis.Statements.Is_Dispatching_Call (E) then
            -- There is no way of determining the location of the "root" declaration
            -- of a dispatching call
            -- *** THIS MAY CREATE FALSE DETECTION OF UNUSED PACKAGES ***
            Uncheckable (Rule_Id, False_Positive, Get_Location (E), "Dispatching call");
            return "";

         elsif Expression_Kind (E) = A_Function_Call then
            declare
               Parameters : constant Asis.Element_List := Function_Call_Parameters (E);
            begin
               E := A4G_Bugs.Corresponding_Expression_Type (Actual_Parameter (Parameters (1)));

               -- Annoying cases:
               -- A string litteral will return a nil element for E
               -- A universal value will return a declaration, however there is
               -- not much we can do with it. We recognize universal values by the
               -- fact that the declaration has no enclosing element. If someone knows
               -- a better way...
               if (Is_Nil (E) or else Is_Nil (Enclosing_Element (E)))
                 and Parameters'Length > 1
               then
                  E := A4G_Bugs.Corresponding_Expression_Type (Actual_Parameter (Parameters (2)));
               end if;

               if Is_Nil (E) or else Is_Nil (Enclosing_Element (E)) then
                  -- All operands universal or equivalent => give up
                  -- (anyway, it's a language defined operator, not
                  -- subject to use clauses)
                  return "";
               end if;

               -- Go to the full declaration if necessary (incomplete and private)
               if Declaration_Kind (E) in
                 An_Incomplete_Type_Declaration .. A_Private_Extension_Declaration
               then
                  E := Corresponding_Type_Declaration (E);
               end if;

               E := Corresponding_First_Subtype (E);
            end;
         elsif Element_Kind (E) = An_Association
           or Declaration_Kind (E) in A_Renaming_Declaration
           or Declaration_Kind (E) in A_Generic_Instantiation
         then
            -- This is an actual in an instantiation, or something similar where
            -- the function name appears, but it's not a call.
            -- There is nothing we can hook on, thus ignore.
            -- *** THIS MAY CREATE FALSE DETECTION OF UNUSED PACKAGES ***
            -- (but very unlikely).
            -- If you have a solution for this case, please mail to
            -- rosen@adalog.fr
            Uncheckable (Rule_Id,
                         False_Positive,
                         Get_Location (E),
                         "unable to determine origin of " & A4G_Bugs.Name_Image (N));
            return "";
         elsif Declaration_Kind (E) in
           A_Formal_Procedure_Declaration .. A_Formal_Package_Declaration_With_Box
         then
            -- These are not visible outside the generic, so we don't care
            return "";
         else
            Failure ("Enclosing_Package_Name, unexpected nil_element", N);
         end if;
      end if;

      if Is_Nil (Enclosing_Element (E)) then
         -- Knowing that N is a name, this happens only in the case of
         -- a compilation unit. It can be use-visible only if it is a
         -- child unit => take the parent as the enclosing unit
         C := Corresponding_Parent_Declaration (Enclosing_Compilation_Unit (E));
         if Is_Nil (C) then
            -- This happens only if the package is Standard itself. This one is
            -- certainly not use-visible...
            return "";
         end if;
         E := Unit_Declaration (C);
      else
         E := Enclosing_Element (E);
      end if;

      if Element_Kind (E) = A_Definition then
         -- This is an enumeration_literal, or an implicitely declared name
         -- for an inherited operation and thus appears as enclosed in the
         -- corresponding type definition.
         -- The type definition is enclosed in a type declaration, whose
         -- enclosing element is the scope we are interested in.
         E := Enclosing_Element (Enclosing_Element (E));
      end if;

      if Declaration_Kind (E) = A_Package_Declaration then
         return To_Upper (Full_Name_Image (Names (E) (1)));
      else
         return "";
      end if;
   end Enclosing_Package_Name;


   ----------------------
   -- Initialize_Table --
   ----------------------

   type Null_State is null record;

   procedure Pre_Procedure  (Element : in     Asis.Element;
                             Control : in out Asis.Traverse_Control;
                             State   : in out Null_State)
   is
      pragma Unreferenced (Control, State);
      use Asis, Asis.Declarations, Asis.Elements;
      use Element_Map, Utilities;
   begin
      if Element_Kind (Element) = A_Defining_Name then
         Add (Symbol_Map,
              To_Unbounded_Wide_String (To_Upper (Defining_Name_Image (Element))),
              Enclosing_Element (Element));
      end if;
   end Pre_Procedure;

   procedure Post_Procedure  (Element : in     Asis.Element;
                             Control : in out Asis.Traverse_Control;
                             State   : in out Null_State) is
      pragma Unreferenced (Element, Control, State);
   begin
      null;
   end Post_Procedure;

   procedure Initialize_Table is new Asis.Iterator.Traverse_Element (Null_State,
                                                                     Pre_Procedure,
                                                                     Post_Procedure);

   ------------------
   -- System_Value --
   ------------------

   function System_Value (Name : Wide_String) return Asis.Declaration is
      use Asis, Asis.Compilation_Units, Asis.Elements;
      use Element_Map, Utilities;
   begin
      if Is_Empty (Symbol_Map) then
         -- not initialized
         declare
            S     : Null_State;
            Contr : Asis.Traverse_Control := Continue;
         begin
            Initialize_Table (Unit_Declaration (Library_Unit_Declaration ("SYSTEM", Framework.Adactl_Context)),
                              Contr,
                              S);
            Balance (Symbol_Map);
         end;
      end if;

      return Fetch (Symbol_Map, To_Unbounded_Wide_String (Name));
   exception
      when Not_Present =>
         Failure ("Not found in System: " & Name);
   end System_Value;

end Framework.Queries;