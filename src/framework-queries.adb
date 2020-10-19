----------------------------------------------------------------------
--  Framework.Queries - Package body                                --
--                                                                  --
--  This software  is (c) The European Organisation  for the Safety --
--  of Air Navigation (EUROCONTROL) and Adalog 2004-2012.           --
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
  Utilities;

-- Adacontrol
with
  Framework.Reports,
  Framework.Locations;

package body Framework.Queries is

   package Element_Map is new Binary_Map (Unbounded_Wide_String, Asis.Element);

   Standard_Unit       : Asis.Compilation_Unit := Asis.Nil_Compilation_Unit;
   Standard_Symbol_Map : Element_Map.Map;
   System_Symbol_Map   : Element_Map.Map;

   ----------------------------
   -- Enclosing_Package_Name --
   ----------------------------

   function Enclosing_Package_Name (Rule_Id : Wide_String; N : in Asis.Name) return Wide_String is
      use Asis, Asis.Compilation_Units, Asis.Declarations, Asis.Elements, Asis.Expressions;
      use Framework.Locations, Framework.Reports, Thick_Queries, Utilities;

      E : Asis.Element := Corresponding_Name_Declaration (N);
      C : Asis.Compilation_Unit;

      subtype A_Comparison_Operator is Operator_Kinds range An_Equal_Operator .. A_Greater_Than_Or_Equal_Operator;
   begin
      if Is_Nil (E) then
         -- This should be:
         -- 1) A dispatching call
         --    There is nothing we can do in this case
         -- 2) an implicitely defined operation for which the
         --    implementation does not build an artificial declaration
         --    We have no way to access the declaration, but this declaration is
         --    at the same place as the type it operates on.
         --    => for an operator other than a comparison, this is the type returned by the operator
         --    => for a comparison (that returns Boolean), take it from one of the operands.
         --       if both operands are universal, there is no type in sight, return ""
         --    => for a function that is an attribute, take it from the prefix of the attribute

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
            case Expression_Kind (N) is
               when An_Attribute_Reference =>
                  E := Corresponding_Name_Declaration (Prefix (N));
               when An_Operator_Symbol =>
                  case Operator_Kind (N) is
                     when A_Comparison_Operator =>
                        declare
                           Parameters : constant Asis.Element_List := Function_Call_Parameters (E);
                        begin
                           E := A4G_Bugs.Corresponding_Expression_Type (Actual_Parameter (Parameters (1)));
                           if (Is_Nil (E) or else Type_Kind (Type_Declaration_View (E)) = A_Root_Type_Definition)
                               and Parameters'Length > 1
                           then
                              E := A4G_Bugs.Corresponding_Expression_Type (Actual_Parameter (Parameters (2)));
                           end if;

                           if Is_Nil (E) or else Type_Kind (Type_Declaration_View (E)) = A_Root_Type_Definition then
                              -- All operands universal or equivalent => give up
                              -- (anyway, it's a language defined operator, not
                              -- subject to use clauses)
                              return "";
                           end if;
                        end;
                     when others =>
                        E := A4G_Bugs.Corresponding_Expression_Type (E);
                        if Is_Nil (E) then -- Catenation of string literals for example => give up
                           return "";
                        end if;
                  end case;
                  -- Go to the full declaration if necessary (incomplete and private)
                  E := A4G_Bugs.Corresponding_First_Subtype (Corresponding_Full_Type_Declaration (E));
               when others =>
                  Failure ("Unexpected predefined operation", N);
            end case;
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
                         "unable to determine origin of " & Name_Image (N));
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

      -- Here, E is an element located in the same declarative part as N
      if Is_Nil (Enclosing_Element (E)) then
         -- Knowing that N is a name, this happens only in the case of
         -- a compilation unit => take the parent as the enclosing unit
         C := Corresponding_Parent_Declaration (Enclosing_Compilation_Unit (E));
         if Is_Nil (C) then
            -- This happens only if the package is Standard itself.
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
         return Full_Name_Image (Names (E) (1), With_Profile => True);
      else
         return "";
      end if;
   end Enclosing_Package_Name;


   ----------------------
   -- Initialize_Table --
   ----------------------

   procedure Pre_Procedure  (Element    : in     Asis.Element;
                             Control    : in out Asis.Traverse_Control;
                             Symbol_Map : in out Element_Map.Map)
   is
      pragma Unreferenced (Control);
      use Asis, Asis.Declarations, Asis.Elements;
      use Element_Map, Utilities;
   begin
      if Element_Kind (Element) = A_Defining_Name then
         declare
            Name : constant Wide_String := To_Upper (Defining_Name_Image (Element));
         begin
            Add (Symbol_Map, To_Unbounded_Wide_String (Name), Enclosing_Element (Element));
            if Name'Length >= 9 and then Name (Name'Last - 8 .. Name'Last) = "CHARACTER" then
               -- Don't traverse all the character litterals, skip the type definition
               Control := Abandon_Siblings;
            end if;
         end;
      end if;
   end Pre_Procedure;

   procedure Post_Procedure  (Element    : in     Asis.Element;
                              Control    : in out Asis.Traverse_Control;
                              Symbol_Map : in out Element_Map.Map)
   is
      pragma Unreferenced (Element, Control, Symbol_Map);
   begin
      null;
   end Post_Procedure;

   procedure Traverse_Unit is new Asis.Iterator.Traverse_Element (Element_Map.Map,
                                                                  Pre_Procedure,
                                                                  Post_Procedure);

   procedure Initialize_Table (Table : in out Element_Map.Map; Unit_Decl : Asis.Declaration) is
      use Asis;
      use Element_Map;

      Contr : Asis.Traverse_Control := Continue;
   begin
      Traverse_Unit (Unit_Decl, Contr, Table);
      Balance (Table);
   end Initialize_Table;

   -------------------
   -- Init_Standard --
   -------------------

   procedure Init_Standard (A_Unit : Asis.Compilation_Unit) is
      use Asis, Asis.Compilation_Units;
   begin
      if Unit_Kind (Standard_Unit) /= Not_A_Unit then
         -- already initialized
         return;
      end if;
      Standard_Unit := Corresponding_Parent_Declaration (A_Unit);
   end Init_Standard;

   --------------------
   -- Standard_Value --
   --------------------

   function Standard_Value (Name : Wide_String) return Asis.Declaration is
      use Asis.Elements;
      use Element_Map, Utilities;
   begin
      if Is_Empty (Standard_Symbol_Map) then
         -- not initialized
         Initialize_Table (Standard_Symbol_Map, Unit_Declaration (Standard_Unit));
      end if;

      return Fetch (Standard_Symbol_Map, To_Unbounded_Wide_String (Name));
   exception
      when Not_Present =>
         Failure ("Not found in Standard: " & Name);
   end Standard_Value;

   ------------------
   -- System_Value --
   ------------------

   function System_Value (Name : Wide_String) return Asis.Declaration is
      use Asis.Compilation_Units, Asis.Elements;
      use Element_Map, Utilities;
      System_Unit : Asis.Compilation_Unit;
   begin
      if Is_Empty (System_Symbol_Map) then
         System_Unit := Library_Unit_Declaration ("SYSTEM", Adactl_Context);
         if Is_Nil (System_Unit) then
            Failure ("Unable to access System for " & Name);
         end if;
         Initialize_Table (System_Symbol_Map, Unit_Declaration (System_Unit));
      end if;

      return Fetch (System_Symbol_Map, To_Unbounded_Wide_String (Name));
   exception
      when Not_Present =>
         Failure ("Not found in System: " & Name);
   end System_Value;

   ------------------
   -- System_Value --
   ------------------

   function System_Value (Name : Wide_String) return Thick_Queries.Extended_Biggest_Int is
      use Asis.Declarations;
      use Element_Map, Thick_Queries, Utilities;
   begin
      return Discrete_Static_Expression_Value (Initialization_Expression (System_Value (Name)));
   exception
      when Not_Present =>
         -- There seems to be a bug/feature in recent versions of Gnat that the System we get
         -- get is not the real one, and that a number of elements are not found by traversing it,
         -- and especially Storage_Unit. Since it is highly unlikely to be anything but 8, we force it
         -- to 8 if not found
         if Name = "STORAGE_UNIT" then
            return 8;
         end if;
         Failure ("Not found in System: " & Name);
   end System_Value;

end Framework.Queries;
