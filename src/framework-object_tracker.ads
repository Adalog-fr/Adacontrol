----------------------------------------------------------------------
--  Framework.Object_Tracker - Package specification                --
--                                                                  --
--  This software is (c) Adalog 2020.                               --
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
   Asis;

-- Adalog
with
   Thick_Queries;

package Framework.Object_Tracker is

   -- This package keeps track of the static values assigned to local unshared variables of an elementary type
   -- A local variable is a variable which is not declared within the declarative part of a (generic) package
   -- specification or body.
   --    - An elementary type is a type which is neither an array nor a record. This should include real types,
   --      but they are not currently tracked. Maybe later... Access types are tracked, with a value of 0 if
   --      known to be null, a value of 1 if known to be not null, and a value of Not_Static (like any other type)
   --      if unknown.
   --    - A variable is unshared if it is not visible from any nested subprogram or task and it is not declared
   --      as volatile.
   -- For each such variable, the module keeps track at any point within the scope of the variable if its content is
   -- known to be equal to, greater than, less than, or different from a certain value, or unknown.


   type Content_Kinds  is (Enumerated, Integer, Modular, Pointer, Untracked);
   subtype Discrete_Content_Kinds is Content_Kinds range Enumerated .. Modular;
   type Object_Value_Set (Kind : Content_Kinds := Content_Kinds'First) is
      record
         case Kind is
            when Untracked =>
               null;
            when others =>
               Imin, Imax : Thick_Queries.Extended_Biggest_Int;
         end case;
      end record;

   Unknown_Value : constant array (Content_Kinds) of Object_Value_Set :=
                     ((Kind => Enumerated, Imin | Imax => Thick_Queries.Not_Static),
                      (Kind => Integer,    Imin | Imax => Thick_Queries.Not_Static),
                      (Kind => Modular,    Imin | Imax => Thick_Queries.Not_Static),
                      (Kind => Pointer,    Imin | Imax => Thick_Queries.Not_Static),
                      (Kind => Untracked));
   Null_Value : constant Object_Value_Set := (Kind => Pointer, Imin | Imax => 0);

   function Expression_Value (Expr : Asis.Expression; RM_Static : Boolean := False) return Object_Value_Set;
   -- Evaluates Expr, and return the value in Imin and Imax
   -- Kind is set according to Expr

   function Object_Value (Var : Asis.Element; Discr : Asis.Name := Asis.Nil_Element; From_Expansion : Boolean := False)
                          return Object_Value_Set;
   -- Provides the range of possible values of Var at the current place, or Unknown.
   -- If Descr is not Nil_Element, returns the value of the corresponding discriminant of Var
   -- Alternatively, Var may be a selected component whose selector is a discriminant
   --
   -- Appropriate Element_Kinds:
   --   An_Expression
   --   A_Declaration
   --   A_Definition
   --   A_Defining_Name

   function Object_Value_Image (Var            : Asis.Element;
                                Wanted         : Thick_Queries.Expression_Info;
                                From_Expansion : Boolean := False)
                                return Wide_String;
   -- If Variable_Value (Var) is a single known value, returns the image of the value
   -- Otherwise, returns ""
   --
   -- Appropriate Element_Kinds:
   --   like Variable_Value


   ----------------------------------------------------------------------------------------
   --
   --  Declarations below this line are for the use of the framework
   --

   procedure Process_Type_Declaration (Decl : in Asis.Declaration);

   procedure Process_Object_Declaration (Decl : in Asis.Declaration);
   -- To be called for each object (constant, variable, loop parameter, or formal parameter) declaration

   procedure Process_Attribute (Attr_Ref : Asis.Expression);

   procedure Process_Instantiation (Inst : Asis.Declaration);

   procedure      Process_Outer_Statement (Stmt : in Asis.Statement);
   procedure      Process_Statement       (Stmt : in Asis.Statement);
   procedure Post_Process_Statement       (Stmt : in Asis.Statement);

   procedure Process_Path      (Path : in Asis.Path);
   procedure Post_Process_Path (Path : in Asis.Path);

   procedure Process_Function_Call (Call : in Asis.Expression);

   procedure Process_Handler (Handler : Asis.Exception_Handler);
end Framework.Object_Tracker;
