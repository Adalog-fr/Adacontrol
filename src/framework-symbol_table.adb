----------------------------------------------------------------------
--  Framework.Symbol_Table - Package body                           --
--                                                                  --
--  This software  is (c) Adalog  2004-2007. The Ada  Controller is --
--  free software;  you can redistribute it and/or  modify it under --
--  terms of  the GNU  General Public License  as published  by the --
--  Free Software Foundation; either version 2, or (at your option) --
--  any later version.   This unit is distributed in  the hope that --
--  it will be  useful, but WITHOUT ANY WARRANTY;  without even the --
--  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR --
--  PURPOSE.  See the GNU  General Public License for more details. --
--  You  should have  received a  copy  of the  GNU General  Public --
--  License distributed  with this  program; see file  COPYING.  If --
--  not, write to  the Free Software Foundation, 59  Temple Place - --
--  Suite 330, Boston, MA 02111-1307, USA.                          --
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
  Ada.Unchecked_Deallocation;

-- ASIS
with
  Asis.Declarations,
  Asis.Elements,
  Asis.Expressions;

-- Adalog
with
  Binary_Map,
  Thick_Queries;

-- AdaControl
with
  Utilities;

package body Framework.Symbol_Table is
   use Framework.Scope_Manager;

   -- Algorithm:
   --
   -- The symbol table is basically a mapping from the full name image of an entity
   -- to the value (provided at instantiation).
   --
   -- Since several rules may need the symbol table, the actual value of the map is
   -- an array of pointers to the actual content for each rule. Each instantiation
   -- gets an index, and uses the corresponding slot in the array.

   type Content_Count is range 0 .. Max_Instances;
   subtype Content_Inx is Content_Count range 1 .. Content_Count'Last;
   Nb_Instantiated : Content_Count := 0;

   type Content_Access is access Root_Content'Class;
   procedure Free is new Ada.Unchecked_Deallocation (Root_Content'Class, Content_Access);

   type Content_Tab is array (Content_Inx) of Content_Access;
   Empty_Content : constant Content_Tab := (others => null);
   type Symbol_Entry is
      record
         Name              : Asis.Defining_Name;
         Depths_OK         : Boolean;
         Visibility_Depth  : Scope_Range;
         Declaration_Depth : Scope_Range;
         Contents          : Content_Tab;
      end record;

   package Symbols is new Binary_Map (Unbounded_Wide_String, Symbol_Entry);
   use Symbols;

   Global_Map : Symbols.Map;

   -----------------
   -- Clear_Entry --
   -----------------

   procedure Clear_Entry  (Entry_Inx : Content_Inx) is
      procedure Clear_One_Entry (Key : Unbounded_Wide_String; Symbol : in out Symbol_Entry) is
         pragma Unreferenced (Key);
      begin
         Free (Symbol.Contents (Entry_Inx));
         if Symbol.Contents = Empty_Content then
            -- no more used
            raise Symbols.Delete_Current;
         end if;
      end Clear_One_Entry;

      procedure Clear_All_Entries is new Symbols.Iterate (Clear_One_Entry);

   begin  -- Clear_Entry
      Clear_All_Entries (Global_Map);
   end Clear_Entry;

   -----------------
   -- Data_Access --
   -----------------

   package body Data_Access is
      My_Inx : Content_Inx;

      -----------
      -- Store --
      -----------

      procedure Store (Element : Asis.Element; Content_Value : Content; At_Declaration_Scope : Boolean := True) is
         -- Pre-condition: If At_Declaration_Scope is true, Element is a Defining_Name
         use Asis, Asis.Declarations, Asis.Elements, Asis.Expressions;
         use Thick_Queries, Utilities;

         Key : constant Unbounded_Wide_String := To_Unbounded_Wide_String (To_Upper
                                                                           (Full_Name_Image
                                                                            (Element, With_Profile => True)));
         Symbol : Symbol_Entry;
         Decl   : Asis.Declaration;
      begin
         if Is_Present (Global_Map, Key) then
            Symbol := Fetch (Global_Map, Key);
         else
            Symbol.Depths_OK := False;
            case Element_Kind (Element) is
               when A_Defining_Name =>
                  Symbol.Name := Element;
               when An_Expression =>
                  case Expression_Kind (Element) is
                     when An_Identifier
                        | An_Operator_Symbol
                        | An_Enumeration_Literal
                          =>
                        Symbol.Name := Corresponding_Name_Definition (Element);
                     when others =>
                        Failure ("Symbol table: not a name (1)", Element);
                  end case;
               when others =>
                  Failure ("Symbol table: not a name (2)", Element);
            end case;
         end if;

         if not Symbol.Depths_OK and At_Declaration_Scope then
            Symbol.Depths_OK := True;
            Decl := Enclosing_Element (Symbol.Name);

            -- The symbol's declaration declaration depth is the depth of its declaration, unless it
            -- is the defining name of a (generic) subprogram, package, task, or protected, in which
            -- case it is one less (since we are already within the scope of its own declaration).
            -- We do not have to care about loop and block names, since they cannot be used outside the
            -- statement.
            case Element_Kind (Decl) is
               when A_Declaration =>
                  case Declaration_Kind (Decl) is
                     when A_Procedure_Declaration
                        | A_Function_Declaration
                        | An_Entry_Declaration
                        | A_Package_Declaration
                        | A_Task_Type_Declaration
                        | A_Single_Task_Declaration
                        | A_Protected_Type_Declaration
                        | A_Single_Protected_Declaration

                        | A_Procedure_Body_Declaration
                        | A_Function_Body_Declaration
                        | An_Entry_Body_Declaration
                        | A_Package_Body_Declaration
                        | A_Task_Body_Declaration
                        | A_Protected_Body_Declaration

                        | A_Generic_Procedure_Declaration
                        | A_Generic_Function_Declaration
                        | A_Generic_Package_Declaration

                        | A_Formal_Procedure_Declaration
                        | A_Formal_Function_Declaration

                        | A_Procedure_Renaming_Declaration
                        | A_Function_Renaming_Declaration
                        | A_Package_Renaming_Declaration
                        | A_Generic_Package_Renaming_Declaration
                        | A_Generic_Procedure_Renaming_Declaration
                        | A_Generic_Function_Renaming_Declaration

                        | A_Procedure_Instantiation
                        | A_Function_Instantiation
                        | A_Package_Instantiation
                          =>
                        if Is_Equal (Symbol.Name, Names (Decl)(1)) then
                           Symbol.Declaration_Depth := Current_Depth - 1;
                           -- The search for the visibility depth must also start one level higher:
                           Decl := Enclosing_Element (Decl);
                        else
                           Symbol.Declaration_Depth := Current_Depth;
                        end if;
                     when others =>
                        Symbol.Declaration_Depth := Current_Depth;
                  end case;
               when others =>
                  Symbol.Declaration_Depth := Current_Depth;
            end case;

            -- The Symbol's visibility depth is the depth of its declaration, unless the declaration
            -- is in a package specification, in which case the depth is the first enclosing
            -- non-package-spec scope (possibly 0 if the symbol is from a library package spec).
            -- Similarly, (non generic) formal parameters have the visibility of their enclosing
            -- callable unit

            -- Find the enclosing scope of Decl (in the sense of the scope manager)
            -- except that we skip package specifications
            loop
               case Element_Kind (Decl) is
                  when A_Declaration =>
                     case Declaration_Kind (Decl) is
                        when A_Procedure_Declaration
                           | A_Function_Declaration
                           | An_Entry_Declaration
                           -- A_Package_Declaration     Not this one! (see below)
                           | A_Task_Type_Declaration
                           | A_Single_Task_Declaration
                           | A_Protected_Type_Declaration
                           | A_Single_Protected_Declaration

                           | A_Procedure_Body_Declaration
                           | A_Function_Body_Declaration
                           | An_Entry_Body_Declaration
                           | A_Package_Body_Declaration
                           | A_Task_Body_Declaration
                           | A_Protected_Body_Declaration

                           | A_Generic_Procedure_Declaration
                           | A_Generic_Function_Declaration
                           -- A_Generic_Package_Declaration    Same thing here

                           | A_Formal_Procedure_Declaration
                           | A_Formal_Function_Declaration

                           | A_Procedure_Renaming_Declaration
                           | A_Function_Renaming_Declaration
                           | A_Package_Renaming_Declaration
                           | A_Generic_Package_Renaming_Declaration
                           | A_Generic_Procedure_Renaming_Declaration
                           | A_Generic_Function_Renaming_Declaration

                           | A_Procedure_Instantiation
                           | A_Function_Instantiation
                           | A_Package_Instantiation
                             =>
                           exit;
                        when A_Package_Declaration
                           | A_Generic_Package_Declaration
                             =>
                           Decl := Enclosing_Element (Decl);
                           exit when Is_Nil (Decl);  -- went out of library package spec
                           if Declaration_Kind (Decl) = A_Package_Instantiation then
                              -- The package was obtained by instantiation
                              Decl := Enclosing_Element (Decl);
                           end if;
                        when A_Parameter_Specification =>
                           -- Parameters are visible outside their declaration
                           -- Note that A_Parameter_Specification does not include generic formals,
                           --      which is appropriate since those are not visible outside the generic
                           -- Skip the Parameter_Specification and the enclosing subprogram or entry declaration
                           Decl := Enclosing_Element (Enclosing_Element (Decl));
                        when others =>
                           Decl := Enclosing_Element (Decl);
                     end case;
                  when A_Statement
                     | An_Exception_Handler
                       =>
                     exit;
                  when others =>
                     Decl := Enclosing_Element (Decl);
               end case;
            end loop;

            if Is_Nil (Decl) then
               Symbol.Visibility_Depth := 0;
            else
               declare
                  Scopes : constant Scope_List := Active_Scopes;
                  Found  : Boolean := False;
               begin
                  for I in reverse Scopes'Range loop
                     if Is_Equal (Decl, Scopes (I)) then
                        Symbol.Visibility_Depth := I;
                        Found := True;
                        exit;
                     end if;
                  end loop;
                  Assert (Found, "Symbol table: enclosing scope not found in table", Decl);
               end;
            end if;
         end if;

         if Symbol.Contents (My_Inx) = null then
            Symbol.Contents (My_Inx) := new Content_Hook;
         end if;
         Content_Hook(Symbol.Contents (My_Inx).all).The_Content := Content_Value;
         Add (Global_Map, Key, Symbol);
      end Store;

      -----------
      -- Fetch --
      -----------

      function Fetch (Element : Asis.Element) return Content is
         use Thick_Queries, Utilities;
         Key : constant Unbounded_Wide_String
           := To_Unbounded_Wide_String (To_Upper(Full_Name_Image (Element, With_Profile => True)));
         Ptr : Content_Access;
      begin
         if not Is_Present (Global_Map, Key) then
            raise Not_In_Table;
         end if;

         Ptr := Fetch (Global_Map, Key).Contents (My_Inx);
         if Ptr = null then
            -- The symbol exists, but for someone else
            raise Not_In_Table;
         end if;

         return Content_Hook (Ptr.all).The_Content;
      end Fetch;

      -----------
      -- Fetch --
      -----------

      function Fetch (Element : Asis.Element; Default : Content) return Content is
      begin
         return Fetch (Element);
      exception
         when Not_In_Table =>
            return Default;
      end Fetch;

      ----------------
      -- Is_Present --
      ----------------

      function Is_Present (Element : Asis.Element) return Boolean is
         use Thick_Queries, Utilities;
         Key : constant Unbounded_Wide_String := To_Unbounded_Wide_String (To_Upper
                                                                           (Full_Name_Image
                                                                            (Element, With_Profile => True)));
         Data : Symbol_Entry;
      begin
         if not Is_Present (Global_Map, Key) then
            return False;
         end if;

         Data := Fetch (Global_Map, Key);
         if Data.Contents (My_Inx) = null then
            -- The symbol exists, but for someone else
            return False;
         end if;

         return True;
      end Is_Present;

      --------------
      -- Depth_Of --
      --------------

      function Depth_Of (Element : Asis.Element) return Framework.Scope_Manager.Scope_Range is
         use Thick_Queries, Utilities;

         Key : constant Unbounded_Wide_String := To_Unbounded_Wide_String (To_Upper
                                                                           (Full_Name_Image
                                                                            (Element, With_Profile => True)));
         Data : Symbol_Entry;
      begin
         if not Is_Present (Global_Map, Key) then
            raise Not_In_Table;
         end if;

         Data := Fetch (Global_Map, Key);
         if Data.Contents (My_Inx) = null then
            -- The symbol exists, but for someone else
            Failure ("Symbol table: depth_of of missing element 2");
         end if;
         if not Data.Depths_OK then
            Failure ("Symbol table: depth_of of non Depths_OK element");
         end if;

         return Data.Declaration_Depth;
      end Depth_Of;

      -----------
      -- Clear --
      -----------

      procedure Clear is
      begin
         Clear_Entry (My_Inx);
      end Clear;

      --------------------------------
      -- On_Every_Entity_From_Scope --
      --------------------------------

      procedure On_Every_Entity_From_Scope (Scope_Kind : Scope_Kinds) is
         Depth : constant Scope_Range := Current_Depth;

         procedure On_One_Entity (Key : Unbounded_Wide_String; Symbol : in out Symbol_Entry) is
            pragma Unreferenced (Key);
            Sym_Depth : Scope_Range;
         begin
            if not Symbol.Depths_OK then
               -- If it were from the good scope, its declaration would have been processed...
               return;
            end if;

            case Scope_Kind is
               when Declaration =>
                  Sym_Depth := Symbol.Declaration_Depth;
               when Visibility =>
                  Sym_Depth := Symbol.Visibility_Depth;
            end case;
            if Sym_Depth = Depth and then Symbol.Contents (My_Inx) /= null then
               begin
                  Action (Symbol.Name, Content_Hook (Symbol.Contents (My_Inx).all).The_Content);
               exception
                  when Delete_Current =>
                     Free (Symbol.Contents (My_Inx));
                     if Symbol.Contents = Empty_Content then
                        -- no more used
                        raise Symbols.Delete_Current;
                     end if;
               end;
            end if;
         end On_One_Entity;

         procedure On_Every_Entity is new Symbols.Iterate (On_One_Entity);

      begin  -- On_Every_Entity_From_Scope
         On_Every_Entity (Global_Map);
      end On_Every_Entity_From_Scope;

      use Utilities;
   begin -- Data_Access
      if Nb_Instantiated = Content_Inx'Last then
         Failure ("Too many instantiations of Framework.Symbol_Table.Data_Access");
      end if;
      Nb_Instantiated := Nb_Instantiated + 1;
      My_Inx          := Nb_Instantiated;
   end Data_Access;

   ----------------
   -- Exit_Scope --
   ----------------

   procedure Exit_Scope (Element : in Asis.Element) is
      pragma Unreferenced (Element);

      Depth : constant Scope_Range := Current_Depth;

      procedure Clear_One_Entry (Key : Unbounded_Wide_String; Symbol : in out Symbol_Entry) is
         pragma Unreferenced (Key);
      begin
         if Symbol.Depths_OK and then Symbol.Visibility_Depth = Depth then
            for I in Content_Inx range 1 .. Nb_Instantiated loop
               Free (Symbol.Contents (I));
            end loop;
            raise Symbols.Delete_Current;
         end if;
      end Clear_One_Entry;
      procedure Clear_All_Entries is new Symbols.Iterate (Clear_One_Entry);

   begin  -- Exit_Scope
      Clear_All_Entries (Global_Map);
   end Exit_Scope;

end Framework.Symbol_Table;
