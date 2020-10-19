----------------------------------------------------------------------
--  Framework.Symbol_Table - Package body                           --
--                                                                  --
--  This software is (c) Adalog 2004-2007.                          --
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

-- Ada
with
  Ada.Unchecked_Deallocation;

-- ASIS
with
  Asis.Declarations,
  Asis.Definitions,
  Asis.Elements;

-- Adalog
with
  Binary_Map,
  Scope_Manager,
  Thick_Queries;

-- AdaControl
with
  Framework.Queries,
  Utilities;

package body Framework.Symbol_Table is
   use Scope_Manager;

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
         Visibility_Scope  : Asis.Element;
         Declaration_Scope : Asis.Element;
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

      procedure Store (Element : Asis.Element; Content_Value : Content) is
         use Asis, Asis.Declarations, Asis.Definitions, Asis.Elements;
         use Framework.Queries, Thick_Queries, Utilities;

         Key    : constant Unbounded_Wide_String := To_Key_Upper (Element);
         Symbol : Symbol_Entry;
      begin
         if Is_Present (Global_Map, Key) then
            Symbol := Fetch (Global_Map, Key);
         else
            Symbol.Name := First_Defining_Name (Element);

            -- The element's declaration scope is the first enclosing element considered a scope
            -- starting from the enclosing element of its own declaration
            Symbol.Declaration_Scope := Enclosing_Element (Enclosing_Element (Symbol.Name));
            while not Is_Nil (Symbol.Declaration_Scope) and then not Is_Scope (Symbol.Declaration_Scope) loop
               Symbol.Declaration_Scope := Enclosing_Element (Symbol.Declaration_Scope);
            end loop;

            -- The Element's visibility scope is its declaration scope, unless the declaration scope
            -- is a package specification, in which case the visibility scope is the first enclosing
            -- non-package-spec scope (possibly Nil_Element if the symbol is from a library package spec).
            -- Similarly, (non generic) formal parameters have the visibility scope of their enclosing
            -- callable unit

            if Declaration_Kind (Enclosing_Element (Symbol.Name)) = A_Parameter_Specification then
               -- Parameters are visible outside their declaration
               -- Note that A_Parameter_Specification does not include generic formals,
               --      which is appropriate since those are not visible outside the generic
               -- Skip the Parameter_Specification and the enclosing subprogram or entry declaration
               Symbol.Visibility_Scope := Enclosing_Element (Symbol.Declaration_Scope);
            else
               Symbol.Visibility_Scope := Symbol.Declaration_Scope;
            end if;
            if Definition_Kind (Symbol.Visibility_Scope) = A_Protected_Definition then
               -- Normally, we don't find definitions, except in the weird case of protected types/objects
               Symbol.Visibility_Scope := Enclosing_Element (Symbol.Visibility_Scope);
            end if;

            loop
               case Element_Kind (Symbol.Visibility_Scope) is
                  when Not_An_Element =>
                     exit;  -- went out of library package spec
                  when A_Declaration =>
                     case Declaration_Kind (Symbol.Visibility_Scope) is
                        when A_Procedure_Declaration
                           | A_Null_Procedure_Declaration
                           | A_Function_Declaration
                           | An_Expression_Function_Declaration   -- Ada 2012
                           | An_Entry_Declaration
                           -- A_Package_Declaration     Not this one! (see below)
                           -- A_Task_Type_Declaration
                           -- A_Single_Task_Declaration
                           -- A_Protected_Type_Declaration   Elements of protected elements are visible outside
                           -- A_Single_Protected_Declaration like for packages

                           | A_Procedure_Body_Declaration
                           | A_Function_Body_Declaration
                           | An_Entry_Body_Declaration
                           | A_Package_Body_Declaration
                           | A_Task_Body_Declaration
                           | A_Protected_Body_Declaration

                           | A_Generic_Procedure_Declaration
                           | A_Generic_Function_Declaration
                           | A_Generic_Package_Declaration   -- Elements from a generic are not visible outside

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
                        when A_Package_Declaration =>
                           if Is_Part_Of (Symbol.Name, Private_Part_Declarative_Items (Symbol.Visibility_Scope)) then
                              -- Elements from the private part are not visible outside the package
                              if not Is_Nil (Corresponding_Body (Symbol.Visibility_Scope)) then
                                 Symbol.Visibility_Scope := Corresponding_Body (Symbol.Visibility_Scope);
                              end if;
                              exit;
                           end if;
                           Symbol.Visibility_Scope := Enclosing_Element (Symbol.Visibility_Scope);
                           if Declaration_Kind (Symbol.Visibility_Scope) = A_Package_Instantiation then
                              -- A package that was obtained by instantiation
                              Symbol.Visibility_Scope := Enclosing_Element (Symbol.Visibility_Scope);
                           end if;
                        when A_Task_Type_Declaration
                           | A_Protected_Type_Declaration
                             =>
                           if Is_Part_Of (Symbol.Name,
                                          Private_Part_Items (Type_Declaration_View (Symbol.Visibility_Scope)))
                           then
                              -- Elements from the private part are not visible outside the package
                              -- Tasks and protected always have a body
                              Symbol.Visibility_Scope := Corresponding_Body (Symbol.Visibility_Scope);
                              exit;
                           end if;
                           Symbol.Visibility_Scope := Enclosing_Element (Symbol.Visibility_Scope);
                        when A_Single_Task_Declaration
                           | A_Single_Protected_Declaration
                           =>
                           if Is_Part_Of (Symbol.Name,
                                          Private_Part_Items (Object_Declaration_View (Symbol.Visibility_Scope)))
                           then
                              -- Elements from the private part are not visible outside the package
                              -- Tasks and protected always have a body
                              Symbol.Visibility_Scope := Corresponding_Body (Symbol.Visibility_Scope);
                              exit;
                           end if;
                           Symbol.Visibility_Scope := Enclosing_Element (Symbol.Visibility_Scope);
                        when A_Formal_Package_Declaration
                           | A_Formal_Package_Declaration_With_Box
                             =>
                           -- Elements from a formal package are visible only in the enclosing generic
                           Symbol.Visibility_Scope := Enclosing_Element (Symbol.Visibility_Scope);
                           exit;
                        when others =>
                           Failure ("wrong enclosing scope 1", Symbol.Visibility_Scope);
                     end case;
                  when A_Statement            -- for loop, block
                     | An_Exception_Handler   -- Element is an exception occurrence
                       =>
                     exit;
                  when others =>
                     Failure ("wrong enclosing scope 2", Symbol.Visibility_Scope);
               end case;
            end loop;
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
         use Framework.Queries;
         Key : constant Unbounded_Wide_String := To_Key_Upper (Element);
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

      ------------
      -- Delete --
      ------------

      procedure Delete (Element : Asis.Element) is
         use Framework.Queries;
         Key    : constant Unbounded_Wide_String := To_Key_Upper (Element);
         Symbol : Symbol_Entry;
      begin
         if not Is_Present (Global_Map, Key) then
            raise Not_In_Table;
         end if;

         Symbol := Fetch (Global_Map, Key);
         if Symbol.Contents (My_Inx) = null then
            -- The symbol exists, but for someone else
            raise Not_In_Table;
         end if;

         Free (Symbol.Contents (My_Inx));
         if Symbol.Contents = Empty_Content then
            -- no more used
            Delete (Global_Map, Key);
         else
            Add (Global_Map, Key, Symbol); -- Update the map!
         end if;
      end Delete;

      ----------------
      -- Is_Present --
      ----------------

      function Is_Present (Element : Asis.Element) return Boolean is
         use Framework.Queries;
         Key  : constant Unbounded_Wide_String := To_Key_Upper (Element);
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
      -- Scope_Of --
      --------------

      function Scope_Of (Element : Asis.Element; Scope_Kind : Usage_Scope_Kinds := Declaration_Scope)
                         return Asis.Element
      is
         use Framework.Queries;

         Key : constant Unbounded_Wide_String := To_Key_Upper (Element);
         Data : Symbol_Entry;
      begin
         if not Is_Present (Global_Map, Key) then
            raise Not_In_Table;
         end if;

         Data := Fetch (Global_Map, Key);
         if Data.Contents (My_Inx) = null then
            -- The symbol exists, but for someone else
            raise Not_In_Table;
         end if;

         case Scope_Kind is
            when Declaration_Scope =>
               return Data.Declaration_Scope;
            when Visibility_Scope =>
               return Data.Visibility_Scope;
         end case;
      end Scope_Of;

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
         Scope : constant Asis.Element := Current_Scope;

         procedure On_One_Entity (Key : Unbounded_Wide_String; Symbol : in out Symbol_Entry) is
            pragma Unreferenced (Key);
            use Asis.Elements;
            Sym_Scope : Asis.Element;
         begin
            case Scope_Kind is
               when Declaration_Scope =>
                  Sym_Scope := Symbol.Declaration_Scope;
               when Visibility_Scope =>
                  Sym_Scope := Symbol.Visibility_Scope;
               when All_Scopes =>
                  null;
            end case;
            if (Scope_Kind = All_Scopes or else Is_Equal (Sym_Scope, Scope)) and then Symbol.Contents (My_Inx) /= null
            then
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

      Scope : constant Asis.Element := Current_Scope;

      procedure Clear_One_Entry (Key : Unbounded_Wide_String; Symbol : in out Symbol_Entry) is
         use Asis.Elements;
         pragma Unreferenced (Key);
      begin
         if Is_Equal (Symbol.Visibility_Scope, Scope) then
            for C : Content_Access of Symbol.Contents (1 .. Nb_Instantiated) loop
               Free (C);
            end loop;
            raise Symbols.Delete_Current;
         end if;
      end Clear_One_Entry;
      procedure Clear_All_Entries is new Symbols.Iterate (Clear_One_Entry);

   begin  -- Exit_Scope
      Clear_All_Entries (Global_Map);
   end Exit_Scope;

end Framework.Symbol_Table;
