----------------------------------------------------------------------
--  Framework.Control_Manager - Package body                        --
--                                                                  --
--  This software  is (c) The European Organisation  for the Safety --
--  of Air Navigation (EUROCONTROL) and Adalog 2004-2008.           --
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
  Asis.Elements,
  Asis.Expressions,
  Asis.Statements;

-- Adalog
with
  Thick_Queries,
  Utilities;

package body Framework.Control_Manager is
   procedure Free is new Ada.Unchecked_Deallocation (Root_Context'Class, Context_Access);

   procedure Release (List : in out Context_Node_Access) is
      procedure Free is new Ada.Unchecked_Deallocation (Context_Node, Context_Node_Access);

      Next_Node : Context_Node_Access;
   begin
      while List /= null loop
         Next_Node := List.Next;
         Clear (List.Value.all);
         Free (List.Value);
         Free (List);
         List := Next_Node;
      end loop;
   end Release;

   -------------------
   -- Basic_Context --
   -------------------

   package body Basic is
      function New_Context (With_Type : in Control_Kinds; With_Label : in Wide_String) return Basic_Rule_Context is
      begin
         return (Ctl_Kind    => With_Type,
                 Ctl_Label   => To_Unbounded_Wide_String (With_Label),
                 With_Count  => False,
                 Count_Label => Null_Unbounded_Wide_String);
      end New_Context;

      function Merge_Context (Context    : in out Basic_Rule_Context;
                              With_Type  : in     Control_Kinds;
                              With_Label : in     Wide_String) return Boolean
      is
      begin
         if (With_Type = Count) = (Context.Ctl_Kind = Count) then
            -- Both or neither are Count
            return False;
         end if;

         if Context.Ctl_Kind = Count then
            Context := (Ctl_Kind    => With_Type,
                        Ctl_Label   => To_Unbounded_Wide_String (With_Label),
                        With_Count  => True,
                        Count_Label => Context.Ctl_Label);
         else
            Context := (Ctl_Kind    => Context.Ctl_Kind,
                        Ctl_Label   => Context.Ctl_Label,
                        With_Count  => True,
                        Count_Label => To_Unbounded_Wide_String (With_Label));
         end if;
         return True;
      end Merge_Context;

   end Basic;

   -------------
   -- Balance --
   -------------

   procedure Balance (Store : in out Context_Store) is
      use Context_Tree;
   begin
      Balance (Store.Simple_Names);
      Balance (Store.Qualified_Names);
   end Balance;

   -----------
   -- Clear --
   -----------

   procedure Clear   (Store : in out Context_Store) is
      procedure Clear is new Context_Tree.Generic_Clear_And_Release (Release);
   begin
      Clear (Store.Simple_Names);
      Clear (Store.Qualified_Names);
      Store.Last_Returned := null;
      Store.Last_Name     := Null_Unbounded_Wide_String;
   end Clear;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Store : in Context_Store) return Boolean is
      use Context_Tree;
   begin
      return Is_Empty (Store.Simple_Names) and Is_Empty (Store.Qualified_Names);
   end Is_Empty;

   -------------------
   -- Query_Context --
   -------------------

   procedure Query_Context (Into : in Context_Store; Name : in Asis.Element) is
      -- Note: Into must be in (not in out), because it is called from a function, on the function's parameter
      use Context_Tree, Utilities, Thick_Queries;
      use Asis, Asis.Elements, Asis.Expressions, Asis.Statements;

      Good_Name        : Asis.Element := Simple_Name (Name);
      Name_Enclosing   : Asis.Element;
      Name_Key         : Unbounded_Wide_String;
      Name_Extra       : Unbounded_Wide_String; -- Initialized to Null_Unbounded_Wide_String
      Result           : Context_Node_Access := null;
      Is_Predefined_Op : Boolean := False;
   begin  -- Query_Context
      if Is_Nil (Name) then
         Result := null;

      -- Special case for pragmas: they have no "name" in the Asis sense.
      elsif Element_Kind (Name) = A_Pragma then
         -- No overloading, no "all"
         Name_Key := To_Unbounded_Wide_String (To_Upper (Pragma_Name_Image (Good_Name)));
         Result   := Fetch (Into.Qualified_Names, Name_Key, Default_Value => null);

      else
         -- Find the place where the name is used (skipping all selections)
         Name_Enclosing := Enclosing_Element (Name);
         while Expression_Kind (Name_Enclosing) = A_Selected_Component loop
            Name_Enclosing := Enclosing_Element (Name_Enclosing);
         end loop;

         -- Only "all" (without overloading) is acceptable for dispatching calls
         if Is_Dispatching_Call (Name_Enclosing)
           and then Is_Equal (Good_Name, Called_Simple_Name (Name_Enclosing))
         then
            Name_Key := To_Unbounded_Wide_String (To_Upper (Name_Image (Good_Name)));
            Result   := Fetch (Into.Simple_Names,
                               Name_Key,
                               Default_Value => null);
         -- Normal case
         else
            if Element_Kind (Good_Name) = An_Expression then
               -- Build Good_Name for attributes
               if Expression_Kind (Good_Name) = An_Attribute_Reference then
                  while Expression_Kind (Good_Name) = An_Attribute_Reference loop
                     Name_Extra := ''' & To_Upper (Attribute_Name_Image (Good_Name)) & Name_Extra;
                     Good_Name  := Prefix (Good_Name);
                  end loop;
                  Good_Name := Simple_Name (Good_Name);
               end if;

               -- Check if the name is a predefined operator
               case Expression_Kind (Good_Name) is
                  when An_Identifier | An_Enumeration_Literal =>
                     null;
                  when An_Operator_Symbol =>
                     declare
                        Op_Decl : constant Asis.Element := Corresponding_Name_Declaration (Good_Name);
                     begin
                        if Is_Nil (Op_Decl) then
                           Is_Predefined_Op := True;
                        else
                           -- This should be a function !
                           case Declaration_Kind (Op_Decl) is
                              when A_Function_Declaration
                                 | An_Expression_Function_Declaration   -- Ada 2012
                                 | A_Function_Body_Declaration
                                 | A_Function_Renaming_Declaration
                                 | A_Generic_Function_Renaming_Declaration
                                 | A_Function_Body_Stub
                                 | A_Generic_Function_Declaration
                                 | A_Function_Instantiation
                                 | A_Formal_Function_Declaration
                                   =>
                                 Is_Predefined_Op := False;
                              when others =>
                                 -- Safety due to previous (now extinct) A4G_Bug
                                 Failure ("Matching_Context: Bad declaration of predefined operator", Op_Decl);
                           end case;
                        end if;
                     end;
                  when others =>
                     -- This can happen if we were given something that is not appropriate,
                     -- or for something dynamic used as the prefix of an attribute.
                     Into.This.Self.Last_Returned := null;
                     Into.This.Self.Last_Name     := Name_Key;
                     return;
               end case;
            end if;

            -- Search from most specific to least specific
            -- Predefined operators cannot be searched with profile

            -- Search without "all", with overloading
            if not Is_Predefined_Op and Result = null then
               Name_Key := To_Unbounded_Wide_String (To_Upper (Full_Name_Image (Good_Name, With_Profile => True)))
                           & Name_Extra;
               Result := Fetch (Into.Qualified_Names,
                                Name_Key,
                                Default_Value => null);
            end if;

            -- Search without "all", without overloading
            if Result = null then
               Name_Key := To_Unbounded_Wide_String (To_Upper (Full_Name_Image (Good_Name, With_Profile => False)))
                           & Name_Extra;
               Result := Fetch (Into.Qualified_Names,
                                Name_Key,
                                Default_Value => null);
            end if;

            -- Search with "all", with overloading
            if not Is_Predefined_Op and Result = null then
               Name_Key := To_Unbounded_Wide_String (To_Upper
                                                     (Extended_Name_Image (Good_Name)
                                                      & Profile_Image (Good_Name, With_Profile => True)))
                           & Name_Extra;
               Result := Fetch (Into.Simple_Names,
                                Name_Key,
                                Default_Value => null);
            end if;

            -- Search with "all", without overloading
            if Result = null then
               Name_Key := To_Unbounded_Wide_String (To_Upper (Extended_Name_Image (Good_Name)))
                           & Name_Extra;
               Result := Fetch (Into.Simple_Names,
                                Name_Key,
                                Default_Value => null);
            end if;

            -- For attribute references, search attribute name (or 'all)
            if Result = null
              and then Expression_Kind (Name) = An_Attribute_Reference
            then
               -- Name'all
               Name_Key := To_Unbounded_Wide_String (To_Upper (Full_Name_Image (Good_Name, With_Profile => False)))
                             & "'ALL";  -- No "extras" here
               Result := Fetch (Into.Qualified_Names,
                                Name_Key,
                                Default_Value => null);

               -- Type'Attr
               if Result = null
                 and then Declaration_Kind (Corresponding_Name_Declaration (Good_Name))
                          in An_Ordinary_Type_Declaration .. A_Subtype_Declaration -- =All type and subtype declarations
               then
                  Name_Key := To_Unbounded_Wide_String ("TYPE'" & To_Upper (Attribute_Name_Image (Name)));
                  Result   := Fetch (Into.Simple_Names, Name_Key, Default_Value => null);
                  -- Type'all
                  if Result = null then
                     Name_Key := To_Unbounded_Wide_String ("TYPE'ALL");
                     Result := Fetch (Into.Simple_Names,
                                      Name_Key,
                                      Default_Value => null);
                  end if;
               end if;

               -- 'Attr
               if Result = null then
                  Name_Key := To_Unbounded_Wide_String (''' & To_Upper (Attribute_Name_Image (Name)));
                  Result := Fetch (Into.Simple_Names,
                                   Name_Key,
                                   Default_Value => null);
               end if;

               -- 'all
               if Result = null then
                  Name_Key := To_Unbounded_Wide_String ("'ALL");
                  Result := Fetch (Into.Simple_Names,
                                   Name_Key,
                                   Default_Value => null);
               end if;
            end if;

         end if;
      end if;

      if Result = null then
         Into.This.Self.Last_Kind := None;
      else
         Into.This.Self.Last_Kind := Original;
      end if;
      Into.This.Self.Last_Returned := Result;
      Into.This.Self.Last_Name     := Name_Key;
   end Query_Context;

   ---------------
   -- Associate --
   ---------------

   procedure Associate (Into          : in out Context_Store;
                        Specification : in     Entity_Specification;
                        Context       : in     Root_Context'Class;
                        Additive      : in     Boolean := False)
   is
      use Utilities;

      procedure Add_To_Map (Names : in out Context_Tree.Map) is
         use Context_Tree;
         Node        : Context_Node_Access := null;
         Current     : Context_Node_Access := null;
         Count_Label : Unbounded_Wide_String;
      begin
         if Is_Present (Names, Specification.Specification) then
            if Additive then
               Node := Fetch (Names, Specification.Specification);

               -- Check value not already in the list
               Current := Node;
               while Current /= null loop
                  if Equivalent_Values (Current.Value.all, Context) then
                     exit;
                  end if;
                  Current := Current.Next;
               end loop;
            else
               Current := Fetch (Names, Specification.Specification);
            end if;
         end if;

         -- Here, Current designates the context if already in Names,
         -- is null otherwise
         if Current = null then
            Current := new Context_Node'(new Root_Context'Class'(Context), Node);
         else
            -- Check for the special case to allow Count in addition to another rule type
            if Context not in Basic_Rule_Context'Class or Current.Value.all not in Basic_Rule_Context'Class then
               raise Already_In_Store;
            end if;

            if (Basic_Rule_Context (Context).Ctl_Kind = Count)
              = (Basic_Rule_Context (Current.Value.all).Ctl_Kind = Count)
            then
               -- Both are Count, or neither
               raise Already_In_Store;
            end if;

            -- One and only one is a Count
            if Basic_Rule_Context (Current.Value.all).Ctl_Kind = Count then
               Count_Label :=  Basic_Rule_Context (Current.Value.all).Ctl_Label;
               Free (Current.Value);
               Current.Value := new Root_Context'Class'(Context);
            else
               Count_Label := Basic_Rule_Context (Context).Ctl_Label;
            end if;
            Basic_Rule_Context (Current.Value.all).With_Count  := True;
            Basic_Rule_Context (Current.Value.all).Count_Label := Count_Label;
         end if;

         Add (Names, Specification.Specification, Current);
      end Add_To_Map;

   begin  -- Associate
      case Specification.Kind is
         when Regular_Id =>
            Add_To_Map (Into.Qualified_Names);
         when All_Id =>
            Add_To_Map (Into.Simple_Names);
         when others =>
            Failure ("Associate : bad kind " & Entity_Specification_Kinds'Wide_Image (Specification.Kind));
      end case;
   end Associate;

   ----------------------
   -- Matching_Context --
   ----------------------

   function Matching_Context (Into      : in Context_Store;
                              Name      : in Asis.Element;
                              Extend_To : Extension_Set := No_Extension) return Root_Context'Class
   is
      use Asis, Asis.Declarations, Asis.Elements, Asis.Expressions, Thick_Queries;
      Good_Name        : constant Asis.Element := Simple_Name (Name);
      Name_Declaration : Asis.Declaration;
      Name_Definition  : Asis.Definition;
   begin
      if Is_Nil (Good_Name) then
         return No_Matching_Context;
      end if;

      -- 1) Check provided name
      Query_Context (Into, Good_Name);
      if Into.Last_Returned /= null then
         return Into.Last_Returned.Value.all;
      end if;

      -- No extended context for attribute references or pragmas
      -- (at least for the moment)
      if Element_Kind (Good_Name) = A_Pragma
        or else Expression_Kind (Good_Name) = An_Attribute_Reference
      then
         return No_Matching_Context;
      end if;

      if Element_Kind (Good_Name) = A_Defining_Name then
         Name_Declaration := Enclosing_Element (Good_Name);
      else
         Name_Declaration := Corresponding_Name_Declaration (Good_Name);
      end if;

      -- 2) if name is a renaming, try the ultimate name
      if Extend_To (Renaming)
        and then Declaration_Kind (Name_Declaration) in A_Renaming_Declaration
      then
         Query_Context (Into, Ultimate_Name (Good_Name));
         if Into.Last_Returned /= null then
            Into.This.Self.Last_Kind := Renamed;
            return Into.Last_Returned.Value.all;
         end if;
      end if;

      -- 3) if name is an instantiation, try the corresponding generic
      if Extend_To (Instance)
        and then Declaration_Kind (Name_Declaration) in A_Generic_Instantiation
      then
         Query_Context (Into, Generic_Unit_Name (Name_Declaration));
         if Into.Last_Returned /= null then
            Into.This.Self.Last_Kind := Instance;
            return Into.Last_Returned.Value.all;
         end if;
      end if;

      -- 4) if name is from an instantiation, try the corresponding generic element
      if Extend_To (Instance)
        and then Is_Part_Of_Instance (Name_Declaration)
      then
         if Element_Kind (Good_Name) = A_Defining_Name then
            Name_Definition := Good_Name;
         else
            Name_Definition := Corresponding_Name_Definition (Good_Name);
         end if;
         Query_Context (Into, Corresponding_Generic_Element (Name_Definition));
         if Into.Last_Returned /= null then
            Into.This.Self.Last_Kind := From_Instance;
            return Into.Last_Returned.Value.all;
         end if;
      end if;

      -- Nothing found
      return No_Matching_Context;
   end Matching_Context;

   ------------------------
   -- Last_Matching_Name --
   ------------------------

   function Last_Matching_Name (Into : in Context_Store) return Wide_String is
   begin
      return To_Wide_String (Into.Last_Name);
   end Last_Matching_Name;

   ------------------------
   -- Last_Matching_Kind --
   ------------------------

   function Last_Matching_Kind (Into : in Context_Store) return Matching_Kind is
   begin
      return Into.Last_Kind;
   end Last_Matching_Kind;

   ------------
   -- Update --
   ------------

   procedure Update (Into          : in out Context_Store;
                     Context       : in     Root_Context'Class)
   is
   begin
      Into.Last_Returned.Value.all := Context;
   end Update;

   -----------------
   -- Association --
   -----------------

   function Association (Into          : in Context_Store;
                         Specification : in Entity_Specification) return Root_Context'Class
   is
      use Context_Tree, Utilities;
      Result : Context_Node_Access;
   begin
      case Specification.Kind is
         when Regular_Id =>
            Result := Fetch (Into.Qualified_Names, Specification.Specification, Default_Value => null);
         when All_Id =>
            Result := Fetch (Into.Simple_Names, Specification.Specification, Default_Value => null);
         when others =>
            Failure ("Association: bad kind");
      end case;

      Into.This.Self.Last_Returned := Result;
      Into.This.Self.Last_Name     := Specification.Specification;

      if Result = null then
         Into.This.Self.Last_Kind := None;
         return No_Matching_Context;
      else
         Into.This.Self.Last_Kind := Original;
         return Result.Value.all;
      end if;
   end Association;

   -----------------
   -- Association --
   -----------------

   function  Association (Into : in Context_Store;
                          Key  : in Wide_String) return Root_Context'Class
   is
      use Context_Tree, Utilities;
      Result : Context_Node_Access;
      Unbounded_Key : constant Unbounded_Wide_String := To_Unbounded_Wide_String (To_Upper (Key));
   begin
      Result := Fetch (Into.Qualified_Names, Unbounded_Key, Default_Value => null);

      Into.This.Self.Last_Returned := Result;
      Into.This.Self.Last_Name     := Unbounded_Key;

      if Result = null then
         Into.This.Self.Last_Kind := None;
         return No_Matching_Context;
      else
         Into.This.Self.Last_Kind := Original;
         return Result.Value.all;
      end if;
   end Association;

   -----------
   -- Clear --
   -----------

   procedure Clear (Context : in out Root_Context) is
      pragma Unreferenced (Context);
   begin
      null;
   end Clear;

   ----------------
   -- Dissociate --
   ----------------

   procedure Dissociate (From          : in out Context_Store;
                         Specification : in     Entity_Specification)
   is
      use Context_Tree, Utilities;
      Temp : Context_Node_Access;
   begin
      case Specification.Kind is
         when Regular_Id =>
            if not Is_Present (From.Qualified_Names, Specification.Specification) then
               raise Not_In_Store;
            end if;

            Temp := Fetch (From.Qualified_Names, Specification.Specification);
            Release (Temp);
            Delete (From.Qualified_Names, Specification.Specification);
         when All_Id =>
            if not Is_Present (From.Simple_Names, Specification.Specification) then
               raise Not_In_Store;
            end if;

            Temp := Fetch (From.Simple_Names, Specification.Specification);
            Release (Temp);
            Delete (From.Simple_Names, Specification.Specification);
         when others =>
            Failure ("Dissociate : bad kind");
      end case;
   end Dissociate;

   -----------------------
   -- Equivalent_Values --
   -----------------------

   function Equivalent_Values (Left, Right : Root_Context) return Boolean is
   begin
      return Root_Context'Class (Left) = Root_Context'Class (Right);
   end Equivalent_Values;

   -----------
   -- Reset --
   -----------

   procedure Reset (Iter      : in out Context_Iterator;
                    Name      : in     Asis.Element;
                    Extend_To : in     Extension_Set := No_Extension)
   is
      -- Querying the context initializes the node to the proper state
      Discarded : constant Root_Context'Class := Matching_Context (Iter.all, Name, Extend_To);
      pragma Unreferenced (Discarded);
   begin
      null;
   end Reset;

   -----------
   -- Reset --
   -----------

   procedure Reset (Iter : in out Context_Iterator; Name : in Entity_Specification) is
      Unused : Root_Context'Class := Association (Iter.This.Self.all, Name);
      pragma Unreferenced (Unused);
   begin
      null;
   end Reset;

   -----------
   -- Value --
   -----------

   function  Value (Iter : in Context_Iterator) return Root_Context'Class is
   begin
      if Is_Exhausted (Iter) then
         raise Not_In_Store;
      end if;
      return Iter.Last_Returned.Value.all;
   end Value;

   ------------------------
   -- Last_Matching_Name --
   ------------------------

   function Last_Matching_Name (Iter : in Context_Iterator) return Wide_String is
   begin
      return Last_Matching_Name (Iter.all);
   end Last_Matching_Name;

   ----------
   -- Next --
   ----------

   procedure Next (Iter : in out Context_Iterator) is
   begin
      if Is_Exhausted (Iter) then
         raise Not_In_Store;
      end if;
      Iter.Last_Returned := Iter.Last_Returned.Next;
   end Next;


   ------------------
   -- Is_Exhausted --
   ------------------

   function Is_Exhausted (Iter : in Context_Iterator) return Boolean is
   begin
      return Iter.Last_Returned = null;
   end Is_Exhausted;

   ----------
   -- Save --
   ----------

   procedure Save (Iter : in Context_Iterator; Into : out Iterator_Position) is
   begin
      Into := Iterator_Position (Iter.Last_Returned.Value);
   end Save;

   -----------
   -- Value --
   -----------

   function Value (Position : in Iterator_Position) return Root_Context'Class is
   begin
      if Position = null then
         return No_Matching_Context;
      else
         return Position.all;
      end if;
   end Value;

end Framework.Control_Manager;
