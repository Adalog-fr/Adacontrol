----------------------------------------------------------------------
--  Rules.Default_Parameter - Package body                          --
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

-- ASIS
with
  Asis.Declarations,
  Asis.Elements,
  Asis.Expressions,
  Asis.Statements;

-- Adalog
with
  Binary_Map,
  Utilities;

-- Adactl
with
  Framework.Language,
  Framework.Rules_Manager,
  Framework.Reports;

package body Rules.Default_Parameter is
   use Framework, Utilities;

   Rule_Used : Boolean := False;

   type Usage_Kind is (Used, Not_Used);
   type Usage_Rec is
      record
         Active     : Boolean;
         Rule_Type  : Rule_Types;
         Rule_Label : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
      end record;
   type Usage_Tab is array (Usage_Kind) of Usage_Rec;


   package Parameter_Tree is new Binary_Map
     (Key_Type   => Ada.Strings.Wide_Unbounded.Unbounded_Wide_String,
      Value_Type => Usage_Tab,
      "<"        => Ada.Strings.Wide_Unbounded."<",
      ">"        => Ada.Strings.Wide_Unbounded.">");

   type Entity_Context is new Rule_Context with
      record
         Formals_Map : Parameter_Tree.Map;
      end record;
   Entities  : Context_Store;

   ----------
   -- Help --
   ----------

   procedure Help is
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Parameter 1: Subprogram or generic name");
      User_Message ("Parameter 2: Formal parameter name");
      User_Message ("Parameter 3: ""Used"" or ""Not_Used""");
      User_Message ("This rule can be used to check/search for subprogram calls or generic");
      User_Message ("instantiations that use (or not) the default value for a given parameter");
   end Help;

   -------------
   -- Add_Use --
   -------------

   procedure Add_Use (Label     : in Wide_String;
                      Rule_Type : in Rule_Types) is
      use Ada.Strings.Wide_Unbounded;
      use Framework.Language;

      Entity : Entity_Specification;
      Formals_Map : Parameter_Tree.Map;
   begin
      if not Parameter_Exists then
         Parameter_Error ("Missing subprogram or generic name for rule " & Rule_Id);
      end if;
      Entity := Get_Entity_Parameter;

      if not Parameter_Exists then
         Parameter_Error ("Missing formal name for rule " & Rule_Id);
      end if;

      declare
         Formal : constant Wide_String := To_Upper (Get_String_Parameter);
         Usage  : Usage_Kind;
      begin
         if Parameter_Exists then
            begin
               Usage := Usage_Kind'Wide_Value (Get_String_Parameter);
            exception
               when Constraint_Error =>
                  Parameter_Error ("Parameter should be ""Used"" or ""Not_Used"" for rule "
                                     & Rule_Id);
            end;
         else
            Usage := Used;
         end if;

         begin
            Associate (Entities, Entity, Entity_Context'(Formals_Map => Formals_Map));
         exception
            when Already_In_Store =>
               null;
         end;

         -- Note: Maps have object (reference) semantics
         Formals_Map := Entity_Context (Association (Entities, Entity)).Formals_Map;
         declare
            Val : Usage_Tab := Parameter_Tree.Fetch (Formals_Map,
                                                     To_Unbounded_Wide_String (Formal),
                                                     Default_Value => (others => (False,
                                                                                  Search,
                                                                                  Null_Unbounded_Wide_String)));
         begin
            Val (Usage) := (True, Rule_Type, To_Unbounded_Wide_String (Label));
            Parameter_Tree.Add (Formals_Map, To_Unbounded_Wide_String (Formal), Val);
            Update (Entities, Entity_Context'(Formals_Map => Formals_Map));
         exception
            when Already_In_Store =>
               Parameter_Error ("Formal already specified: " & Formal);
         end;
      end;
      Rule_Used  := True;
   end Add_Use;

   -------------
   -- Prepare --
   -------------

   procedure Prepare is
   begin
      Balance (Entities);
      -- We do not balance the trees for each formal.
      -- Since we do not expect more than 1 or 2 entries in each...
   end Prepare;

   -----------------------------------
   -- Process_Call_Or_Instantiation --
   -----------------------------------

   procedure Process_Call_Or_Instantiation (Element : in Asis.Element) is
      use Ada.Strings.Wide_Unbounded;
      use Asis, Asis.Elements, Asis.Expressions, Asis.Declarations, Asis.Statements;
      use Parameter_Tree;

      function Get_Association_List (E : in Asis.Element) return Asis.Association_List is
      begin
         if Expression_Kind (Element) = A_Function_Call then
            return Function_Call_Parameters (E, Normalized => True);
         elsif Statement_Kind (Element) = A_Procedure_Call_Statement or
           Statement_Kind (Element) = An_Entry_Call_Statement
         then
            return Call_Statement_Parameters (E, Normalized => True);
         elsif Declaration_Kind (Element) in A_Generic_Instantiation then
            return Generic_Actual_Part (E, Normalized => True);
         else
            Failure ("Unexpected element in Process for Default_Parameter", Element);
         end if;
      end Get_Association_List;

      Name : Asis.Expression;
   begin
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      if Expression_Kind (Element) = A_Function_Call then
         Name := Prefix (Element);
      elsif Statement_Kind (Element) = A_Procedure_Call_Statement
        or  Statement_Kind (Element) = An_Entry_Call_Statement
      then
         Name := Called_Name (Element);
      elsif Declaration_Kind (Element) in A_Generic_Instantiation then
         Name := Generic_Unit_Name (Element);
      else
         Failure ("Unexpected element in Process for Default_Parameter", Element);
      end if;

      declare
         use Framework.Reports;
         Current_Context : Rule_Context'Class := Matching_Context (Entities, Name);
      begin
         if Current_Context = No_Matching_Context then
            return;
         end if;

         declare
            Associations    : constant Asis.Association_List := Get_Association_List (Element);
            Formal          : Asis.Element;
            Formals_Context : Entity_Context renames Entity_Context (Current_Context);
            Usage           : Usage_Tab;
         begin
            for I in Associations'Range loop
               Formal := Formal_Parameter (Associations (I));
               if Is_Present (Formals_Context.Formals_Map,
                              To_Unbounded_Wide_String (To_Upper (Defining_Name_Image (Formal))))
               then
                  Usage := Fetch (Formals_Context.Formals_Map,
                            To_Unbounded_Wide_String (To_Upper
                                                        (Defining_Name_Image (Formal))));
                  if Usage (Used).Active then
                     if Is_Defaulted_Association (Associations (I)) then
                        Report (Rule_Id,
                                To_Wide_String (Usage (Used).Rule_Label),
                                Usage (Used).Rule_Type,
                                Get_Location (Element),
                                "default use of formal """ & Defining_Name_Image (Formal) & '"');
                     end if;
                  end if;

                  if Usage (Not_Used).Active then
                     if not Is_Defaulted_Association (Associations (I)) then
                        Report (Rule_Id,
                                To_Wide_String (Usage (Not_Used).Rule_Label),
                                Usage (Not_Used).Rule_Type,
                                Get_Location (Element),
                                "non default use of formal """ & Defining_Name_Image (Formal) & '"');
                     end if;
                  end if;
               end if;
            end loop;
         end;
      end;
   end Process_Call_Or_Instantiation;

begin
   Framework.Rules_Manager.Register (Rule_Id,
                                     Help    => Help'Access,
                                     Prepare => Prepare'Access,
                                     Add_Use => Add_Use'Access);
end Rules.Default_Parameter;
