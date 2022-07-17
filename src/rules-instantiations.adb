----------------------------------------------------------------------
--  Rules.Instantiations - Package body                             --
--                                                                  --
--  This software  is (c) The European Organisation  for the Safety --
--  of Air Navigation (EUROCONTROL) and Adalog 2004-2022.           --
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
  Ada.Strings.Wide_Unbounded;

-- ASIS
with
  Asis.Elements,
  Asis.Declarations,
  Asis.Expressions;

-- Adalog
with
  Linear_Queue,
  Thick_Queries,
  Utilities;

-- Adactl
with
  Framework.Control_Manager.Generic_Context_Iterator,
  Framework.Language.Shared_Keys, Framework.Variables.Shared_Types;
pragma Elaborate (Framework.Language.Shared_Keys);

package body Rules.Instantiations is
   use Asis, Framework, Framework.Control_Manager, Framework.Language.Shared_Keys, Framework.Variables.Shared_Types;

   -- Algorithm:
   --
   -- A multi-valued context is associated to each generic, containing the various specifications as
   -- given in the rule.
   -- For each instantiation, the parameter list is matched against the specifications
   --
   -- Case of "=":
   -- The context contains a boolean discriminant to tell if the rule's parameter list contains at least one "="
   -- If true, the context contains a list of parameters list already encountered:
   --  - For an in parameter, the parameter is the expression
   --  - For other parameters, it is a name, and the parameter is its full name image
   -- When an instantiation is encountered, the same comparison as with the parameters of the rule can be used,
   -- except for the special case of in parameters.
   -- "others =" is like "=", except that the index of comparison does not move when it reaches the final "="

   Rule_Used : Boolean := False;
   Save_Used : Boolean;

   type Parameter_Descr (Param_Mode : Mode_Kinds := An_In_Out_Mode) is
      record
         case Param_Mode is
            when A_Default_In_Mode | An_In_Mode =>
               Value : Asis.Expression;
            when An_In_Out_Mode =>
               Spec : Entity_Specification;
            when others =>
               null; -- impossible for generic parameters
         end case;
      end record;

   type Generic_Parameters is array (List_Index range <>) of Parameter_Descr;
   No_Parameters : constant Generic_Parameters (1 .. 0) := (others => (An_In_Out_Mode, Value ("Junk")));

   type Instance_Info (Nb_Param : List_Index) is
      record
         Loc    : Locations.Location;
         Values : Generic_Parameters (1 .. Nb_Param);
      end record;
   package Instance_Info_List is new Linear_Queue (Instance_Info);

   type Instantiation_Context (Nb_Values : ASIS_Natural; Has_Repeated : Boolean) is new Basic_Rule_Context with
      record
         Values : Generic_Parameters (1 .. Nb_Values);
         Places : Framework.Language.Shared_Keys.Places_Set;
         case Has_Repeated is
            when False =>
               null;
            when True =>
               Has_Others_Equal : Boolean;
               All_Instances    : Instance_Info_List.Queue;
         end case;
      end record;

   Rule_Uses : Context_Store;
   package Rule_Uses_Iterator is new Framework.Control_Manager.Generic_Context_Iterator (Rule_Uses);

   Expected_Categories : constant Categories_Set := Basic_Set + Cat_Any + Cat_New + Cat_Private;

   -- Rule variable
   Type_Matches_Subtype : aliased Switch_Type.Object := (Value => On);

   -----------
   -- Image --
   -----------

   function Image (Values : in Generic_Parameters) return Wide_String is
      -- Precondition: Values /= null
      use Ada.Strings.Wide_Unbounded;

      function Descr_Image (D : Parameter_Descr) return Wide_String is
         use Thick_Queries, Utilities;
      begin
         case D.Param_Mode is
            when An_In_Mode | A_Default_In_Mode =>
               return Static_Expression_Value_Image (D.Value);
            when An_In_Out_Mode =>
               return Image (D.Spec);
            when others =>
               Failure ("Descr_Image: bad mode: " & Mode_Kinds'Wide_Image (D.Param_Mode));
         end case;
      end Descr_Image;

      Dummy : Unbounded_Wide_String := Null_Unbounded_Wide_String;
   begin  -- Image
      Append (Dummy, "(");
      Append (Dummy, Descr_Image (Values (Values'First)));

      for V : Parameter_Descr of Values (Values'First + 1 .. Values'Last) loop
         Append (Dummy, ", ");
         Append (Dummy, Descr_Image (V));
      end loop;

      Append (Dummy, ")");

      return To_Wide_String (Dummy);
   end Image;


   ----------
   -- Help --
   ----------

   procedure Help is
      use Framework.Variables, Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Control generic instantiations of specified units, either all of them");
      User_Message ("or those made with the indicated actual parameters.");
      User_Message ("Optionally, control is restricted to instantiations appearing at indicated locations");
      User_Message;
      User_Message ("Parameter(1): {<location>} <Generic name>");
      User_Message ("Parameter(2..): <Entity name> | <category> | [others] = (optional)");
      Help_On_Scope_Places (Header => "<location>:");
      Help_On_Categories (Expected => Expected_Categories);
      User_Message ("Variable:");
      Help_On_Variable (Rule_Id & ".Type_Matches_Subtype");

   end Help;


   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Framework.Language;

      function Build_Context (Places : Places_Set) return Instantiation_Context is
         use Instance_Info_List;
      begin
         if not Parameter_Exists then
            return (Basic.New_Context (Ctl_Kind, Ctl_Label) with
                    Nb_Values    => 0,
                    Has_Repeated => False,
                    Values       => No_Parameters,
                    Places       => Places);
         end if;

         declare
            Has_Others : constant Boolean := Get_Modifier ("OTHERS");
            Spec       : Entity_Specification := Get_Entity_Parameter (Allow_Extended => All_OK);
            Rest       : constant Instantiation_Context := Build_Context (Places);
         begin
            if Has_Others then
               if Entity_Specification_Kind (Spec) /= Equal then
                  Parameter_Error (Rule_Id, "Only ""="" allowed after ""others""");
               end if;
               if Rest.Nb_Values /= 0 then
                  Parameter_Error (Rule_Id, """others ="" must be last");
               end if;
            else
               Check_Category (Rule_Id, Spec, Expected => Expected_Categories);
            end if;

            if Spec = Value ("()") then
               Spec := Value ("ENUM");
            end if;

            if Rest.Has_Repeated or Entity_Specification_Kind (Spec) = Equal then
               return (Basic_Rule_Context (Rest) with
                       Nb_Values        => Rest.Nb_Values + 1,
                       Has_Repeated     => True,
                       Values           => Parameter_Descr'(An_In_Out_Mode, Spec) & Rest.Values,
                       Places           => Rest.Places,
                       Has_Others_Equal => Has_Others,
                       All_Instances    => Empty_Queue);
            else
               return (Basic_Rule_Context (Rest) with
                       Nb_Values    => Rest.Nb_Values + 1,
                       Has_Repeated => False,
                       Values       => Parameter_Descr'(An_In_Out_Mode, Spec) & Rest.Values,
                       Places       => Rest.Places);
            end if;
         end;
      end Build_Context;

   begin   -- Add_Control
      if not Parameter_Exists then
         Parameter_Error (Rule_Id, "At least one parameter required");
      end if;

      declare
         Places         : constant Places_Set           := Get_Places_Set_Modifiers (Rule_Id);
         Generic_Name   : constant Entity_Specification := Get_Entity_Parameter;
      begin
         Associate (Rule_Uses,
                    Generic_Name,
                    Build_Context (Places),
                    Additive => True);
         Rule_Used := True;
      exception
         when Already_In_Store =>
            Parameter_Error (Rule_Id, "this combination of parameters already specified for " & Image (Generic_Name));
      end;
   end Add_Control;

   -------------
   -- Command --
   -------------

   procedure Command (Action : Framework.Rules_Manager.Rule_Action) is
      use Framework.Rules_Manager;
   begin
      case Action is
         when Clear =>
            Rule_Used := False;
            Clear (Rule_Uses);
            Type_Matches_Subtype := (Value => On);
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := False;
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;

   -------------
   -- Prepare --
   -------------

   procedure Prepare is
   begin
      Balance (Rule_Uses);
   end Prepare;

   ----------------------
   -- Is_Corresponding --
   ----------------------

   function Is_Corresponding (Descriptor : in Parameter_Descr;
                              Name       : in Asis.Expression) return Boolean
   is
      use Asis.Elements, Asis.Expressions;
      use Utilities, Thick_Queries;

      Declaration  : Asis.Declaration;
      Is_Attribute : Boolean := False;
   begin
      if Descriptor.Param_Mode in An_In_Mode | A_Default_In_Mode then
         return Same_Value (Descriptor.Value, Name);
      end if;

      if Entity_Specification_Kind (Descriptor.Spec) in Equal | Box then
         return True;
      end if;

      case Expression_Kind (Name) is
         when An_Identifier | A_Selected_Component =>
            null;
         when An_Attribute_Reference =>
            case Attribute_Kind (Name) is
               when A_Base_Attribute | A_Class_Attribute =>
                  -- The only ones that are names
                  Is_Attribute := True;
               when others =>
                  return False;
            end case;
         when others =>
            -- An arithmetic expression for example, matches nothing but <> and =
            return False;
      end case;

      if Matches (Descriptor.Spec, Name) then
         return True;
      end if;

      -- No direct match here

      if Is_Attribute then
         return False;
      end if;

      -- Special case if Name designates a type: try category
      -- (+ first named subtype if it is a subtype)
      Declaration := Corresponding_Name_Declaration (Simple_Name (Name));
      case Declaration_Kind (Declaration) is
         when A_Type_Declaration =>
            null;
         when A_Subtype_Declaration =>
            if Type_Matches_Subtype.Value = On
              and then Matches (Descriptor.Spec, First_Subtype_Name (Name))
            then
               return True;
            end if;
         when others =>
            -- Not a type or subtype
            -- (includes Not_An_Element if Name is class wide or predefined)
            return False;
      end case;

      -- Here we have a type or subtype
      if Image (Type_Category (Declaration)) = To_Upper (Image (Descriptor.Spec)) then
         return True;
      end if;

      return False;
   end Is_Corresponding;

   -----------
   -- Match --
   -----------

   function Match (Actual_Part      : in Asis.Association_List;
                   Values           : in Generic_Parameters;
                   Has_Others_Equal : in Boolean := False) return Boolean
   is
      use Asis.Expressions;

      Values_Index : List_Index := Values'First;
   begin
      for Assoc : Asis.Association of Actual_Part loop
         if not Is_Corresponding (Values (Values_Index), Actual_Parameter (Assoc)) then
            return False;
         end if;

         if not Has_Others_Equal then
            -- otherwise stay at the Equal

            -- Safety if there are too few parameters specified by user:
            exit when Values_Index = Values'Last;

            Values_Index := Values_Index + 1;
         end if;
      end loop;

      return True;
   end Match;

   ---------------------------
   -- Process_Instantiation --
   ---------------------------

   procedure Process_Instantiation (Instantiation : in Asis.Declaration) is
      use Asis.Declarations;

      Iter : Context_Iterator := Rule_Uses_Iterator.Create;

      ---------------
      -- Make_Info --
      ---------------

      function Make_Info (Origin : Instantiation_Context; Using  : Asis.Association_List) return Instance_Info is
         use Asis.Elements, Asis.Expressions;
         use Framework.Locations, Thick_Queries;
         Result : Instance_Info := ((if Origin.Has_Repeated and then Origin.Has_Others_Equal
                                       then Using'Length
                                       else List_Index'Min (Origin.Values'Length, Using'Length)),
                                    Get_Location (Instantiation),
                                    (others => <>));
         P_Mode : Asis.Mode_Kinds;
         In_Others_Equal : Boolean := False;
      begin
         for I in Result.Values'Range loop
            -- We know that both Origin and Result have 'First = 1, so we can use the same index for both
            -- Replace any "=" by the actual parameter, except if the "=" corresponds to an "in" object
            if In_Others_Equal or else Entity_Specification_Kind (Origin.Values (I).Spec) = Equal then
               P_Mode := Mode_Kind (Enclosing_Element (Formal_Parameter (Using (I))));
               case P_Mode is
                  when A_Default_In_Mode | An_In_Mode =>
                     Result.Values (I) := (An_In_Mode, Actual_Parameter (Using(I)));
                  when others =>
                     Result.Values (I) := (An_In_Out_Mode,
                                           Value (Full_Name_Image (Actual_Parameter (Using (I)),
                                                  With_Profile => True)));
               end case;
               if I = Origin.Values'Last and then Origin.Has_Others_Equal then
                  In_Others_Equal := True;
               end if;
            else
               Result.Values (I) := Origin.Values (I);
            end if;
         end loop;

         return Result;
      end Make_Info;

      procedure Process_Context is
         use Framework.Locations, Framework.Reports;
         use Utilities;
         Good_Context : Instantiation_Context := Instantiation_Context (Value (Iter));
      begin
         if not Is_Applicable (Good_Context.Places) then
            return;
         end if;

         if Good_Context.Nb_Values = 0 then
            Report (Rule_Id,
                    Good_Context,
                    Get_Location (Instantiation),
                    Image (Good_Context.Places, Default => Everywhere)
                    & "instantiation of "
                    & To_Title (Last_Matching_Name (Rule_Uses)));

         elsif Good_Context.Has_Repeated then
            declare
               use Instance_Info_List;
               Actual_Part : constant Asis.Association_List := Generic_Actual_Part (Instantiation,
                                                                                    Normalized => True);
               Current     : Cursor;
               Found       : Boolean := False;
            begin
               Current := First (Good_Context.All_Instances);
               while Has_Element (Current) loop
                  if Match (Actual_Part, Fetch (Current).Values) then
                     Found := True;
                     Report (Rule_Id,
                             Good_Context,
                             Get_Location (Instantiation),
                             Image (Good_Context.Places, Default => Everywhere)
                             & "instantiation of "
                             & To_Title (Last_Matching_Name (Rule_Uses))
                             & " with " & Image (Fetch (Current).Values)
                             & " already provided at " & Image (Fetch (Current).Loc));
                  end if;
                  Current := Next (Current);
               end loop;
               if not Found and Match (Actual_Part, Good_Context.Values, Good_Context.Has_Others_Equal) then
                  Append (Good_Context.All_Instances, Make_Info (Good_Context, Actual_Part));
                  Update (Rule_Uses, Good_Context);
               end if;
            end;

         else
            declare
               Actual_Part : constant Asis.Association_List := Generic_Actual_Part (Instantiation,
                                                                                    Normalized => True);
            begin
               if Match (Actual_Part, Good_Context.Values) then
                  Report (Rule_Id,
                          Good_Context,
                          Get_Location (Instantiation),
                          Image (Good_Context.Places, Default => Everywhere)
                          & "instantiation of "
                          & To_Title (Last_Matching_Name (Rule_Uses))
                          & " with " & Image (Good_Context.Values));
               end if;
            end;
         end if;
      end Process_Context;

   begin  -- Process_Instantiation
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      Reset (Iter, Generic_Unit_Name (Instantiation), All_Extensions);
      while not Is_Exhausted (Iter) loop
         Process_Context;
         Next (Iter);
      end loop;
   end Process_Instantiation;

begin  -- Rules.Instantiations
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access,
                                     Prepare_CB     => Prepare'Access);
   Framework.Variables.Register (Type_Matches_Subtype'Access,
                                 Variable_Name => Rule_Id & ".TYPE_MATCHES_SUBTYPE");

end Rules.Instantiations;
