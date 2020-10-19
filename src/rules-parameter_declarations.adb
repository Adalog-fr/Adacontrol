----------------------------------------------------------------------
--  Rules.Parameter_Declarations - Package body                     --
--                                                                  --
--  This software is (c) SAGEM DS and Adalog 2004-2006.             --
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
  Asis.Declarations,
  Asis.Elements;

-- Adalog
with
  Thick_Queries,
  Utilities;

-- Adactl
with
  Framework.Language,
  Framework.Language.Shared_Keys;
pragma Elaborate (Framework.Language);

package body Rules.Parameter_Declarations is
   use Framework, Framework.Language.Shared_Keys;

   type Subrules is (All_Parameters,
                     In_Parameters,         Defaulted_Parameters,
                     Out_Parameters,        In_Out_Parameters,
                     Access_Parameters,     Tagged_Parameters,
                     Class_Wide_Parameters, Single_Out_Parameter);
   subtype Valued_Subrules is Subrules range All_Parameters .. Subrules'Pred(Single_Out_Parameter);
   package Subrules_Flag_Utilities is new Framework.Language.Flag_Utilities (Subrules);

   type Callable_Kinds is (C_Function,             C_Procedure,
                           C_Dispatching_Function, C_Dispatching_Procedure,
                           C_Protected_Function,   C_Protected_Procedure,   C_Protected_Entry,
                           C_Task_Entry);
   subtype Dispatching_Kinds is Callable_Kinds range C_Dispatching_Function .. C_Dispatching_Procedure;
   package Callable_Kinds_Flag_Utilities  is new Framework.Language.Flag_Utilities (Callable_Kinds, Prefix => "C_");

   type Usage is array (Subrules, Callable_Kinds) of Control_Kinds_Set;
   Rule_Used : Usage := (others => (others => Empty_Control_Kinds_Set));
   Save_Used : Usage;

   Ctl_Labels : array (Subrules, Callable_Kinds, Control_Kinds) of Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
   Ctl_Values : array (Subrules, Callable_Kinds, Control_Kinds) of Bounds_Values
     := (others => (others => (others => Unlimited_Bounds)));

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      User_Message ("Controls form and metrics of parameters of callable entities");
      User_Message;
      Subrules_Flag_Utilities.Help_On_Flags (Header => "Parameter(1): [no_inout]");
      User_Message;
      User_Message ("For all subrules except Single_Out_Parameter:");
      User_Message ("Parameter(2..3): <bound> <value>");
      User_Message ("                (at least one parameter required)");
      Help_On_Bounds (Header => "   <bound>:");
      User_Message;
      User_Message ("For Single_Out_Parameter:");
      User_Message ("  No value allowed");
      User_Message;
      Callable_Kinds_Flag_Utilities.Help_On_Flags (Header => "Other parameters:",
                                                   Footer => "(optional, default = all)");
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Framework.Language, Callable_Kinds_Flag_Utilities, Subrules_Flag_Utilities, Utilities;
      use Ada.Strings.Wide_Unbounded;
      Subrule  : Subrules;
      Callable : Callable_Kinds;
      Value    : Bounds_Values;
      No_Inout : Boolean;
   begin
      if not Parameter_Exists then
         Parameter_Error (Rule_Id, "subrule not specified");
      end if;

      No_Inout := Get_Modifier ("NO_INOUT");
      Subrule  := Get_Flag_Parameter (Allow_Any => False);

      case Subrule is
         when Valued_Subrules =>
            if No_Inout then
               Parameter_Error (Rule_Id, "No_Inout allowed only with Single_Out_Parameter");
            end if;
            if Parameter_Exists then
               Value := Get_Bounds_Parameters (Rule_Id);
            else
               Parameter_Error (Rule_Id, "missing bounds of allowed value");
            end if;
         when Single_Out_Parameter =>
            -- Value is the number of allowed inout parameters (0 or infinity)
            if No_Inout then
               Value := (1, Thick_Queries.Biggest_Int'Last);
            else
               Value := Empty_Bounds;
            end if;
            if Parameter_Exists and then Is_Integer_Parameter then
               Parameter_Error (Rule_Id, "no value allowed for Single_Out_Parameter");
            end if;
      end case;

      if Parameter_Exists then
         while Parameter_Exists loop
            Callable := Get_Flag_Parameter (Allow_Any => False);
            if Rule_Used (Subrule, Callable)(Ctl_Kind) then
               Parameter_Error (Rule_Id, Image (Callable, Lower_Case)
                                         & " already specified for "
                                         & Control_Kinds'Wide_Image (Ctl_Kind));
            end if;
            Ctl_Labels (Subrule, Callable, Ctl_Kind) := To_Unbounded_Wide_String (Ctl_Label);
            Ctl_Values (Subrule, Callable, Ctl_Kind) := Value;
            Rule_Used  (Subrule, Callable)(Ctl_Kind) := True;
         end loop;
      else
         -- no callable kind specified => applies to all
         for C in Callable_Kinds loop
            Ctl_Labels (Subrule, C, Ctl_Kind) := To_Unbounded_Wide_String (Ctl_Label);
            Ctl_Values (Subrule, C, Ctl_Kind) := Value;
            if Rule_Used (Subrule, C)(Ctl_Kind) then
               Parameter_Error (Rule_Id, Image (Callable, Lower_Case)
                                         & " already specified for "
                                         & Control_Kinds'Wide_Image (Ctl_Kind));
            end if;
            Rule_Used  (Subrule, C)(Ctl_Kind) := True;
         end loop;
      end if;
   end Add_Control;

   -------------
   -- Command --
   -------------

   procedure Command (Action : in Framework.Rules_Manager.Rule_Action) is
      use Framework.Rules_Manager;
   begin
      case Action is
         when Clear =>
            Rule_Used := (others => (others => Empty_Control_Kinds_Set));
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := (others => (others => Empty_Control_Kinds_Set));
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;


   -------------------------
   -- Process_Declaration --
   -------------------------

   procedure Process_Declaration (Declaration : Asis.Declaration) is
      use Asis, Asis.Declarations, Asis.Elements;
      use Thick_Queries, Utilities;

      Good_Decl : Asis.Declaration := Declaration;

      procedure Do_Report (Subrule  : Subrules;
                           Ctl_Kind : Control_Kinds;
                           Entity   : Callable_Kinds;
                           Value    : Biggest_Int)
      is
         use Ada.Strings.Wide_Unbounded;
         use Callable_Kinds_Flag_Utilities, Framework.Locations, Framework.Reports;

         Loc : constant Location := Get_Location (Names (Declaration)(1));
      begin

         case Subrule is
            when All_Parameters =>
               Report (Rule_Id,
                       To_Wide_String (Ctl_Labels (All_Parameters, Entity, Ctl_Kind)),
                       Ctl_Kind,
                       Loc,
                       "number of parameters is "
                       & Bound_Image (Ctl_Values (All_Parameters, Entity, Ctl_Kind))
                       & " for " & Image (Entity, Lower_Case)
                       & " ("   & Biggest_Int_Img (Value) & ')');
            when In_Parameters =>
               Report (Rule_Id,
                       To_Wide_String (Ctl_Labels (In_Parameters, Entity, Ctl_Kind)),
                       Ctl_Kind,
                       Loc,
                       "number of ""in"" parameters is "
                       & Bound_Image (Ctl_Values (In_Parameters, Entity, Ctl_Kind))
                       & " for " & Image (Entity, Lower_Case)
                       & " ("   & Biggest_Int_Img (Value) & ')');
            when Defaulted_Parameters =>
               Report (Rule_Id,
                       To_Wide_String (Ctl_Labels (Defaulted_Parameters, Entity, Ctl_Kind)),
                       Ctl_Kind,
                       Loc,
                       "number of defaulted parameters is "
                       & Bound_Image (Ctl_Values (Defaulted_Parameters, Entity, Ctl_Kind))
                       & " for " & Image (Entity, Lower_Case)
                       & " ("   & Biggest_Int_Img (Value) & ')');
            when Out_Parameters =>
               Report (Rule_Id,
                       To_Wide_String (Ctl_Labels (Out_Parameters, Entity, Ctl_Kind)),
                       Ctl_Kind,
                       Loc,
                       "number of ""out"" parameters is "
                       & Bound_Image (Ctl_Values (Out_Parameters, Entity, Ctl_Kind))
                       & " for " & Image (Entity, Lower_Case)
                       & " ("   & Biggest_Int_Img (Value) & ')');
            when In_Out_Parameters =>
               Report (Rule_Id,
                       To_Wide_String (Ctl_Labels (In_Out_Parameters, Entity, Ctl_Kind)),
                       Ctl_Kind,
                       Loc,
                       "number of ""in out"" parameters is "
                       & Bound_Image (Ctl_Values (In_Out_Parameters, Entity, Ctl_Kind))
                       & " for " & Image (Entity, Lower_Case)
                       & " ("   & Biggest_Int_Img (Value) & ')');
            when Access_Parameters =>
               Report (Rule_Id,
                       To_Wide_String (Ctl_Labels (Access_Parameters, Entity, Ctl_Kind)),
                       Ctl_Kind,
                       Loc,
                       "number of access parameters is "
                       & Bound_Image (Ctl_Values (Access_Parameters, Entity, Ctl_Kind))
                       & " for " & Image (Entity, Lower_Case)
                       & " ("   & Biggest_Int_Img (Value) & ')');
            when Tagged_Parameters =>
               Report (Rule_Id,
                       To_Wide_String (Ctl_Labels (Tagged_Parameters, Entity, Ctl_Kind)),
                       Ctl_Kind,
                       Loc,
                       "number of tagged parameters is "
                       & Bound_Image (Ctl_Values (Tagged_Parameters, Entity, Ctl_Kind))
                       & " for " & Image (Entity, Lower_Case)
                       & " ("   & Biggest_Int_Img (Value) & ')');
            when Class_Wide_Parameters =>
               Report (Rule_Id,
                       To_Wide_String (Ctl_Labels (Class_Wide_Parameters, Entity, Ctl_Kind)),
                       Ctl_Kind,
                       Loc,
                       "number of class-wide parameters is "
                       & Bound_Image (Ctl_Values (Class_Wide_Parameters, Entity, Ctl_Kind))
                       & " for " & Image (Entity, Lower_Case)
                       & " ("   & Biggest_Int_Img (Value) & ')');
            when Single_Out_Parameter =>
               Report (Rule_Id,
                       To_Wide_String (Ctl_Labels (Single_Out_Parameter, Entity, Ctl_Kind)),
                       Ctl_Kind,
                       Loc,
                       "Single out parameter in " & Image (Entity, Lower_Case)
                       & Choose (Value = 0, "", " (with in out parameters)"));
         end case;
      end Do_Report;

      procedure Check_Value (Subrule  : Subrules;
                             Callable : Callable_Kinds;
                             Value    : Biggest_Int)
      is
         Non_Dispatching : constant array (Dispatching_Kinds) of Callable_Kinds
           := (C_Dispatching_Function  => C_Function,
               C_Dispatching_Procedure => C_Procedure);
      begin
         if Rule_Used (Subrule, Callable) (Check)
           and then not Is_In (Value, Ctl_Values (Subrule, Callable, Check))
         then
            Do_Report (Subrule, Check, Callable, Value);
         elsif Rule_Used (Subrule, Callable) (Search)
           and then not Is_In (Value, Ctl_Values (Subrule, Callable, Search))
         then
            Do_Report (Subrule, Search, Callable, Value);
         elsif Callable in Dispatching_Kinds then
            -- Try with the non-dispatching equivalent
            if Rule_Used (Subrule, Non_Dispatching (Callable)) (Check)
              and then not Is_In (Value, Ctl_Values (Subrule, Non_Dispatching (Callable), Check))
            then
               Do_Report (Subrule, Check, Non_Dispatching (Callable), Value);
            elsif Rule_Used (Subrule, Non_Dispatching (Callable)) (Search)
              and then not Is_In (Value, Ctl_Values (Subrule, Non_Dispatching (Callable), Search))
            then
               Do_Report (Subrule, Search, Non_Dispatching (Callable), Value);
            end if;
         end if;

         if Rule_Used (Subrule, Callable) (Count)
           and then not Is_In (Value, Ctl_Values (Subrule, Callable, Count))
         then
            Do_Report (Subrule, Count, Callable, Value);
         elsif Callable in Dispatching_Kinds then
            -- Try with the non-dispatching equivalent
            if Rule_Used (Subrule, Non_Dispatching (Callable)) (Count)
              and then not Is_In (Value, Ctl_Values (Subrule, Non_Dispatching (Callable), Count))
            then
               Do_Report (Subrule, Count, Non_Dispatching (Callable), Value);
            end if;
         end if;
      end Check_Value;

      C : Callable_Kinds;

   begin  -- Process_Declaration
      if Rule_Used = (Subrules => (Callable_Kinds => Empty_Control_Kinds_Set)) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      case Declaration_Kind (Good_Decl) is
         when A_Procedure_Body_Declaration
            | A_Null_Procedure_Declaration
            | A_Function_Body_Declaration
            | A_Procedure_Body_Stub
            | A_Function_Body_Stub
            =>
            if Is_Subunit (Good_Decl) or not Is_Nil (Corresponding_Declaration (Good_Decl)) then
               -- The check is performed on the specification or the stub, no need to repeat here
               return;
            end if;
         when others =>
            null;
      end case;

      -- Determine the kind of callable entity
      case Declaration_Kind (Good_Decl) is
         when A_Procedure_Declaration =>
            if Definition_Kind (Enclosing_Element (Good_Decl)) = A_Protected_Definition then
               C := C_Protected_Procedure;
            elsif Is_Dispatching_Operation (Good_Decl) then
               C := C_Dispatching_Procedure;
            else
               C := C_Procedure;
            end if;
         when A_Null_Procedure_Declaration
            | A_Generic_Procedure_Declaration
            | A_Procedure_Body_Declaration
            | A_Procedure_Body_Stub
            =>
            if Is_Dispatching_Operation (Good_Decl) then
               C := C_Dispatching_Procedure;
            else
               C := C_Procedure;
            end if;
         when A_Procedure_Instantiation =>
            if Is_Dispatching_Operation (Good_Decl) then
               C := C_Dispatching_Procedure;
            else
               C := C_Procedure;
            end if;
            Good_Decl := Corresponding_Declaration (Good_Decl);

         when A_Function_Declaration
            | An_Expression_Function_Declaration   -- Ada 2012
            =>
            if Definition_Kind (Enclosing_Element (Good_Decl)) = A_Protected_Definition then
               C := C_Protected_Function;
            elsif Is_Dispatching_Operation (Good_Decl) then
               C := C_Dispatching_Function;
            else
               C := C_Function;
            end if;
         when A_Generic_Function_Declaration
            | A_Function_Body_Declaration
            | A_Function_Body_Stub
              =>
            if Is_Dispatching_Operation (Good_Decl) then
               C := C_Dispatching_Function;
            else
               C := C_Function;
            end if;
         when A_Function_Instantiation =>
            if Is_Dispatching_Operation (Good_Decl) then
               C := C_Dispatching_Function;
            else
               C := C_Function;
            end if;
            Good_Decl := Corresponding_Declaration (Good_Decl);

         when An_Entry_Declaration =>
            if Is_Task_Entry (Good_Decl) then
               C := C_Task_Entry;
            else
               C := C_Protected_Entry;
            end if;
         when others =>
            -- We don't plug this procedure on entry bodies, since those always have
            -- a specification
            Failure ("not a callable entity");
      end case;

      -- Count parameters for each mode
      declare
         In_Param_Count         : Biggest_Natural := 0; -- Not counting defaulted ones
         Def_Param_Count        : Biggest_Natural := 0;
         Out_Param_Count        : Biggest_Natural := 0;
         In_Out_Param_Count     : Biggest_Natural := 0;
         Tagged_Param_Count     : Biggest_Natural := 0;
         Access_Param_Count     : Biggest_Natural := 0;
         Class_Wide_Param_Count : Biggest_Natural := 0;
         Nb_Names               : Biggest_Natural;
      begin
         for P : Asis.Declaration of Parameter_Profile (Good_Decl) loop
            Nb_Names := Names (P)'Length;
            if Definition_Kind (Object_Declaration_View (P)) = An_Access_Definition then
               Access_Param_Count := Access_Param_Count + Nb_Names;
            else
               case Mode_Kind (P) is
                  when Not_A_Mode =>
                     Failure ("Parameter_Declarations: not a mode", Good_Decl);
                  when An_In_Mode | A_Default_In_Mode =>
                     if Is_Nil (Initialization_Expression (P)) then
                        In_Param_Count := In_Param_Count + Nb_Names;
                     else
                        Def_Param_Count := Def_Param_Count + Nb_Names;
                     end if;
                  when An_Out_Mode =>
                     Out_Param_Count := Out_Param_Count + Nb_Names;
                  when An_In_Out_Mode =>
                     In_Out_Param_Count := In_Out_Param_Count + Nb_Names;
               end case;
            end if;
            if Rule_Used (Tagged_Parameters, C) /= Empty_Control_Kinds_Set then
               if Type_Category (Object_Declaration_View (P),
                                 Follow_Derived     => True,
                                 Privacy            => Follow_Private,
                                 Separate_Extension => False) = A_Tagged_Type
               then
                  Tagged_Param_Count := Tagged_Param_Count + Nb_Names;
               end if;
            elsif Rule_Used (Class_Wide_Parameters, C) /= Empty_Control_Kinds_Set then
               if Is_Class_Wide_Subtype (Object_Declaration_View (P)) then
                  Class_Wide_Param_Count := Class_Wide_Param_Count + Nb_Names;
               end if;
            end if;
         end loop;

         --
         -- All_Parameters
         --
         Check_Value (All_Parameters, C, Value => In_Param_Count     + Def_Param_Count
                                                + Out_Param_Count    + In_Out_Param_Count
                                                + Access_Param_Count);

         --
         -- In_Parameters
         --
         Check_Value (In_Parameters, C, Value => In_Param_Count + Def_Param_Count);

         --
         -- Defaulted_Parameters
         --
         Check_Value (Defaulted_Parameters, C, Value => Def_Param_Count);

         --
         -- Out_Parameters
         --
         Check_Value (Out_Parameters, C, Value => Out_Param_Count);

         --
         -- In_Out_Parameters
         --
         Check_Value (In_Out_Parameters, C, Value => In_Out_Param_Count);

         --
         -- Access_Parameters
         --
         Check_Value (Access_Parameters, C, Value => Access_Param_Count);

         --
         -- Tagged_Parameters
         --
         Check_Value (Tagged_Parameters, C, Value => Tagged_Param_Count);

         --
         -- Class_Wide_Parameters
         --
         Check_Value (Class_Wide_Parameters, C, Value => Class_Wide_Param_Count);

         --
         -- Single_Out_Parameter
         --
         if Out_Param_Count = 1 then
            Check_Value (Single_Out_Parameter, C, Value => In_Out_Param_Count);
         end if;

      end;
   end Process_Declaration;

begin  -- Rules.Parameter_Declarations
   Rules_Manager.Register (Rule_Id,
                           Rules_Manager.Semantic,
                           Help_CB        => Help'Access,
                           Add_Control_CB => Add_Control'Access,
                           Command_CB     => Command'Access);
end Rules.Parameter_Declarations;
