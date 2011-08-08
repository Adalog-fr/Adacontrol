----------------------------------------------------------------------
--  Rules.Parameter_Declarations - Package body                     --
--                                                                  --
--  This software  is (c) SAGEM DS and  Adalog  2004-2006.  The Ada --
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
  Framework.Rules_Manager,
  Framework.Reports;
pragma Elaborate (Framework.Language);

package body Rules.Parameter_Declarations is
   use Framework, Thick_Queries;

   type Subrules is (Max_Parameters, Min_Parameters, Max_Defaulted_Parameters, Single_Out_Parameter);
   subtype Valued_Subrules is Subrules range Max_Parameters .. Max_Defaulted_Parameters;
   package Subrules_Flag_Utilities is new Framework.Language.Flag_Utilities (Subrules);

   type Callable_Kinds is (C_Function,           C_Procedure,           C_Protected_Entry,
                           C_Protected_Function, C_Protected_Procedure, C_Task_Entry);
   package Callable_Kinds_Flag_Utilities  is new Framework.Language.Flag_Utilities (Callable_Kinds, Prefix => "C_");

   type Usage is array (Subrules, Callable_Kinds) of Control_Kinds_Set;
   Rule_Used : Usage := (others => (others => Empty_Control_Kinds_Set));
   Save_Used : Usage;

   Ctl_Labels : array (Subrules, Callable_Kinds, Control_Kinds) of Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
   Ctl_Values : array (Valued_Subrules, Callable_Kinds, Control_Kinds) of Biggest_Natural := (others =>
                                                                                                (others =>
                                                                                                   (others => 0)));

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message ("Rule: " & Rule_Id);
      Subrules_Flag_Utilities.Help_On_Flags (Header => "Parameter (1):");
      User_Message ("For Min_Parameters:");
      User_Message ("  Parameter (2) : minimum required number of parameters");
      User_Message ("For Max_Parameters:");
      User_Message ("  Parameter (2) : maximum allowed number of parameters");
      User_Message ("For Max_Defaulted_Parameters:");
      User_Message ("  Parameter (2) : maximum allowed number of parameters with default values");
      User_Message ("For Single_Out_Parameter:");
      User_Message ("  No value allowed");
      Callable_Kinds_Flag_Utilities.Help_On_Flags (Header => "Other parameters:",
                                           Footer => "(optional, default = all)");
      User_Message ("Controls form and metrics of parameters of callable entities");
   end Help;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind : in Control_Kinds) is
      use Framework.Language, Callable_Kinds_Flag_Utilities, Subrules_Flag_Utilities;
      use Ada.Strings.Wide_Unbounded;
      Subrule  : Subrules;
      Callable : Callable_Kinds;
      Value    : Biggest_Int;
   begin
      if not Parameter_Exists then
         Parameter_Error (Rule_Id, "subrule not specified");
      end if;

      Subrule := Get_Flag_Parameter (Allow_Any => False);

      case Subrule is
         when Max_Parameters | Max_Defaulted_Parameters =>
            if not Parameter_Exists or else not Is_Integer_Parameter then
               Parameter_Error (Rule_Id, "missing max allowed value");
            end if;
            Value := Get_Integer_Parameter (Min => 0);
         when Min_Parameters =>
            if not Parameter_Exists or else not Is_Integer_Parameter then
               Parameter_Error (Rule_Id, "missing min required value");
            end if;
            Value := Get_Integer_Parameter (Min => 1);
         when Single_Out_Parameter =>
            if Parameter_Exists and then Is_Integer_Parameter then
               Parameter_Error (Rule_Id, "no value allowed for Single_Out_Parameter");
            end if;
      end case;

      if Parameter_Exists then
         while Parameter_Exists loop
            Callable := Get_Flag_Parameter (Allow_Any => False);
            if Rule_Used (Subrule, Callable)(Ctl_Kind) then
               Parameter_Error (Rule_Id, Image (Callable)
                                         & " already specified for "
                                         & Control_Kinds'Wide_Image (Ctl_Kind));
            end if;
            Ctl_Labels (Subrule, Callable, Ctl_Kind) := To_Unbounded_Wide_String (Ctl_Label);
            if Subrule in Valued_Subrules then
               Ctl_Values (Subrule, Callable, Ctl_Kind) := Value;
            end if;
            Rule_Used  (Subrule, Callable)(Ctl_Kind) := True;
         end loop;
      else
         -- no callable kind specified => applies to all
         for C in Callable_Kinds loop
            Ctl_Labels (Subrule, C, Ctl_Kind) := To_Unbounded_Wide_String (Ctl_Label);
            if Subrule in Valued_Subrules then
               Ctl_Values (Subrule, C, Ctl_Kind) := Value;
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
      use Utilities;

      Good_Decl : Asis.Declaration := Declaration;

      procedure Do_Report (Subrule  : Subrules;
                           Ctl_Kind : Control_Kinds;
                           Entity   : Callable_Kinds;
                           Value    : Biggest_Int)
      is
         use Ada.Strings.Wide_Unbounded;
         use Callable_Kinds_Flag_Utilities, Framework.Reports;

         Loc : constant Location := Get_Location (Names (Declaration)(1));
      begin

         case Subrule is
            when Max_Parameters =>
               Report (Rule_Id,
                       To_Wide_String (Ctl_Labels (Max_Parameters, Entity, Ctl_Kind)),
                       Ctl_Kind,
                       Loc,
                       "more than " & Biggest_Int_Img (Ctl_Values (Max_Parameters, Entity, Ctl_Kind))
                       & " parameters in " & Image (Entity)
                       & " ("   & Biggest_Int_Img (Value) & ')');
            when Max_Defaulted_Parameters =>
               Report (Rule_Id,
                       To_Wide_String (Ctl_Labels (Max_Parameters, Entity, Ctl_Kind)),
                       Ctl_Kind,
                       Loc,
                       "more than " & Biggest_Int_Img (Ctl_Values (Max_Defaulted_Parameters, Entity, Ctl_Kind))
                       & " defaulted parameters in " & Image (Entity)
                       & " ("   & Biggest_Int_Img (Value) & ')');
            when Min_Parameters =>
               Report (Rule_Id,
                       To_Wide_String (Ctl_Labels (Min_Parameters, Entity, Ctl_Kind)),
                       Ctl_Kind,
                       Loc,
                       "less than " & Biggest_Int_Img (Ctl_Values (Min_Parameters, Entity, Ctl_Kind))
                       & " parameters in " & Image (Entity)
                       & " ("   & Biggest_Int_Img (Value) & ')');
            when Single_Out_Parameter =>
               Report (Rule_Id,
                       To_Wide_String (Ctl_Labels (Single_Out_Parameter, Entity, Ctl_Kind)),
                       Ctl_Kind,
                       Loc,
                       "Single out parameter in " & Image (Entity));
         end case;
      end Do_Report;


      C : Callable_Kinds;


   begin  -- Process_Declaration
      if Rule_Used = (Subrules => (Callable_Kinds => Empty_Control_Kinds_Set)) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      case Declaration_Kind (Good_Decl) is
         when A_Procedure_Body_Declaration
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

      case Declaration_Kind (Good_Decl) is
         when A_Procedure_Declaration =>
            if Definition_Kind (Enclosing_Element (Good_Decl)) = A_Protected_Definition then
               C := C_Protected_Procedure;
            else
               C := C_Procedure;
            end if;
         when A_Generic_Procedure_Declaration
            | A_Procedure_Body_Declaration
            | A_Procedure_Body_Stub
              =>
            C := C_Procedure;
         when A_Procedure_Instantiation =>
            C         := C_Procedure;
            Good_Decl := Corresponding_Declaration (Good_Decl);

         when A_Function_Declaration =>
            if Definition_Kind (Enclosing_Element (Good_Decl)) = A_Protected_Definition then
               C := C_Protected_Function;
            else
               C := C_Function;
            end if;
         when A_Generic_Function_Declaration
            | A_Function_Body_Declaration
            | A_Function_Body_Stub
              =>
            C := C_Function;
         when A_Function_Instantiation =>
            C         := C_Function;
            Good_Decl := Corresponding_Declaration (Good_Decl);

         when An_Entry_Declaration =>
            if Is_Task_Entry (Good_Decl) then
               C := C_Task_Entry;
            else
               C := C_Protected_Entry;
            end if;
         when others =>
            -- We don't call this procedure on entry bodies, since those always have
            -- a specification
            Failure ("not a callable entity");
      end case;

      declare
         Profile         : constant Parameter_Specification_List := Parameter_Profile (Good_Decl);
         Param_Count     : Biggest_Natural := 0;
         Def_Param_Count : Biggest_Natural := 0;
         Out_Param_Count : Biggest_Natural := 0;
         Nb_Names        : Biggest_Natural;
      begin
         for P in Profile'Range loop
            Nb_Names := Names (Profile (P))'Length;
            Param_Count := Param_Count + Nb_Names;
            if Mode_Kind (Profile (P)) = An_Out_Mode then
               Out_Param_Count := Out_Param_Count + Nb_Names;
            end if;
            if not Is_Nil (Initialization_Expression (Profile (P))) then
               Def_Param_Count := Def_Param_Count + Nb_Names;
            end if;
         end loop;

         --
         -- Max_Parameters
         --
         -- Note that allowed values are at least 0, therefore the checks cannot fail
         -- if Profile'Length = 0
         if Rule_Used (Max_Parameters, C) (Check)
           and then Param_Count > Ctl_Values (Max_Parameters, C, Check)
         then
            Do_Report (Max_Parameters, Check, C, Param_Count);
         elsif Rule_Used (Max_Parameters, C) (Search)
           and then Param_Count > Ctl_Values (Max_Parameters, C, Search)
         then
            Do_Report (Max_Parameters, Search, C, Param_Count);
         end if;

         if Rule_Used (Max_Parameters, C) (Count)
           and then Param_Count > Ctl_Values (Max_Parameters, C, Count)
         then
            Do_Report (Max_Parameters, Count, C, Param_Count);
         end if;

         --
         -- Max_Defaulted_Parameters
         --
         if Rule_Used (Max_Defaulted_Parameters, C) (Check)
           and then Def_Param_Count > Ctl_Values (Max_Defaulted_Parameters, C, Check)
         then
            Do_Report (Max_Defaulted_Parameters, Check, C, Def_Param_Count);
         elsif Rule_Used (Max_Defaulted_Parameters, C) (Search)
           and then Def_Param_Count > Ctl_Values (Max_Defaulted_Parameters, C, Search) then
            Do_Report (Max_Defaulted_Parameters, Search, C, Def_Param_Count);
         end if;

         if Rule_Used (Max_Defaulted_Parameters, C) (Count)
           and then Def_Param_Count > Ctl_Values (Max_Defaulted_Parameters, C, Count)
         then
            Do_Report (Max_Defaulted_Parameters, Count, C, Def_Param_Count);
         end if;

         --
         -- Min_Parameters
         --
         -- It is here possible to fail with 0 parameters. Always refer to the declararation.
         if Rule_Used (Min_Parameters, C) (Check)
           and then Param_Count < Ctl_Values (Min_Parameters, C, Check)
         then
            Do_Report (Min_Parameters, Check, C, Param_Count);
         elsif Rule_Used (Min_Parameters, C) (Search)
           and then Param_Count < Ctl_Values (Min_Parameters, C, Search)
         then
            Do_Report (Min_Parameters, Search, C, Param_Count);
         end if;

         if Rule_Used (Min_Parameters, C) (Count)
           and then Param_Count < Ctl_Values (Min_Parameters, C, Count)
         then
            Do_Report (Min_Parameters, Count, C, Param_Count);
         end if;

         --
         -- Single_Out_Parameter
         --
         -- Note that 0 is not controlled, therefore the checks cannot fail
         -- if Profile'Length = 0
         if Rule_Used (Single_Out_Parameter, C) (Check) and then Out_Param_Count = 1 then
            Do_Report (Single_Out_Parameter, Check, C, 1);
         elsif Rule_Used (Single_Out_Parameter, C) (Search) and then Out_Param_Count = 1 then
            Do_Report (Single_Out_Parameter, Search, C, 1);
         end if;

         if Rule_Used (Single_Out_Parameter, C) (Count) and then Out_Param_Count = 1 then
            Do_Report (Single_Out_Parameter, Count, C, 1);
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
