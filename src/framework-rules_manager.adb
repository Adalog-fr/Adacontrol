----------------------------------------------------------------------
--  Framework.Rules_Manager - Package body                          --
--                                                                  --
--  This software  is (c) The European Organisation  for the Safety --
--  of Air Navigation (EUROCONTROL) and Adalog 2004-2005.           --
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
  Ada.Calendar,
  Ada.Characters.Handling,
  Ada.Exceptions,
  Ada.Strings.Wide_Fixed;

-- ASIS
with
  Asis.Declarations,
  Asis.Elements,
  Asis.Expressions;

-- Adalog
with
  Binary_Map,
  String_Matching,
  Thick_Queries,
  Utilities;
pragma Elaborate_All (Utilities);
pragma Elaborate_All (Binary_Map);

-- Adacontrol
with
  Framework.Control_Manager,
  Framework.Control_Manager.Generic_Context_Iterator,
  Framework.Pattern_Queues,
  Framework.Pattern_Queues_Matchers,
  Framework.Reports,
  Framework.Variables;

package body Framework.Rules_Manager is
   use Framework.Variables;

   --
   -- User settable variables
   --
   type Timing_Switch is (Off, On, Global);
   package Timing_Switch_Type is new Discrete_Type (Timing_Switch);
   Timing_Option : aliased Timing_Switch_Type.Object := (Value => Off);


   --
   -- Local variables
   --
   Last_Rule_Name   : Wide_String (1 .. Max_Rule_Name_Length);
   Last_Rule_Length : Natural := 0;
   Last_Rule_Start  : Ada.Calendar.Time;
   Max_Name_Length  : Natural := 0;

   Nb_Rules : Rules_Count := 0;

   type Rule_Info is record
      Kind         : Extended_Rule_Kind;
      Help         : Help_Procedure;
      Add_Control  : Add_Control_Procedure;
      Command      : Command_Procedure;
      Prepare      : Prepare_Procedure;
      Finalize     : Finalize_Procedure;
      Reset        : Reset_Procedure;
      Used         : Boolean;
      Total_Time   : Duration;
      Off_Patterns : Pattern_Queues.Queue;
   end record;

   package Rule_List is new Binary_Map (Unbounded_Wide_String, Rule_Info);
   Rule_Map : Rule_List.Map;

   All_Off_Patterns : Pattern_Queues.Queue;

   Kinds_Count : array (Extended_Rule_Kind) of Rules_Count := (others => 0);

   --
   -- Inhibition
   --

   type Inhibited_Rule is new Control_Manager.Root_Context with
      record
         Rule_Name : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
         Is_Banned : Boolean;
      end record;
   Inhibited : Control_Manager.Context_Store;

   package Inhibited_Iterator is new Control_Manager.Generic_Context_Iterator (Inhibited);

   --------------
   -- Register --
   --------------

   procedure Register (Rule           : Wide_String;
                       R_Kind         : Extended_Rule_Kind;
                       Help_CB        : Help_Procedure;
                       Add_Control_CB : Add_Control_Procedure;
                       Command_CB     : Command_Procedure;
                       Prepare_CB     : Prepare_Procedure  := null;
                       Finalize_CB    : Finalize_Procedure := null;
                       Reset_CB       : Reset_Procedure    := null)
   is
      use Utilities;
   begin
      if Rule'Length > Max_Rule_Name_Length then
         Failure ("Name of rule too long");
      end if;

      if Help_CB = null or Add_Control_CB = null or Command_CB = null then
         Failure ("Missing Help, Add_Control or Command procedure");
      end if;

      Rule_List.Add (Rule_Map,
                     To_Unbounded_Wide_String (To_Upper (Rule)),
                     (R_Kind,
                      Help_CB, Add_Control_CB, Command_CB, Prepare_CB, Finalize_CB, Reset_CB,
                      Used         => False,
                      Total_Time   => 0.0,
                      Off_Patterns => Pattern_Queues.Empty_Queue));
      Nb_Rules := Nb_Rules + 1;

      if Rule'Length > Max_Name_Length then
         Max_Name_Length := Rule'Length;
      end if;
   end Register;

   ---------------------
   -- Accumulate_Time --
   ---------------------

   procedure Accumulate_Time is
      use Ada.Calendar;
      use Rule_List;
      Rule_Name : constant Unbounded_Wide_String := To_Unbounded_Wide_String (Last_Rule_Name (1 .. Last_Rule_Length));
      Info      : Rule_Info := Fetch (Rule_Map, Rule_Name);
   begin
      Info.Total_Time := Info.Total_Time + (Clock - Last_Rule_Start);
      Add (Rule_Map, Rule_Name, Info);
   end Accumulate_Time;

   -----------
   -- Enter --
   -----------

   procedure Enter (Rule : Wide_String) is
      use Ada.Calendar;
   begin
      if Timing_Option.Value /= Off and Last_Rule_Length /= 0 then
         Accumulate_Time;
      end if;

      Last_Rule_Name (1 .. Rule'Length) := Rule;
      Last_Rule_Length                  := Rule'Length;

      if Timing_Option.Value /= Off and Last_Rule_Length /= 0 then
         Last_Rule_Start := Clock;
      end if;
   end Enter;

   ---------------
   -- Last_Rule --
   ---------------

   function Last_Rule return Wide_String is
   begin
      return Last_Rule_Name (1 .. Last_Rule_Length);
   end Last_Rule;

   ------------------
   -- Is_Rule_Name --
   ------------------

   function Is_Rule_Name (Rule : Wide_String) return Boolean is
      use Rule_List;
   begin
      return Is_Present (Rule_Map, To_Unbounded_Wide_String (Rule));
   end Is_Rule_Name;

   -------------------
   -- Help_On_Rules --
   -------------------

   procedure Help_On_Rules (Pattern : Wide_String) is
      use Rule_List, String_Matching, Utilities;
      use Ada.Characters.Handling, Ada.Exceptions;

      Match_Count : Rules_Count := 0;
      Unb_Pattern  : constant Unbounded_Wide_String := To_Unbounded_Wide_String (Pattern);

      procedure One_Help (Key : in Unbounded_Wide_String; Info : in out Rule_Info) is
      begin
         if not Match (To_Wide_String (Key), Pattern) then
            return;
         end if;

         if Match_Count /= 0 then
            User_Message ("----");
         end if;
         Info.Help.all;
         Match_Count := Match_Count + 1;
      end One_Help;

      procedure Help_Iterate is new Rule_List.Iterate (One_Help);

   begin  -- Help_On_Rules
      if Is_Present (Rule_Map, To_Unbounded_Wide_String (Pattern)) then
         -- exact match
         Fetch (Rule_Map, Unb_Pattern).Help.all;
      else
         -- interpret as pattern
         Help_Iterate (Rule_Map);
         if Match_Count = 0 then
            Error ("No rule matches " & Pattern);
         end if;
      end if;
   exception
      when Occur : Pattern_Error =>
         Error ("Incorrect pattern: """ & Pattern & """, " & To_Wide_String (Exception_Message (Occur)));
   end Help_On_Rules;

   -------------------
   -- Help_On_Names --
   -------------------

   procedure Help_On_Names (Pretty : Boolean) is
      use Utilities;
      Spaces : constant Unbounded_Wide_String := 3 * ' ';
      Line : Unbounded_Wide_String;

      procedure One_Name (Key : in Unbounded_Wide_String; Info : in out Rule_Info) is
         pragma Unreferenced (Info);
         Name : constant Wide_String := To_Wide_String (Key);
      begin
         if Pretty then
            if Length (Line) + Max_Name_Length + 1 >= 80 then
               User_Message (To_Wide_String (Line));
               Line := Spaces;
            end if;
            Append (Line, To_Title (Name) & (Max_Name_Length - Name'Length +1) * ' ');
         else
            User_Message (To_Title (Name));
         end if;
      end One_Name;

      procedure Help_Iterate is new Rule_List.Iterate (One_Name);

   begin  -- Help_On_Names
      if Pretty then
         Line := Spaces;
         User_Message ("Rules:");
      end if;
      Help_Iterate (Rule_Map);
      User_Message (To_Wide_String (Line));
   end Help_On_Names;

   -----------------
   -- Add_Control --
   -----------------

   procedure Add_Control (Ctl_Label : in Wide_String;
                          Ctl_Kind  : in Control_Kinds;
                          Rule_Name : in Wide_String)
   is
      use Rule_List, Framework.Reports;

      -- Existence of rule checked by the language (ours, not Ada!)
      Info : Rule_Info := Fetch (Rule_Map, To_Unbounded_Wide_String (Rule_Name));
   begin
      Info.Add_Control (Ctl_Label, Ctl_Kind);
      if not Info.Used then
         Kinds_Count (Info.Kind) := Kinds_Count (Info.Kind) + 1;
         Info.Used := True;
         Add (Rule_Map, To_Unbounded_Wide_String (Rule_Name), Info);
      end if;

      if Ctl_Kind = Count then
         Init_Counts (Rule_Name, Ctl_Label);
      end if;
      Init_Stats (Rule_Name, Ctl_Label);
   end Add_Control;

   -----------------
   -- Prepare_All --
   -----------------

   procedure Prepare_All is
      procedure Prepare_One (Key : in Unbounded_Wide_String; Info : in out Rule_Info) is
         pragma Unreferenced (Key);
      begin
         if Info.Prepare /= null then
            Info.Prepare.all;
         end if;
      end Prepare_One;

      procedure Iterate_On_Prepare is new Rule_List.Iterate (Prepare_One);
   begin  -- Prepare_All
      Iterate_On_Prepare (Rule_Map);
   end Prepare_All;

   ------------------
   -- Finalize_All --
   ------------------

   procedure Finalize_All is
      procedure Finalize_One (Key : in Unbounded_Wide_String; Info : in out Rule_Info) is
         pragma Unreferenced (Key);
      begin
         if Info.Finalize /= null then
            Info.Finalize.all;
         end if;
      end Finalize_One;

      procedure Iterate_On_Finalize is new Rule_List.Iterate (Finalize_One);

   begin  -- Finalize_All
      Iterate_On_Finalize (Rule_Map);
   end Finalize_All;

   ---------------
   -- Reset_All --
   ---------------

   procedure Reset_All is
      procedure Reset_One (Key : in Unbounded_Wide_String; Info : in out Rule_Info) is
         pragma Unreferenced (Key);
      begin
         if Info.Reset /= null then
            Info.Reset.all;
         end if;
      end Reset_One;
      procedure Iterate_On_Reset is new Rule_List.Iterate (Reset_One);

   begin  -- Reset_All
      Iterate_On_Reset (Rule_Map);
   end Reset_All;

   -----------------
   -- Command_All --
   -----------------

   procedure Command_All (Action : Rule_Action) is
      procedure One_Command (Key : in Unbounded_Wide_String; Info : in out Rule_Info) is
         pragma Unreferenced (Key);
      begin
         Info.Command (Action);
         case Action is
            when Clear =>
               Info.Used := False;
            when others =>
               null;
         end case;
      end One_Command;

      procedure Command_Iterate is new Rule_List.Iterate (One_Command);

   begin  -- Command_All
      Command_Iterate (Rule_Map);
      case Action is
         when Clear =>
            Kinds_Count  := (others => 0);
            Framework.Reports.Clear_All;
         when others =>
            null;
      end case;
   end Command_All;

  -------------
  -- Command --
  -------------

  procedure Command (Rule_Id : in Wide_String; Action : Rule_Action) is
      use Utilities, Rule_List;

      Good_Id   : constant Wide_String           := To_Upper (Rule_Id);
      Rule_Name : constant Unbounded_Wide_String := To_Unbounded_Wide_String (Good_Id);
  begin
     declare
        Info : Rule_Info :=  Fetch (Rule_Map, Rule_Name);
     begin
        Info.Command (Action);
        case Action is
           when Clear =>
              Kinds_Count (Info.Kind) := Kinds_Count (Info.Kind) - 1;
              Info.Used := False;
              Add (Rule_Map, Rule_Name, Info);
              Framework.Reports.Clear (Good_Id);
           when others =>
              null;
        end case;
     end;
   exception
      when Not_Present =>
         Error ("Unknown rule: " & To_Wide_String (Rule_Name));
  end Command;

  ---------------------
  -- Number_Of_Rules --
  ---------------------

  function Number_Of_Rules return Rules_Count is
  begin
     return Nb_Rules;
  end Number_Of_Rules;

  ----------------------
  -- Has_Active_Rules --
  ----------------------

  function Has_Active_Rules (R_Kind : Rule_Kind) return Boolean is
  begin
     return Kinds_Count (R_Kind) + Kinds_Count (Semantic_Textual) /= 0;
  end Has_Active_Rules;

   --------------------
   -- Report_Timings --
   --------------------

   procedure Report_Timings (Global_Report : Boolean) is
      use Utilities;
      Total_Rules_Time : Duration := 0.0;

      procedure Add_One_Timing (Rule : Unbounded_Wide_String; Info : in out Rule_Info) is
         pragma Unreferenced (Rule);
      begin
         Total_Rules_Time := Total_Rules_Time + Info.Total_Time;
      end Add_One_Timing;

      procedure Add_All_Timings is new Rule_List.Iterate (Add_One_Timing);


      procedure Report_One_Timing (Rule : Unbounded_Wide_String; Info : in out Rule_Info) is
         use Framework.Reports;

         function Format_Duration (Item : Duration; Aft : Natural) return Wide_String is
            -- Remove leading space and leaves only Aft digits after decimal point
            use Ada.Strings.Wide_Fixed;
            Img     : constant Wide_String := Duration'Wide_Image (Item);
            Dot_Pos : constant Natural := Index (Img, ".");
         begin
            return Img (Img'First+1 .. Dot_Pos + Aft);
         end Format_Duration;

      begin  -- Report_One_Timing
         if Info.Total_Time = 0.0 then
            return;
         end if;

         Report_Timings (Rule             => To_Title (To_Wide_String (Rule)),
                         Duration         => Format_Duration (Info.Total_Time, Aft => 3),
                         Percent_Duration => Format_Duration ((Info.Total_Time * 100)/Total_Rules_Time, Aft => 1));
         Info.Total_Time := 0.0;
      end Report_One_Timing;

      procedure Report_All_Timings is new Rule_List.Iterate (Report_One_Timing);

   begin  -- Report_Timings
      if Timing_Option.Value = Off then
         Last_Rule_Length := 0;
         return;
      end if;

      if not Global_Report then
         if Last_Rule_Length = 0 then
            return; -- empty run
         end if;
         Accumulate_Time;  -- Close counter for last rule
      end if;
      Last_Rule_Length := 0;

      if Global_Report /= (Timing_Option.Value = Global) then
         return;
      end if;

      Add_All_Timings (Rule_Map);

      User_Message;
      User_Message ("Rules timing statistics (in s.)");
      Report_All_Timings (Rule_Map);

   end Report_Timings;

   -------------
   -- Inhibit --
   -------------

   procedure Inhibit (Rule_Name : Wide_String; Entity : Entity_Specification; Is_All : Boolean) is
      use Framework.Control_Manager;
   begin
      -- Check that inhibition is not already specified
      -- (otherwise, the suspend/resume mechanism won't work since
      -- suspensions are not stacked)
      if Rule_Name = "ALL" then
         -- Can be given only once for each unit, and is incompatible with a specific rule name
         --   => Associate in non additive mode
         --   => will raise Parameter_Error if already specified for any rule
         Associate (Inhibited,
                    Entity,
                    Inhibited_Rule'(Rule_Name => To_Unbounded_Wide_String (Rule_Name), Is_Banned => Is_All),
                    Additive => False);
      else
         -- Check that it is not already specified for "ALL". If it is, it is the only association,
         -- per previous test
         declare
            Cont : constant Root_Context'Class := Association (Inhibited, Entity);
         begin
            if Cont /= No_Matching_Context and then Inhibited_Rule (Cont).Rule_Name = "ALL" then
               raise Already_In_Store;
            end if;
         end;

         Associate (Inhibited,
                    Entity,
                    Inhibited_Rule'(Rule_Name => To_Unbounded_Wide_String (Rule_Name), Is_Banned => Is_All),
                    Additive => True);
      end if;
   end Inhibit;

   ------------------------
   -- Process_Inhibition --
   ------------------------

   procedure Process_Inhibition (Unit : Asis.Compilation_Unit; State : Framework.Rules_Manager.Rule_Action) is
      use Asis.Declarations, Asis.Elements;
      use Framework.Control_Manager;

      Iter : Context_Iterator := Inhibited_Iterator.Create;
   begin
      Reset (Iter, Names (Unit_Declaration (Unit))(1));
      if not Is_Exhausted (Iter) then
         if Inhibited_Rule (Value (Iter)).Rule_Name = "ALL" then
            Rules_Manager.Command_All (State);
            -- There is no other value
         else
            while not Is_Exhausted (Iter) loop
               Rules_Manager.Command (To_Wide_String (Inhibited_Rule (Value (Iter)).Rule_Name), State);
               Next (Iter);
            end loop;
         end if;
      end if;
   end Process_Inhibition;

   ---------------
   -- Is_Banned --
   ---------------

   function Is_Banned (Element : in Asis.Element; For_Rule : in Wide_String) return Boolean is
      use Asis, Asis.Declarations, Asis.Elements, Asis.Expressions;
      use Framework.Control_Manager, Thick_Queries, Utilities;
      Good_Elem : Asis.Element;
      Iter : Context_Iterator := Inhibited_Iterator.Create;
   begin
      case Element_Kind (Element) is
         when A_Declaration | A_Defining_Name =>
            Good_Elem := Element;
         when An_Expression =>
            case Expression_Kind (Element) is
               when An_Identifier | An_Enumeration_Literal | A_Character_Literal =>
                  Good_Elem := Corresponding_Name_Declaration (Element);
               when A_Selected_Component =>
                  Good_Elem := Corresponding_Name_Declaration (Selector (Element));
               when others =>
                  Failure ("Is_Banned: unexpected expression", Element);
            end case;
         when A_Pragma =>
            if Pragma_Kind (Element) = An_Import_Pragma then
               -- anyway, we won't analyse this...
               return False;
            end if;
            Failure ("Is_Banned: unexpected pragma", Element);
         when A_Definition =>
            case Definition_Kind (Element) is
               when An_Aspect_Specification =>
                  -- The only aspect that can appear as a body is "Import"
                  -- Same as pragma Import
                  return False;
               when others =>
                  Failure ("Is_Banned: unexpected definition");
            end case;
         when others =>
            Failure ("Is_Banned: unexpected element", Element);
      end case;

      Reset (Iter, Names (Unit_Declaration (Enclosing_Compilation_Unit (Good_Elem)))(1));
      if Is_Exhausted (Iter) then
         if Is_Part_Of_Instance (Good_Elem) then
            return Is_Banned (Generic_Unit_Name (First_Enclosing_Instantiation (Good_Elem)), For_Rule);
         else
            return False;
         end if;
      end if;

      if Inhibited_Rule (Value (Iter)).Rule_Name = "ALL" then
         return Inhibited_Rule (Value (Iter)).Is_Banned;
      end if;

      declare
         Rule_Name : constant Unbounded_Wide_String := To_Unbounded_Wide_String (For_Rule);
      begin
         while not Is_Exhausted (Iter) loop
            if Inhibited_Rule (Value (Iter)).Rule_Name = Rule_Name then
               return Inhibited_Rule (Value (Iter)).Is_Banned;
            end if;
            Next (Iter);
         end loop;
      end;

      if Is_Part_Of_Instance (Good_Elem) then
         return Is_Banned (Generic_Unit_Name (First_Enclosing_Instantiation (Good_Elem)), For_Rule);
      else
         return False;
      end if;
   end Is_Banned;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Rule_List.Balance (Rule_Map);
   end Initialize;

   ------------------
   -- File_Disable --
   ------------------

   procedure File_Disable (Rule_Id : Wide_String; Pattern : Wide_String) is
      use Framework.Pattern_Queues, Rule_List, String_Matching, Utilities;
      Rule_Name : constant Unbounded_Wide_String := To_Unbounded_Wide_String (To_Upper (Rule_Id));
   begin
      if To_Wide_String (Rule_Name) = "ALL" then
         Append (All_Off_Patterns, Compile (Pattern));
      else
         declare
            Info : Rule_Info :=  Fetch (Rule_Map, Rule_Name);
         begin
            Append (Info.Off_Patterns, Compile (Pattern));
            Add (Rule_Map, Rule_Name, Info);
         end;
      end if;
   exception
      when Pattern_Error =>
         Error ("Incorrect pattern: " & Pattern);
      when Not_Present =>
         Error ("Unknown rule: " & To_Wide_String (Rule_Name));
   end File_Disable;

   -----------------------------
   -- Initial_Disabling_State --
   -----------------------------

   function Initial_Disabling_State (Rule_Id : Wide_String; File : Wide_String) return Boolean is
      use Framework.Pattern_Queues_Matchers, Utilities;
      use Rule_List;
      Info : constant Rule_Info :=  Fetch (Rule_Map, To_Unbounded_Wide_String (To_Upper (Rule_Id)));
   begin
      return not Match_Any (File, All_Off_Patterns) and not Match_Any (File, Info.Off_Patterns);
   end Initial_Disabling_State;


begin  -- Framework.Rules_Manager
     Framework.Variables.Register (Timing_Option'Access, Variable_Name => "TIMING");
end Framework.Rules_Manager;
