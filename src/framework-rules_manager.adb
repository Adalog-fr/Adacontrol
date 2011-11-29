----------------------------------------------------------------------
--  Framework.Rules_Manager - Package body                          --
--                                                                  --
--  This software  is (c) The European Organisation  for the Safety --
--  of Air  Navigation (EUROCONTROL) and Adalog  2004-2005. The Ada --
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
  Ada.Calendar;

-- ASIS
with
  Asis.Declarations,
  Asis.Elements;

-- Adalog
with
  String_Matching,
  Utilities,
  Binary_Map;
pragma Elaborate_All (Utilities);
pragma Elaborate_All (Binary_Map);

-- Adacontrol
with
  Framework.Control_Manager,
  Framework.Control_Manager.Generic_Context_Iterator,
  Framework.Reports;

package body Framework.Rules_Manager is
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
      Used         : Boolean;
      Total_Time   : Duration;
   end record;

   package Rule_List is new Binary_Map (Unbounded_Wide_String, Rule_Info);
   Rule_Map : Rule_List.Map;

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
                       Finalize_CB    : Finalize_Procedure := null) is
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
                      Help_CB, Add_Control_CB, Command_CB, Prepare_CB, Finalize_CB,
                      Used  => False,
                      Total_Time => 0.0));
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
      if Timing_Option and Last_Rule_Length /= 0 then
         Accumulate_Time;
      end if;

      Last_Rule_Name (1 .. Rule'Length) := Rule;
      Last_Rule_Length                  := Rule'Length;

      if Timing_Option then
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
      Match_Count : Natural := 0;
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
                          Rule_Name : in Wide_String) is
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

   procedure Report_Timings is
      use Framework.Reports, Utilities;

      procedure Report_One_Timing (Rule : Unbounded_Wide_String; Info : in out Rule_Info) is
      begin
         if Info.Total_Time = 0.0 then
            return;
         end if;

         Raw_Trace (To_Wide_String (Rule)
                    and Duration'Wide_Image (Info.Total_Time)
                        & Choose (Format_Option in CSV .. None, "", "s."));
         Info.Total_Time := 0.0;
      end Report_One_Timing;

      procedure Report_All_Timings is new Rule_List.Iterate (Report_One_Timing);

   begin  -- Report_Timings
      if Last_Rule_Length = 0 then
         --? empty run
         return;
      end if;

      if Timing_Option then
         Accumulate_Time;

         if Format_Option in CSV .. CSVX then
            Raw_Trace ("Rule" and "Time");
         else
            Raw_Trace ("Rules timing statistics (in s.)");
         end if;
         Report_All_Timings (Rule_Map);
      end if;

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
      use Framework, Framework.Control_Manager;

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
      use Asis.Declarations, Asis.Elements;
      use Framework.Control_Manager;
      Iter : Context_Iterator := Inhibited_Iterator.Create;
   begin
      Reset (Iter, Names (Unit_Declaration (Enclosing_Compilation_Unit (Element)))(1));
      if Is_Exhausted (Iter) then
         return False;
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

      return False;
   end Is_Banned;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Rule_List.Balance (Rule_Map);
   end Initialize;
end Framework.Rules_Manager;
