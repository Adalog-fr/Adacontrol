----------------------------------------------------------------------
--  Framework.Reports - Package specification                       --
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

-- AdaControl
with
   Framework.Control_Manager,
   Framework.Variables,
   Framework.Locations;

package Framework.Reports is
   use Framework.Locations;

   procedure Report (Rule_Id   : in Wide_String;
                     Ctl_Label : in Wide_String;
                     Ctl_Kind  : in Control_Kinds;
                     Loc       : in Location;
                     Msg       : in Wide_String);
   -- Reports rule match to current output

   procedure Report (Rule_Id    : in Wide_String;
                     Context    : in Control_Manager.Root_Context'Class;
                     Loc        : in Location;
                     Msg        : in Wide_String);
   -- Id, but get Rule_Label and Rule_Type from Context
   -- Context must be either No_Matching_Context (and Report does nothing)
   --   or a descendant of Simple_Context.

   procedure Uncheckable (Rule_Id : in Wide_String;
                          Risk    : in Uncheckable_Consequence;
                          Loc     : in Location;
                          Msg     : in Wide_String);
   -- Called when a rule cannot check something, because f.e. it depends on a non
   -- statically analyzable construct

   procedure Clear_Delayed_Uncheckable_Messages (Rule_Id : in Wide_String);
   -- False positive messages are delayed until there is an actual Report from the same rule.
   -- If it is known that there will be no (appropriate) report, this procedure clears all pending
   -- messages.

   Cancellation : exception;
   -- Raised when the number of allowed errors/messages is exceeded
   -- Occurrence message gives the exact cause


   ----------------------------------------------------------------------------------------
   --
   --  Declarations below this line are for the use of the framework
   --

   -- These two procedures are for the rule Uncheckable
   procedure Set_Uncheckable (Risk : Uncheckable_Consequence; Ctl_Kind : Control_Kinds; Label : Wide_String);
   procedure Reset_Uncheckable;

   procedure Raw_Report (Message : Wide_String);
   -- Just output the string without formating

   function Generate_Fixes return Boolean;
   -- Are fixes to be generated, depending on option, kind and possible inhibition of previous message...

   type Output_Format is (Source, Gnat, CSV, CSVX, None);
   -- None must stay last
   procedure Set_Output_Format (Value : Wide_String);
   -- Raises Constraint_Error if Value is not correct

   type Stats_Levels  is range 0 .. 3;
   No_Stats   : constant Stats_Levels := 0;
   General    : constant Stats_Levels := 1;
   Nulls_Only : constant Stats_Levels := 2;
   Full       : constant Stats_Levels := 3;
   package Stats_Levels_Type is new Framework.Variables.Integer_Type (Stats_Levels);

   Just_Created : Boolean := True;  -- State for Console, since output is Console when the program starts
   Total_Lines  : Natural := 0;
   -- Lines are counted by Semantic_Traverse during the first run only (to avoid counting many times, and since
   -- units can't change between runs, it's sufficient). If there are no semantic rules, they are counted by
   -- Textual_Traverse. Note that stubs are also traversed by Semantic_Traverse and/or Textual_Traverse, so there
   -- is no need to special-case them.

   function Nb_Errors   return Natural;
   function Nb_Warnings return Natural;

   procedure Clear (Rule : Wide_String);
   procedure Clear_All;
   procedure Reset;

   procedure Init_Counts (Rule : Wide_String; Label : Wide_String);
   procedure Report_Counts;

   procedure Init_Stats  (Rule : Wide_String; Label : Wide_String);
   procedure Report_Stats;

   procedure Report_Timings (Rule, Duration, Percent_Duration : Wide_String);
end Framework.Reports;
