package Adactl_Options is

   type Action_Kinds is (Help, Process, Interactive_Process, Dependents);

   Action : Action_Kinds;

   Spec_Option      : Boolean;
   Recursive_Option : Boolean;
   Ignore_Option    : Boolean;
   Unit_Option      : Boolean;
   Exit_Option      : Boolean;
   Overwrite_Option : Boolean;

   procedure Analyse_Options;
   -- Analyses and sets program options

   function Asis_Options return Wide_String;
   -- Returns ASIS options passed by command line

   function Command_Line_Commands return Wide_String;
   -- Return the commands stated as options on the command line

   function Ada_Units_List return Wide_String;
   -- Returns list of Ada units to process

   function Initialize_String return Wide_String;
   -- Returns a initialize string

   Options_Error : exception;

end Adactl_Options;
