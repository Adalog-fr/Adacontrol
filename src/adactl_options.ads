package Adactl_Options is

   type Action_Kinds is (Help, Process, Dependents);

   Action : Action_Kinds;

   Spec_Option      : Boolean;
   Recursive_Option : Boolean;
   Ignore_Option    : Boolean;
   Unit_Option      : Boolean;
   Exit_Option      : Boolean;

   procedure Analyse_Options;
   -- Analyses and sets program options

   function Asis_Options return Wide_String;
   -- Returns ASIS options passed by command line

   function Command_Line_Rules return String;
   -- Return rules stated on the command line (option -l)

   function Rules_File return String;
   -- Return name of the rules file (option -f)

   function Ada_Units_List return Wide_String;
   -- Returns list of Ada units to process

   function Initialize_String return Wide_String;
   -- Returns a initialize string

   procedure Finalize_Adactl_Output;
   -- Closes Adactl output if necessary

   Options_Error : exception;

end Adactl_Options;
