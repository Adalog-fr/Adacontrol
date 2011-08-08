"""  This file adds support for AdaControl
     It provides a new menu, entries in the "help" menu, and
     buttons to make use of AdaControl easier
"""

#######################################################################
import GPS, os, glob, re, sets

def pre_clean ():
   """clean-up various windows before running AdaControl
   """
   global adactl_cats, result

   GPS.MDI.save_all (GPS.Preference ("force-save").get())
   for C in adactl_cats:
      GPS.Locations.remove_category (C)
   adactl_cats = sets.Set()
   result      = ""
   GPS.MDI.get("Messages").raise_window()

def post_clean ():
   """Clean-up after running AdaControl
   """
   # Raise "Locations" window from menu, because it works even if the window has not been created yet
   # GPS.execute_action ("/Window/Locations");
   loc = GPS.MDI.get ("Locations")
   loc.raise_window ()
   if GPS.Preference ("delete-trees").get():
      del_tree (confirm=False)

def command_name ():
   """Get the command name for AdaControl
   """
   return GPS.Project.root().get_attribute_as_string("Compiler_Command", "Ide", "adacontrol") or "adactl"

def Project_units ():
   """Make a '+'-separated list of all units in the project
   """
   files=GPS.Project.root().sources()
   result= ""
   prev_unit=""
   for f in files:
      if f.language () == "ada":
         unit= os.path.splitext(os.path.basename (f.name()))[0]
         unit= re.sub("-", ".", unit)
         if unit != prev_unit:
            result= result + '+' + unit
            prev_unit=unit
   return result

def parse (output):
   """Sort and parse the result of running Adacontrol
   """
   global adactl_cats,long_mess_str, long_mess_pat, short_mess_pat, rule_statistics_pat\
             ,message_statistics_pat

   def sloc_cmp (l,r):
      """Compare two source references (file:line:col)
         If the references are the same, keep the original order
         (for the case of error messages that span several lines)
      """
      ls=l.split(':');
      rs=r.split(':');
      if len(ls[0]) == 1:
         # Presumably, we have a "C:"...
         Offset=1
      else:
         Offset=0

      if len(ls) < 5+Offset and len(rs) < 3+Offset:
         # No reference =>Keep order
         return -1
      if len(ls) < 5+Offset:
         # Reference only in r: put references ahead
         return 1
      if len(rs) < 5+Offset:
         # Reference only in l: put references ahead
         return -1

      if ls[0+Offset] != rs [0+Offset]:
         return cmp (ls[0+Offset], rs[0+Offset])
      if ls[1+Offset] != rs[1+Offset]:
         return cmp (int(ls[1+Offset]), int(rs[1+Offset]))
      return cmp (int(ls[2+Offset]), int(rs[2+Offset]))

   list=output.splitlines()
   list.sort(sloc_cmp)
   pos=0
   category = "Adacontrol"
   sep = ';'
   for Mess in list:
     pos+=1
     if long_mess_pat.match (Mess):
       # Regular AdaCtl message
       if GPS.Preference ("separate-rules").get():
          category = long_mess_pat.sub (r"\8", Mess)
       adactl_cats.add (category)
       GPS.Locations.parse (Mess,
         category,
         long_mess_str,
         1, 2, 3, 4, 6, -1,
         "Builder results",
         "Style errors",
         "Builder warnings")
     elif short_mess_pat.match (Mess):
       # AdaCtl message without location, or just a column location (syntax error in interactive command)
       if GPS.Preference ("separate-rules").get():
          category = short_mess_pat.sub (r"\3", Mess)
       adactl_cats.add (category)
       message  = short_mess_pat.sub (r"\2", Mess) + short_mess_pat.sub (r"\3", Mess)
       try:
          GPS.Locations.add (category, GPS.File("none"), 1, 1, message)
       except:
          # Always an exception, since file "none" does not exist (presumably)
          pass
     elif rule_statistics_pat.match (Mess):
       # Rule statistics summary
       adactl_cats.add ("Statistics")
       try:
          GPS.Locations.add ("Statistics",
             GPS.File(rule_statistics_pat.sub(r"\1", Mess)), 1, 1,
             rule_statistics_pat.sub(r"\2", Mess))
       except:
          # Always an exception, since file does not exist
          pass
     elif message_statistics_pat.match (Mess):
       # Message statistics summary
       adactl_cats.add ("Statistics")
       try:
          GPS.Locations.add ("Statistics",
             GPS.File(message_statistics_pat.sub(r"\1", Mess)), 1, 1,
             message_statistics_pat.sub(r"\2", Mess))
       except:
          # Always an exception, since file does not exist
          pass
     else:
       # Assume it is a compilation message
       if not Mess in list[max (0,pos-5):pos-1]:
         GPS.Locations.parse (Mess, "Compilation")
         adactl_cats.add ("Compilation")

def load_result (file = ""):
   """Load a result file in location window
   """
   global previous_locfile, adactl_cats

   if not file:
      previous_locfile = previous_locfile or get_file("-o")
      value=GPS.MDI.input_dialog ("Load result", "File=" + previous_locfile)
      if value == [] or value [0] == "":
         # User cancelled action
         return
      previous_locfile = value[0]
   else:
      previous_locfile = file

   try:
      f = open (previous_locfile, 'r')
   except:
      GPS.MDI.dialog ("File " + previous_locfile + " not found in " + GPS.pwd())
      return

   for C in adactl_cats:
      GPS.Locations.remove_category (C)
   adactl_cats = sets.Set()

   try:
      parse (f.read())
   except:
      GPS.MDI.dialog ("File " + previous_locfile + " is not a valid result file");
      return
   GPS.MDI.get("Locations").raise_window()

def del_tree (confirm):
   """Ask for confirmation, then delete tree files (and possibly .ali files)
   """
   dir      = os.getcwd()
   ali_also = GPS.Preference ("delete-ali").get()
   if ali_also:
      supp = "and .ali "
   else:
      supp = ""
   if not confirm or GPS.MDI.yes_no_dialog ("Remove all tree "
                                            + supp
                                            + "files from " + dir + "?"):
      for I in glob.glob (os.path.join (dir, "*.adt")):
         os.remove (I)
      if ali_also:
         for I in glob.glob (os.path.join (dir, "*.ali")):
            os.remove (I)

def create_adp ():
   """Create adp file from project
   """
   name = get_file ("-p")
   if not name:
      name= GPS.Project.file(GPS.Project.root()).name()[:-4]+".adp"
   value = GPS.MDI.input_dialog ("Create .adp file",  "File=" + name)
   if value != [] and value [0] != "":
      name = value [0]
      f=open (name, 'w')
      for I in GPS.Project.dependencies (GPS.Project.root(), recursive=True) :
         for J in GPS.Project.source_dirs(I) :
            f.write ("src_dir=" + J + "\n")
         for J in GPS.Project.object_dirs(I) :
            f.write ("obj_dir=" + J + "\n")
      f.close()
      GPS.MDI.dialog ("Project file " + name + " created")

def create_units ():
   """Create units file from project
   """
   name = get_file ("-@")
   if not name:
      name= GPS.Project.file(GPS.Project.root()).name()[:-4]+".txt"
   value = GPS.MDI.input_dialog ("Create units file",  "File=" + name)
   if value != [] and value [0] != "":
      name = value [0]
      unitsFile=open (name, 'w')
      files=GPS.Project.root().sources()
      prev_unit=""
      for f in files:
         if f.language() == "ada":
            unit= os.path.splitext(os.path.basename (f.name()))[0]
            unit= re.sub("-", ".", unit)
            if unit != prev_unit:
                unitsFile.write(unit+"\n")
                prev_unit=unit
      unitsFile.close()
      print ("Units file " + name + " created")

def get_file (option):
   """Get the file corresponding to the given option
   """
   opt_list=GPS.Project.root().get_tool_switches_as_list("AdaControl")
   try:
       return opt_list[ opt_list.index(option) + 1 ]
   except:
       return ""

def rules_file_defined ():
   if get_file ("-f"):
      return "true"
   else:
      return "false"

def units_file_defined ():
   if get_file ("-@"):
      return "true"
   else:
      return "false"

def options (rules, files):
   """Builds the options string
      Options -f and @ (actually -@) are defined as switches for the convenience
      of the user but they are actually passed as explicit parameters depending on
      the way Adacontrol is launched. We must therefore remove these options from
      the switches string provided by GPS.
      Similarly, the -o option depends on the setting of the (pseudo) switches
      -GPS and -NOGPS
   """
   global output_name, gps, previous_command

   result = "-v"

   # set files
   if rules != "check" and files == "":
      tmp = GPS.Preference ("button-target").get();
      if tmp == "Current File":
         files = "current"
      elif tmp == "Root Project":
         files = "project"
      else: # Units from list
         files = "list"

   if rules == "check":
      result = "-C " + result
   elif files == "current":
      try:
         win = GPS.current_context().file()
      except:
         GPS.MDI.dialog ("no active window")
         raise ValueError
      if win.language() != "ada" :
         GPS.MDI.dialog ("active window is not Ada")
         raise ValueError
      result = result + ' ' + win.name()
   elif files == "list":
      result = result + " @" + get_file("-@")
   else:   # "project"
      result = result + ' ' + Project_units()

   # set rules
   if rules == "ask":
      value=GPS.MDI.input_dialog ("Interactive run", "Command(s)=" + previous_command)
      if value != [] and value [0] != "":
         previous_command= value[0]
         res = run("check", previous_command)
         if res != 0:
            raise ValueError
         result = result + " -l \"\"\"" + re.sub('"', '~', previous_command) + "\"\"\""
      else:
         raise ValueError
   elif rules == "file":
      if not get_file("-f") :
         GPS.MDI.dialog ("no rules file defined in AdaControl properties")
         raise ValueError
      result = result + " -f " + get_file("-f")
   else:   #  "check"
      if files == "":
         try:
            win = GPS.current_context().file()
         except:
            GPS.MDI.dialog ("no active window")
            raise ValueError
         if win.language() != "adacontrol" :
            GPS.MDI.dialog ("active window is not AdaControl rules file")
            raise ValueError
         result = result + " -f " + win.name()
      else:
         result = result + " -l \"\"\"" + re.sub('"', '~', files) + "\"\"\""

   # set other_params
   gps         = True
   outfile     = False
   p_option    = False
   output_name = ""
   ASIS_option = ""

   opt_list      = GPS.Project.root().get_tool_switches_as_list("AdaControl")
   skip_next     = False
   next_is_ofile = False
   next_is_pfile = False
   next_is_ASIS  = False
   for O in opt_list:
      if len(O) == 0:
         pass
      elif next_is_ofile:
         output_name  = O
         next_is_ofile = False
      elif next_is_pfile:
         if rules != "check":
            result = result + ' ' + O
         next_is_pfile = False
      elif next_is_ASIS:
         if rules != "check":
            ASIS_option = O
      elif skip_next:
         skip_next = False
      elif O[0] == "@":
         # A remaining of the old syntax, ignore
         pass
      elif O == "-@":
         skip_next = True
      elif O == "--":
         next_is_ASIS = True
      elif O == "-GPS":
         gps     = True
         outfile = True
      elif O == "-NOGPS":
         gps     = False
         outfile = True
      elif O == "-f":
         skip_next = True
      elif O == "-F" and gps:
         skip_next = True
      elif O[0:2] == "-F":
         # Bug in GPS: Combo does not add separator
         if not gps:
            result = result + ' ' + "-F " + O[2:]
      elif O == "-o":
         next_is_ofile = True
      elif O == "-p":
         p_option      = True
         next_is_pfile = True
         if rules != "check":
            result = result + ' ' + O
      elif O[0:2] == "-S" and O[2:] != "":
         # Bug in GPS: Combo does not add separator
         result = result + ' ' + "-S " + O[2:]
      else:
         result = result + ' ' + O

   val = GPS.Preference ("max-messages").get()
   if val != 0:
      result = result + ' ' + "-M " + repr(val)

   val = GPS.Preference ("max-errors").get()
   if val != 0:
      result = result + ' ' + "-m " + repr(val)

   if outfile:
      if output_name == "":
         GPS.MDI.dialog ("Result file is not defined, please set it in Project properties/Switches/AdaControl")
         raise ValueError
      elif gps:
         # always override
         result= result + " -wo " + output_name
      elif len (glob.glob(output_name)) != 0:
         if GPS.MDI.yes_no_dialog ("File " + output_name + " exists, override?"):
            result= result + " -wo " + output_name
         else:
            raise ValueError
      else:
         result= result + " -o " + output_name
   else:
      # file is specified but not used
      output_name = ""


   if ASIS_option or not p_option:
      result = result  + " -- "
      if ASIS_option:
         result = result + ASIS_option

      if not p_option:
         for I in GPS.Project.dependencies (GPS.Project.root(), recursive=True) :
            for J in GPS.Project.source_dirs(I) :
               result = result + " -I" + J.replace("\\", "/")

   return result

def process_line (process, matching, rest):
   global result
   if GPS.Preference ("display-run").get():
      print matching
   result = result + '\n' + matching

def run (rules, files):
   """Run Adacontrol
      rules = "ask" | "file" | "check"
      files = "current" | "list" | "project" | "" (means: depending on button definition)
   """
   global previous_command, result
   global output_name, gps

   try:
     command_line = command_name () + ' ' + options (rules, files)
   except ValueError:
     return

   pre_clean()
   if GPS.Preference ("display-run").get():
      print command_line
   proc = GPS.Process (command          = command_line,
                       task_manager     = True,
                       regexp           = ".+",
                       on_match         = process_line,
                       progress_regexp  = "^\((\d+)/(\d+)\)",
                       progress_current = 1,
                       progress_total   = 2)
   res=proc.wait()
   parse (result)
   if gps and output_name:
      load_result (output_name)
   post_clean()

   # return the return code only if checking interactive rule syntax
   if rules == "check" and files:
      return res

def Help_On_Rule (self):
   """provide help in order to use AdaControl
   """
   if GPS.Preference ("help-format").get() == "Pop-up":
     proc = GPS.Process (command_name()+" -h " + self.name)
     GPS.MDI.dialog (proc.get_result())
   else:
     GPS.HTML.browse ("adacontrol_ug.html", self.name, navigation = False)

def Add_Rule_Menu (self, matched, unmatched):
   """Add a new entry to the "Help on rule" menu
   """

   # '_' is interpreted as a special mark in menus, double it
   entry=GPS.Menu.create ("Help/AdaControl/Help on rule/" + matched.replace("_","__"),
                          Help_On_Rule,
                          ref="About",
                          add_before=True);
   entry.name=matched

def about ():
   proc = GPS.Process (command_name()+" -h version license")
   GPS.MDI.dialog (proc.get_result())

def on_pref_changed (H):
   """Hook on preference changes
   """
   if GPS.Preference ("delete-ali").get():
      DelTree_Menu.rename("Delete Tree and .ali Files")
   else:
      DelTree_Menu.rename("Delete Tree Files")

def on_GPS_start (H):
   """Hook on GPS start
      Initialization actions
      They are defined as a hook rather than directly in this file, because when this file
       is elaborated, the GPS context is not yet set (project variables for example)
   """
   global adactl_cats, previous_command, previous_locfile, DelTree_Menu\
             ,long_mess_str, long_mess_pat, short_mess_pat, rule_statistics_pat, message_statistics_pat

   # Global variables initialization
   adactl_cats      = sets.Set()
   previous_command = ""
   previous_locfile = ""

   #                  1    2     3      456        7                           8
   long_mess_str = r"^(.+):(\d+):(\d+): (((Found:)|(Error:|Parameter:|Syntax:))( .*?):.*)$"
   long_mess_pat = re.compile (long_mess_str)
   #                               1       2                                  3
   short_mess_pat = re.compile (r"^(\d+: )?(Found:|Error:|Parameter:|Syntax:) (.*)$")
   #                                    1     23                                     4
   rule_statistics_pat = re.compile (r"^(.*): ((Check: \d+, Search: \d+, Count: \d+)|(not triggered))$")
   #                                      1                  2
   message_statistics_pat = re.compile(r"^(Issued messages:) (.*)$")


   # We must define the buttons here in order to compute the place of the icons from
   # the GPS directory, but we cannot call GPS.Button(), because it does not allow
   # the declaration of an icon (hence we use parse_xml).
   GPS.parse_xml("""
   <button action='Check_Unknown_File'>
      <title>Launch AdaControl (rules file)</title>
      <pixmap>"""
              + GPS.get_system_dir()
              + """share/gps/plug-ins/adactl.gif</pixmap>
   </button>
   <button action='Check_Unknown_Ask'>
      <title>Launch AdaControl (interactive)</title>
      <pixmap>"""
              + GPS.get_system_dir()
              + """share/gps/plug-ins/adactl_ask.gif</pixmap>
   </button>
   """)

   # Create the Help/Adacontrol/Help rule menu
   GPS.Process (command_name()+" -h list", ".+", Add_Rule_Menu)

   # Create hook on preference changes, and keep the "Delete Tree" menu in it
   GPS.Hook("preferences_changed").add (on_pref_changed)
   DelTree_Menu = GPS.Menu.get ("AdaControl/Delete Tree Files")
   on_pref_changed (GPS.Hook("preferences_changed"))

################################################################
# Create hook on GPS start
GPS.Hook("gps_started").add (on_GPS_start)
