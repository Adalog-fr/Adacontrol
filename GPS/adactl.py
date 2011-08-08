#  This file adds support for AdaControl

import GPS, os, glob, re, sets

# Clean-up various windows before running AdaControl
def pre_clean ():
   GPS.MDI.save_all (False)
   for C in adactl_cats:
      GPS.Locations.remove_category (C)
   GPS.MDI.get("Messages").raise_window()

# Clean-up after running AdaControl
def post_clean ():
   if GPS.Preference ("delete-trees").get():
      del_tree (confirm=False)

# Get the command name for AdaControl
def command_name ():
   result = GPS.Project.root().get_attribute_as_string("compiler_command", "Ide", "AdaControl")
   if result == "":
      return "adactl"
   else:
      return result

# Builds the options string
# Options -f and @ are defined as switches for the convenience of the user
# but they are actually passed as explicit parameters depending on the way
# Adacontrol is launched. We must therefore remove these options from the
# switches string provided by GPS.
# Similarly, the -o option depends on the setting of the (pseudo) switch
# -NOGPS
def options ():
   opt_list=GPS.Project.root().get_tool_switches_as_list("AdaControl")
   gps=True
   result="-v "
   skip_next=False
   outfile=""
   next_is_file=False
   for O in opt_list:
      if next_is_file:
         outfile=O
         result= result + ' ' + O
         next_is_file=False
      elif skip_next:
         skip_next=False
      elif O == "-NOGPS":
         gps=False
      elif O == "-F" and gps:
         skip_next=True
      elif O == "-o":
         if gps:
            skip_next=True
         else:
            next_is_file=True
            result= result + ' ' + O
      elif O[0] == "@":
         pass
      elif O == "-f":
         skip_next=True
      else:
         result= result + ' ' + O

   if not gps:
      if outfile == "":
         GPS.MDI.dialog ("Result file is not defined, please set it in Project properties/Switches/AdaControl")
         raise ValueError
      elif len (glob.glob(outfile)) != 0:
         if GPS.MDI.yes_no_dialog ("File " + outfile + " exists, override?"):
            result= result + " -w"
         else:
            raise ValueError

   result = result  + " --"
   for I in [GPS.Project.root()] + GPS.Project.dependencies (GPS.Project.root(), recursive=True) :
      for J in GPS.Project.source_dirs(I) :
         result=result +  ' -I' + J

   return result

# Make a '+'-separated of all units in the project
def Project_units ():
   files=GPS.Project.root().sources()
   result= ""
   prev_name=""
   for f in files:
      name= os.path.splitext(os.path.basename (f.name()))[0]
      name= re.sub("-", ".", name)
      if name != prev_name:
         result= result + '+' + name
         prev_name=name
   return result

# Compare two source references
# If the references are the same, keep the original order
# (for the case of error messages that span several lines)
def sloc_cmp (l,r):
   "Compare SLOC references (file:line:col)"
   ls=l.split(':');
   rs=r.split(':');
   if len(ls[0]) == 1:
      # Presumably, we have a "C:"...
      Offset=1
   else:
      Offset=0

   if len(ls) < 3+Offset and len(rs) < 3+Offset:
      # No reference =>Keep order
      return -1
   if len(ls) < 3+Offset:
      # Reference only in r: put references ahead
      return 1
   if len(rs) < 3+Offset:
      # Reference only in l: put references ahead
      return -1

   if ls[0+Offset] != rs [0+Offset]:
      return cmp (ls[0+Offset], rs[0+Offset])
   if ls[1+Offset] != rs[1+Offset]:
      return cmp (int(ls[1+Offset]), int(rs[1+Offset]))
   return cmp (int(ls[2+Offset]), int(rs[2+Offset]))

# Sort and parse the result of running Adacontrol
def parse (output):
   global adactl_cats
   list=output.splitlines()
   list.sort(sloc_cmp)
   pos=0
   category = "Adacontrol"
   adactl_cats = sets.Set()
   for Mess in list:
     pos+=1
     if re.match ("^(.+):(\\d+):(\\d+):((( Found:)|( Error:| Parameter:| Syntax:)).*)$", Mess):
       if GPS.Preference ("separate-rules").get():
          category = re.sub (r"^.+:\d+:\d+: \w+: (.*?):.*$", r"\1", Mess)
       adactl_cats.add (category)
       GPS.Locations.parse (Mess,
         category,
         "^(.+):(\\d+):(\\d+):((( Found:)|( Error:| Parameter:| Syntax:)).*)$",
         1, 2, 3, 4, 7, 6,
         "",
         "Adactl_check",
         "Adactl_found")
     else:
       redundant=False
       for Other in list[max (0,pos-5):pos-1]:
         if Mess == Other:
           redundant=True
           break
       if not redundant:
         GPS.Locations.parse (Mess, "Compilation")
         adactl_cats.add ("Compilation")

# Ask for confirmation, then delete tree files (and possibly .ali files)
def del_tree (confirm):
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

# Create adp file from project
def create_adp ():
   name= GPS.Project.file(GPS.Project.root()).name()[:-4]+".adp"
   if GPS.MDI.yes_no_dialog ("Create " + name + "?"):
      f=open (name, 'w')
      for I in [GPS.Project.root()] + GPS.Project.dependencies (GPS.Project.root()) :
         for J in GPS.Project.source_dirs(I) :
            f.write ("src_dir=" + J + "\n")
         for J in GPS.Project.object_dirs(I) :
            f.write ("obj_dir=" + J + "\n")
      f.close
      print ("Project file " + name + " created")

# Return the units file
def get_units_file ():
   opt_list=GPS.Project.root().get_tool_switches_as_list("AdaControl")
   for O in opt_list:
      if O[0] == "@":
         return O[1:]
   return ""

# Is the units file defined?
def units_file_defined ():
   if get_units_file() == "":
      return "false"
   else:
      return "true"

# Get the rules file
def get_rules_file ():
   opt_list=GPS.Project.root().get_tool_switches_as_list("AdaControl")
   next_is_result=False
   for O in opt_list:
      if next_is_result:
         return O
      elif O == "-f":
         next_is_result=True
   return ""

# Is the rules file defined?
def rules_file_defined ():
   if get_rules_file() == "":
      return "false"
   else:
      return "true"

# raise an exception if parameter is the empty string
def check_empty (value):
   if value == "":
      raise ValueError

# A filter for the button version of running AdaControl
# Filters have no effect on a button, therefore we must do the corresponding checks
def filter_check_file ():
   try:
      win = GPS.current_context().file()
   except:
     GPS.MDI.dialog ("no active window")
     raise ValueError

   if win.language() != "ada" :
      GPS.MDI.dialog ("active window is not Ada")
      raise ValueError

   if rules_file_defined () == "false" :
      GPS.MDI.dialog ("no rules file defined")
      raise ValueError

# Displays help for a given rule
def Help_On_Rule (self):
   if GPS.Preference ("help-format").get() == "Pop-up":
     GPS.execute_action("Help_Rule", self.name)
   else:
     GPS.HTML.browse ("adacontrol_ug.html", self.name, False)

# Add a new entry to the "help on rule" menu
def Add_Rule_Menu (self, matched, unmatched):
   entry=GPS.Menu.create ("Help/AdaControl/Help on rule/" + matched, Help_On_Rule);
   entry.name=matched

# Hook on preference changes
def on_pref_changed (H):
   if GPS.Preference ("delete-ali").get():
      DelTree_Menu.rename("Delete Tree and .ali Files")
   else:
      DelTree_Menu.rename("Delete Tree Files")

################################################################
# Initialization actions

# Global variables
adactl_cats= sets.Set()

# Colors used in highliting the results
GPS.Editor.register_highlighting ("Adactl_check", "red", "True")
GPS.Editor.register_highlighting ("Adactl_found", "orange", "True")

# We must define the button here in order to compute the place of the icon from
# the GPS directory, but we cannot call GPS.Button(), because it does not allow
# the declaration of an icon (hence we use parse_xml).
GPS.parse_xml("""
<button action='Check_File_File_Button'>
   <title>Control Current File (rules file)</title>
   <pixmap>"""
           + GPS.get_system_dir()
           + """share/gps/plug-ins/adactl.gif</pixmap>
</button>
""")

# Create the Help/Adacontrol/Help rule menu
GPS.Process ("adactl -h list", ".+", Add_Rule_Menu)

# Create hook on preference changes, and keep the "Delete Tree" menu in it
GPS.Hook("preferences_changed").add (on_pref_changed)
DelTree_Menu = GPS.Menu.get ("AdaControl/Delete Tree Files")
on_pref_changed (GPS.Hook("preferences_changed"))
