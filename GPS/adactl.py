#  This file adds support for AdaControl

import GPS, os, glob, re

GPS.Editor.register_highlighting ("Adactl_check", "#FF0000", "True")
GPS.Editor.register_highlighting ("Adactl_found", "#FFB000", "True")

def Ioptions ():
   result=''
   for I in [GPS.Project.root()] + GPS.Project.dependencies (GPS.Project.root()) :
      for J in GPS.Project.source_dirs(I) :
         result=result +  ' -I' + J
   return result

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

def parse (output):
   GPS.Locations.parse (output,
     "AdaControl",
     "^(.+):(\\d+):(\\d+):((( Found:)|( Error:)).*)$",
     1, 2, 3, 4, 7, 6,
     "",
     "Adactl_check",
     "Adactl_found")

def clean_up ():
   if GPS.Preference ("delete-trees").get():
      del_tree (confirm=False)

def del_tree (confirm):
   dir = os.getcwd()
   if not confirm or GPS.MDI.yes_no_dialog ("Remove all tree files from " + dir + "?"):
      for I in glob.glob (os.path.join (dir, "*.adt")):
         os.remove (I)

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

def units_file_defined ():
   if GPS.Project.root().get_attribute_as_string ("units_file", "Ide").strip() == "":
      return "false"
   else:
      return "true"

def rules_file_defined ():
   if GPS.Project.root().get_attribute_as_string ("rules_file", "Ide").strip() == "":
      return "false"
   else:
      return "true"

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


# We must define the button here in order to compute the place of the icon from
# the GPS directory, but we cannot call GPS.Button(), because it does not allow
# the declaration of an icon (hence we use parse_xml).
GPS.parse_xml("""
<button action='Check_File'>
   <title>Control Current File</title>
   <pixmap>"""
           + GPS.get_system_dir()
           + """share/gps/plug-ins/adactl.gif</pixmap>
</button>
""")
