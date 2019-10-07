"""  This file adds support for AdaControl
     It provides a new menu, entries in the "help" menu, and
     buttons to make use of AdaControl easier
"""

#
import GPS
import gps_utils
import os
import glob
import re
import sets
import sys
import traceback

#
# General utilities
#


def get_file(option):
    """Get the file corresponding to the given option
    """
    opt_list = GPS.Project.root().get_tool_switches_as_list("AdaControl")
    try:
        return opt_list[opt_list.index(option) + 1]
    except:
        return ""


def open_file(name):
    """Open the given file in a new buffer (unless it exists)
       and raise the corresponding window
    """
    ed = GPS.EditorBuffer.get(GPS.File(name))
    GPS.MDI.get_by_child(ed.current_view()).raise_window()


def protect(mess):
    """ protects a message by changing "<" and ">" to "&lt" and "&gt"
        This is requirede when the message is passed to Pango
    """
    return mess.replace("<", "&lt;").replace(">", "&gt;")


#
# Units
#
def Project_units():
    """Make a '+'-separated list of all units in the project
    """
    files = GPS.Project.root().sources()
    result = ""
    prev_unit = ""
    for f in files:
        if f.language() == "ada":
            unit = os.path.splitext(os.path.basename(f.name()))[0]
            unit = re.sub("-", ".", unit)
            if unit != prev_unit:
                result = result + '+' + unit
                prev_unit = unit
    return result


def create_units():
    """Create units file from project
    """
    name = get_units_file()
    if not name:
        name = GPS.Project.file(GPS.Project.root()).name()[:-4] + ".txt"
    value = GPS.MDI.input_dialog("Create units file",  "File=" + name)
    if value != [] and value[0] != "":
        name = value[0]
        unitsFile = open(name, 'w')
        files = GPS.Project.root().sources()
        prev_unit = ""
        for f in files:
            if f.language() == "ada":
                unit = os.path.splitext(os.path.basename(f.name()))[0]
                unit = re.sub("-", ".", unit)
                if unit != prev_unit:
                    unitsFile.write(unit + "\n")
                    prev_unit = unit
        unitsFile.close()
        print ("Units file " + name + " created")


def get_units_file():
    """Return the units file name
    """
    return get_file("-@")


def units_file_defined():
    if get_file("-@"):
        return "true"
    else:
        return "false"


#
# Rules
#
def rules_file_defined():
    if get_file("-f"):
        return "true"
    else:
        return "false"


#
# Project
#
def create_adp():
    """Create adp file from project
    """
    name = GPS.Project.file(GPS.Project.root()).name()[:-4] + ".adp"
    value = GPS.MDI.input_dialog("Create .adp file",  "File=" + name)
    if value != [] and value[0] != "":
        name = value[0]
        f = open(name, 'w')
        for I in GPS.Project.dependencies(GPS.Project.root(), recursive=True):
            for J in GPS.Project.source_dirs(I):
                f.write("src_dir=" + J + "\n")
            for J in GPS.Project.object_dirs(I):
                f.write("obj_dir=" + J + "\n")
        f.close()
        GPS.MDI.dialog("Project file " + name + " created")


#
# Run AdaControl
#
def pre_clean():
    """clean-up various windows before running AdaControl
    """
    global adactl_cats, result

    GPS.MDI.save_all(GPS.Preference("force-save").get())
    for C in adactl_cats:
        GPS.Locations.remove_category(C)
    adactl_cats = sets.Set()
    result = ""
    GPS.MDI.get("Messages").raise_window()


def post_clean():
    """Clean-up after running AdaControl
    """
    # Raise "Locations" window if possible
    loc = GPS.MDI.get("Locations")
    if loc is not None:
        loc.raise_window()
    if GPS.Preference("delete-trees").get():
        del_tree(confirm=False)


def command_name():
    """Get the command name for AdaControl
    """
    return GPS.Project.root().get_attribute_as_string("Compiler_Command",
                                                      "Ide",
                                                      "adacontrol") or "adactl"


#
# Management of fixes
#
class Fix:
    def apply(self):
        start_loc = self.fix_start.location()
        end_loc = self.fix_end.location()
        if self.key == "Delete":
            start_loc.buffer().delete(start_loc, end_loc)
        elif self.key in ["Replace", "Insert"]:
            if self.key == "Replace":
                start_loc.buffer().delete(start_loc, end_loc)

            # There is at least one line of data, otherwise use Delete
            start_loc.buffer().insert(start_loc, self.fix_data[0])
            for line in self.fix_data[1:]:
                end_loc.buffer().insert(end_loc, "\n" + line)
        elif self.key == "Refactor":
            GPS.EditorView(start_loc.buffer()).goto(start_loc)
            GPS.Action("rename entity").execute_if_possible()


def fixer(self):
    """ Method of a message called when clicking the wrench
    """
    for fix in self.fix_list:
        fix.apply()
    self.set_subprogram(no_action, '', '')


def no_action(self):
    """ placeholder when the fix has been performed
    """
    pass


def apply_fixes():
    """ Automatically perform all possible fixes
    """
    global adactl_cats

    for cat in adactl_cats:
        for mess in GPS.Message.list(category=cat):
            mess.execute_action()


def parse(output):
    """Sort and parse the result of running Adacontrol
    """
    global adactl_cats, long_mess_pat, syntax_mess_pat, short_mess_pat, \
        fix_mess_pat, rule_statistics_pat, message_statistics_pat, \
        errors_style, warnings_style

    list = output.splitlines()
    pos = 0
    category = "Adacontrol"
    sep = ';'
    for Mess in list:
        pos += 1

        if Mess == "":
            continue

        # Regular AdaCtl message
        match = long_mess_pat.match(Mess)
        if match:
            if GPS.Preference("separate-rules").get():
                category = match.group('rule')
            adactl_cats.add(category)
            mess = GPS.Message(category,
                               file=GPS.File(match.group('file')),
                               line=int(match.group('line')),
                               column=int(match.group('col')),
                               text=protect(match.group('message')))
            key = match.group('key')
            if key == "Found":
                mess.set_style(warnings_style)
            else:
                mess.set_style(errors_style)
            prev_mess = mess
            prev_mess.fix_list = []
            continue

        # Fix message
        match = fix_mess_pat.match(Mess)
        if match:
            key = match.group('key')
            buf = GPS.EditorBuffer.get(GPS.File(match.group('file')))
            loc = GPS.EditorLocation(buf,
                                     int(match.group('startline')),
                                     int(match.group('startcol')))
            fix = Fix()
            fix.key = key
            fix.fix_start = loc.create_mark(left_gravity=False)
            fix.fix_data = []
            if key == "Delete":
                col = int(match.group('endcol'))
                # col=0 means that the preceding end of line is to be deleted
                if col == 0:
                    loc = GPS.EditorLocation(buf,
                                             int(match.group('endline'))-1,
                                             1)
                    loc = loc.end_of_line()
                else:
                    loc = GPS.EditorLocation(buf,
                                             int(match.group('endline')),
                                             col)
                fix.fix_end = loc.create_mark(left_gravity=True)
                prev_mess.set_subprogram(fixer, 'gps-codefix', "Delete it")
            elif key == "Replace":
                loc = GPS.EditorLocation(buf,
                                         int(match.group('endline')),
                                         int(match.group('endcol')))
                fix.fix_end = loc.create_mark(left_gravity=True)
                prev_mess.set_subprogram(fixer, 'gps-codefix', "Replace it")
            elif key == "Refactor":
                loc = GPS.EditorLocation(buf,
                                         int(match.group('endline')),
                                         int(match.group('endcol')))
                fix.fix_end = loc.create_mark(left_gravity=True)
                prev_mess.set_subprogram(fixer, 'gps-codefix', "Refactor it")
            elif key == "Insert":
                fix.fix_end = fix.fix_start
                prev_mess.set_subprogram(fixer, 'gps-codefix', "Insert it")
            prev_mess.fix_list.append(fix)
            continue

        # Fix data
        if Mess[0] == '!':
            prev_mess.fix_list[-1].fix_data.append(Mess[1:])
            continue

        # Rules file check message
        match = syntax_mess_pat.match(Mess)
        if match:
            adactl_cats.add(category)
            mess = GPS.Message(category,
                               file=GPS.File(match.group('file')),
                               line=int(match.group('line')),
                               column=int(match.group('col')),
                               text=protect(match.group('message')))
            mess.set_style(errors_style)
            prev_mess = mess
            prev_mess.fix_list = []
            continue

        # AdaCtl message without location, or just a column location
        # (syntax error in interactive command)
        match = short_mess_pat.match(Mess)
        if match:
            if GPS.Preference("separate-rules").get():
                category = match.group('category')
            adactl_cats.add(category)
            message = protect(match.group('message'))
            try:
                mess = GPS.Message(category, GPS.File("none"), 1, 1, message)
            except:
                # Always an exception, since file "none" does not exist
                # (presumably)
                pass
            continue

        # Rule statistics summary
        match = rule_statistics_pat.match(Mess)
        if match:
            adactl_cats.add("Statistics")
            try:
                mess = GPS.Message("Statistics",
                                   GPS.File(match.group('rule')),
                                   1,
                                   1,
                                   match.group('counts'))
            except:
                # Always an exception, since file does not exist
                pass
            continue

        # Message statistics summary
        match = message_statistics_pat.match(Mess)
        if match:
            adactl_cats.add("Statistics")
            try:
                mess = GPS.Message("Statistics",
                                   GPS.File(match.group('title')),
                                   1,
                                   1,
                                   match.group('counts'))
            except:
                # Always an exception, since file does not exist
                pass
            continue

        # Assume it is a compilation message
        if Mess not in list[max(0, pos - 5):pos - 1]:
            adactl_cats.add("Compilation")
            GPS.Locations.parse(Mess, "Compilation")


def load_result(file=""):
    """Load a result file in location window
    """
    global previous_locfile, adactl_cats

    if not file:
        previous_locfile = previous_locfile or get_file("-o")
        value = GPS.MDI.input_dialog(
            "Load result", "File=" + previous_locfile)
        if value == [] or value[0] == "":
                # User cancelled action
            return
        previous_locfile = value[0]
    else:
        previous_locfile = file

    try:
        f = open(previous_locfile, 'r')
    except:
        GPS.MDI.dialog(
            "File " + previous_locfile + " not found in " + GPS.pwd())
        return

    for C in adactl_cats:
        GPS.Locations.remove_category(C)
    adactl_cats = sets.Set()

    try:
        parse(f.read())
    except:
        GPS.MDI.dialog(
            "File " + previous_locfile + " is not a valid result file")
        print (sys.exc_info())
        traceback.print_tb(sys.exc_info()[2])
        return
    GPS.MDI.get("Locations").raise_window()


def options(rules, files):
    """Builds the options string
       Options -f and @ (actually -@) are defined as switches for the
       convenience of the user but they are actually passed as explicit
       parameters depending on the way Adacontrol is launched. We must
       therefore remove these options from the switches string provided by GPS.
       Similarly, the -o option depends on the setting of the (pseudo) switches
       -GPS and -NOGPS
    """
    global output_name, gps, previous_command

    result = "-v"

    # always set project except for checking rules file
    if rules != "check":
        result = result + ' -p "' + GPS.Project.root().file().name() + '"'

    # set files
    if rules != "check" and files == "":
        tmp = GPS.Preference("button-target").get()
        if tmp == "Current File":
            files = "current"
        elif tmp == "Root Project":
            files = "project"
        else:  # Units from list
            files = "list"

    if rules == "check":
        result = "-C " + result
    elif files == "current":
        try:
            win = GPS.current_context().file()
        except:
            GPS.MDI.dialog("no active window")
            raise ValueError
        if win is None:
            GPS.MDI.dialog("no active window")
            raise ValueError
        if win.language() != "ada":
            GPS.MDI.dialog("active window is not Ada")
            raise ValueError
        result = result + ' "' + win.name() + '"'
    elif files == "list":
        result = result + " @\"" + get_file("-@") + "\""
    else:   # "project"
        result = result + ' ' + Project_units()

    # set rules
    if rules == "ask":
        value = GPS.MDI.input_dialog(
            "Interactive run", "Command(s)=" + previous_command)
        if value != [] and value[0] != "":
            previous_command = value[0]
            res = run("check", previous_command)
            if res != 0:
                raise ValueError
            result = result + " -f -l \"" + \
                re.sub('"', '~', previous_command) + "\""
        else:
            raise ValueError
    elif rules == "file":
        if not get_file("-f"):
            GPS.MDI.dialog("no rules file defined in AdaControl properties")
            raise ValueError
        result = result + " -f \"" + get_file("-f") + "\""
    else:  # "check"
        if files == "":
            try:
                win = GPS.current_context().file()
            except:
                GPS.MDI.dialog("no active window")
                raise ValueError
            if win.language() != "adacontrol":
                GPS.MDI.dialog("active window is not AdaControl rules file")
                raise ValueError
            result = result + " -f " + win.name()
        else:
            result = result + " -f -l \"" + re.sub('"', '~', files) + "\""

    # set other_params
    gps = True
    outfile = False
    fixes = False
    output_name = ""
    ASIS_option = ""

    opt_list = GPS.Project.root().get_tool_switches_as_list("AdaControl")
    skip_next = False
    next_is_ofile = False
    next_is_pfile = False
    next_is_ASIS = False
    next_is_format = False
    next_is_fixes = False
    next_is_stats = False
    for O in opt_list:
        if len(O) == 0:
            pass
        elif next_is_ofile:
            output_name = O
            next_is_ofile = False
        elif next_is_pfile:
            next_is_pfile = False
        elif next_is_ASIS:
            if rules != "check":
                ASIS_option = O
        elif next_is_format:
            result = result + " -F " + O
            next_is_format = False
        elif next_is_fixes:
            if rules != "check":
                result = result + " -G " + O
            next_is_fixes = False
        elif next_is_stats:
            if rules != "check":
                result = result + " -S " + O
            next_is_stats = False
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
            gps = True
            outfile = True
        elif O == "-NOGPS":
            gps = False
            outfile = True
        elif O == "-f":
            skip_next = True
        elif O == "-F":
            next_is_format = True
        elif O[0:2] == "-F":
            # Bug in GPS: Combo does not add separator
            if gps:
                result = result + " -F gnat_short"
            else:
                result = result + ' ' + "-F " + O[2:]
        elif O == "-G":
            fixes = True
            next_is_fixes = True
        elif O[0:2] == "-G":
            # Bug in GPS: Combo does not add separator
            fixes = True
            if rules != "check":
                result = result + ' ' + "-G " + O[2:]
        elif O == "-o":
            next_is_ofile = True
        elif O == "-p":
            # compatibility: ignore -p option and following parameter
            next_is_pfile = True
        elif O == "-P":
            next_is_fixfile = True
        elif O == "-S":
            next_is_stats = True
        elif O[0:2] == "-S" and O[2:] != "":
            # Bug in GPS: Combo does not add separator
            if rules != "check":
                result = result + ' ' + "-S " + O[2:]
        else:
            result = result + ' ' + O

    val = GPS.Preference("max-messages").get()
    if val != 0:
        result = result + ' ' + "-M " + repr(val)

    val = GPS.Preference("max-errors").get()
    if val != 0:
        result = result + ' ' + "-m " + repr(val)

    if outfile:
        if output_name == "":
            GPS.MDI.dialog(
                "Result file is not defined, please set it in Project properties/Switches/AdaControl")
            raise ValueError
        elif gps:
            # always override
            result = result + " -wo " + output_name
        elif len(glob.glob(output_name)) != 0:
            if GPS.MDI.yes_no_dialog("File " +
                                     output_name +
                                     " exists, override?"):
                result = result + " -wo " + output_name
            else:
                raise ValueError
        else:
            result = result + " -o " + output_name
    else:
        # file is specified but not used => print messages
        output_name = ""
        result = result + " -o CONSOLE"

    if rules != "check":
        if gps and not fixes:
            result = result + " -G check"

        if ASIS_option:
            result = result + " -- " + ASIS_option

    return result


def process_line(process, matching, rest):
    global result
    if GPS.Preference("display-run").get():
        print matching,
    result = result + matching


def exit_check(process, status, output):
    if status > 1:
        print "status:", status
        print "output:", output


def run(rules, files):
    """Run Adacontrol
       rules = "ask" | "file" | "check"
       files = "current" | "list" | "project" | "" (means: depending on button definition)
    """
    global previous_command, result
    global output_name, gps

    try:
        command_line = command_name() + ' ' + options(rules, files)
    except ValueError:
        print "option error", rules, files
        return

    pre_clean()
    if GPS.Preference("display-run").get():
        print command_line
    proc = GPS.Process(command=command_line,
                       task_manager=True,
                       on_exit=exit_check,
                       regexp="^.+$",
                       single_line_regexp=True,
                       on_match=process_line,
                       progress_regexp="^\((\d+)/(\d+)\)",
                       progress_current=1,
                       progress_total=2)
    res = proc.wait()
    parse(result)
    if gps and output_name:
        load_result(output_name)
    post_clean()

    # return the return code only if checking interactive rule syntax
    if rules == "check" and files:
        return res


#
# GUI
#
def Help_On_Rule(name):
    """provide help in order to use AdaControl
    """
    if GPS.Preference("help-format").get() == "Pop-up":
        proc = GPS.Process(command_name() + " -h " + name)
        GPS.MDI.dialog(proc.get_result())
    else:
        # for mysterious reasons, Texinfo replaces "_" in anchors by "_005f"
        GPS.HTML.browse(
            "adacontrol_ug.html",
            anchor=name.replace("_", "_005f"),
            navigation=False)


def Help_On_Rule_Old(self):
    """Old version: called with menu entry
    """
    Help_On_Rule(self.name)


def Add_Rule_Menu(self, matched, unmatched):
    """Add a new entry to the "Help on rule" menu
    """
    global gps_version

    # '_' is interpreted as a special mark in menus, double it
    if gps_version < '17.0':
        entry = GPS.Menu.create("Help/AdaControl/Help on rule/" + matched.replace("_", "__"),
                                Help_On_Rule_Old,
                                ref="About",
                                add_before=True)
        entry.name = matched
    else:
        @gps_utils.interactive(name='Show rule %s' % matched,
                               menu="Help/AdaControl/Help on rule/" + matched.replace("_", "__"))
        def Help_Rule_Entry():
            Help_On_Rule(matched)


def about():
    proc = GPS.Process(command_name() + " -h version license")
    GPS.MDI.dialog(proc.get_result())


def del_tree(confirm):
    """Ask for confirmation, then delete tree files (and possibly .ali files)
    """
    dir = os.getcwd()
    ali_also = GPS.Preference("delete-ali").get()
    if ali_also:
        supp = "and .ali "
    else:
        supp = ""
    if not confirm or GPS.MDI.yes_no_dialog("Remove all tree " +
                                            supp +
                                            "files from " + dir + "?"):
        for I in glob.glob(os.path.join(dir, "*.adt")):
            os.remove(I)
        if ali_also:
            for I in glob.glob(os.path.join(dir, "*.ali")):
                os.remove(I)


def on_pref_changed(H):
    """Hook on preference changes
    """
    global gps_version

    # Menu.rename disappeared after GPL2016
    if gps_version > '16.0':
        return

    if GPS.Preference("delete-ali").get():
        DelTree_Menu.rename("Delete Tree and .ali Files")
    else:
        DelTree_Menu.rename("Delete Tree Files")


def on_GPS_start(H):
    """Hook on GPS start
       Initialization actions
       They are defined as a hook rather than directly in this file, because
       when this file is elaborated, the GPS context is not yet set (project
       variables for example)
    """
    global adactl_cats, previous_command, previous_locfile, DelTree_Menu,\
        long_mess_pat, syntax_mess_pat, short_mess_pat, rule_statistics_pat,\
        fix_mess_pat, message_statistics_pat, gps_version, errors_style, \
        warnings_style

    # Global variables initialization
    adactl_cats = sets.Set()
    previous_command = ""
    previous_locfile = ""

    styles_dict = {s.get_name(): s for s in GPS.Style.list()}
    errors_style = styles_dict['Builder results']
    warnings_style = styles_dict['Builder warnings']

    # Normalize version numbers to allow for correct ordering of (string) comparison
    # 6.1.1 => 06.1.1
    # 2016  => 16.0
    # 17.0w => 17.0w
    gps_version = GPS.version()
    if gps_version.find('.') == -1:
        gps_version = gps_version[-2:] + '.0'
    elif gps_version.find('.') == 1:
        gps_version = '0' + gps_version

    # Message recognition patterns
    long_mess_pat = re.compile(r'(?P<file>.+?):'
                               r'(?P<line>\d+):'
                               r'(?P<col>\d+): '
                               r'(?P<message>(?P<key>Found|Error|Parameter): (?P<rule>.*?):.*)$')

    syntax_mess_pat = re.compile(r'(?P<file>.+?):'
                                 r'(?P<line>\d+):'
                                 r'(?P<col>\d+): '
                                 r'(?P<message>Syntax: .*)$')

    short_mess_pat = re.compile(r"^(?P<category>.+):(?: )?(?P<message>(?P<key>Found|Error|Parameter|Syntax): (.*))$")

    fix_mess_pat = re.compile(r'"?(?P<file>.+?)"?:'
                              r'(?P<startline>\d+):'
                              r'(?P<startcol>\d+): '
                              r'(?P<key>Delete|Insert|Replace|Refactor)(:(?P<endline>\d+):(?P<endcol>\d+))?$')

    rule_statistics_pat = re.compile(r"^(?P<rule>.*): (?P<counts>(Check: \d+, Search: \d+, Count: \d+)|(not triggered))$")

    message_statistics_pat = re.compile(r"^(?P<title>(Issued messages|Units processed|Lines processed):) (?P<counts>.*)$")

    # Actions and submenus are now parsed from Python because otherwise
    # filters involving Python actions do not work anymore
    # (starting from GPS 6.1.1)

    # Actions
    GPS.parse_xml("""
   <action name="Check_File_File">
     <filter_and>
       <filter language="ada" />
       <filter shell_cmd="adactl.rules_file_defined()" shell_lang="python" />
     </filter_and>

     <shell lang="Python" show-command="false" output="">adactl.run("file", "current")</shell>
   </action>

   <action name="Check_Project_File">
     <filter shell_cmd="adactl.rules_file_defined()" shell_lang="python" />
     <shell lang="Python" show-command="false" output="">adactl.run("file", "project")</shell>
   </action>

   <action name="Check_Units_File">
     <filter_and>
       <filter shell_cmd="adactl.rules_file_defined()" shell_lang="python" />
       <filter shell_cmd="adactl.units_file_defined()" shell_lang="python" />
     </filter_and>
     <shell lang="Python" show-command="false" output="">adactl.run("file", "list")</shell>
   </action>

   <action name="Check_Unknown_File">
      <description>Launch AdaControl (rules file)</description>
      <shell lang="Python" show-command="false" output="">adactl.run("file", "")</shell>
   </action>

   <action name="Check_File_Ask">
     <filter language="ada" />
     <shell lang="Python" show-command="false" output="">adactl.run("ask", "current")</shell>
   </action>

   <action name="Check_Project_Ask">
     <shell lang="Python" show-command="false" output="">adactl.run("ask", "project")</shell>
   </action>

   <action name="Check_Units_Ask">
     <filter shell_cmd="adactl.units_file_defined()" shell_lang="python" />
     <shell lang="Python" show-command="false" output="">adactl.run("ask", "list")</shell>
   </action>

   <action name="Check_Unknown_Ask">
     <description>Launch AdaControl (interactive)</description>
     <shell lang="Python" show-command="false" output="">adactl.run("ask", "")</shell>
   </action>

   <action name="Check_AdaCtl">
     <filter language="AdaControl" />
     <shell lang="Python" show-command="false" output="">adactl.run("check", "")</shell>
   </action>

   <action name="Apply_Fixes">
     <shell lang="Python" show-command="false" output="">adactl.apply_fixes()</shell>
   </action>

   <action name="Get_Units" show-command="false" output="none">
     <filter shell_cmd="adactl.units_file_defined()" shell_lang="python" />
     <shell lang="python">adactl.open_file(adactl.get_units_file())</shell>
    </action>

   <action name="Get_Rules" show-command="false" output="none">
     <filter shell_cmd="adactl.rules_file_defined()" shell_lang="python" />
     <shell lang="python">adactl.open_file(adactl.get_file("-f"))</shell>
   </action>

   <action name="Del_Tree">
     <shell lang="python" show-command="false" output="none">adactl.del_tree(confirm=True)</shell>
   </action>

   <action name="Create_adp">
     <shell lang="python" show-command="false" output="">adactl.create_adp()</shell>
   </action>

   <action name="Load_Result">
     <shell lang="python" show-command="false" output="none">adactl.load_result()</shell>
   </action>

    <action name="Create_units">
     <shell lang="python" show-command="false" output="">adactl.create_units()</shell>
    </action>

   <action name="About">
     <shell lang="python" show-command="false" output="none">adactl.about()</shell>
   </action>
   """)

    # Submenus
    GPS.parse_xml("""
   <submenu before="About">
     <title>/Help/AdaControl</title>

     <menu action="About">
        <title>About</title>
     </menu>

   </submenu>

   <submenu before="Help">
     <title>/AdaControl</title>

     <menu action="Check_File_File">
        <title>Control Current File (rules file)</title>
     </menu>

     <menu action="Check_Project_File">
        <title>Control Root Project (rules file)</title>
     </menu>

     <menu action="Check_Units_File">
        <title>Control Units from List (rules file)</title>
     </menu>

     <menu />

     <menu action="Check_File_Ask">
        <title>Control Current File (interactive)</title>
     </menu>

     <menu action="Check_Project_Ask">
        <title>Control Root Project (interactive)</title>
     </menu>

     <menu action="Check_Units_Ask">
        <title>Control Units from List (interactive)</title>
     </menu>

     <menu />

     <menu action="Apply_Fixes">
        <title>Apply all fixes</title>
     </menu>

     <menu />

     <menu action="Check_AdaCtl">
        <title>Check Rules File Syntax</title>
     </menu>

     <menu action="Get_Rules">
        <title>Open rules file</title>
     </menu>

     <menu action="Get_Units">
        <title>Open units file</title>
     </menu>

     <menu action="Load_Result">
        <title>Load results file</title>
     </menu>

     <menu />

     <menu action="Create_units">
        <title>Create units file</title>
     </menu>

     <menu action="Create_adp">
        <title>Create .adp project</title>
     </menu>
   <menu action="Del_Tree">
      <title>Delete Tree Files</title>
   </menu>

   </submenu>
   """)

    # We must define the buttons here in order to compute the place of the
    # icons from the GPS directory, but we cannot call GPS.Button(), because it
    # does not allow the declaration of an icon (hence we use parse_xml).
    if gps_version < "06.1.1":
        # Old GPS: Buttons have title and pixmap ('twas simple at that time!)
        GPS.parse_xml("""
      <button action='Check_Unknown_File'>
         <title>Launch AdaControl (rules file)</title>
         <pixmap>{base_dir}share/gps/plug-ins/adactl-file.gif</pixmap>
      </button>
      <button action='Check_Unknown_Ask'>
         <title>Launch AdaControl (interactive)</title>
         <pixmap>{base_dir}share/gps/plug-ins/adactl-ask.gif</pixmap>
      </button>
      """ .format(base_dir=GPS.get_system_dir()))

    elif gps_version <= "06.2.1":
        # GPS 06.1.2 up to 06.2.1: Buttons use stock
        GPS.parse_xml('''
      <stock>
         <icon id="adactl-file"
               label="Launch AdaControl (rules file)"
               file="{base_dir}share/gps/plug-ins/adactl-file.gif" />
         <icon id="adactl-ask"
               label="Launch AdaControl (interactive)"
               file= "{base_dir}share/gps/plug-ins/adactl-ask.gif" />
      </stock>
      ''' .format(base_dir=GPS.get_system_dir()))

        GPS.parse_xml("""
      <button action='Check_Unknown_File' stock="adactl-file" />
      <button action='Check_Unknown_Ask'  stock="adactl-ask" />
      """)
    else:
        # Starting with GPL 2016: buttons have iconname attribute
        GPS.parse_xml("""
        <button action='Check_Unknown_File' iconname='adactl-file' />
        <button action='Check_Unknown_Ask'  iconname='adactl-ask'  />
      """)

    # Create the Help/Adacontrol/Help rule menu
    GPS.Process(command_name() + " -h list", ".+", Add_Rule_Menu)

    # Create hook on preference changes, and keep the "Delete Tree" menu in it
    GPS.Hook("preferences_changed").add(on_pref_changed)
    DelTree_Menu = GPS.Menu.get("/AdaControl/Delete Tree Files")
    on_pref_changed(GPS.Hook("preferences_changed"))


#
# Create hook on GPS start
GPS.Hook("gps_started").add(on_GPS_start)
