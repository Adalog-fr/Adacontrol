"""  This file adds support for Pfni
     It provides a contextual entry that displays the full name image
     of any identifier
"""

import GPS

# Builds the options string
def options ():
   result = " --"
   for I in [GPS.Project.root()] + GPS.Project.dependencies (GPS.Project.root(), recursive=True) :
      for J in GPS.Project.source_dirs(I) :
         result=result +  ' -I' + J

   return result
