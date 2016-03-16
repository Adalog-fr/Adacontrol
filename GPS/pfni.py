"""  This file adds support for Pfni
     It provides a contextual entry that displays the full name image
     of any identifier
"""

import GPS


# Builds the options string
def options():
    result = " -q "
    result = result \
        + GPS.current_context().file().name() \
        + ":" \
        + str(GPS.current_context().location().line()) \
        + ":" \
        + str(GPS.current_context().location().column())

    result = result + ' -p "' + GPS.Project.root().file().name() + '"'

    return result
