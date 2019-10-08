# Copyright (C) 2016-2019, Adalog
#
# This is free software;  you can redistribute it  and/or modify it  under
# terms of the  GNU General Public License as published  by the Free Soft-
# ware  Foundation;  either version 3,  or (at your option) any later ver-
# sion.  This software is distributed in the hope  that it will be useful,
# but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
# TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
# License for  more details.  You should have  received  a copy of the GNU
# General  Public  License  distributed  with  this  software;   see  file
# COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy
# of the license.
#
# This file is derived from gnatcheck.py by AdaCore

"""GNAThub plug-in for the AdaControl command-line tool

It exports the AdaControl class which implements the :class:`GNAThub.Plugin`
interface. This allows GNAThub's plug-in scanner to automatically find this
module and load it as part of the GNAThub default execution.
"""

import collections
import os
import re

from _gnat import SLOC_PATTERN

import GNAThub
from GNAThub import Console, Plugin, Reporter, Runner

class AdaControl(Plugin, Runner, Reporter):
    """AdaControl plugin for GNAThub

    Configures and executes AdaControl, then analyzes the output.
    """

    # Regex to identify lines that contain messages
    _RULE_PATTERN = r'(?P<mess_class>.+?): (?P<label>.+?): (?P<message>.+)$'

    # Regular expression to match AdaControl output and extract all relevant
    # information stored in it.
    _MESSAGE = re.compile(r'%s:\s%s' % (SLOC_PATTERN, _RULE_PATTERN))

    # AdaControl exits with an error code of 1 even on a successful run
    VALID_EXIT_CODES = (0, 1)

    def __init__(self):
        super(AdaControl, self).__init__()

        self.tool = None
        self.output = os.path.join(GNAThub.Project.object_dir(),
                                   '%s.out' % self.name)

        # Map of rules (couple (name, rule): dict[str,Rule])
        self.rules = {}

        # Map of messages (couple (rule, message): dict[str,Message])
        self.messages = {}

        # Map of bulk data (couple (source, message_data): dict[str,list])
        self.bulk_data = collections.defaultdict(list)

    def __cmd_line(self):
        """Creates AdaControl command line arguments list

        :return: the AdaControl command line
        :rtype: list[str]
        """

        cmd_line = [
            'adactl',
            '-wo', self.output,
            '-F', "gnat_short",
            '-p', GNAThub.Project.path(),
            ]
        return cmd_line

    def run(self):
        """Executes AdaControl
        """
        print ("AdaControl: start as " + ' '.join(self.__cmd_line()))
        status = GNAThub.Run(self.name, self.__cmd_line()).status

        if status in AdaControl.VALID_EXIT_CODES:
            print ("AdaControl: Execution completed")
            return GNAThub.EXEC_SUCCESS
        else:
            print ("AdaControl: Execution failed")
            return GNAThub.EXEC_FAILURE

    def report(self):
        """Parses AdaControl output file report

        Sets the exec_status property according to the success of the
        analysis:

            * ``GNAThub.EXEC_SUCCESS``: on successful execution and analysis
            * ``GNAThub.EXEC_FAILURE``: on any error

        """

        # Clear existing references
        self.info('AdaControl: clear existing results if any')
        GNAThub.Tool.clear_references(self.name)

        self.info('AdaControl: analyse report')

        self.tool = GNAThub.Tool(self.name)
        self.log.debug('parse report: %s', self.report)

        if not os.path.exists(self.output):
            self.error('AdaControl: error: no report found')
            return GNAThub.EXEC_FAILURE

        try:
            with open(self.output, 'r') as output:
                lines = output.readlines()
                total = len(lines)
                print ("total lines", total)

                for index, line in enumerate(lines, start=1):
                    self.log.debug('parse line: %s', line)
                    match = self._MESSAGE.match(line)

                    if match:
                        self.__parse_line(match)

                    Console.progress(index, total, new_line=(index == total))

        except IOError as why:
            self.log.exception('AdaControl: failed to parse report')
            self.error('%s (%s:%d)' % (
                why, os.path.basename(self.output), total))
            return GNAThub.EXEC_FAILURE

        else:
            self.__do_bulk_insert()
            return GNAThub.EXEC_SUCCESS

    def __parse_line(self, regex):
        """Parses an AdaControl message line

        Add the message to the current database session.

        Retrieves following information:

            * source basename
            * line in source
            * rule identification
            * message description

        :param re.RegexObject regex: the result of the _MESSAGE regex
        """

        # The following Regex results are explained using this example.
        # 'input.adb:3:19: Error: UNNECESSARY_USE_CLAUSE: unused use clause for Text_IO'

        # Extract each component from the message (keeping the same order as in gnatcheck):
        #       ('input.adb', '3', '19', 'unused use clause for Text_IO',
        #        'UNNECESSARY_USE_CLAUSE')
        base = regex.group('file')
        src = GNAThub.Project.source_file(base)
        line = regex.group('line')
        column = regex.group('column')
        message = regex.group('message')
        label = regex.group('label').lower()

        self.__add_message(src, line, column, label, message)

    def __add_message(self, src, line, column, rule_id, msg):
        """Adds AdaControl message to current session database

        :param src: Message source file.
        :type src: str
        :param line: Message line number.
        :type line: str
        :param column: Message column number.
        :type column: str
        :param rule_id: Message's rule identifier.
        :type rule_id: str
        :param msg: Description of the message.
        :type msg: str
        """

        # Cache the rules
        if rule_id in self.rules:
            rule = self.rules[rule_id]
        else:
            rule = GNAThub.Rule(rule_id, rule_id, GNAThub.RULE_KIND, self.tool)
            self.rules[rule_id] = rule

        # Cache the messages
        if (rule, msg) in self.messages:
            message = self.messages[(rule, msg)]
        else:
            message = GNAThub.Message(rule, msg)
            self.messages[(rule, msg)] = message

        # Add the message to the given resource
        self.bulk_data[src].append(
            [message, int(line), int(column), int(column)])

    def __do_bulk_insert(self):
        """Insert the adacontrol messages in bulk on each resource"""

        for src in self.bulk_data:
            base = GNAThub.Project.source_file(os.path.basename(src))
            resource = GNAThub.Resource.get(base)

            if resource:
                resource.add_messages(self.bulk_data[src])
