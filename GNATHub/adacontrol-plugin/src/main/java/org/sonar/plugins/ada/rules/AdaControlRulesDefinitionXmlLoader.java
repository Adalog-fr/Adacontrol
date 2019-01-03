/*
 * GNATdashboard
 * Copyright (C) 2017-2018, Adalog
 *
 * This is free software;  you can redistribute it  and/or modify it  under
 * terms of the  GNU General Public License as published  by the Free Soft-
 * ware  Foundation;  either version 3,  or (at your option) any later ver-
 * sion.  This software is distributed in the hope  that it will be useful,
 * but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
 * TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
 * License for  more details.  You should have  received  a copy of the GNU
 * General  Public  License  distributed  with  this  software;   see  file
 * COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy
 * of the license.
 */

package org.sonar.plugins.ada.rules;

import org.sonar.api.server.rule.RulesDefinitionXmlLoader;

/**
 * AdaControl rules definition.
 *
 * Loads the XML file containing each rule definition and register them in
 * SonarQube as part of a new rule repository.
 */
public class AdaControlRulesDefinitionXmlLoader extends CustomRulesDefinitionXmlLoader {
  public AdaControlRulesDefinitionXmlLoader(final RulesDefinitionXmlLoader xmlLoader) {
    super(xmlLoader);
  }

  public static final String REPOSITORY_KEY = "adacontrol";
  public static final String RULES_DEFINITION_FILE = "/adacontrol.xml";

  @Override
  public String getRepositoryKey() {
    return REPOSITORY_KEY;
  }

  @Override
  public String getToolName() {
    return "AdaControl";
  }

  @Override
  public String getRulesDefinitionXMLFile() {
    return RULES_DEFINITION_FILE;
  }
}
