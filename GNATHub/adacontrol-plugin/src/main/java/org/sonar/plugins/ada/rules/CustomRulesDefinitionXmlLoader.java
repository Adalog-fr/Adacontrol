/*
 * GNATdashboard
 * Copyright (C) 2016, AdaCore
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

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.sonar.api.server.rule.RulesDefinition;
import org.sonar.api.server.rule.RulesDefinitionXmlLoader;

import java.io.InputStreamReader;

/**
 * Base class for rules definitions.
 *
 * Provides the common mechanism to load a rules definition XML file from the
 * project resources.
 */
@Slf4j
@AllArgsConstructor
public abstract class CustomRulesDefinitionXmlLoader implements RulesDefinition {
  private final RulesDefinitionXmlLoader xmlLoader;

  /**
   * @return The repository key.
   */
  protected abstract String getRepositoryKey();

  /**
   * @return The name of the tool (eg. "GNATcheck").
   */
  protected abstract String getToolName();

  /**
   * @return The path to the XML file containing the rules definition.
   */
  protected abstract String getRulesDefinitionXMLFile();

  @Override
  public void define(final Context context) {
    final NewRepository repository = context
        .createRepository(getRepositoryKey(), "ada")
        .setName(String.format("%s rules", getToolName()));

    final String definitions = getRulesDefinitionXMLFile();

    log.debug("Loading XML definition file: {}", definitions);
    final InputStreamReader reader = new InputStreamReader(
        CustomRulesDefinitionXmlLoader.class.getResourceAsStream(definitions));

    xmlLoader.load(repository, reader);
    repository.done();
  }
}
