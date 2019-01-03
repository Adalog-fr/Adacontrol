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

package org.sonar.plugins.ada;

import org.sonar.api.Plugin;
import org.sonar.plugins.ada.rules.AdaControlRulesDefinitionXmlLoader;

/**
 * Entry point to the SonarQube's plug-in.
 *
 * Lists all extensions to SonarQube defined and implemented in this plug-in.
 */
public final class AdacontrolPlugin implements Plugin {


  /**
   * Initialize SonarQube plugin for AdaControl.
   */
  @Override
  public void define(final Context context) {
    context.addExtensions(
        // Declare the Ada language.
        AdacontrolProfile.class,
        AdaControlRulesDefinitionXmlLoader.class
    );
  }
}
