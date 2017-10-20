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

package org.sonar.plugins.ada;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.sonar.api.profiles.ProfileDefinition;
import org.sonar.api.profiles.RulesProfile;
import org.sonar.api.profiles.XMLProfileParser;
import org.sonar.api.utils.ValidationMessages;

import java.io.InputStreamReader;

/**
 * Load the quality profile for AdaControl.
 *
 * The quality profile is defined in an XML file shipped with this plug-in.
 * It activates all known rules by default. See /default-profile.xml for more details.
 */
@Slf4j
@AllArgsConstructor
public class AdacontrolProfile extends ProfileDefinition {
  private final XMLProfileParser xmlLoader;

  public static final String RULES_PROFILE_FILE = "/adacontrol-profile.xml";

  @Override
  public RulesProfile createProfile(final ValidationMessages messages) {
    log.debug("Loading XML profile file: {}", RULES_PROFILE_FILE);
    final InputStreamReader reader = new InputStreamReader(
        AdacontrolProfile.class.getResourceAsStream(RULES_PROFILE_FILE));

    final RulesProfile adactlProfile = xmlLoader.parse(reader, messages);
    adactlProfile.setDefaultProfile(false);

    return adactlProfile;
  }
}
