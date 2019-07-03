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

import lombok.extern.slf4j.Slf4j;
import org.sonar.api.SonarRuntime;
import org.sonar.api.server.profile.BuiltInQualityProfilesDefinition;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import java.io.InputStream;

/**
 * Load the quality profile for AdaControl.
 *
 * The quality profile is defined in an XML file shipped with this plug-in.
 * It activates all known rules by default. See /default-profile.xml for more details.
 */
@Slf4j
public class AdacontrolProfile implements BuiltInQualityProfilesDefinition {
  public static final String ADACONTROL_WAY_PROFILE = "AdaControl way";
  public static final String RULES_PROFILE_FILE = "/adacontrol-profile.xml";

  private final SonarRuntime sonarRuntime;

  public AdacontrolProfile(SonarRuntime sonarRuntime) {
    this.sonarRuntime = sonarRuntime;
  }

  /**
   * Read default-profile.xml and activate all rules for the built in Quality profile.
   * @param profile  built in quality profile
   * @param fileName the default profile XML file
   */
  private void readXMLFile(NewBuiltInQualityProfile profile, String fileName) {
    try {
      log.debug("Loading XML profile file: {}", fileName);
      InputStream fXmlFileStream = this.getClass().getResourceAsStream(fileName);
      DocumentBuilderFactory dbFactory = DocumentBuilderFactory.newInstance();
      DocumentBuilder dBuilder = dbFactory.newDocumentBuilder();
      Document doc = dBuilder.parse(fXmlFileStream);
      doc.getDocumentElement().normalize();
      NodeList nList = doc.getElementsByTagName("rule");
      for (int ruleIdx = 0; ruleIdx < nList.getLength(); ruleIdx++) {
        Node nNode = nList.item(ruleIdx);
        if (nNode.getNodeType() == Node.ELEMENT_NODE) {
          Element eElement = (Element) nNode;
          String ruleName = eElement.getElementsByTagName("key").item(0).getTextContent();
          String repoName = eElement.getElementsByTagName("repositoryKey").item(0).getTextContent();
          profile.activateRule(repoName, ruleName);
        }
      }
    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  public void define(Context context) {
    NewBuiltInQualityProfile profile = context.createBuiltInQualityProfile(ADACONTROL_WAY_PROFILE, "ada");
    profile.setDefault(false);
    readXMLFile(profile, RULES_PROFILE_FILE);
    profile.done();
  }
}
