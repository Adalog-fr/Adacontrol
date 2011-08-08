----------------------------------------------------------------------
--  Rules.Representation_Clauses - Package body                     --
--                                                                  --
--  This software  is (c) The European Organisation  for the Safety --
--  of Air  Navigation (EUROCONTROL) and Adalog  2004-2005. The Ada --
--  Controller  is  free software;  you can redistribute  it and/or --
--  modify  it under  terms of  the GNU  General Public  License as --
--  published by the Free Software Foundation; either version 2, or --
--  (at your  option) any later version.  This  unit is distributed --
--  in the hope  that it will be useful,  but WITHOUT ANY WARRANTY; --
--  without even the implied warranty of MERCHANTABILITY or FITNESS --
--  FOR A  PARTICULAR PURPOSE.  See the GNU  General Public License --
--  for more details.   You should have received a  copy of the GNU --
--  General Public License distributed  with this program; see file --
--  COPYING.   If not, write  to the  Free Software  Foundation, 59 --
--  Temple Place - Suite 330, Boston, MA 02111-1307, USA.           --
--                                                                  --
--  As  a special  exception, if  other files  instantiate generics --
--  from the units  of this program, or if you  link this unit with --
--  other files  to produce  an executable, this  unit does  not by --
--  itself cause the resulting executable  to be covered by the GNU --
--  General  Public  License.   This  exception  does  not  however --
--  invalidate any  other reasons why the executable  file might be --
--  covered by the GNU Public License.                              --
--                                                                  --
--  This  software is  distributed  in  the hope  that  it will  be --
--  useful,  but WITHOUT  ANY  WARRANTY; without  even the  implied --
--  warranty  of  MERCHANTABILITY   or  FITNESS  FOR  A  PARTICULAR --
--  PURPOSE.                                                        --
----------------------------------------------------------------------

-- Ada
with
  Ada.Strings.Wide_Unbounded;

-- ASIS
with
  Asis.Clauses,
  Asis.Elements,
  Asis.Expressions;

-- Adalog
with
  Binary_Map,
  Thick_Queries,
  Utilities;

-- Adactl
with
  Framework.Language,
  Framework.Rules_Manager,
  Framework.Reports;
pragma Elaborate (Framework.Language);

package body Rules.Representation_Clauses is
   use Framework, Ada.Strings.Wide_Unbounded, Utilities;

   type Clause_Names is (Cl_Attribute, Cl_At, CL_At_Mod, Cl_Enumeration, Cl_Record);

   package Clause_Flags_Utilities is new Framework.Language.Flag_Utilities (Clause_Names, "CL_");
   use Clause_Flags_Utilities;

   -- Clause => Label
   package Context_Map is new Binary_Map (Unbounded_Wide_String, Unbounded_Wide_String);
   use Context_Map;

   Usage     : array (Rule_Types) of Context_Map.Map;
   Rule_Used : Boolean := False;
   Save_Used : Boolean;
   Key       : array (Clause_Names range Clause_Names'Succ (Cl_Attribute) .. Clause_Names'Last)
                  of Unbounded_Wide_String;
   Key_All   : constant Unbounded_Wide_String := To_Unbounded_Wide_String ("all");

   ----------------
   -- Proper_Key --
   ----------------

   function Proper_Key (The_Key : Unbounded_Wide_String) return Wide_String is
      Img : constant Wide_String := To_Wide_String (The_Key);
   begin
      if Img (1) = ''' then
         -- attribute
         return Img;
      else
         -- Remove "Cl_"
         return To_Lower (Img (4 .. Img'Last));
      end if;
   end Proper_Key;

   ----------
   -- Help --
   ----------

   procedure Help is
   begin
      User_Message  ("Rule: " & Rule_Id);
      Help_On_Flags (Header => "Parameter(s):", Footer => "(optional)", Extra_Value => "<specifiable attribute>");
      User_Message  ("Control occurrences of representation clauses");
   end Help;

   -------------
   -- Add_Use --
   -------------

   procedure Add_Use (Label     : in Wide_String;
                      Rule_Type : in Rule_Types) is
      use Framework.Language;
      Clause : Clause_Names;
      Param  : Unbounded_Wide_String;

   begin
      if Parameter_Exists then
         if Is_Present (Usage (Rule_Type), Key_All) then
            Parameter_Error (Rule_Id, "rule already specified for all representation clauses");
         end if;
      else
         if not Is_Empty (Usage (Rule_Type)) then
            Parameter_Error (Rule_Id, "some representation clauses already specified");
         end if;
         Add (Usage (Rule_Type), Key_All, To_Unbounded_Wide_String (Label));
      end if;

      while Parameter_Exists loop
         Clause := Get_Flag_Parameter (Allow_Any => True);
         if Clause = Cl_Attribute then
            Param := To_Unbounded_Wide_String (To_Upper (Get_String_Parameter));
            if Element (Param, 1) /= ''' then
               Parameter_Error (Rule_Id, "parameter must be at, at_mod, enumeration, record, or an attribute");
            end if;
         else
            Param := To_Unbounded_Wide_String (Clause_Names'Wide_Image (Clause));
         end if;

         if Is_Present (Usage (Rule_Type), Param) then
            Parameter_Error (Rule_Id, "clause already given: " & Proper_Key (Param));
         end if;

         Add (Usage (Rule_Type), Param, To_Unbounded_Wide_String (Label));
      end loop;

      Rule_Used := True;
   end Add_Use;

   -------------
   -- Command --
   -------------

   procedure Command (Action : Framework.Rules_Manager.Rule_Action) is
      use Framework.Rules_Manager;
   begin
      case Action is
         when Clear =>
            for T in Usage'Range loop
               Clear (Usage (T));
            end loop;
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := False;
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;

   --------------------
   -- Process_Clause --
   --------------------

   procedure Process_Clause (Element : in Asis.Representation_Clause) is
      use Asis, Asis.Clauses, Asis.Elements, Asis.Expressions, Framework.Reports, Thick_Queries;
      Attribute : Unbounded_Wide_String;

      procedure Check_Usage (Clause : Clause_Names; Message : Wide_String) is
         Key_Map : Unbounded_Wide_String;
      begin
         if Clause = Cl_Attribute then
            Key_Map := Attribute;
         else
            Key_Map := Key (Clause);
         end if;

         -- Report the highest priority from Check/Search
         if Is_Present (Usage (Check), Key_Map) then
            Report (Rule_Id,
                    To_Wide_String (Fetch (Usage (Check), Key_Map)),
                    Check,
                    Get_Location (Element),
                    Message);
         elsif Is_Present (Usage (Check), Key_All) then
            Report (Rule_Id,
                    To_Wide_String (Fetch (Usage (Check), Key_All)),
                    Check,
                    Get_Location (Element),
                    Message);
         elsif Is_Present (Usage (Search), Key_Map) then
            Report (Rule_Id,
                    To_Wide_String (Fetch (Usage (Search), Key_Map)),
                    Search,
                    Get_Location (Element),
                    Message);
         elsif Is_Present (Usage (Search), Key_All) then
            Report (Rule_Id,
                    To_Wide_String (Fetch (Usage (Search), Key_All)),
                    Search,
                    Get_Location (Element),
                    Message);
         end if;

         -- Always report Count
         if Is_Present (Usage (Count), Key_Map) then
            Report (Rule_Id,
                    To_Wide_String (Fetch (Usage (Count), Key_Map)),
                    Count,
                    Get_Location (Element),
                    Message);
         elsif Is_Present (Usage (Count), Key_All) then
            Report (Rule_Id,
                    To_Wide_String (Fetch (Usage (Count), Key_All)),
                    Count,
                    Get_Location (Element),
                    Message);
         end if;
     end Check_Usage;

   begin   -- Process_Clause
      if not Rule_Used then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      case Representation_Clause_Kind (Element) is
         when Not_A_Representation_Clause =>
            Failure ("Not a representation clause in " & Rule_Id);

         when An_Attribute_Definition_Clause =>
            Attribute := To_Unbounded_Wide_String (''' & To_Upper (Attribute_Name_Image
                                                                   (Representation_Clause_Name (Element))));
            if Expression_Kind (Prefix (Representation_Clause_Name (Element))) = An_Attribute_Reference then
               -- This happens only in for T'Class'Read and similar

               -- ASIS BUG:
               -- Attribute_Designator_Identifier returns wrong value on double attributes.
               -- (Attribute_Designator_Identifer is used by Attribute_Name_Image)

               -- The following case statement should be:
               --    Attribute := "'CLASS" & Attribute;
               -- Unfortunately, Attribute is wrong, hence the following kludge.
               -- Note that we call Asis.Elements.Attribute_Kind, not A4G_Bugs.Attribute_Kind
               -- because the version in A4G_Bugs does not work due to the same ASIS bug, but the
               -- regular version seems to work in this case. Sigh.
               case Attribute_Kind (Representation_Clause_Name (Element)) is --## RULE LINE OFF Use_A4G_Bugs
                  when A_Read_Attribute =>
                     Attribute := To_Unbounded_Wide_String ("'CLASS'READ");
                  when A_Write_Attribute =>
                     Attribute := To_Unbounded_Wide_String ("'CLASS'WRITE");
                  when An_Input_Attribute =>
                     Attribute := To_Unbounded_Wide_String ("'CLASS'INPUT");
                  when An_Output_Attribute =>
                     Attribute := To_Unbounded_Wide_String ("'CLASS'OUTPUT");
                  when others =>
                     Failure ("Unexpected double attribute in " & Rule_Id, Element);
               end case;
            end if;

            Check_Usage (Cl_Attribute, "use of representation clause for " & To_Wide_String (Attribute));

         when An_Enumeration_Representation_Clause =>
            Check_Usage (Cl_Enumeration, "use of enumeration representation clause");

         when A_Record_Representation_Clause =>
            Check_Usage (Cl_Record, "use of record representation clause");

            if not Is_Nil (Mod_Clause_Expression (Element)) then
               Check_Usage (CL_At_Mod, "use of Ada 83 alignment clause");
            end if;

         when An_At_Clause =>
            Check_Usage (Cl_At, "use of Ada 83 address clause");
      end case;
   end Process_Clause;

begin
   for K in Clause_Names range Clause_Names'Succ (Cl_Attribute) .. Clause_Names'Last loop
      Key (K) := To_Unbounded_Wide_String (Clause_Names'Wide_Image (K));
   end loop;

   Framework.Rules_Manager.Register_Semantic (Rule_Id,
                                              Help    => Help'Access,
                                              Add_Use => Add_Use'Access,
                                              Command => Command'Access);
end Rules.Representation_Clauses;
