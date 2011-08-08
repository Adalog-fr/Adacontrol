----------------------------------------------------------------------
--  Rules.Parameter_Aliasing - Package body                         --
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
  Ada.Strings.Wide_Fixed,
  Ada.Strings.Wide_Unbounded;

-- Asis
with
  Asis.Declarations,
  Asis.Elements,
  Asis.Expressions,
  Asis.Statements,
  Asis.Text;

-- Adalog
with
  Thick_Queries,
  Utilities;

-- Adactl
with
  Framework.Language,
  Framework.Rules_Manager,
  Framework.Reports;
pragma Elaborate (Framework.Language);

package body Rules.Parameter_Aliasing is
   use Framework, Utilities;

   --  Algorithm:
   --  Simply determine the "proximity" between each pair of [in] out parameters.
   --  See Thick_Queries.Variables_Proximity for the definition of proximity.

   subtype Rule_Detail is Thick_Queries.Result_Confidence;

   package Detail_Flags_Utilities is new Framework.Language.Flag_Utilities (Rule_Detail);
   use Detail_Flags_Utilities;

   type Usage is array (Rule_Detail) of Boolean;
   Rule_Used  : Usage := (others => False);
   Save_Used  : Usage;
   Rule_Type  : array (Rule_Detail) of Rule_Types;
   Rule_Label : array (Rule_Detail) of Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;

   ----------
   -- Help --
   ----------

   procedure Help is
   begin
      User_Message  ("Rule: " & Rule_Id);
      Help_On_Flags (Header => "Parameter 1:", Footer => "(optional, default=certain)");
      User_Message  ("Control subprogram or entry calls where the same variable is given");
      User_Message  ("for more than one [in] out parameter.");
      User_Message  ("This rule can detect non-straightforward aliasing cases, see doc for details");
   end Help;

   -------------
   -- Add_Use --
   -------------

   procedure Add_Use (Label         : in Wide_String;
                      Rule_Use_Type : in Rule_Types) is
      use Ada.Strings.Wide_Unbounded;
      use Framework.Language, Thick_Queries;

      Detail  : Rule_Detail := Certain;
   begin
      if Parameter_Exists then
         Detail := Get_Flag_Parameter (Allow_Any => False);
      end if;

      if Rule_Used (Detail) then
         Parameter_Error ("Rule " & Rule_Id & " can be called only once for ""Certain"","
                            & " once for ""Possible"","
                            & " and once for ""Unlikely""");
      end if;

      if Parameter_Exists then
         Parameter_Error ("Only one parameter for rule " & Rule_Id);
      end if;

      Rule_Type  (Detail) := Rule_Use_Type;
      Rule_Label (Detail) := To_Unbounded_Wide_String (Label);
      Rule_Used  (Detail) := True;
   end Add_Use;


   -------------
   -- Command --
   -------------

   procedure Command (Action : Framework.Rules_Manager.Rule_Action) is
      use Ada.Strings.Wide_Unbounded, Framework.Rules_Manager;
   begin
      case Action is
         when Clear =>
            Rule_Used  := (others => False);
            Rule_Label := (others => Null_Unbounded_Wide_String);
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := (others => False);
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;

   -------------
   -- Prepare --
   -------------

   procedure Prepare is
      use Thick_Queries;
   begin
      -- If weaker checks have been specified, force them for stronger ones
      if Rule_Used (Unlikely) and not Rule_Used (Possible) then
         Rule_Used  (Possible) := True;
         Rule_Type  (Possible) := Rule_Type  (Unlikely);
         Rule_Label (Possible) := Rule_Label (Unlikely);
      end if;
      if Rule_Used (Possible) and not Rule_Used (Certain) then
         Rule_Used  (Certain) := True;
         Rule_Type  (Certain) := Rule_Type  (Possible);
         Rule_Label (Certain) := Rule_Label (Possible);
      end if;
   end Prepare;


   ------------------
   -- Process_Call --
   ------------------

   type Parameters_Table is array (Asis.List_Index range <>) of Asis.List_Index;

   -- NB:
   -- Some of the algorithms in this procedure are a bit convoluted, because we avoid
   -- using normalized formals and actuals list, which are UNIMPLEMENTED in some versions
   -- of ASIS-for-Gnat.
   -- Some rewriting might be in order when the problem goes away...

   procedure Process_Call (Call : in Asis.Statement) is
      use Asis, Asis.Declarations, Asis.Elements, Asis.Expressions, Asis.Statements;
      use Thick_Queries, Framework.Reports, Ada.Strings.Wide_Unbounded;

   begin
      if Rule_Used = (Rule_Detail => False) then
         return;
      end if;
      Rules_Manager.Enter (Rule_Id);

      if Is_Dispatching_Call (Call) then
         -- Improvement needed here, but it's quite difficult
         return;
      end if;

      if Expression_Kind (Called_Simple_Name (Call)) = An_Attribute_Reference then
         -- These ('Read and 'Write) are known to not have parameters that allow aliasing
         -- Moreover, the rest of the algorithm wouldn't work since parameters of
         -- attributes SP have no "name"
         return;
      end if;

      declare
         Actuals : constant Asis.Association_List := Call_Statement_Parameters (Call);
         To_Check_Parameters : Parameters_Table (Actuals'Range);

         function Association_Image (Position : List_Index) return Wide_String is
            -- Image of a parameter association
            -- Calls the correct function depending on whether Name is a Defining_Name or a
            -- plain identifier.
            -- This kludge is needed because currently the function Formal_Name is
            -- inconsistent, depending on whether the actual association is positionnal or named
            use Asis.Text, Ada.Strings, Ada.Strings.Wide_Fixed;

            Name : constant Asis.Name := Formal_Name (Call, Position);
         begin
            if Element_Kind (Name) = A_Defining_Name then
               return '"' & Defining_Name_Image (Name) & " => "
                 & Trim (Element_Image (Actual_Parameter (Actuals (Position))), Both) & '"';
            else
               return '"' & Name_Image (Name) & " => "
                 & Trim (Element_Image (Actual_Parameter (Actuals (Position))), Both) & '"';
            end if;
         end Association_Image;

         Mode    : Mode_Kinds;
         TCP_Top : ASIS_Natural := To_Check_Parameters'First - 1;

         pragma Warnings (Off, To_Check_Parameters);
         -- GNAT warns that To_Check_Parameters may be used before it has a value,
         -- but the algorithm ensures that this does not happen, because the loop on J
         -- is not executed the first time.

         Param_Proximity : Proximity;
      begin
         for I in Actuals'Range loop
            Mode := Mode_Kind (Enclosing_Element (Formal_Name (Call, I)));

            if Mode in An_Out_Mode .. An_In_Out_Mode then
               for J in List_Index range To_Check_Parameters'First .. TCP_Top loop
                  Param_Proximity := Variables_Proximity (Actual_Parameter (Actuals (To_Check_Parameters (J))),
                                                          Actual_Parameter (Actuals (I)));
                  if Rule_Used (Param_Proximity.Confidence) and then Param_Proximity.Overlap /= None then
                     Report (Rule_Id,
                             To_Wide_String (Rule_Label (Param_Proximity.Confidence)),
                             Rule_Type (Param_Proximity.Confidence),
                             Get_Location (Call),
                             Choose (Param_Proximity.Confidence = Certain,
                                     "Certain",
                                     Choose (Param_Proximity.Confidence = Possible,
                                             "Possible",
                                             "Unlikely"))
                             & " aliasing between parameters "
                             & Association_Image (To_Check_Parameters (J))
                             & " and "
                             & Association_Image (I)
                            );
                  end if;
               end loop;

               TCP_Top := TCP_Top + 1;
               To_Check_Parameters (TCP_Top) := I;
            end if;
         end loop;
      end;
   end Process_Call;

begin
   Framework.Rules_Manager.Register_Semantic (Rule_Id,
                                              Help    => Help'Access,
                                              Add_Use => Add_Use'Access,
                                              Command => Command'Access,
                                              Prepare => Prepare'Access);
end Rules.Parameter_Aliasing;
