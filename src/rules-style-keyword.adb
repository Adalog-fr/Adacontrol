----------------------------------------------------------------------
--  Rules.Style.Keyword - Package body                              --
--                                                                  --
--  This software  is (c) The European Organisation  for the Safety --
--  of Air Navigation (EUROCONTROL) and Adalog 2004-2005.           --
--  The Ada Controller is  free software; you can  redistribute  it --
--  and/or modify it under  terms of the GNU General Public License --
--  as published by the Free Software Foundation; either version 2, --
--  or (at your option) any later version. This unit is distributed --
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
----------------------------------------------------------------------

-- Ada
with
  Ada.Characters.Handling,
  Ada.Strings.Wide_Maps.Wide_Constants;

--ASIS
with
  Asis.Text;

-- Adalog
with
  Utilities;

-- AdaCtl
with
  Framework.Reports.Fixes,
  Framework.Reports;

package body Rules.Style.Keyword is

   -- Algorithm
   --
   -- There is no way to manage keywords from the tree, since keywords have disappeared at that level!
   -- Therefore, we need to scan the source line, which requires a kind of lexical analyzer.
   --
   -- We use an automat, where each "state" is the current letter. If the letter matches, the next state
   -- is the next entry in the automat. If it does not match, the next possible state (if any) is given
   -- by the "if_not_matched" entry.
   --
   -- Note that this algorithm is such that the source is scanned only once, with only one comparison
   -- per letter. Efficiency is a concern here, since the whole text is parsed!

   type Index is range 0 .. 352;
   subtype Positive_Index is Index range 1 .. Index'Last;
   type Node is
      record
         Char           : Wide_Character;
         If_Not_Matched : Index;
      end record;

   Automat : constant array (Positive_Index) of Node :=
                -------------'a'
                --1 abort
               (('b',14), ('o',6),  ('r',0), ('t',0),  ('.',0),
                --6 abs
                          ('s',0),  ('.',8),
                --8 abstract
                                    ('t',0), ('r',0),  ('a',0), ('c',0), ('t',0), ('.',0),
                --14 accept
                ('c',23), ('c',0),  ('e',0), ('p',20), ('t',0), ('.',0),
                --20 access
                                             ('s',0),  ('s',0), ('.',0),
                --23 aliased
                ('l',32), ('i',30), ('a',0), ('s',0),  ('e',0), ('d',0), ('.',0),
                --30 all
                          ('l',0),  ('.',0),
                --32 and
                ('n',35), ('d',0),  ('.',0),
                --35 array
                ('r',40), ('r',0),  ('a',0), ('y',0), ('.',0),
                --40 at
                ('t', 0),  ('.', 0),

                -------------'b'
                --42 begin
                ('e',47), ('g',0),  ('i',0), ('n',0), ('.',0),
                --47 body
                ('o',0),  ('d',0),  ('y',0), ('.',0),

                -------------'c'
                --51 case
                ('a',55), ('s',0),  ('e',0), ('.',0),
                --55 constant
                ('o',0),  ('n',0),  ('s',0), ('t',0), ('a',0), ('n',0), ('t',0), ('.',0),

                -------------'d'
                --63 declare
                ('e',77), ('c',70), ('l',0), ('a',0), ('r',0), ('e',0), ('.',0),
                --70 delay
                          ('l',0),  ('a',74), ('y',0), ('.',0),
                --74 delta
                                    ('t',0), ('a',0), ('.',0),
                --77 digits
                ('i',83), ('g',0),  ('i',0), ('t',0), ('s',0), ('.',0),
                --83 do
                ('o',0),  ('.',0),

                -------------'e'
                --85 else
                ('l',92),  ('s',0),   ('e',89), ('.',0),
                --89 elsif
                                      ('i',0),  ('f',0), ('.',0),
                --92 end
                ('n',99),  ('d',95),  ('.',0),
                --95 entry
                           ('t',0),   ('r',0), ('y',0), ('.',0),
                --99 exception
                ('x',0),   ('c',108), ('e',0), ('p',0), ('t',0), ('i',0), ('o',0), ('n',0), ('.',0),
                --108 exit
                           ('i',0),   ('t',0), ('.',0),

                -------------'f'
                --111 for
                ('o',114), ('r',0),   ('.',0),
                --114 function
                ('u',0),   ('n',0),   ('c',0), ('t',0), ('i',0), ('o',0), ('n',0), ('.',0),

                -------------'g'
                --122 generic
                ('e',129), ('n',0),   ('e',0), ('r',0), ('i',0), ('c',0), ('.',0),
                --129 goto
                ('o',0),   ('t',0),   ('o',0), ('.',0),

                -------------'i'
                --133 if
                ('f',135),  ('.',0),
                --135 in
                ('n', 145), ('.',137),
                --137 interface
                            ('t',0),   ('e',0), ('r',0), ('f',0), ('a',0), ('c',0), ('e',0), ('.',0),
                --145 is
                ('s',0),    ('.',0),

                -------------'l'
                --147 limited
                ('i',154), ('m',0), ('i',0), ('t',0), ('e',0), ('d',0), ('.',0),
                --154 loop
                ('o',0),   ('o',0), ('p',0), ('.',0),

                -------------'m'
                --158 mod
                ('o',0),   ('d',0), ('.',0),

                -------------'n'
                --161 new
                ('e',164), ('w',0), ('.',0),
                --164 not
                ('o',167), ('t',0), ('.',0),
                --167 null
                ('u',0),   ('l',0), ('l',0), ('.',0),

                -------------'o'
                --171 of
                ('f',173), ('.',0),
                --173 or
                ('r',175), ('.',0),
                --175 others
                ('t',181), ('h',0), ('e',0), ('r',0), ('s',0), ('.',0),
                --181 out
                ('u',184), ('t',0), ('.',0),
                -- 184 overriding
                ('v',0),   ('e',0), ('r',0), ('r',0), ('i',0), ('d',0), ('i',0), ('n',0), ('g',0), ('.',0),

                -------------'p'
                --194 package
                ('a',201), ('c',0),   ('k',0),   ('a',0), ('g',0), ('e',0), ('.',0),
                --201 pragma
                ('r',0),   ('a',207), ('g',0),   ('m',0), ('a',0), ('.',0),
                --207 private
                           ('i', 213), ('v',0),   ('a',0), ('t',0), ('e',0), ('.',0),
                --213 procedure
                           ('o',0),   ('c',221), ('e',0), ('d',0), ('u',0), ('r',0), ('e',0), ('.',0),
                --221 protected
                                      ('t',0), ('e',0), ('c',0), ('t',0), ('e',0), ('d',0), ('.',0),

                -------------'r'
                --228 raise
                ('a',237), ('i',233), ('s',0), ('e',0), ('.',0),
                --233 range
                           ('n',0),   ('g',0), ('e',0), ('.',0),
                --237 record
                ('e',0),   ('c',243), ('o',0), ('r',0), ('d',0), ('.',0),
                --243 rem
                           ('m',245), ('.',0),
                --245 renames
                           ('n',251), ('a',0), ('m',0), ('e',0), ('s',0), ('.',0),
                --251 requeue
                           ('q',257), ('u',0), ('e',0), ('u',0), ('e',0), ('.',0),
                --257 return
                           ('t',262), ('u',0), ('r',0), ('n',0), ('.',0),
                --262 reverse
                           ('v',0),   ('e',0), ('r',0), ('s',0), ('e',0), ('.',0),

                -------------'s'
                --268 select
                ('e',281), ('l',274), ('e',0), ('c',0), ('t',0), ('.',0),
                --274 separate
                           ('p',0),   ('a',0), ('r',0), ('a',0), ('t',0), ('e',0), ('.',0),
                --281 some
                ('o',285), ('m',0),   ('e',0), ('.',0),
                --285 subtype
                ('u',292), ('b',0),   ('t',0), ('y',0), ('p',0), ('e',0), ('.',0),
                --292 Synchronized
                ('y', 0), ('n',0),    ('c',0), ('h',0), ('r',0), ('o',0), ('n',0), ('i',0), ('z',0), ('e',0), ('d',0),
                          ('.', 0),

                -------------'t'
                --304 tagged
                ('a',313), ('g',310), ('g',0), ('e',0), ('d',0), ('.',0),
                --310 task
                           ('s',0),   ('k',0), ('.',0),
                --313 terminate
                ('e',322), ('r',0),   ('m',0), ('i',0), ('n',0), ('a',0), ('t',0), ('e',0), ('.',0),
                --322 then
                ('h',326), ('e',0),   ('n',0), ('.',0),
                --326 type
                ('y',0),   ('p',0),   ('e',0), ('.',0),

                -------------'u'
                --330 until
                ('n',335), ('t',0),   ('i',0), ('l',0), ('.',0),
                --335 use
                ('s',0),   ('e',0),   ('.',0),

                -------------'w'
                --338 when
                ('h',346), ('e',342), ('n',0), ('.',0),
                --342 while
                           ('i',0),   ('l',0), ('e',0), ('.',0),
                --346 with
                ('i',0), ('t',0), ('h',0), ('.',0),

                -------------'x'
                --350 xor
                ('o',0), ('r',0), ('.',0)
               );
   Start : constant array (Wide_Character range 'a' .. 'z') of Index :=
             ('a' => 1,
              'b' => 42,
              'c' => 51,
              'd' => 63,
              'e' => 85,
              'f' => 111,
              'g' => 122,
              'i' => 133,
              'l' => 147,
              'm' => 158,
              'n' => 161,
              'o' => 171,
              'p' => 194,
              'r' => 228,
              's' => 268,
              't' => 304,
              'u' => 330,
              'w' => 338,
              'x' => 350,
              others => 0);

   use Ada.Characters.Handling, Ada.Strings.Wide_Maps;
   Number_Set : constant Wide_Character_Set
     := To_Set (Ranges => (('0', '9'), ('_', '_'), ('#', '#'), ('a', 'f'), ('A', 'F')));
   Identifier_Set : constant Ada.Strings.Wide_Maps.Wide_Character_Set
     := To_Set (Ranges => (('a', 'z'), ('A', 'Z'), ('_', '_'), ('0', '9'),
                           (Wide_Character'Succ (To_Wide_Character (Character'Last)), Wide_Character'Last)));
   ------------------
   -- Process_Line --
   ------------------

   Previous_Is_Tick : Boolean := False;
   -- If the character before the start of a word is a single quote, the word cannot be
   -- a keyword. We need this special trick because of 'Access and 'Range. This needs to
   -- be a global variable, because the quote is not necessarily on the same line as the
   -- word; the following is legal Ada:
   --    for I in S
   --    '
   --    Range loop ...
   -- We need to be careful however, because 'in' is a keyword in:
   --    if 'a' in character then ...
   procedure Process_Line (Line     : in Asis.Program_Text;
                           Loc      : in Framework.Locations.Location;
                           Expected : in Casing_Set)
   is
      use Ada.Strings, Ada.Strings.Wide_Maps.Wide_Constants;
      use Utilities;

      type States is (Search_Begin, In_Quotes, In_Number, Skipping, Analyzing);
      State    : States := Search_Begin;
      Kw_State : Index;
      Lower_C  : Wide_Character;
      First    : Positive;
      Last     : Natural := Line'Last;

      type Casing is (Upper, Lower, Title, Mixed, Unknown);
      Case_First, Case_Others : Casing;

      procedure Do_Report (Kw_Start, Kw_Stop : Positive) is
         use Framework.Locations, Framework.Reports;
         KW_Loc : constant Location := Create_Location (Get_File_Name (Loc),
                                                        Get_First_Line (Loc),
                                                        Asis.Text.Character_Position (Kw_Start));
      begin  -- Do_Report
         Report (Rule_Id,
                 Corresponding_Context (St_Casing_Keyword),
                 KW_Loc,
                 "Wrong casing of """ & Line (Kw_Start .. Kw_Stop)
                 & """, should be " & Should_Be (Line (Kw_Start .. Kw_Stop), Expected, For_Fix => False));
         Fixes.Replace (KW_Loc, Kw_Stop-Kw_Start+1, Should_Be (Line (Kw_Start .. Kw_Stop), Expected, For_Fix =>  True));
      end Do_Report;

   begin  -- Process_Line
      for I in Line'Range loop
         if Line (I) = '-' and then I /= Line'Last and then Line (I + 1) = '-' then
            -- Comment
            Last := I-1;
            exit;
         end if;

         case State is
            when Search_Begin =>
               if Previous_Is_Tick and Is_In (Line (I), Identifier_Set) then
                  -- Assume it is the beginning of an identifier, cannot be a keyword
                  State := Skipping;
               elsif Line (I) = '"' then
                  -- beware of '"'
                  if I = Line'First or else Line (I - 1) /= ''' then
                     State := In_Quotes;
                  end if;
               elsif Line (I) in '0' .. '9' then
                  State := In_Number;
               else
                  Lower_C := Value (Lower_Case_Map, Line (I));
                  if Lower_C in 'a' .. 'z' then
                     Kw_State := Start (Lower_C);
                     if Kw_State = 0 then
                        State := Skipping;
                     else
                        State := Analyzing;
                        if Line (I) = Lower_C then
                           Case_First := Lower;
                        else
                           Case_First := Upper;
                        end if;
                        Case_Others := Unknown;
                        First := I;
                     end if;
                  end if;
               end if;

            when In_Quotes =>
               if Line (I) = '"' then
                  State := Search_Begin;
               end if;

            when In_Number =>
               if not Is_In (Line (I), Number_Set) then
                  State := Search_Begin;
               end if;

            when Skipping =>
               if not Is_In (Line (I), Identifier_Set) then
                  State := Search_Begin;
               end if;

            when Analyzing =>
               Lower_C := Value (Lower_Case_Map, Line (I));
               if Lower_C in 'a' .. 'z' then
                  loop
                     if Lower_C = Automat (Kw_State).Char then
                        Kw_State := Kw_State + 1;
                        case Case_Others is
                           when Upper =>
                              if Line (I) = Lower_C then
                                 Case_Others := Mixed;
                              end if;
                           when Lower =>
                              if Line (I) /= Lower_C then
                                 Case_Others := Mixed;
                              end if;
                           when Title =>
                              Failure ("Case_Others is Title");
                           when Mixed =>
                              null;
                           when Unknown =>
                              if Line (I) = Lower_C then
                                 Case_Others := Lower;
                              else
                                 Case_Others := Upper;
                              end if;
                        end case;
                        exit;
                     end if;
                     Kw_State := Automat (Kw_State).If_Not_Matched;
                     if Kw_State = 0 then
                        State := Skipping;
                        exit;
                     end if;
                  end loop;
               elsif not Is_In (Lower_C, Identifier_Set) and then Automat (Kw_State).Char = '.' then
                  -- Keyword found
                  case Case_Others is
                     when Upper =>
                        if Case_First /= Upper or else not Expected (Ca_Uppercase) then
                           Do_Report (First, I-1);
                        end if;
                     when Lower =>
                        if Case_First = Upper then
                           if not Expected (Ca_Titlecase) then
                              Do_Report (First, I - 1);
                           end if;
                        else
                           if not Expected (Ca_Lowercase) then
                              Do_Report (First, I - 1);
                           end if;
                        end if;
                     when Title =>
                        Failure ("Case_Others is Title");
                     when Mixed =>
                        Do_Report (First, I - 1);
                     when Unknown =>
                        Failure ("Case_Others is Unknown");
                  end case;

                  State := Search_Begin;
               elsif not Is_In (Line (I), Identifier_Set) then
                  State := Search_Begin;
               else
                  State := Skipping;
               end if;
         end case;

         if State /= In_Quotes then
            case Line (I) is
               when Wide_Space | Framework.Wide_HT =>
                  null;
               when ''' =>
                  -- The following is not perfectly correct in a general parser to recognize
                  -- a tick from an attribute, because of things like character'('a')
                  -- However, it is sufficient here since we just want to protect against
                  -- 'range and 'access
                  Previous_Is_Tick := I > Line'First + 1 and then Line (I-2) /= ''';
               when others =>
                  Previous_Is_Tick := False;
            end case;
         end if;
      end loop;

      if State = Analyzing and then Automat (Kw_State).Char = '.' then
         -- Line ended with keyword
         case Case_Others is
            when Upper =>
               if Case_First /= Upper or else not Expected (Ca_Uppercase) then
                  Do_Report (First, Last);
               end if;
            when Lower =>
               if Case_First = Upper then
                  if not Expected (Ca_Titlecase) then
                     Do_Report (First, Last);
                  end if;
               else
                  if not Expected (Ca_Lowercase) then
                     Do_Report (First, Last);
                  end if;
               end if;
            when Title =>
               Failure ("Case_Others is Title");
            when Mixed =>
               Do_Report (First, Last);
            when Unknown =>
               Failure ("Case_Others is Unknown");
         end case;
      end if;
   end Process_Line;

end Rules.Style.Keyword;
