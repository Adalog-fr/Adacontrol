----------------------------------------------------------------------
--  Framework.String_Set - Package specification                    --
--  Copyright (C) 2005 Adalog                                       --
--  Author: J-P. Rosen                                              --
--                                                                  --
--  ADALOG   is   providing   training,   consultancy,   expertise, --
--  assistance and custom developments  in Ada and related software --
--  engineering techniques.  For more info about our services:      --
--  ADALOG                   Tel: +33 1 41 24 31 40                 --
--  19-21 rue du 8 mai 1945  Fax: +33 1 41 24 07 36                 --
--  94110 ARCUEIL            E-m: info@adalog.fr                    --
--  FRANCE                   URL: http://www.adalog.fr              --
--                                                                  --
--  This  unit is  free software;  you can  redistribute  it and/or --
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
--  from  this unit,  or you  link this  unit with  other  files to --
--  produce an executable,  this unit does not by  itself cause the --
--  resulting executable  to be covered  by the GNU  General Public --
--  License.  This exception does  not however invalidate any other --
--  reasons why  the executable  file might be  covered by  the GNU --
--  Public License.                                                 --
----------------------------------------------------------------------
with --  Adalog
  Binary_Map;

package Framework.String_Set is
   type Set is private;     -- Object semantic
   Empty_Set : constant Set;

   procedure Add        (To   : in out Set; Key : in Wide_String);
   procedure Delete     (From : in out Set; Key : in Wide_String);

   function Is_Empty (The_Set : in Set) return Boolean;
   -- Check if there are elements
   function  Is_Present (Within : in Set; Key : in Wide_String) return Boolean;

   procedure Balance (The_Set : in out Set);
   -- Rebalance the binary map.

   procedure Clear (The_Set : in out Set);
   -- Clear all elements

private
   type Null_Record is null record;
   package Null_Map is new Binary_Map (Unbounded_Wide_String, Null_Record);
   type Set is new Null_Map.Map;
   Empty_Set : constant Set := Set (Null_Map.Empty_Map);
end Framework.String_Set;


