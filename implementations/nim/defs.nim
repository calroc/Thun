#[

    Copyright © 2021 Simon Forman

    This file is part of Bliss

    Bliss is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Bliss is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Bliss.  If not see <http://www.gnu.org/licenses/>.


]#
import streams, strutils, fp, reader, types

proc add_def(def: string, dictionary: var JoyMapType) =
  let d = read_str(def)
  let sym = d.head
  case sym.kind:
  of joyAtom:
    dictionary = dictionary + (sym.atomVal, d.tail)
  else:
    raise newException(ValueError, def)

proc defs_file2dict(defs_filename: string = "defs.txt"): JoyMapType =
  var strm = newFileStream(defs_filename, fmRead)
  var dictionary = newMap[string, JoyListType]()
  var line = ""
  if not isNil(strm):
    while strm.readLine(line):
      if line.isEmptyOrWhitespace:
        continue
      add_def(line, dictionary)
    strm.close()
  return dictionary

var dictionary* = defs_file2dict()
