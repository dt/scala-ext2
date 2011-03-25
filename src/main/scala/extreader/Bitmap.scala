// This file is part of ScalaFSR.  ScalaFSR is free software: you can
// redistribute it and/or modify it under the terms of the GNU General Public
// License as published by the Free Software Foundation, version 2.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// this program; if not, write to the Free Software Foundation, Inc., 51
// Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// (c) David Taylor and Daniel Freeman

package extreader

/**
*	Wrap a block representing a bitmap and provides methods to read a given bit
*/
class Bitmap(val block : Block ) {
	def bit(i: Long) = block.get1(i / 8).bit( i.toInt % 8 )
	def set(i: Long) = (bit(i) == 1)
}