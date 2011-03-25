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

import java.io.{File, OutputStream, FileOutputStream}

/**
*	Wraps an inode (along with name from dir-rec) adding file-specific methods
*/
class FsFile(val inode: Inode, val name : String) {
	def dumpTo(targetDir: File) {
		val out = new FileOutputStream( new File(targetDir, name) )

		var copied = 0

		for(block <- inode.blocks) {
			val remaining = inode.size - copied
			if(remaining > inode.fs.blockSize) {
				block.writeTo(out)
				copied += inode.fs.blockSize
			} else {
				block.getRange(0, remaining).writeTo(out)
			}
		}
	}
}