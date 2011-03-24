package extreader

import java.io.{File, OutputStream, FileOutputStream}

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