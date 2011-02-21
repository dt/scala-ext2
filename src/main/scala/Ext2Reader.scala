import java.io.File
/*
  Some inspiration taken from jNode's EXT2 implementation
  http://gitorious.org/jnode/svn-mirror/trees/master/fs/src/fs/org/jnode/fs/ext2
*/

object Ext2Reader { 
	def main(args: Array[String]) { 
	  val file = new File(args(0))
	  println("File: "+file.getAbsolutePath)

	  val bytes = Bytes.readFromFile(file)

	  val sb = Superblock.loadFrom(bytes)

	  for(i <- 1024 to 2200 ) {
	  	print(i - 1024)
	  	print("\t")
	  	print("0x" + Hex.valueOf(bytes.get1(i)) )
	  	print("\t")
	  	println( bytes.get1(i) )
	  }
	  

	  println("Magic num: "+Hex.valueOf(sb.magicNum))
	  println("Log Block Size: "+sb.logBlockSize)
	  println("Block Size: "+sb.blockSize)

	}
}