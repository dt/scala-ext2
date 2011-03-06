import java.io.File
/*
  Some inspiration taken from jNode's EXT2 implementation
  http://gitorious.org/jnode/svn-mirror/trees/master/fs/src/fs/org/jnode/fs/ext2
*/

object Ext2Reader { 
	def main(args: Array[String]) { 
	  val image = new File(args(0))
	  println("File: "+image.getAbsolutePath)

	  val bytes = Bytes fromFile image
	  
		val searchForSuperblocks = false
		val searchForRootdir = true

	  if(searchForSuperblocks) {
			println("Searching for superblocks...")	  
		  var validSBs = Superblock findAllPossible bytes

		  println("Possible SBs: "+validSBs)
		  for( (i, score) <- validSBs.sortBy( t => t._2 ) ) {
		  	println("Possible Superblock ("+score+") at "+i)
		  	val sb = Superblock.loadFrom(bytes, i) 
		  			println("\tLocation: "+sb.bytes.trueOffset)
		  			println("\tMagic num: "+Hex.valueOf(sb.magicNum))
		 		 		println("\tLooks valid: "+sb.looksValid)
		  			println("\tLog Block Size: "+sb.logBlockSize)
		  			println("\tBlock Size: "+sb.blockSize)
		  			println("\tFirst Block: "+sb.firstBlock)
				
			}
		}

		println( Directory.findRootdir(bytes) )
		// Directory.scanAndBuildTree(bytes)
		

	}
}