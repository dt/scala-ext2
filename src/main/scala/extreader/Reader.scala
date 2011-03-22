package extreader

import java.io.File
/*
	Some inspiration taken from jNode's EXT2 implementation
	http://gitorious.org/jnode/svn-mirror/trees/master/fs/src/fs/org/jnode/fs/ext2
*/

object Reader { 
	def main(args: Array[String]) { 
		if(args.length < 1) {
			println("image name must be first argument")
			System.exit(-1)
		}

		val Assign = "(.*)=(.*)".r

		val image = new File(args(0))

		var cleanImg = Option.empty[String] 
		var forceBlocksize = Option.empty[Int]
		var overrideSB = Option.empty[Long]
		var groupPad = Option.empty[Int] 
		var skipJournal = false
		var findDeleted = false
		var dumpFiles = false

		for(i <- 1 until args.length) {
			args(i) match {
				case Assign(a,v) => {
					a.toLowerCase match {
						case "metaimage" => { cleanImg = Some(v) } 
						case "overrideSB" => { overrideSB = Some(v.toLong) }
						case "debug" => { extreader.showDebug = v.toBoolean }
						case "grouppad" => { groupPad = Some(v.toInt)}
						case "blocksize" => {
							val bs = v.toInt
							if((bs % 1024) != 0)
								println("Invalid block size")
							else 
								forceBlocksize = Some(bs)
						}
						case "skipjournal" => { skipJournal = v.toBoolean }
						case "finddeleted" => { findDeleted = v.toBoolean }
						case "dumpfiles" => { dumpFiles = v.toBoolean }

						case _ => { println("Unknown option: '"+a+"'") }
					}
				}
				case _ => println("Malformed option: "+args(i))
			}
		}

		println("Image file: "+image.getAbsolutePath)
		
		debug("Debug output enabled...")

		val bytes = Bytes fromFile image

		val cleanBytes = cleanImg.map{ cleanFile => {
			println("Loading alternate metadata image: "+cleanFile) 
			Bytes fromFile (new File(cleanFile)) 
		}}

		val superblock: Option[Superblock] = overrideSB match {
			case Some(pos) => {
				println("Using specified superblock location: "+pos)
				Some(Superblock(bytes.getRange(pos, Superblock.size))) }
			case None => {
				//finding our own superblock...
				// default first
				val primarySB = Superblock in bytes
			  if( primarySB isValid ) {
			  	println("Valid superblock in image")
					Some(primarySB)
				} else {
					cleanBytes flatMap { cbytes => { 
						val cleanSB = Superblock in cbytes

						if( cleanSB isValid ) {
							println("Superblock in image invalid, using one from alternate image")
							Some(cleanSB)
						} else { 
							None 
						}
					}
				}}
			}
		}

		superblock match {
			case Some(sb) => {
				println("Loading filesystem...")
				val fs = new FileSystem(bytes, sb, cleanBytes)
				groupPad.map{ x => fs.groupDescPad = x }
				forceBlocksize.map{ x => fs.blockSize = x }

				debug("File system info:")
				debug("\tblock size: "+fs.blockSize)
				debug("\tinode size: "+fs.inodeSize)
				debug("\tInodes per group: "+fs.inodesPerGroup)
				debug("\tBlocks per group: "+fs.blocksPerGroup)

				if ( !skipJournal ) {
					if( sb.journalEnabled) {
						print("Dumping journal... ")
						val journal = new FsFile(fs.inode(sb.journalInode), "journal")
						journal.dumpTo(new java.io.File("."))
						println("done.")
					} else println("Journal not enabled in superblock.")
				} else println("Skipping journal")

				val rootPos = DirectoryFinder findRootdir fs 

				debug(rootPos)

				val rootInode = fs.inode(2)

				val rootDir = Directory(rootInode, "/", findDeleted)

				printTree(rootDir, "")

				if(dumpFiles) 
					dumpTree(rootDir, new File("dump"))
			}
			case None => {
				println()
				println(Console.RED + "No superblock: " + Console.WHITE)
				println()

				println("\t* Provide a alternate location in image: overrideSB=<pos>")
				println("\t* Provide a alternate image for metadata: metaimage=<metaimg>")
				println()
				println("Would you like to search for possible superblocks? (y/n)")
				if( Console readBoolean ) {
					SuperblockFinder search bytes 
				}
			}
		}
	}

	def printTree(root: Directory, prefix: String) {
		println(prefix+ "+ "+root.name)
		for(dir <- root.subdirs) {
			printTree(dir, prefix + "	")
		}

		for(file <- root.files) {
			println(prefix+" - "+file.name)
		}
	}

	def dumpTree(root: Directory, target: File) {
		println("Dumping '"+root.name+"' to "+target+"...")

		target.mkdir()

		for(dir <- root.subdirs)
			dumpTree(dir, new File(target, dir.name))

		for(file <- root.files) {
			println("Dumping contents of '"+file.name+"' ("+file.inode.size+" bytes)...")
			file.dumpTo(target)
		}
	}
}
