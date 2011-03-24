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
		var dumpJournal = false
		var parseJournal = false
		var findDeleted = false
		var dumpFiles = false
		var extractDirTree = false
		var loadTree = true

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
						case "dumpjournal" => { dumpJournal = v.toBoolean }
						case "parsejournal" => { parseJournal = v.toBoolean }
						case "finddeleted" => { findDeleted = v.toBoolean }
						case "dumpfiles" => { dumpFiles = v.toBoolean }
						case "loadTree"	=> { loadTree = v.toBoolean }
						case "extractdirtree" => { extractDirTree = v.toBoolean }

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

				var journalFile = Option.empty[FsFile]

				if ( !skipJournal ) {
					if( sb.journalEnabled) {
						println("Reading journal...")
						val journalContent = new FsFile(fs.inode(sb.journalInode), "journal")
						journalFile = Some(journalContent)
						if (dumpJournal || parseJournal) {
							print("\t* Dumping to disk...")
							debugOff { journalContent.dumpTo(new File(".")) }
							println(" done.")
						}
						if (parseJournal) {
							print("\t* Parsing...")

							val journal = debugOff {
								new Journal(Bytes.fromFile(new File("journal")))
							}

							println(" done.")
							debug("Journal info:")
							debug(String.format("\tsuperblock header signature: 0x%x", 
								journal.sb.header.signature.asInstanceOf[AnyRef]))
							debug("\tblock size: " + journal.blockSize)
							debug("\tblock count: " + journal.blockCount)

						}

						if (parseJournal)
							return 0;
					} else println("Journal not enabled in superblock.")
				} else println("Skipping journal")

				//val rootPos = DirectoryFinder findRootdir fs 

				//debug(rootPos)

				println("loading root inode")	
				val rootInode = fs.inode(2)
				println(rootInode)
				println(rootInode.blockNums)

				println("loading home inode")
				val homeInode = fs.inode(3841)
				println(homeInode)
				println(homeInode.blockNums)

				println("loading broken inode")
				val badInode = fs.inode(3849)
				println(badInode)
				//println(badInode.blockNums)


				if(extractDirTree) {
					print("Looking through FS for dirs... ")
					val inodeLinks = extractDirInodeTree(fs)
					println(" done.")
					printRawTree(2, "", inodeLinks)
				}	

				if(loadTree) {
					print("Loading fs tree...")
					val rootDir = Directory(rootInode, "/")
					println(" done.")

					println("File system contents: ")
					println("")

					printTree(rootDir, "")

					if(dumpFiles) 
						dumpTree(rootDir, new File("dump"))
				}
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

	def extractDirInodeTree(fs:FileSystem) = {
		val dirs = collection.mutable.Map[Long, List[Long]]()

		debugOff{ DirectoryFinder find(fs, (startBlock, self, parent) => {
			if(self.inodeNum != 2) {
				dirs.update(parent.inodeNum, 
					self.inodeNum :: dirs.getOrElse(parent.inodeNum, List.empty[Long])) 
			}

			false
		})}

		dirs

	}

	def scanDirRecs(fs:FileSystem) = {

		DirectoryFinder find(fs, (startBlock, self, parent) => {
			var blockNum = startBlock num
			var valid = true

			while( valid ) {
				val block = fs.block(blockNum)
				var i = 0
			
				while( valid && i < block.length - DirRec.minLength ) {
					val rec = new DirRec( block.getFrom(i) )

					if(rec.next == 0) {
						valid = false
					} else
						i = i + rec.next
				}

				blockNum += 1
			}

			false
		})

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

	def printRawTree(idx:Long, prefix: String, inodes: collection.Map[Long, List[Long]]) {
		println(prefix + idx)
		for( child <- inodes.getOrElse(idx, List.empty[Long]).distinct.sorted ) {
			printRawTree(child, prefix+"  ", inodes)
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
