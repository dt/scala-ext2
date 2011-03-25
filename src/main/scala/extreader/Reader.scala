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

import java.io.File

/*
	Some inspiration taken from jNode's EXT2 implementation
	http://gitorious.org/jnode/svn-mirror/trees/master/fs/src/fs/org/jnode/fs/ext2
*/

object Reader { 
	type InodeParents = collection.Map[Long, List[Long]]
	
	def main(args: Array[String]) { 
		if(args.length < 1) {
			println("image name must be first argument")
			System.exit(-1)
		}

		val image = new File(args(0))

		// regex for matching assignments for options
		val Assign = "(.*)=(.*)".r

		// default options
		var cleanImg = Option.empty[String] 
		var forceBlocksize = Option.empty[Int]
		var overrideSB = Option.empty[Long]
		var skipJournal = false
		var dumpJournal = false
		var parseJournal = false
		var findDeleted = false
		var dumpFiles = false
		var extractDirTree = false
		var loadTree = true
		var guessBlockSize = false

		// process arguments one by one
		for(i <- 1 until args.length) {
			args(i) match {
				case Assign(a,v) => {
					a.toLowerCase match {
						case "metaimage" => { cleanImg = Some(v) } 
						case "overrideSB" => { overrideSB = Some(v.toLong) }
						case "debug" => { extreader.showDebug = v.toBoolean }
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
						case "loadtree"	=> { loadTree = v.toBoolean }
						case "extractdirtree" => { extractDirTree = v.toBoolean }
						case "guessblocksize" => { guessBlockSize = v.toBoolean }

						case _ => { println("Unknown option: '"+a+"'") }
					}
				}
				case _ => println("Malformed option: "+args(i))
			}
		}

		println("Image file: "+image.getAbsolutePath)
		
		debug("Debug output enabled...")

		// load the image file into our byte wrapper
		val bytes = Bytes fromFile image

		// load alternate bytes if specified
		val cleanBytes = cleanImg.map{ cleanFile => {
			println("Loading alternate metadata image: "+cleanFile) 
			Bytes fromFile (new File(cleanFile)) 
		}}
		println()


		if(guessBlockSize) {
			println("Attempting to guess block size...")
			BlockSizeGuesser search bytes
			return 0;
		}

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

		println("")

		if(superblock.isEmpty) {
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
			return 0;
		}

		val sb = superblock.get // it's not empty (we checked above) so de-ref now

		println("Loading filesystem...")
		val fs = new FileSystem(bytes, sb, cleanBytes)

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
				val dumpFile = new File("journal")

				if (dumpJournal || (parseJournal && !dumpFile.exists)) {
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
					debug("\tfirst journal block: " + journal.firstJournalBlock)
					debug("\tfirst sequence number: " + journal.firstSeqNum)
					debug("\tfirst transaction block: " + journal.firstTransBlock)

					val journalBlock = new JournalDescriptor(journal.block(journal.firstJournalBlock))
					debug("First journal block info:")
					debug("\theader signature: " + journalBlock.header.signature)
					debug("\tblock type: " + journalBlock.header.blockType)
					debug("\tsequence number: " + journalBlock.header.seqNum)
				}

				if (parseJournal)
					return 0;
			} else println("Journal not enabled in superblock.")
		} else println("Skipping journal")

		var rawDirTree = Option.empty[InodeParents]

		if(extractDirTree) {
			print("Looking through FS for dirs... ")
			val (inode2block, block2inode, inodeLinks) = extractDirInodeTree(fs)
			rawDirTree = Some(inodeLinks)
			println(" done.")
			printRawTree(2, "", inodeLinks)

			println("Searching for inodes which point to blocks...")

	//		val inodeNum = 2
	//		var dirBlock = fs.block(501)
			for( (inodeNum, dirBlock) <- inode2block ) {

				val idxInBlock = fs.inodeIndexInBlock(inodeNum)

				val expectedBlock = fs.blockNumOfInode(inodeNum)

				println("\t inode "+inodeNum+"["+expectedBlock+"/"+idxInBlock+"]: block 0: "+dirBlock.num+"... ")

				for ( block <- fs.blocks if !journalFile.exists(_.inode.blockNums.contains(block.num))) {
					val inode = block.fakeInode(idxInBlock)
					try {
					if(inode.isDir && inode.blockNum(0) == dirBlock.num)			
						println(Console.GREEN + "\tFound: "+block.num+Console.WHITE)
					} 
				}
				println(Console.WHITE)

			}

			println()
		}	

		if(loadTree) {
			print("Loading fs tree...")
			val rootDir = Directory(fs.inode(2), "/")
			println(" done.")

			println("File system contents: ")
			println("")

			printTree(rootDir, "")

			if(dumpFiles) 
				dumpTree(rootDir, new File("dump"))
		}
	}
	
	// manually attempts to build a tree of inode/directory relationships
	// 	directory contents follows a known format, so use those to map inodes
	def extractDirInodeTree(fs:FileSystem) = {
		val dirs = collection.mutable.Map[Long, List[Long]]()
		val inode2block = collection.mutable.Map[Long, Block]()
		val block2inode = collection.mutable.Map[Block, Long]()

		debugOff{ DirectoryFinder find(fs, (startBlock, self, parent) => {
			inode2block += ((self.inodeNum, startBlock))
			block2inode += ((startBlock, self.inodeNum))

			if(self.inodeNum != 2) {
				dirs.update(parent.inodeNum, 
					self.inodeNum :: dirs.getOrElse(parent.inodeNum, List.empty[Long])) 
			}

			false
		})}

		( inode2block, block2inode, dirs)

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

	def printRawTree(idx:Long, prefix: String, inodes: InodeParents) {
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
