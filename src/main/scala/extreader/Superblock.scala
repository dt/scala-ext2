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
*	Utility object for methods which search for superblocks in some bytes
*/
object SuperblockFinder {
	def search(bytes: Bytes) : Option[Superblock] = {
		println("Searching for superblocks...")
		var validSBs = allPossible(bytes)

		println("Possible SBs: "+validSBs)
		for( (i, score) <- validSBs.sortBy( t => t._2 ) ) {
			println("Possible Superblock ("+score+") at "+i)
			val sb = Superblock.at(bytes, i)
			println("\tLocation: "+sb.bytes.trueOffset)
			println("\tMagic num: "+hex(sb.magicNum))
			println("\tLooks valid: "+sb.isValid)
			println("\tLog Block Size: "+sb.logBlockSize)
			println("\tBlock Size: "+sb.blockSize)
			println("\tFirst Block: "+sb.firstBlock)
			//todo: interactive q/a to accept as sueprblock
		}
		None
	}

	def allPossible(bytes: Bytes) : List[(Long, Int)] = { 
		println("Sorry, not implemented")
		List[(Long,Int)]()
	}
}

/**
*	http://www.nongnu.org/ext2-doc/ext2.html Section 3.1
Table 3-3. Superblock Structure
	Offset 	Size 	Description
		0 		4			s_inodes_count
		4			4			s_blocks_count
		8			4			s_r_blocks_count
		12		4			s_free_blocks_count
		16		4			s_free_inodes_count
		20		4			s_first_data_block
		24		4			s_log_block_size
		28		4			s_log_frag_size
		32		4			s_blocks_per_group
		36		4			s_frags_per_group
		40		4			s_inodes_per_group
		44		4			s_mtime
		48		4			s_wtime
		52		2			s_mnt_count
		54		2			s_max_mnt_count
		56		2			s_magic
		58		2			s_state
		60		2			s_errors
		62		2			s_minor_rev_level
		64		4			s_lastcheck
		68		4			s_checkinterval
		72		4			s_creator_os
		76		4			s_rev_level
		80		2			s_def_resuid
		82		2			s_def_resgid
		-- EXT2_DYNAMIC_REV Specific --
		84		4			s_first_ino
		88		2			s_inode_size
		90		2			s_block_group_nr
		92		4			s_feature_compat
		96		4			s_feature_incompat
		100		4			s_feature_ro_compat
		104		16		s_uuid
		120		16		s_volume_name
		136		64		s_last_mounted
		200		4			s_algo_bitmap
		-- Performance Hints --
		204		1			s_prealloc_blocks
		205		1			s_prealloc_dir_blocks
		206		2			(alignment)
		-- Journaling Support --
		208		16		s_journal_uuid
		224		4			s_journal_inum
		228		4			s_journal_dev
		232		4			s_last_orphan
		-- Directo	ry Indexing Support --
		236		4 		x 4	s_hash_seed
		252		1			s_def_hash_version
		253		3			padding - reserved for future expansion
		-- Other options --
		256		4			s_default_mount_options
		260		4			s_first_meta_bg
		264		760		Unused - reserved for future revisions
*/
object Superblock {
	val size = 1024
	val defaultOffset = 1024

	// treat the given bytes as a superblock
	def apply(bytes: Bytes) = new Superblock(bytes)

	// get the superblock at the default position in bytes
	def in(bytes: Bytes) = {
		at(bytes, defaultOffset)
	}

	// get the superblock at pos in bytes
	def at(bytes : Bytes, pos : Long ) = {
		new Superblock( bytes.getRange(pos, size ) )
	}
	
}

class Superblock(val bytes : Bytes) {
	// magic numbers (see above)
	def inodeCount = { bytes.get4(0) }
	def blockCount = { bytes.get4(4) }
	def firstBlock = { bytes.get4(20) }
	def blockSize = { 1024 << logBlockSize } // = math.pow(1024, logBlockSize)
	def logBlockSize = { bytes.get4(24) }
	def logFragSize = { bytes.get4(28) }
	def fragSize = { 1024 << logFragSize }
	def blocksPerGroup = { bytes.get4(32) }
	def fragsPerGroup = { bytes.get4(36) }
	def inodesPerGroup = { bytes.get4(40) }
	def revision = { bytes.get4(76) }

	def inodeSize = { 
		if(revision == Constants.EXT2_GOOD_OLD_REV)
			Constants.EXT2_GOOD_OLD_INODE_SIZE
		else
			bytes.get2(88)
	}
	def mtime = { bytes.get4(44) }
	def wtime = { bytes.get4(48) }
	def mnt_count = { bytes.get2(52) }
	def magicNum = { bytes.get2(56) }

	def feature_compat = { bytes.get4(92) }

	//Journaling
	def journalEnabled = { 
		(feature_compat & Constants.EXT3_FEATURE_COMPAT_HAS_JOURNAL) != 0
	}
	def journalInode = { bytes.get4(224) }

	def isValid = { 
		magicNum == 0xEF53 && (
		( firstBlock == 1 && logBlockSize == 0) || // sb: 1k-2k : block 1: 1k-2k
		( firstBlock == 0 && logBlockSize > 0)) &&	//sb: 1k-2k : block 0 : 0k-2+k
		inodeCount > 0 && blockCount > 0 &&
		(inodeCount <= inodesPerGroup * (blockCount /^ blocksPerGroup)) &&
		(inodeSize.isPowerOfTwo && inodeSize <= blockSize) 

	}
		
	def looksPossible = { magicNum == 0xEF53 && blocksPerGroup <= (blockSize * 8) }

	// try to compute a score based on how sane this superblock's values look
	def score = {
		var s = 0
		if(magicNum == 0xEF53) s += 2
		if(firstBlock >= 0) s += 1
		if(logBlockSize >= 0 && logBlockSize <= 16) s+= 2
		if(blockCount > 0 ) s += 1
		if(inodeCount > 0 ) s += 1
		s
	}

}