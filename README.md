# Scala Ext2 FS Reader

Ext2 reader and recovery in Scala

# Dependencies
	* sbt

# Running 
	> sbt reload update compile
	> sbt
	> sbt> run ext2fs.dd


# Possible methods of figuring out a trashed FS image

1) Trial-and-error with reasonable guesses
	1.1) Using a freshly created image as a source of guesses
	1.2) Generate lots of values, test and score
2) Reverse engineering
	2.1) Look for known patterns and work backwards


## 1.1 Using a known-good image for metadata

FS creation tools are one good way of guessing the irrecoverable metadata, like locations of superblocks or group descriptors, since they write these in the first place.

Currently the FileSystem class can try to load fs metadata (superblock, gdt) from the main image, or from a separate image 
	Thus, if superblock & gdt's are trashed, for ext2, you could:
	>	cp <bad image file> <new image file>
	>	mkfs.ext2 <new image>

	Now point cleanBytes at <new image file> in Reader.scala.
Otherwise, pass a None for the cleanBytes option.

## 1.2 Test-and-score

There for each combination of possible values for various metadata values, try reading and see if the results make sense.

	Not yet implemented.

## 2) Find some known points and work backwards


	1) Find root dir listing 
		find a dir rec by scanning for their pattern in raw bytes
		Scan for rootdir's contents (it'll be in a datablock):

		You can recognize the root dir's contents by the presence of two sequenctial records for '.' and '..' both pointing to inode 2. You now know that the first record is the begining of the first data block for inode 2.

		Could be used on other dirs too: inode in entry for . points to that rec.


	2) Find inode 2
		By searching for an inode-like thing which points to the dir rec location obtained above, one finds the inode
		The dir rec points to inode num -- you can then find that inode by scanning for an inode pattern pointing to the offset where you found the dir rec.

	3) inode 2 is reliably going to start 128 bytes into the first block of inodes. You know know the first block of inodes.

	4) For each blocksize, what is the block number of this block? make a list

	5) Looking where you expect a GDT, look for these block numbers in the right place. When you get a match, you've probably figured out the blocksize.

	6) Assume blocks per group and inodes per group < blockSize * 8. 

	7) ??

