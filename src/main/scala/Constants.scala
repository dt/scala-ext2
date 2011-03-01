/*
copied from jnode's ext2 impl
*/

object Constants {
	// file type values used in directory records
	val EXT2_FT_UNKNOWN = 0;
	val EXT2_FT_REG_FILE = 1;
	val EXT2_FT_DIR = 2;
	val EXT2_FT_CHRDEV = 3;
	val EXT2_FT_BLKDEV = 4;
	val EXT2_FT_FIFO = 5;
	val EXT2_FT_SOCK = 6;
	val EXT2_FT_SYMLINK = 7;
	val EXT2_FT_MAX = 8;

	// inodes
	val EXT2_BAD_INO = 0x01;         // bad blocks inode
	val EXT2_ROOT_INO = 0x02;        // root directory inode
	val EXT2_ACL_IDX_INO = 0x03;     // ACL index node
	val EXT2_ACL_DATA_INO = 0x04;    // ACL data inode
	val EXT2_BOOT_LOADER_INO = 0x05; // boot loader inode
	val EXT2_UNDEL_DIR_INO = 0x06;   // undelete directory inode

	val EXT2_S_IFMT = 0xF000;  // format mask
	val EXT2_S_IFSOCK = 0xC000; // socket
	val EXT2_S_IFLNK = 0xA000; // symbolic link
	val EXT2_S_IFREG = 0x8000; // regular file
	val EXT2_S_IFBLK = 0x6000; // block device
	val EXT2_S_IFDIR = 0x4000; // directory
	val EXT2_S_IFCHR = 0x2000; // character device
	val EXT2_S_IFIFO = 0x1000; // fifo
	
	// access rights
	val EXT2_S_ISUID = 0x0800; // SUID
	val EXT2_S_ISGID = 0x0400; // SGID
	val EXT2_S_ISVTX = 0x0200; // sticky bit
	val EXT2_S_IRWXU = 0x01C0; // user access right mask
	val EXT2_S_IRUSR = 0x0100; // read
	val EXT2_S_IWUSR = 0x0080; // write
	val EXT2_S_IXUSR = 0x0040; // execute
	val EXT2_S_IRWXG = 0x0038; // group access right mask
	val EXT2_S_IRGRP = 0x0020; // read
	val EXT2_S_IWGRP = 0x0010; // write
	val EXT2_S_IXGRP = 0x0008; // execute
	val EXT2_S_IRWXO = 0x0007; // others access right mask
	val EXT2_S_IROTH = 0x0004; // read
	val EXT2_S_IWOTH = 0x0002; // write
	val EXT2_S_IXOTH = 0x0001; // execute

	// revision level values (stored in the superblock)
	val EXT2_GOOD_OLD_REV = 0;
	val EXT2_DYNAMIC_REV = 1;
	val EXT2_PREALLOC_BLOCK = 7;

	// behaviour control flags in the inode
	val EXT2_INDEX_FL = 0x00010000; // hash indexed directory

	// fs state
	val EXT2_VALID_FS = 0x0001; // cleanly unmounted
	val EXT2_ERROR_FS = 0x0002;

	// error behavior
	val EXT2_ERRORS_CONTINUE = 0x0001;
	val EXT2_ERRORS_RO = 0x0002;
	val EXT2_ERRORS_PANIC = 0x0003;
	val EXT2_ERRORS_DEFAULT = EXT2_ERRORS_CONTINUE;

	// S_FEATURE_RO_COMPAT values
	val EXT2_FEATURE_RO_COMPAT_SPARSE_SUPER = 0x0001L;
	val EXT2_FEATURE_RO_COMPAT_LARGE_FILE = 0x0002L;
	val EXT2_FEATURE_RO_COMPAT_BTREE_DIR = 0x0004L;

	// S_FEATURE_INCOMPAT values
	val EXT2_FEATURE_INCOMPAT_COMPRESSION = 0x0001L;
	val EXT2_FEATURE_INCOMPAT_FILETYPE = 0x0002L;
	val EXT3_FEATURE_INCOMPAT_RECOVER = 0x0004L;
	val EXT3_FEATURE_INCOMPAT_JOURNAL_DEV = 0x0008L;
	val EXT2_FEATURE_INCOMPAT_META_BG = 0x0010L;
}
