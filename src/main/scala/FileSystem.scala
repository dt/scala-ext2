class FileSystem(val bytes: Bytes) {
	var blockSize = 1024
	def intsPerBlock = blockSize / 4

	def blockAt(at : Int) = new Block( bytes.getRange(at, blockSize) )
}