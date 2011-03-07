class Block( var bytes : Bytes) {
	override def toString = "block at "+Hex.valueOf(bytes.trueOffset)
}