package extreader

class Block(val num: Long, var bytes : Bytes ) extends BytesWrapper(bytes) {
	override def toString = "block at "+hex(bytes.trueOffset)
}