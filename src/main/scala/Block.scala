package extreader

class Block( var bytes : Bytes ) extends BytesWrapper(bytes) {
	override def toString = "block at "+hex(bytes.trueOffset)
}