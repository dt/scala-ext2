import java.io._

object Ext2Reader { 
	def main(args: Array[String]) { 
	  val file = new File(args(0))
	  println("File: "+file.getAbsolutePath)
	  val in = new FileInputStream(file)
	  
	  val bytes = new Array[Byte](file.length.toInt)
	  
    var position = 0
    var bytesRead = 0

    print("Reading file...")

    while (position < file.length && bytesRead >= 0) {
      bytesRead = in.read(bytes, position, bytes.length-position )
      position += bytesRead
    }

    println(" done.")

    print("Converting file data...")
    val contents = new Bytes(bytes.map{_.toChar})
    println(" done.")
    
	}
}