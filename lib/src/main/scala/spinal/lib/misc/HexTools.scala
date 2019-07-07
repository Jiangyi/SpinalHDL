package spinal.lib.misc

import spinal.core.{Data, Mem}

object HexTools{
  def readHexFile(path : String, hexOffset : Long, callback : (Long, Long) => Unit) : Unit ={
    import scala.io.Source
    def hToL(that : String, start : Int, size : Int) = java.lang.Long.parseLong(that.substring(start,start + size), 16)

    var offset: Long = 0
    for (line <- Source.fromFile(path).getLines) {
      if (line.charAt(0) == ':'){
        val byteCount = hToL(line, 1, 2)
        val nextAddr = hToL(line, 3, 4) + offset
        val key = hToL(line, 7, 2)
        key match {
          case 0 =>
            for(i <- 0 until byteCount.toInt){
              callback(nextAddr + i - hexOffset, hToL(line, 9 + i * 2, 2))
            }
          case 2 =>
            offset = hToL(line, 9, 4) << 4
          case 4 =>
            offset = hToL(line, 9, 4) << 16
          case 3 =>
          case 5 =>
          case 1 =>
        }
      }
    }
  }

  def readHexFile(path : String, hexOffset : Long): Array[BigInt] ={
    var onChipRomSize: Long = 0
    readHexFile(path, hexOffset ,(address, _) => {
      onChipRomSize = Math.max(address, onChipRomSize) + 1
    })

    val initContent = Array.fill[BigInt](((onChipRomSize+3)/4).toInt)(0)
    readHexFile(path, hexOffset,(address,data) => {
      val addressWithoutOffset: Long = address
      if(addressWithoutOffset < onChipRomSize)
        initContent((addressWithoutOffset >> 2).toInt) |= BigInt(data) << ((addressWithoutOffset & 3)*8).toInt
    })
    initContent
  }

  def initRam[T <: Data](ram : Mem[T], onChipRamHexFile : String, hexOffset : BigInt): Unit ={
    val initContent = Array.fill[BigInt](ram.wordCount)(0)
    HexTools.readHexFile(onChipRamHexFile, 0,(address,data) => {
      val addressWithoutOffset: Long = (address - hexOffset).toLong
      initContent((addressWithoutOffset >> 2).toInt) |= BigInt(data) << ((addressWithoutOffset & 3)*8).toInt
    })
    ram.initBigInt(initContent)
  }
}
