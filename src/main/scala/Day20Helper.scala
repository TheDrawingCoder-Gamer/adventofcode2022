import scala.collection.mutable.ArrayBuffer

object Day20Helper {
  def mix(pos: Int, n: Long, dataSize: Int): Long = {
    if (n == 0) return pos
    
    val idx = pos + n
    if (n > 0) {
      if (n >= dataSize) {
        val m = n % (dataSize - 1)
        (m + pos) % (dataSize - 1)
      } else if (idx == 0) {
        1
      } else idx % (dataSize - 1)
    } else {
      math.floorMod(idx, dataSize - 1) 

    }
  }
  def mixNumbers(arr: ArrayBuffer[Int]): Unit = {
    val numbers = arr.toVector 
    val datasize = numbers.size 
    for {
      n <- numbers 
    } {
      val pos = arr.indexOf(n)
      val newpos = mix(pos, n, datasize)
      arr.remove(pos)
      arr.insert(newpos.toInt, n)
    }
  }
  def mixNumbersReal(arr: ArrayBuffer[(Long, Int)]): Unit = {
    val numbers = arr.toVector.sortBy(_._2)
    val dataSize = numbers.size 
    for {
      r @ (n, m) <- numbers 
    } {
      val pos = arr.indexOf(r)
      val newPos = mix(pos, n, dataSize)
      arr.remove(pos)
      arr.insert(newPos.toInt, r)
    }
  }
  def run(input: String): Long = {
    var marker = 0
    val data = input.linesIterator.map { it => 
      val cur = marker 
      marker += 1
      (it.toLong, cur) 
    }.to(ArrayBuffer)
    val dataSize = data.size 
    mixNumbersReal(data)
    val zeroIndex = data.indexWhere((n, _) => n == 0)
    if (dataSize < 20)
      println(data) 
    val xIdx = (zeroIndex + 1000) % dataSize
    val yIdx = (zeroIndex + 2000) % dataSize
    val zIdx = (zeroIndex + 3000) % dataSize
    println(xIdx)
    println(yIdx)
    println(zIdx)
    data(xIdx)._1 + data(yIdx)._1 + data(zIdx)._1
  }
  def run2(input: String): Long = {
    var marker = 0
    val data = input.linesIterator.map { it => 
      val cur = marker 
      marker += 1
      (it.toLong * 811589153L, cur) 
    }.to(ArrayBuffer)
    val dataSize = data.size 
    for {
      _ <- 0 until 10 
    } mixNumbersReal(data)
    val zeroIndex = data.indexWhere((n, _) => n == 0)
    if (dataSize < 20)
      println(data) 
    val xIdx = (zeroIndex + 1000) % dataSize
    val yIdx = (zeroIndex + 2000) % dataSize
    val zIdx = (zeroIndex + 3000) % dataSize
    println(xIdx)
    println(yIdx)
    println(zIdx)
    data(xIdx)._1 + data(yIdx)._1 + data(zIdx)._1
  }
}
