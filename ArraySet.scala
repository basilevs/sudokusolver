package sudokusolver;
object ArraySet {
	def createMap(that: Set[Int]): Seq[Boolean] = {
		val max = Iterable.max(that)
		val map = new Array[Boolean](max+1)
		for (i <- map.indices) map.update(i, that(i))
		map
	}
	
}
class ArraySet(map: Seq[Boolean]) extends collection.Set[Int] {
	def this(that: Set[Int]) = this(ArraySet.createMap(that))
	def contains(elem: Int) = if (elem>=map.size || elem < 0) false else map(elem)
	def elements = new Iterator[Int] {
		var i = 0;
		def hasNext = i+1 < ArraySet.this.map.size
		def next: Int = {
			while(hasNext) {
				i+=1
				if (ArraySet.this.map(i-1)) {
					return i-1
				}
			}
			throw new ArrayIndexOutOfBoundsException 
		}
		
	}
	def -(elem : Int) = {
		if (elem+1 == map.size) { // Removing last element. Size is reduced.
			val size = map.size-1
			val newMap = new Array[Boolean](size);
			map.slice(0, size).copyToArray(newMap, 0)
			new ArraySet(newMap)
		} else if (contains(elem)) {
			val size = map.size
			val newMap = new Array[Boolean](size);
			for (i <- newMap.indices) newMap.update(i, map(i) && i != elem)
			new ArraySet(newMap)
		} else {
			this
		}
	}
	def +(elem : Int) = {
		if (elem >= map.size) {
			val newMap = new Array[Boolean](map.size+1);
			for (i <- newMap.indices) newMap.update(i, map(i) || i == elem)
			new ArraySet(newMap)
		} else if (contains(elem)) {
			this
		} else {
			val newMap = new Array[Boolean](map.size);
			map.copyToArray(newMap, 0)
			newMap.update(elem, true) //throws if elem is negative
			new ArraySet(newMap)
		}
	}
	val _size = map.filter((x:Boolean)=>x).size
	def size = _size
}