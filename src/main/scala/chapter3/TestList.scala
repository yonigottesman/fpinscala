package chapter3

object TestList {
  def main(args: Array[String]): Unit = {
    val l = List(1,2,3,4,5,6)
//    print(List.tail(l))
//    print(List.setHead(l,0))
//    print(List.drop(l,5))
//    print(List.dropWhile(l)(x=> x > 3 ))
//    print(List.init(l))
//    print(List.length(l))
//    println(List.product3(l))
//    print(List.reverse(l))
//    val l2 = List(10,11,12,13,14)
//    print(List.foldAppend(l,l2))
//    val l2 = List(List(1,2,3),List(4,5,6),List(7,8,9))
//    print(List.concat(l2))
//    val res = List.filter(l)(_% 2== 0)
//    val res = List.flatMap(l)(x=>List(x,x,x))
//    val res = List.filterWithFlatMap(l)(_%2==0)

//    val res = List.zipWith(l,l)
    val res = List.hasSubsequence(List(1,2,3,4),List(1,3))
    println(res)


  }

}
