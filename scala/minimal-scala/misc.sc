object misc {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  val a = Map("hello" -> 1, "world" -> 1)         //> a  : scala.collection.immutable.Map[String,Int] = Map(hello -> 1, world -> 1)
                                                  //| 
  val b = Map("hello" -> 2, "sad" -> 1)           //> b  : scala.collection.immutable.Map[String,Int] = Map(hello -> 2, sad -> 1)
                                                  //| 
  //b.contains("hello")
  //b.get("hello").getOrElse(0)
  val merged:Map[String,Int] =  a.map{case(x,y) => {val count = b.get(x).getOrElse(0) + y; (x,count)}}
                                                  //> merged  : Map[String,Int] = Map(hello -> 3, world -> 1)
   val c = b ++ merged                            //> c  : scala.collection.immutable.Map[String,Int] = Map(hello -> 3, sad -> 1, 
                                                  //| world -> 1)
                                                  
}