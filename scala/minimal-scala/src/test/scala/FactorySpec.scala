import com.example.FactoryBoy
import org.scalatest._
class FactorySpec extends FlatSpec with Matchers {
  "FactoryBoy" should "calc squares " in {
    val squares:(Int => Int) = {x => x*x}
    val sqFactory = FactoryBoy(squares)
    sqFactory.calc(3) should be (9)
    
  }
  "FactoryBoy" should "calc cubes " in {
    val cubes:(Int => Int) = {x => x*x*x}
    val cbFactory = FactoryBoy(cubes)
    cbFactory.calc(3) should be (27)
    
  }
  
}