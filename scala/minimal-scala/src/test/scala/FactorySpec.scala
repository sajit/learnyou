import com.example.FactoryBoy
import org.scalatest._
class FactorySpec extends FlatSpec with Matchers {
  "FactoryBoy" should "calc squares " in {
    val f:(Int => Int) = {x => x*x}
    val sqFactory = FactoryBoy(f)
    sqFactory.calc(3) should be (9)
    
  }
  
}