import com.example.Arsenal
import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by sajit on 6/14/15.
 */
class MySpec  extends FlatSpec with Matchers {

  "A test " should "have tests " in {
    true should be === true
  }
  "Also there " should " have stuff " in {
    val el = List(2,4,5,1)
    Arsenal.addX(4,el) should be === (List(6,8,9,5))
  }

}
