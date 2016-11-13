package fpscala.chapter14

/**
  * Created by sajit on 11/13/16.
  */
object SMUtils {

  val st:ST[String,Int] = ST(5)
  val (s,x) = st.map((el => el + 1))


}
