package file.io

import fpscala.BaseSpec

class PipelineSpec extends BaseSpec{

  "pipeline " should "parse filter and convert " in {
    val ss = Stream("hel","#","45","144","-19","world")
    val result = PipeLiningExample.convert(ss).toList
    val valids = List[Double](45,-19)
    val expected = valids.map(el => PipeLiningExample.toCelsius(el))
    result should be (expected)
  }

}
