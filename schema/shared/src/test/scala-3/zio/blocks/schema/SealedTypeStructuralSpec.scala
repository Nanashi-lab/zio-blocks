package zio.blocks.schema

import zio.test._
import zio.blocks.schema.binding.StructuralValue

object SealedTypeStructuralSpec extends ZIOSpecDefault {

  enum Status {
    case Active, Inactive, Suspended
  }

  sealed trait Result
  case class Success(value: Int) extends Result
  case class Failure(error: String) extends Result

  def spec = suite("SealedTypeStructuralSpec")(
    test("enum to structural") {
      val active = Status.Active
      val toStructural = ToStructural.derived[Status]
      val structural = toStructural.toStructural(active)
      
      assertTrue(
        structural.asInstanceOf[StructuralValue].values("Tag") == "Active"
      )
    },
    test("enum to structural (Inactive)") {
      val inactive = Status.Inactive
      val toStructural = ToStructural.derived[Status]
      val structural = toStructural.toStructural(inactive)
      
      assertTrue(
        structural.asInstanceOf[StructuralValue].values("Tag") == "Inactive"
      )
    },
    test("sealed trait to structural (Success)") {
       val success = Success(42)
       val toStructural = ToStructural.derived[Result]
       val structural = toStructural.toStructural(success)
       
       val values = structural.asInstanceOf[StructuralValue].values
       assertTrue(
         values("Tag") == "Success",
         values("value") == 42
       )
    },
    test("sealed trait to structural (Failure)") {
       val failure = Failure("oops")
       val toStructural = ToStructural.derived[Result]
       val structural = toStructural.toStructural(failure)
       
       val values = structural.asInstanceOf[StructuralValue].values
       assertTrue(
         values("Tag") == "Failure",
         values("error") == "oops"
       )
    }
  )
}
