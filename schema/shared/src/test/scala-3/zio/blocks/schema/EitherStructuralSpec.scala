package zio.blocks.schema

import zio.test._
import zio.blocks.schema.binding.StructuralValue

object EitherStructuralSpec extends ZIOSpecDefault {

  case class EitherWrapper(choice: Either[Int, String])
  case class NestedWrapper(choice: Either[Person, Point])
  case class Person(name: String)
  case class Point(x: Int, y: Int)

  def spec: Spec[TestEnvironment, Any] = suite("EitherStructuralSpec (Scala 3)")(
    test("transforms Either[Int, String] to structural types correctly") {
      val ts         = ToStructural.derived[EitherWrapper]
      val input      = EitherWrapper(Left(42))
      val structural = ts.toStructural(input)

      // In Scala 3, we can use Selectable to access fields if the type is precise enough,
      // but here 'structural' is just 'ts.StructuralType'.
      // We know it is a StructuralValue at runtime.
      val sv     = structural.asInstanceOf[StructuralValue]
      val choice = sv.selectDynamic("choice")

      choice match {
        case Left(l) =>
          val lsv = l.asInstanceOf[StructuralValue]
          assertTrue(lsv.selectDynamic("value") == 42)
          // We can check the structural refinement at compile time if we cast
          val typedL = l.asInstanceOf[StructuralValue { type Tag = "Left"; def value: Int }]
          // This assertion is mostly to prove it compiles with the refinement
          assertTrue(typedL.selectDynamic("value") == 42)
        case Right(_) =>
          assertTrue(false)
      }
    },
    test("transforms Right side correctly") {
      val ts         = ToStructural.derived[EitherWrapper]
      val input      = EitherWrapper(Right("hello"))
      val structural = ts.toStructural(input)
      val sv         = structural.asInstanceOf[StructuralValue]
      val choice     = sv.selectDynamic("choice")

      choice match {
        case Right(r) =>
          val rsv = r.asInstanceOf[StructuralValue]
          assertTrue(rsv.selectDynamic("value") == "hello")
        case Left(_) =>
          assertTrue(false)
      }
    },
    test("nested structural types in Either") {
      val ts         = ToStructural.derived[NestedWrapper]
      val input      = NestedWrapper(Left(Person("Alice")))
      val structural = ts.toStructural(input)
      val sv         = structural.asInstanceOf[StructuralValue]

      sv.selectDynamic("choice") match {
        case Left(l) =>
          val lsv   = l.asInstanceOf[StructuralValue]
          val inner = lsv.selectDynamic("value").asInstanceOf[StructuralValue]
          assertTrue(inner.selectDynamic("name") == "Alice")
        case _ => assertTrue(false)
      }
    }
  )
}
