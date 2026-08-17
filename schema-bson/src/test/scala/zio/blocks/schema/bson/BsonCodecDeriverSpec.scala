/*
 * Copyright 2024-2026 John A. De Goes and the ZIO Contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package zio.blocks.schema.bson

import org.bson.{BsonDocument, BsonInt32, BsonString}
import zio.blocks.schema._
import zio.blocks.schema.json.{Json, JsonSchema}
import zio.blocks.typeid.TypeId
import zio.test._

object BsonCodecDeriverSpec extends SchemaBaseSpec {

  final case class Person(name: String, age: Int, score: Int)
  object Person extends CompanionOptics[Person] {
    implicit val schema: Schema[Person] = Schema.derived[Person]

    val age: Lens[Person, Int] = $(_.age)
  }

  sealed trait Event
  object Event {
    final case class Created(id: Int) extends Event
    final case class Deleted(id: Int) extends Event

    implicit val schema: Schema[Event] = Schema.derived[Event]
  }

  sealed trait Chain
  object Chain {
    case object End                         extends Chain
    final case class Link(value: Int, next: Chain) extends Chain

    implicit val schema: Schema[Chain] = Schema.derived[Chain]
  }

  final case class Envelope(payload: Json)
  object Envelope {
    implicit val schema: Schema[Envelope] = Schema.derived[Envelope]
  }

  final case class Nested(label: String)
  object Nested {
    implicit val schema: Schema[Nested] = Schema.derived[Nested]
  }

  final case class Composite(
    nested: Nested,
    events: List[Event],
    attributes: Map[String, Int],
    dynamic: DynamicValue
  )
  object Composite {
    implicit val schema: Schema[Composite] = Schema.derived[Composite]
  }

  private val intAsString: BsonCodec[Int] =
    BsonCodec.string.transform[Int](_.toInt)(_.toString)

  private def roundTrip[A](value: A, codec: BsonCodec[A]): A = {
    val encoded = codec.encoder.toBsonValue(value)
    codec.decoder.fromBsonValueUnsafe(encoded, Nil, BsonDecoder.BsonDecoderContext.default)
  }

  def spec = suite("BsonCodecDeriverSpec")(
    test("derives directly from Schema") {
      val codec   = Schema[Person].derive(BsonCodecDeriver)
      val value   = Person("Ada", 42, 100)
      val encoded = codec.encoder.toBsonValue(value).asDocument()

      assertTrue(
        encoded == BsonDocument.parse("""{"name":"Ada","age":42,"score":100}"""),
        roundTrip(value, codec) == value
      )
    },
    test("compatibility facade and direct deriver produce the same BSON") {
      val value  = Person("Ada", 42, 100)
      val direct = Schema[Person].derive(BsonCodecDeriver)
      val facade = BsonSchemaCodec.bsonCodec(Schema[Person])

      assertTrue(
        direct.encoder.toBsonValue(value) == facade.encoder.toBsonValue(value),
        roundTrip(value, facade) == value
      )
    },
    test("configured deriver controls sum type encoding") {
      val deriver = BsonCodecDeriver.withSumTypeHandling(
        BsonSchemaCodec.SumTypeHandling.DiscriminatorField("kind")
      )
      val codec   = Schema[Event].derive(deriver)
      val value   = Event.Created(1): Event
      val encoded = codec.encoder.toBsonValue(value).asDocument()

      assertTrue(
        encoded.getString("kind").getValue == "Created",
        encoded.getInt32("id").getValue == 1,
        roundTrip(value, codec) == value
      )
    },
    test("DerivationBuilder type override replaces all matching codecs") {
      val codec = Schema[Person]
        .deriving(BsonCodecDeriver)
        .instance(TypeId.int, intAsString)
        .derive
      val encoded = codec.encoder.toBsonValue(Person("Ada", 42, 100)).asDocument()

      assertTrue(
        encoded.get("age") == new BsonString("42"),
        encoded.get("score") == new BsonString("100"),
        roundTrip(Person("Ada", 42, 100), codec) == Person("Ada", 42, 100)
      )
    },
    test("DerivationBuilder optic override replaces only the selected field codec") {
      val codec = Schema[Person]
        .deriving(BsonCodecDeriver)
        .instance(Person.age, intAsString)
        .derive
      val encoded = codec.encoder.toBsonValue(Person("Ada", 42, 100)).asDocument()

      assertTrue(
        encoded.get("age") == new BsonString("42"),
        encoded.get("score") == new BsonInt32(100),
        roundTrip(Person("Ada", 42, 100), codec) == Person("Ada", 42, 100)
      )
    },
    test("DerivationBuilder term override replaces only the selected field codec") {
      val codec = Schema[Person]
        .deriving(BsonCodecDeriver)
        .instance(Person.schema.reflect.typeId, "age", intAsString)
        .derive
      val encoded = codec.encoder.toBsonValue(Person("Ada", 42, 100)).asDocument()

      assertTrue(
        encoded.get("age") == new BsonString("42"),
        encoded.get("score") == new BsonInt32(100),
        roundTrip(Person("Ada", 42, 100), codec) == Person("Ada", 42, 100)
      )
    },
    test("DerivationBuilder term modifier changes the BSON field name") {
      val codec = Schema[Person]
        .deriving(BsonCodecDeriver)
        .modifier(Person.schema.reflect.typeId, "age", Modifier.rename("years"))
        .derive
      val encoded = codec.encoder.toBsonValue(Person("Ada", 42, 100)).asDocument()

      assertTrue(
        encoded.get("age") == null,
        encoded.getInt32("years").getValue == 42,
        roundTrip(Person("Ada", 42, 100), codec) == Person("Ada", 42, 100)
      )
    },
    test("deriver-level instance override is honored") {
      val codec   = Schema[Person].derive(BsonCodecDeriver.withInstance[Int](intAsString))
      val encoded = codec.encoder.toBsonValue(Person("Ada", 42, 100)).asDocument()

      assertTrue(
        encoded.get("age") == new BsonString("42"),
        encoded.get("score") == new BsonString("100")
      )
    },
    test("configured compatibility facade delegates to the configured deriver") {
      val config = BsonSchemaCodec.Config.withSumTypeHandling(
        BsonSchemaCodec.SumTypeHandling.DiscriminatorField("kind")
      )
      val direct = Schema[Event].derive(BsonCodecDeriver.fromConfig(config))
      val facade = BsonSchemaCodec.bsonCodec(Schema[Event], config)
      val value  = Event.Deleted(2): Event

      assertTrue(
        direct.encoder.toBsonValue(value) == facade.encoder.toBsonValue(value),
        roundTrip(value, facade) == value
      )
    },
    test("builder overrides are honored for every nested structural kind") {
      val nestedCodec  = Schema[Nested].derive(BsonCodecDeriver)
      val eventCodec   = Schema[Event].derive(BsonCodecDeriver)
      val eventsCodec  = Schema[List[Event]].derive(BsonCodecDeriver)
      val attrsCodec   = Schema[Map[String, Int]].derive(BsonCodecDeriver)
      val dynamicCodec = Schema[DynamicValue].derive(BsonCodecDeriver)
      val codec = Schema[Composite]
        .deriving(BsonCodecDeriver)
        .instance(Schema[Nested].reflect.typeId, nestedCodec)
        .instance(Schema[Event].reflect.typeId, eventCodec)
        .instance(Schema[List[Event]].reflect.typeId, eventsCodec)
        .instance(Schema[Map[String, Int]].reflect.typeId, attrsCodec)
        .instance(TypeId.of[DynamicValue], dynamicCodec)
        .derive
      val value = Composite(
        Nested("value"),
        List(Event.Created(1), Event.Deleted(2)),
        Map("answer" -> 42),
        DynamicValue.Record("ok" -> DynamicValue.boolean(true))
      )

      assertTrue(roundTrip(value, codec) == value)
    },
    test("direct derivation supports recursive schemas") {
      val codec = Schema[Chain].derive(BsonCodecDeriver)
      val value = Chain.Link(1, Chain.Link(2, Chain.End)): Chain

      assertTrue(roundTrip(value, codec) == value)
    },
    test("direct derivation encodes Json fields as semantic BSON") {
      val codec = Schema[Envelope].derive(BsonCodecDeriver)
      val value = Envelope(Json.parseUnsafe("""{"key":"value","items":[1,true,null]}"""))
      val payload = codec.encoder.toBsonValue(value).asDocument().getDocument("payload")

      assertTrue(
        payload.getString("key").getValue == "value",
        payload.getArray("items").size == 3,
        roundTrip(value, codec) == value
      )
    },
    test("direct derivation preserves JsonSchema wrapper validation") {
      val codec = Schema.fromJsonSchema(JsonSchema.string()).derive(BsonCodecDeriver)

      assertTrue(
        codec.decoder.fromBsonValue(new BsonString("valid")) == Right(Json.String("valid")),
        codec.decoder.fromBsonValue(new BsonInt32(1)).isLeft
      )
    }
  )
}
