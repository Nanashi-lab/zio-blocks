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

import org.bson.BsonDocument
import org.bson.BsonBinary
import org.bson.BsonDateTime
import org.bson.BsonInt32
import zio.blocks.schema._
import zio.blocks.schema.json.{Json, JsonSchema}
import zio.test._

import java.time.Instant
import java.util.UUID

object BsonCodecJsonDynamicSpec extends SchemaBaseSpec {

  final case class Scope(name: String, description: String, payload: Json)

  def spec = suite("BsonCodecJsonDynamicSpec")(
    suite("Json")(
      test("encodes nested Json to semantic BSON and round-trips") {
        val codec = BsonSchemaCodec.bsonCodec(Schema[Json])
        val json  = Json.parseUnsafe("""{"key":"string_value","nested":{"items":[1,true,null,{"flag":false}]}}""")

        val encoded = codec.encoder.toBsonValue(json).asDocument()
        val nested  = encoded.getDocument("nested")
        val items   = nested.getArray("items")
        val decoded = codec.decoder.fromBsonValueUnsafe(encoded, Nil, BsonDecoder.BsonDecoderContext.default)

        assertTrue(
          encoded.get("Object") == null,
          encoded.getString("key").getValue() == "string_value",
          items.size() == 4,
          items.get(0).asInt32().getValue() == 1,
          items.get(1).asBoolean().getValue(),
          items.get(2).isNull,
          items.get(3).asDocument().getBoolean("flag").getValue() == false,
          decoded == json
        )
      },
      test("does not treat Json fields that look like dynamic wrappers specially") {
        val codec = BsonSchemaCodec.bsonCodec(Schema[Json])
        val json  = Json.parseUnsafe(
          """{"$zio_dynamic_map":[1,2],"$zio_dynamic_variant":{"case":"keep","value":"plain"}}"""
        )

        val encoded = codec.encoder.toBsonValue(json).asDocument()
        val decoded = codec.decoder.fromBsonValueUnsafe(encoded, Nil, BsonDecoder.BsonDecoderContext.default)

        assertTrue(
          encoded.get("$zio_dynamic_map").isArray(),
          encoded.get("$zio_dynamic_variant").isDocument(),
          decoded == json
        )
      }
    ),
    suite("Json payload records")(
      test("original Json payload repro encodes to the expected BSON document") {
        val codec = BsonSchemaCodec.bsonCodec(Schema.derived[Scope])
        val value = Scope(
          name = "test",
          description = "test scope",
          payload = Json.parseUnsafe("""{"key": "string_value"}""")
        )

        val encoded  = codec.encoder.toBsonValue(value).asDocument()
        val expected = BsonDocument.parse(
          """{"name":"test","description":"test scope","payload":{"key":"string_value"}}"""
        )

        assertTrue(encoded == expected)
      },
      test("case class payload uses semantic BSON and both codec paths round-trip") {
        val codec = BsonSchemaCodec.bsonCodec(Schema.derived[Scope])
        val value = Scope(
          name = "test",
          description = "test scope",
          payload = Json.parseUnsafe("""{"key":"string_value","nested":[{"n":1},2]}""")
        )

        val encoded = codec.encoder.toBsonValue(value).asDocument()
        val payload = encoded.getDocument("payload")
        val decoded = codec.decoder.fromBsonValueUnsafe(encoded, Nil, BsonDecoder.BsonDecoderContext.default)

        assertTrue(
          payload.get("Object") == null,
          payload.get("value") == null,
          payload.getString("key").getValue() == "string_value",
          payload.getArray("nested").get(0).asDocument().getInt32("n").getValue() == 1,
          decoded == value,
          BsonTestHelpers.roundTripWriterReader(value, codec, isDocument = true)
        )
      }
    ),
    suite("DynamicValue")(
      test("record and sequence values encode semantically and round-trip") {
        val codec = BsonSchemaCodec.bsonCodec(Schema[DynamicValue])
        val value = DynamicValue.Record(
          "name"  -> DynamicValue.string("payload"),
          "items" -> DynamicValue.Sequence(
            DynamicValue.int(1),
            DynamicValue.Record("ok" -> DynamicValue.boolean(true)),
            DynamicValue.Null
          )
        )

        val encoded = codec.encoder.toBsonValue(value).asDocument()
        val items   = encoded.getArray("items")
        val decoded = codec.decoder.fromBsonValueUnsafe(encoded, Nil, BsonDecoder.BsonDecoderContext.default)

        assertTrue(
          encoded.getString("name").getValue() == "payload",
          items.size() == 3,
          items.get(0).asInt32().getValue() == 1,
          items.get(1).asDocument().getBoolean("ok").getValue(),
          items.get(2).isNull,
          decoded == value
        )
      },
      test("variant values round-trip through the dedicated dynamic wrapper") {
        val codec = BsonSchemaCodec.bsonCodec(Schema[DynamicValue])
        val value = DynamicValue.Variant(
          "Wrapped",
          DynamicValue.Record("n" -> DynamicValue.int(1), "ok" -> DynamicValue.boolean(true))
        )

        val encoded = codec.encoder.toBsonValue(value).asDocument()
        val wrapper = encoded.getDocument("$zio_dynamic_variant")
        val payload = wrapper.getDocument("value")
        val decoded = codec.decoder.fromBsonValueUnsafe(encoded, Nil, BsonDecoder.BsonDecoderContext.default)

        assertTrue(
          wrapper.getString("case").getValue() == "Wrapped",
          payload.getInt32("n").getValue() == 1,
          payload.getBoolean("ok").getValue(),
          decoded == value
        )
      },
      test("map values round-trip through the dedicated dynamic wrapper") {
        val codec = BsonSchemaCodec.bsonCodec(Schema[DynamicValue])
        val value = DynamicValue.Map(
          DynamicValue.int(1)        -> DynamicValue.string("one"),
          DynamicValue.string("two") -> DynamicValue.int(2)
        )

        val encoded = codec.encoder.toBsonValue(value).asDocument()
        val entries = encoded.getArray("$zio_dynamic_map")
        val first   = entries.get(0).asDocument()
        val second  = entries.get(1).asDocument()
        val decoded = codec.decoder.fromBsonValueUnsafe(encoded, Nil, BsonDecoder.BsonDecoderContext.default)

        assertTrue(
          entries.size() == 2,
          first.getInt32("key").getValue() == 1,
          first.getString("value").getValue() == "one",
          second.getString("key").getValue() == "two",
          second.getInt32("value").getValue() == 2,
          decoded == value
        )
      },
      test("dynamic primitive encoding follows existing bson codec representations") {
        val codec   = BsonSchemaCodec.bsonCodec(Schema[DynamicValue])
        val instant = Instant.parse("2026-04-22T12:34:56Z")
        val uuid    = UUID.fromString("123e4567-e89b-12d3-a456-426614174000")
        val value   = DynamicValue.Record(
          "amount" -> DynamicValue.bigDecimal(BigDecimal("12.34")),
          "when"   -> new DynamicValue.Primitive(new PrimitiveValue.Instant(instant)),
          "id"     -> new DynamicValue.Primitive(new PrimitiveValue.UUID(uuid))
        )

        val encoded = codec.encoder.toBsonValue(value).asDocument()
        val decoded = codec.decoder.fromBsonValueUnsafe(encoded, Nil, BsonDecoder.BsonDecoderContext.default)

        assertTrue(
          encoded.get("amount").getBsonType().name() == "DECIMAL128",
          encoded.get("when").getBsonType().name() == "STRING",
          encoded.get("id").getBsonType().name() == "STRING",
          decoded == DynamicValue.Record(
            "amount" -> DynamicValue.bigDecimal(BigDecimal("12.34")),
            "when"   -> DynamicValue.string("2026-04-22T12:34:56Z"),
            "id"     -> DynamicValue.string("123e4567-e89b-12d3-a456-426614174000")
          )
        )
      },
      test("native BSON date and uuid decode to typed dynamic primitives") {
        val codec   = BsonSchemaCodec.bsonCodec(Schema[DynamicValue])
        val instant = Instant.parse("2026-04-22T12:34:56Z")
        val uuid    = UUID.fromString("123e4567-e89b-12d3-a456-426614174000")
        val doc     = new BsonDocument()

        doc.put("when", new BsonDateTime(instant.toEpochMilli))
        doc.put("id", new BsonBinary(uuid))

        val decoded = codec.decoder.fromBsonValueUnsafe(doc, Nil, BsonDecoder.BsonDecoderContext.default)

        assertTrue(
          decoded == DynamicValue.Record(
            "when" -> new DynamicValue.Primitive(new PrimitiveValue.Instant(instant)),
            "id"   -> new DynamicValue.Primitive(new PrimitiveValue.UUID(uuid))
          )
        )
      },
      test("invalid dynamic map entry fails decoding") {
        val codec   = BsonSchemaCodec.bsonCodec(Schema[DynamicValue])
        val invalid = BsonDocument.parse("""{"$zio_dynamic_map":[1]}""")

        val result = codec.decoder.fromBsonValue(invalid)

        assertTrue(result.isLeft)
      }
    ),
    suite("Schema.fromJsonSchema")(
      test("JsonSchema.True uses the semantic BSON tree") {
        val codec = BsonSchemaCodec.bsonCodec(Schema.fromJsonSchema(JsonSchema.True))
        val json  = Json.parseUnsafe("""{"k":[1,{"flag":true},null]}""")

        val encoded = codec.encoder.toBsonValue(json).asDocument()
        val decoded = codec.decoder.fromBsonValueUnsafe(encoded, Nil, BsonDecoder.BsonDecoderContext.default)

        assertTrue(
          encoded.getArray("k").size() == 3,
          decoded == json
        )
      },
      test("wrapper-based Json schemas still validate on decode") {
        val codec  = BsonSchemaCodec.bsonCodec(Schema.fromJsonSchema(JsonSchema.string()))
        val result = codec.decoder.fromBsonValue(new BsonInt32(1))

        assertTrue(result.isLeft)
      }
    )
  )
}
