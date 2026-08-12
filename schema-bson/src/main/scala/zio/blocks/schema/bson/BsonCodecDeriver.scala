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

import org.bson.{
  BsonArray,
  BsonBoolean,
  BsonDocument,
  BsonDouble,
  BsonInt32,
  BsonInt64,
  BsonNull,
  BsonReader,
  BsonString,
  BsonType,
  BsonValue,
  BsonWriter
}
import zio.blocks.docs.Doc
import zio.blocks.schema._
import zio.blocks.schema.binding.{Binding, HasBinding, Register, RegisterOffset, Registers}
import zio.blocks.schema.derive.{BindingInstance, Deriver, InstanceOverride}
import zio.blocks.schema.json.Json
import zio.blocks.typeid.TypeId

object BsonCodecDeriver extends BsonCodecDeriver(BsonSchemaCodec.Config) {
  private[bson] def fromConfig(config: BsonSchemaCodec.Config): BsonCodecDeriver =
    new BsonCodecDeriver(config)

  private[bson] object Codecs {
    import org.bson.{BsonDocument, BsonType, BsonValue}

    /**
     * Unit codec - encodes as empty BSON document
     */
    val unitCodec: BsonCodec[Unit] = BsonCodec(
      new BsonEncoder[Unit] {
        def encode(writer: BsonWriter, value: Unit, ctx: BsonEncoder.EncoderContext): Unit = {
          writer.writeStartDocument()
          writer.writeEndDocument()
        }
        def toBsonValue(value: Unit): BsonValue = new BsonDocument()
      },
      new BsonDecoder[Unit] {
        def decodeUnsafe(reader: BsonReader, trace: List[BsonTrace], ctx: BsonDecoder.BsonDecoderContext): Unit = {
          reader.readStartDocument()
          while (reader.readBsonType() != BsonType.END_OF_DOCUMENT) {
            reader.readName()
            reader.skipValue()
          }
          reader.readEndDocument()
          ()
        }
        def fromBsonValueUnsafe(value: BsonValue, trace: List[BsonTrace], ctx: BsonDecoder.BsonDecoderContext): Unit =
          ()
      }
    )

    /**
     * Scala BigInt codec - wraps Java BigInteger codec
     */
    val bigIntCodec: BsonCodec[BigInt] = BsonCodec(
      new BsonEncoder[BigInt] {
        def encode(writer: BsonWriter, value: BigInt, ctx: BsonEncoder.EncoderContext): Unit =
          BsonEncoder.bigInteger.encode(writer, value.bigInteger, ctx)
        def toBsonValue(value: BigInt): BsonValue =
          BsonEncoder.bigInteger.toBsonValue(value.bigInteger)
      },
      new BsonDecoder[BigInt] {
        def decodeUnsafe(reader: BsonReader, trace: List[BsonTrace], ctx: BsonDecoder.BsonDecoderContext): BigInt =
          BigInt(BsonDecoder.bigInteger.decodeUnsafe(reader, trace, ctx))
        def fromBsonValueUnsafe(value: BsonValue, trace: List[BsonTrace], ctx: BsonDecoder.BsonDecoderContext): BigInt =
          BigInt(BsonDecoder.bigInteger.fromBsonValueUnsafe(value, trace, ctx))
      }
    )

    /**
     * Scala BigDecimal codec - wraps Java BigDecimal codec
     */
    val bigDecimalCodec: BsonCodec[BigDecimal] = BsonCodec(
      new BsonEncoder[BigDecimal] {
        private val jEncoder                                                                     = BsonCodec.bigDecimal.encoder
        def encode(writer: BsonWriter, value: BigDecimal, ctx: BsonEncoder.EncoderContext): Unit =
          jEncoder.encode(writer, value.bigDecimal, ctx)
        def toBsonValue(value: BigDecimal): BsonValue =
          jEncoder.toBsonValue(value.bigDecimal)
      },
      new BsonDecoder[BigDecimal] {
        private val jDecoder                                                                                          = BsonCodec.bigDecimal.decoder
        def decodeUnsafe(reader: BsonReader, trace: List[BsonTrace], ctx: BsonDecoder.BsonDecoderContext): BigDecimal =
          BigDecimal(jDecoder.decodeUnsafe(reader, trace, ctx))
        def fromBsonValueUnsafe(
          value: BsonValue,
          trace: List[BsonTrace],
          ctx: BsonDecoder.BsonDecoderContext
        ): BigDecimal =
          BigDecimal(jDecoder.fromBsonValueUnsafe(value, trace, ctx))
      }
    )

    /**
     * Maps a zio-blocks PrimitiveType to the corresponding zio-bson BsonCodec.
     */
    def primitiveCodec[A](primitiveType: PrimitiveType[A]): BsonCodec[A] =
      (primitiveType match {
        case PrimitiveType.Unit              => unitCodec
        case PrimitiveType.Boolean(_)        => BsonCodec.boolean
        case PrimitiveType.Byte(_)           => BsonCodec.byte
        case PrimitiveType.Short(_)          => BsonCodec.short
        case PrimitiveType.Int(_)            => BsonCodec.int
        case PrimitiveType.Long(_)           => BsonCodec.long
        case PrimitiveType.Float(_)          => BsonCodec.float
        case PrimitiveType.Double(_)         => BsonCodec.double
        case PrimitiveType.Char(_)           => BsonCodec.char
        case PrimitiveType.String(_)         => BsonCodec.string
        case PrimitiveType.BigInt(_)         => bigIntCodec
        case PrimitiveType.BigDecimal(_)     => bigDecimalCodec
        case PrimitiveType.DayOfWeek(_)      => BsonCodec.dayOfWeek
        case PrimitiveType.Duration(_)       => BsonCodec.duration
        case PrimitiveType.Instant(_)        => BsonCodec.instant
        case PrimitiveType.LocalDate(_)      => BsonCodec.localDate
        case PrimitiveType.LocalDateTime(_)  => BsonCodec.localDateTime
        case PrimitiveType.LocalTime(_)      => BsonCodec.localTime
        case PrimitiveType.Month(_)          => BsonCodec.month
        case PrimitiveType.MonthDay(_)       => BsonCodec.monthDay
        case PrimitiveType.OffsetDateTime(_) => BsonCodec.offsetDateTime
        case PrimitiveType.OffsetTime(_)     => BsonCodec.offsetTime
        case PrimitiveType.Period(_)         => BsonCodec.period
        case PrimitiveType.Year(_)           => BsonCodec.year
        case PrimitiveType.YearMonth(_)      => BsonCodec.yearMonth
        case PrimitiveType.ZonedDateTime(_)  => BsonCodec.zonedDateTime
        case PrimitiveType.ZoneId(_)         => BsonCodec.zoneId
        case PrimitiveType.ZoneOffset(_)     => BsonCodec.zoneOffset
        case PrimitiveType.Currency(_)       => BsonCodec.currency
        case PrimitiveType.UUID(_)           => BsonCodec.uuid
      }).asInstanceOf[BsonCodec[A]]
  }
}

class BsonCodecDeriver private[bson] (private val config: BsonSchemaCodec.Config) extends Deriver[BsonCodec] {
  import BsonCodecDeriver.Codecs
  import BsonSchemaCodec.SumTypeHandling

  private[this] val DynamicVariantField      = "$zio_dynamic_variant"
  private[this] val DynamicVariantCaseField  = "case"
  private[this] val DynamicVariantValueField = "value"
  private[this] val DynamicMapField          = "$zio_dynamic_map"
  private[this] val DynamicMapKeyField       = "key"
  private[this] val DynamicMapValueField     = "value"

  def withSumTypeHandling(value: BsonSchemaCodec.SumTypeHandling): BsonCodecDeriver =
    new BsonCodecDeriver(config.withSumTypeHandling(value))

  def withClassNameMapping(value: BsonSchemaCodec.TermMapping): BsonCodecDeriver =
    new BsonCodecDeriver(config.withClassNameMapping(value))

  def withIgnoreExtraFields(value: Boolean): BsonCodecDeriver =
    new BsonCodecDeriver(config.withIgnoreExtraFields(value))

  def withNativeObjectId(value: Boolean): BsonCodecDeriver =
    new BsonCodecDeriver(config.withNativeObjectId(value))

  override def derivePrimitive[A](
    primitiveType: PrimitiveType[A],
    typeId: TypeId[A],
    binding: Binding.Primitive[A],
    doc: Doc,
    modifiers: Seq[Modifier.Reflect],
    defaultValue: Option[A],
    examples: Seq[A]
  ): Lazy[BsonCodec[A]] =
    if (binding.isInstanceOf[Binding[?, ?]]) Lazy(Codecs.primitiveCodec(primitiveType))
    else binding.asInstanceOf[BindingInstance[BsonCodec, ?, A]].instance

  private def deriveJsonVariantCodec[A]: BsonCodec[A] =
    deriveSemanticJsonCodec(_.toDynamicValue, value => Right(Json.fromDynamicValue(value)))
      .asInstanceOf[BsonCodec[A]]

  private def deriveJsonWrapperCodec[A, B](binding: Binding.Wrapper[A, B]): BsonCodec[A] = {
    val jsonBinding = binding.asInstanceOf[Binding.Wrapper[Json, DynamicValue]]
    deriveSemanticJsonCodec(
      unwrap = jsonBinding.unwrap,
      wrap = value =>
        try Right(jsonBinding.wrap(value))
        catch {
          case error: SchemaError => Left(error.message)
        }
    ).asInstanceOf[BsonCodec[A]]
  }

  private def deriveSemanticJsonCodec(
    unwrap: Json => DynamicValue,
    wrap: DynamicValue => Either[String, Json]
  ): BsonCodec[Json] = {
    val encoder = new BsonEncoder[Json] {
      def encode(writer: BsonWriter, value: Json, ctx: BsonEncoder.EncoderContext): Unit =
        BsonEncoder.bsonValueEncoder.encode(
          writer,
          dynamicToBsonValue(unwrap(value), preserveDynamicTypes = false),
          ctx
        )

      def toBsonValue(value: Json): BsonValue =
        dynamicToBsonValue(unwrap(value), preserveDynamicTypes = false)
    }

    val decoder = new BsonDecoder[Json] {
      def decodeUnsafe(reader: BsonReader, trace: List[BsonTrace], ctx: BsonDecoder.BsonDecoderContext): Json =
        wrapJson(bsonReaderToDynamicValue(reader, trace, preserveDynamicTypes = false), trace, wrap)

      def fromBsonValueUnsafe(value: BsonValue, trace: List[BsonTrace], ctx: BsonDecoder.BsonDecoderContext): Json =
        wrapJson(bsonValueToDynamicValue(value, trace, preserveDynamicTypes = false), trace, wrap)
    }

    BsonCodec(encoder, decoder)
  }

  private def deriveDynamicCodec: BsonCodec[DynamicValue] = {
    val encoder = new BsonEncoder[DynamicValue] {
      def encode(writer: BsonWriter, value: DynamicValue, ctx: BsonEncoder.EncoderContext): Unit =
        BsonEncoder.bsonValueEncoder.encode(writer, dynamicToBsonValue(value, preserveDynamicTypes = true), ctx)

      def toBsonValue(value: DynamicValue): BsonValue =
        dynamicToBsonValue(value, preserveDynamicTypes = true)
    }

    val decoder = new BsonDecoder[DynamicValue] {
      def decodeUnsafe(reader: BsonReader, trace: List[BsonTrace], ctx: BsonDecoder.BsonDecoderContext): DynamicValue =
        bsonReaderToDynamicValue(reader, trace, preserveDynamicTypes = true)

      def fromBsonValueUnsafe(
        value: BsonValue,
        trace: List[BsonTrace],
        ctx: BsonDecoder.BsonDecoderContext
      ): DynamicValue =
        bsonValueToDynamicValue(value, trace, preserveDynamicTypes = true)
    }

    BsonCodec(encoder, decoder)
  }

  private def wrapJson(
    value: DynamicValue,
    trace: List[BsonTrace],
    wrap: DynamicValue => Either[String, Json]
  ): Json =
    wrap(value) match {
      case Right(json) => json
      case Left(error) => throw BsonDecoder.Error(trace, error)
    }

  private def bsonReaderToDynamicValue(
    reader: BsonReader,
    trace: List[BsonTrace],
    preserveDynamicTypes: Boolean
  ): DynamicValue = {
    val codec = new org.bson.codecs.BsonValueCodec()
    val value = codec.decode(reader, org.bson.codecs.DecoderContext.builder().build())
    bsonValueToDynamicValue(value, trace, preserveDynamicTypes)
  }

  private def dynamicToBsonValue(value: DynamicValue, preserveDynamicTypes: Boolean): BsonValue = value match {
    case primitive: DynamicValue.Primitive                     => primitiveToBsonValue(primitive.value)
    case record: DynamicValue.Record                           => recordToBsonValue(record, preserveDynamicTypes)
    case sequence: DynamicValue.Sequence                       => sequenceToBsonValue(sequence, preserveDynamicTypes)
    case variant: DynamicValue.Variant if preserveDynamicTypes => variantToBsonValue(variant, preserveDynamicTypes)
    case _: DynamicValue.Variant                               =>
      throw new UnsupportedOperationException("Semantic BSON encoding for DynamicValue.Variant is not supported.")
    case map: DynamicValue.Map if preserveDynamicTypes => mapToBsonValue(map, preserveDynamicTypes)
    case _: DynamicValue.Map                           =>
      throw new UnsupportedOperationException("Semantic BSON encoding for DynamicValue.Map is not supported.")
    case DynamicValue.Null => BsonNull.VALUE
  }

  private def primitiveToBsonValue(value: PrimitiveValue): BsonValue = value match {
    case v: PrimitiveValue.Boolean    => BsonBoolean.valueOf(v.value)
    case v: PrimitiveValue.Int        => new BsonInt32(v.value)
    case v: PrimitiveValue.Long       => new BsonInt64(v.value)
    case v: PrimitiveValue.Double     => new BsonDouble(v.value)
    case v: PrimitiveValue.String     => new BsonString(v.value)
    case v: PrimitiveValue.BigDecimal => Codecs.bigDecimalCodec.encoder.toBsonValue(v.value)
    case v: PrimitiveValue.Instant    => BsonCodec.instant.encoder.toBsonValue(v.value)
    case v: PrimitiveValue.UUID       => BsonCodec.uuid.encoder.toBsonValue(v.value)
    case primitive                    =>
      throw new UnsupportedOperationException(
        s"BSON codec for DynamicValue primitive ${primitive.getClass.getSimpleName} is not yet implemented."
      )
  }

  private def recordToBsonValue(record: DynamicValue.Record, preserveDynamicTypes: Boolean): BsonDocument = {
    val doc = new BsonDocument()
    record.fields.foreach { case (name, value) =>
      doc.put(name, dynamicToBsonValue(value, preserveDynamicTypes))
    }
    doc
  }

  private def sequenceToBsonValue(sequence: DynamicValue.Sequence, preserveDynamicTypes: Boolean): BsonArray = {
    val array = new BsonArray()
    sequence.elements.foreach { value =>
      array.add(dynamicToBsonValue(value, preserveDynamicTypes))
    }
    array
  }

  private def variantToBsonValue(variant: DynamicValue.Variant, preserveDynamicTypes: Boolean): BsonDocument = {
    val payload = new BsonDocument()
    payload.put(DynamicVariantCaseField, new BsonString(variant.caseNameValue))
    payload.put(DynamicVariantValueField, dynamicToBsonValue(variant.value, preserveDynamicTypes))

    val doc = new BsonDocument()
    doc.put(DynamicVariantField, payload)
    doc
  }

  private def mapToBsonValue(map: DynamicValue.Map, preserveDynamicTypes: Boolean): BsonDocument = {
    val entries = new BsonArray()
    map.entries.foreach { case (key, value) =>
      val entry = new BsonDocument()
      entry.put(DynamicMapKeyField, dynamicToBsonValue(key, preserveDynamicTypes))
      entry.put(DynamicMapValueField, dynamicToBsonValue(value, preserveDynamicTypes))
      entries.add(entry)
    }

    val doc = new BsonDocument()
    doc.put(DynamicMapField, entries)
    doc
  }

  private def bsonValueToDynamicValue(
    value: BsonValue,
    trace: List[BsonTrace],
    preserveDynamicTypes: Boolean
  ): DynamicValue =
    value.getBsonType match {
      case BsonType.DOCUMENT =>
        val doc = value.asDocument()
        if (preserveDynamicTypes) {
          decodeDynamicVariant(doc, trace)
            .orElse(decodeDynamicMap(doc, trace))
            .getOrElse(documentToDynamicValue(doc, trace, preserveDynamicTypes))
        } else {
          documentToDynamicValue(doc, trace, preserveDynamicTypes)
        }
      case BsonType.ARRAY      => arrayToDynamicValue(value.asArray(), trace, preserveDynamicTypes)
      case BsonType.STRING     => DynamicValue.string(value.asString().getValue)
      case BsonType.BOOLEAN    => DynamicValue.boolean(value.asBoolean().getValue)
      case BsonType.INT32      => DynamicValue.int(value.asInt32().getValue)
      case BsonType.INT64      => DynamicValue.long(value.asInt64().getValue)
      case BsonType.DOUBLE     => DynamicValue.double(value.asDouble().getValue)
      case BsonType.DECIMAL128 =>
        DynamicValue.bigDecimal(
          Codecs.bigDecimalCodec.decoder.fromBsonValueUnsafe(value, trace, BsonDecoder.BsonDecoderContext.default)
        )
      case BsonType.DATE_TIME =>
        new DynamicValue.Primitive(
          new PrimitiveValue.Instant(java.time.Instant.ofEpochMilli(value.asDateTime().getValue))
        )
      case BsonType.BINARY =>
        try new DynamicValue.Primitive(new PrimitiveValue.UUID(value.asBinary().asUuid()))
        catch {
          case _: IllegalArgumentException => throw BsonDecoder.Error(trace, "Expected UUID binary value")
        }
      case BsonType.NULL => DynamicValue.Null
      case _             => throw BsonDecoder.Error(trace, s"Unsupported BSON type ${value.getBsonType} for DynamicValue")
    }

  private def documentToDynamicValue(
    doc: BsonDocument,
    trace: List[BsonTrace],
    preserveDynamicTypes: Boolean
  ): DynamicValue = {
    val fields = scala.collection.mutable.ArrayBuffer.empty[(String, DynamicValue)]
    val iter   = doc.entrySet().iterator()

    while (iter.hasNext()) {
      val entry      = iter.next()
      val fieldTrace = BsonTrace.Field(entry.getKey()) :: trace
      fields += ((entry.getKey(), bsonValueToDynamicValue(entry.getValue(), fieldTrace, preserveDynamicTypes)))
    }

    new DynamicValue.Record(zio.blocks.chunk.Chunk.from(fields))
  }

  private def arrayToDynamicValue(
    array: BsonArray,
    trace: List[BsonTrace],
    preserveDynamicTypes: Boolean
  ): DynamicValue = {
    val elements = scala.collection.mutable.ArrayBuffer.empty[DynamicValue]
    val iter     = array.iterator()
    var idx      = 0

    while (iter.hasNext()) {
      val element = iter.next()
      elements += bsonValueToDynamicValue(element, BsonTrace.Array(idx) :: trace, preserveDynamicTypes)
      idx += 1
    }

    new DynamicValue.Sequence(zio.blocks.chunk.Chunk.from(elements))
  }

  private def decodeDynamicVariant(doc: BsonDocument, trace: List[BsonTrace]): Option[DynamicValue] =
    if (doc.size() != 1) None
    else {
      val wrapper = doc.get(DynamicVariantField)
      if (wrapper == null || !wrapper.isDocument()) None
      else {
        val payload   = wrapper.asDocument()
        val caseName  = payload.get(DynamicVariantCaseField)
        val caseValue = payload.get(DynamicVariantValueField)

        if (caseName == null || !caseName.isString() || caseValue == null) None
        else
          Some(
            new DynamicValue.Variant(
              caseName.asString().getValue,
              bsonValueToDynamicValue(
                caseValue,
                BsonTrace.Field(DynamicVariantValueField) :: BsonTrace.Field(DynamicVariantField) :: trace,
                preserveDynamicTypes = true
              )
            )
          )
      }
    }

  private def decodeDynamicMap(doc: BsonDocument, trace: List[BsonTrace]): Option[DynamicValue] =
    if (doc.size() != 1) None
    else {
      val wrapper = doc.get(DynamicMapField)
      if (wrapper == null || !wrapper.isArray()) None
      else {
        val entries = scala.collection.mutable.ArrayBuffer.empty[(DynamicValue, DynamicValue)]
        val iter    = wrapper.asArray().iterator()
        var idx     = 0

        while (iter.hasNext()) {
          val value      = iter.next()
          val entryTrace = BsonTrace.Array(idx) :: BsonTrace.Field(DynamicMapField) :: trace
          if (!value.isDocument()) {
            throw BsonDecoder.Error(entryTrace, s"Expected DOCUMENT but got ${value.getBsonType}")
          }

          val entryDoc   = value.asDocument()
          val encodedKey = entryDoc.get(DynamicMapKeyField)
          val encodedVal = entryDoc.get(DynamicMapValueField)

          if (encodedKey == null || encodedVal == null) {
            throw BsonDecoder.Error(entryTrace, "Invalid dynamic map entry.")
          }

          entries += ((
            bsonValueToDynamicValue(
              encodedKey,
              BsonTrace.Field(DynamicMapKeyField) :: entryTrace,
              preserveDynamicTypes = true
            ),
            bsonValueToDynamicValue(
              encodedVal,
              BsonTrace.Field(DynamicMapValueField) :: entryTrace,
              preserveDynamicTypes = true
            )
          ))
          idx += 1
        }

        Some(new DynamicValue.Map(zio.blocks.chunk.Chunk.from(entries)))
      }
    }

  // Record (case class) codec derivation
  override def deriveRecord[F[_, _], A](
    fields: IndexedSeq[Term[F, A, ?]],
    typeId: TypeId[A],
    binding: Binding.Record[A],
    doc: Doc,
    modifiers: Seq[Modifier.Reflect],
    defaultValue: Option[A],
    examples: Seq[A]
  )(implicit F: HasBinding[F], D: HasInstance[F]): Lazy[BsonCodec[A]] =
    if (binding.isInstanceOf[Binding[?, ?]]) Lazy {
      val recordBinding   = binding.asInstanceOf[Binding.Record[A]]
      val constructor     = recordBinding.constructor
      val deconstructor   = recordBinding.deconstructor
      val bindingUsedRegs = deconstructor.usedRegisters
      val registers: IndexedSeq[Register[Any]] =
        if (
          RegisterOffset.getObjects(bindingUsedRegs) == fields.length &&
          RegisterOffset.getBytes(bindingUsedRegs) == 0
        ) {
          var offset = 0L
          fields.indices.map { _ =>
            val reg = new Register.Object[AnyRef](offset).asInstanceOf[Register[Any]]
            offset = RegisterOffset.incrementObjects(offset)
            reg
          }
        } else {
          val reflects = fields.iterator.map(_.value).toArray.asInstanceOf[Array[Reflect[F, ?]]]
          Reflect.Record.registers(reflects).toIndexedSeq
        }

      val isRecursive = fields.exists(_.value.isInstanceOf[Reflect.Deferred[F, ?]])
      var fieldCodecs = if (isRecursive) recursiveRecordCache.get().get(typeId) else null
      val deriveCodecs = fieldCodecs eq null
      if (deriveCodecs) {
        fieldCodecs = new Array[BsonCodec[Any]](fields.length)
        if (isRecursive) recursiveRecordCache.get().put(typeId, fieldCodecs)
        var idx = 0
        while (idx < fields.length) {
          fieldCodecs(idx) = D.instance(fields(idx).value.metadata).force.asInstanceOf[BsonCodec[Any]]
          idx += 1
        }
      }

    // Get field names (respecting @rename modifier)
    val fieldNames: Array[String] = fields.map { field =>
      field.modifiers.collectFirst { case m: Modifier.rename =>
        m.name
      }.getOrElse(field.name)
    }.toArray

    // Check for transient fields
    val transientFields: Array[Boolean] = fields.map { field =>
      field.modifiers.exists(_.isInstanceOf[Modifier.transient])
    }.toArray

    val encoder = new BsonEncoder[A] {
      def encode(writer: BsonWriter, value: A, ctx: BsonEncoder.EncoderContext): Unit = {
        writer.writeStartDocument()

        // Deconstruct the value into registers
        val regs = Registers(deconstructor.usedRegisters)
        deconstructor.deconstruct(regs, 0, value)

        // Encode each field
        var idx = 0
        while (idx < fields.length) {
          if (!transientFields(idx)) {
            val fieldValue = registers(idx).get(regs, 0)
            writer.writeName(fieldNames(idx))
            fieldCodecs(idx).encoder.encode(writer, fieldValue, BsonEncoder.EncoderContext.default)
          }
          idx += 1
        }

        writer.writeEndDocument()
      }

      def toBsonValue(value: A): BsonValue = {
        val doc = new org.bson.BsonDocument()

        // Deconstruct the value into registers
        val regs = Registers(deconstructor.usedRegisters)
        deconstructor.deconstruct(regs, 0, value)

        // Encode each field
        var idx = 0
        while (idx < fields.length) {
          if (!transientFields(idx)) {
            val fieldValue = registers(idx).get(regs, 0)
            doc.put(fieldNames(idx), fieldCodecs(idx).encoder.toBsonValue(fieldValue))
          }
          idx += 1
        }

        doc
      }
    }

    val decoder = new BsonDecoder[A] {
      def decodeUnsafe(reader: BsonReader, trace: List[BsonTrace], ctx: BsonDecoder.BsonDecoderContext): A = {
        reader.readStartDocument()

        val regs                     = Registers(constructor.usedRegisters)
        val fieldValues: Array[Any]  = Array.ofDim(fields.length)
        val fieldSet: Array[Boolean] = Array.ofDim(fields.length)

        // Create field name to index map
        val fieldIndexMap = scala.collection.mutable.HashMap[String, Int]()
        var i             = 0
        while (i < fieldNames.length) {
          fieldIndexMap(fieldNames(i)) = i
          i += 1
        }

        // Read all fields from BSON document
        while (reader.readBsonType() != org.bson.BsonType.END_OF_DOCUMENT) {
          val name = reader.readName()
          fieldIndexMap.get(name) match {
            case Some(idx) =>
              val fieldTrace = BsonTrace.Field(name) :: trace
              fieldValues(idx) =
                fieldCodecs(idx).decoder.decodeUnsafe(reader, fieldTrace, BsonDecoder.BsonDecoderContext.default)
              fieldSet(idx) = true
            case None =>
              // Check if we should reject extra fields
              // We also allow fields explicitly ignored by the context (e.g. discriminator fields)
              val isIgnored = ctx.ignoreExtraField.contains(name)
              if (!config.ignoreExtraFields && !isIgnored) {
                throw BsonDecoder.Error(BsonTrace.Field(name) :: trace, "Invalid extra field.")
              }
              // Skip unknown fields
              reader.skipValue()
          }
        }

        reader.readEndDocument()

        // Set field values in registers
        i = 0
        while (i < fields.length) {
          if (fieldSet(i)) {
            registers(i).set(regs, 0, fieldValues(i))
          } else {
            // Field is missing - check if it's transient or has a default value
            if (transientFields(i)) {
              // Transient field - use default value if available
              fields(i).value.getDefaultValue match {
                case Some(defaultValue) =>
                  registers(i).set(regs, 0, defaultValue)
                case None =>
                  throw BsonDecoder.Error(trace, s"Missing required transient field: ${fieldNames(i)}")
              }
            } else {
              // Regular field - use default value if available
              fields(i).value.getDefaultValue match {
                case Some(defaultValue) =>
                  registers(i).set(regs, 0, defaultValue)
                case None =>
                  throw BsonDecoder.Error(trace, s"Missing required field: ${fieldNames(i)}")
              }
            }
          }
          i += 1
        }

        constructor.construct(regs, 0)
      }

      def fromBsonValueUnsafe(value: BsonValue, trace: List[BsonTrace], ctx: BsonDecoder.BsonDecoderContext): A = {
        if (value.getBsonType() != org.bson.BsonType.DOCUMENT) {
          throw BsonDecoder.Error(trace, s"Expected DOCUMENT but got ${value.getBsonType()}")
        }

        val doc                      = value.asDocument()
        val regs                     = Registers(constructor.usedRegisters)
        val fieldValues: Array[Any]  = Array.ofDim(fields.length)
        val fieldSet: Array[Boolean] = Array.ofDim(fields.length)

        // Create field name to index map
        val fieldIndexMap = scala.collection.mutable.HashMap[String, Int]()
        var i             = 0
        while (i < fieldNames.length) {
          fieldIndexMap(fieldNames(i)) = i
          i += 1
        }

        // Read all fields from BSON document
        val iter = doc.entrySet().iterator()
        while (iter.hasNext()) {
          val entry = iter.next()
          val name  = entry.getKey()
          fieldIndexMap.get(name) match {
            case Some(idx) =>
              val fieldTrace = BsonTrace.Field(name) :: trace
              fieldValues(idx) = fieldCodecs(idx).decoder.fromBsonValueUnsafe(
                entry.getValue(),
                fieldTrace,
                BsonDecoder.BsonDecoderContext.default
              )
              fieldSet(idx) = true
            case None =>
              // Check if we should reject extra fields
              // We also allow fields explicitly ignored by the context (e.g. discriminator fields)
              val isIgnored = ctx.ignoreExtraField.contains(name)
              if (!config.ignoreExtraFields && !isIgnored) {
                throw BsonDecoder.Error(BsonTrace.Field(name) :: trace, "Invalid extra field.")
              }
            // Skip unknown fields
          }
        }

        // Set field values in registers
        i = 0
        while (i < fields.length) {
          if (fieldSet(i)) {
            registers(i).set(regs, 0, fieldValues(i))
          } else {
            // Field is missing - check if it's transient or has a default value
            if (transientFields(i)) {
              // Transient field - use default value if available
              fields(i).value.getDefaultValue match {
                case Some(defaultValue) =>
                  registers(i).set(regs, 0, defaultValue)
                case None =>
                  throw BsonDecoder.Error(trace, s"Missing required transient field: ${fieldNames(i)}")
              }
            } else {
              // Regular field - use default value if available
              fields(i).value.getDefaultValue match {
                case Some(defaultValue) =>
                  registers(i).set(regs, 0, defaultValue)
                case None =>
                  throw BsonDecoder.Error(trace, s"Missing required field: ${fieldNames(i)}")
              }
            }
          }
          i += 1
        }

        constructor.construct(regs, 0)
      }
    }

      BsonCodec(encoder, decoder)
    } else binding.asInstanceOf[BindingInstance[BsonCodec, ?, A]].instance

  // Sequence (List, Vector, Set, etc.) codec derivation
  override def deriveSequence[F[_, _], C[_], E](
    element: Reflect[F, E],
    typeId: TypeId[C[E]],
    binding: Binding.Seq[C, E],
    doc: Doc,
    modifiers: Seq[Modifier.Reflect],
    defaultValue: Option[C[E]],
    examples: Seq[C[E]]
  )(implicit F: HasBinding[F], D: HasInstance[F]): Lazy[BsonCodec[C[E]]] =
    if (binding.isInstanceOf[Binding[?, ?]]) {
      val seqBinding = binding.asInstanceOf[Binding.Seq[C, E]]
      val constructor = seqBinding.constructor
      val deconstructor = seqBinding.deconstructor
      implicit val elemClassTag: scala.reflect.ClassTag[E] =
        element.typeId.classTag.asInstanceOf[scala.reflect.ClassTag[E]]

      D.instance(element.metadata).map { elementCodec =>

    val encoder = new BsonEncoder[C[E]] {
      def encode(writer: BsonWriter, value: C[E], ctx: BsonEncoder.EncoderContext): Unit = {
        writer.writeStartArray()

        val iter = deconstructor.deconstruct(value)
        while (iter.hasNext) {
          val elem = iter.next()
          elementCodec.encoder.encode(writer, elem, BsonEncoder.EncoderContext.default)
        }

        writer.writeEndArray()
      }

      def toBsonValue(value: C[E]): BsonValue = {
        val array = new org.bson.BsonArray()

        val iter = deconstructor.deconstruct(value)
        while (iter.hasNext) {
          val elem = iter.next()
          array.add(elementCodec.encoder.toBsonValue(elem))
        }

        array
      }
    }

    val decoder = new BsonDecoder[C[E]] {
      def decodeUnsafe(reader: BsonReader, trace: List[BsonTrace], ctx: BsonDecoder.BsonDecoderContext): C[E] = {
        if (reader.getCurrentBsonType() != org.bson.BsonType.ARRAY) {
          reader.readBsonType()
        }

        if (reader.getCurrentBsonType() == org.bson.BsonType.ARRAY) {
          reader.readStartArray()

          val builder = constructor.newBuilder[E](16)
          var idx     = 0

          while (reader.readBsonType() != org.bson.BsonType.END_OF_DOCUMENT) {
            val elemTrace = BsonTrace.Array(idx) :: trace
            val elem      = elementCodec.decoder.decodeUnsafe(reader, elemTrace, BsonDecoder.BsonDecoderContext.default)
            constructor.add(builder, elem)
            idx += 1
          }

          reader.readEndArray()
          constructor.result(builder)
        } else {
          throw BsonDecoder.Error(trace, s"Expected ARRAY but got ${reader.getCurrentBsonType()}")
        }
      }

      def fromBsonValueUnsafe(value: BsonValue, trace: List[BsonTrace], ctx: BsonDecoder.BsonDecoderContext): C[E] = {
        if (value.getBsonType() != org.bson.BsonType.ARRAY) {
          throw BsonDecoder.Error(trace, s"Expected ARRAY but got ${value.getBsonType()}")
        }

        val array   = value.asArray()
        val builder = constructor.newBuilder[E](array.size())
        var idx     = 0

        val iter = array.iterator()
        while (iter.hasNext()) {
          val elem      = iter.next()
          val elemTrace = BsonTrace.Array(idx) :: trace
          val decoded   =
            elementCodec.decoder.fromBsonValueUnsafe(elem, elemTrace, BsonDecoder.BsonDecoderContext.default)
          constructor.add(builder, decoded)
          idx += 1
        }

        constructor.result(builder)
      }
    }

        BsonCodec(encoder, decoder)
      }
    } else binding.asInstanceOf[BindingInstance[BsonCodec, ?, C[E]]].instance

  // Map codec derivation
  override def deriveMap[F[_, _], M[_, _], K, V](
    key: Reflect[F, K],
    value: Reflect[F, V],
    typeId: TypeId[M[K, V]],
    binding: Binding.Map[M, K, V],
    doc: Doc,
    modifiers: Seq[Modifier.Reflect],
    defaultValue: Option[M[K, V]],
    examples: Seq[M[K, V]]
  )(implicit F: HasBinding[F], D: HasInstance[F]): Lazy[BsonCodec[M[K, V]]] =
    if (binding.isInstanceOf[Binding[?, ?]]) {
      val mapBinding    = binding.asInstanceOf[Binding.Map[M, K, V]]
      val constructor   = mapBinding.constructor
      val deconstructor = mapBinding.deconstructor

      val isStringKey = key.isPrimitive && {
        key.asPrimitive.get.primitiveType match {
          case _: PrimitiveType.String => true
          case _                       => false
        }
      }

      D.instance(value.metadata).map { valueCodec =>
        if (isStringKey) {
      // String keys: encode as BSON document
      val encoder = new BsonEncoder[M[K, V]] {
        def encode(writer: BsonWriter, value: M[K, V], ctx: BsonEncoder.EncoderContext): Unit = {
          writer.writeStartDocument()

          val iter = deconstructor.deconstruct(value)
          while (iter.hasNext) {
            val kv  = iter.next()
            val key = deconstructor.getKey(kv).asInstanceOf[String]
            val v   = deconstructor.getValue(kv)
            writer.writeName(key)
            valueCodec.encoder.encode(writer, v, BsonEncoder.EncoderContext.default)
          }

          writer.writeEndDocument()
        }

        def toBsonValue(value: M[K, V]): BsonValue = {
          val doc = new org.bson.BsonDocument()

          val iter = deconstructor.deconstruct(value)
          while (iter.hasNext) {
            val kv  = iter.next()
            val key = deconstructor.getKey(kv).asInstanceOf[String]
            val v   = deconstructor.getValue(kv)
            doc.put(key, valueCodec.encoder.toBsonValue(v))
          }

          doc
        }
      }

      val decoder = new BsonDecoder[M[K, V]] {
        def decodeUnsafe(reader: BsonReader, trace: List[BsonTrace], ctx: BsonDecoder.BsonDecoderContext): M[K, V] = {
          if (reader.getCurrentBsonType() != org.bson.BsonType.DOCUMENT) {
            reader.readBsonType()
          }

          if (reader.getCurrentBsonType() == org.bson.BsonType.DOCUMENT) {
            reader.readStartDocument()

            val builder = constructor.newObjectBuilder[K, V](16)

            while (reader.readBsonType() != org.bson.BsonType.END_OF_DOCUMENT) {
              val keyStr     = reader.readName()
              val key        = keyStr.asInstanceOf[K]
              val fieldTrace = BsonTrace.Field(keyStr) :: trace
              val v          = valueCodec.decoder.decodeUnsafe(reader, fieldTrace, BsonDecoder.BsonDecoderContext.default)
              constructor.addObject(builder, key, v)
            }

            reader.readEndDocument()
            constructor.resultObject[K, V](builder)
          } else {
            throw BsonDecoder.Error(trace, s"Expected DOCUMENT but got ${reader.getCurrentBsonType()}")
          }
        }

        def fromBsonValueUnsafe(
          value: BsonValue,
          trace: List[BsonTrace],
          ctx: BsonDecoder.BsonDecoderContext
        ): M[K, V] = {
          if (value.getBsonType() != org.bson.BsonType.DOCUMENT) {
            throw BsonDecoder.Error(trace, s"Expected DOCUMENT but got ${value.getBsonType()}")
          }

          val doc     = value.asDocument()
          val builder = constructor.newObjectBuilder[K, V](doc.size())

          val iter = doc.entrySet().iterator()
          while (iter.hasNext()) {
            val entry      = iter.next()
            val keyStr     = entry.getKey()
            val key        = keyStr.asInstanceOf[K]
            val fieldTrace = BsonTrace.Field(keyStr) :: trace
            val v          = valueCodec.decoder.fromBsonValueUnsafe(
              entry.getValue(),
              fieldTrace,
              BsonDecoder.BsonDecoderContext.default
            )
            constructor.addObject(builder, key, v)
          }

          constructor.resultObject[K, V](builder)
        }
      }

          BsonCodec(encoder, decoder)
        } else {
          throw new UnsupportedOperationException(s"Map with non-string keys not yet supported for ${typeId.fullName}")
        }
      }
    } else binding.asInstanceOf[BindingInstance[BsonCodec, ?, M[K, V]]].instance

  override def deriveVariant[F[_, _], A](
    cases: IndexedSeq[Term[F, A, ?]],
    typeId: TypeId[A],
    binding: Binding.Variant[A],
    doc: Doc,
    modifiers: Seq[Modifier.Reflect],
    defaultValue: Option[A],
    examples: Seq[A]
  )(implicit F: HasBinding[F], D: HasInstance[F]): Lazy[BsonCodec[A]] =
    if (!binding.isInstanceOf[Binding[?, ?]])
      binding.asInstanceOf[BindingInstance[BsonCodec, ?, A]].instance
    else if (typeId == TypeId.of[Json]) Lazy(deriveJsonVariantCodec[A])
    else Lazy {
      val variantBinding = binding.asInstanceOf[Binding.Variant[A]]
      val discriminator  = variantBinding.discriminator

      val caseCodecs = new Array[BsonCodec[Any]](cases.length)
      var codecIdx = 0
      while (codecIdx < cases.length) {
        caseCodecs(codecIdx) = D.instance(cases(codecIdx).value.metadata).force.asInstanceOf[BsonCodec[Any]]
        codecIdx += 1
      }

    // Get case names (respecting @rename modifier if present)
    val caseNames: Array[String] = cases.map { case_ =>
      case_.modifiers.collectFirst { case m: Modifier.rename =>
        m.name
      }.getOrElse(config.classNameMapping(case_.name))
    }.toArray

    // Get case aliases (respecting @alias modifier)
    val caseAliases: Array[Seq[String]] = cases.map { case_ =>
      case_.modifiers.collect { case m: Modifier.alias => m.name }
    }.toArray

    // Check for transient cases
    val transientCases: Array[Boolean] = cases.map { case_ =>
      case_.modifiers.exists(_.isInstanceOf[Modifier.transient])
    }.toArray

    // Check if each case is a case object (record with zero fields)
    val isCaseObject: Array[Boolean] = cases.map { case_ =>
      case_.value.isRecord && case_.value.asRecord.get.fields.isEmpty
    }.toArray

    // Build case name to index map for decoding (including aliases)
    val caseNameToIndex = scala.collection.mutable.HashMap[String, Int]()
    var i               = 0
    while (i < caseNames.length) {
      if (!transientCases(i)) {
        caseNameToIndex(caseNames(i)) = i
        caseAliases(i).foreach { alias =>
          caseNameToIndex(alias) = i
        }
      }
      i += 1
    }

    config.sumTypeHandling match {
      case SumTypeHandling.WrapperWithClassNameField =>
        // WrapperWithClassNameField mode: { "CaseName": <case value> }
        val encoder = new BsonEncoder[A] {
          def encode(writer: BsonWriter, value: A, ctx: BsonEncoder.EncoderContext): Unit = {
            val caseIdx = discriminator.discriminate(value)

            if (transientCases(caseIdx)) {
              writer.writeStartDocument()
              writer.writeEndDocument()
            } else {
              val caseName  = caseNames(caseIdx)
              val caseCodec = caseCodecs(caseIdx)

              writer.writeStartDocument()
              writer.writeName(caseName)
              caseCodec.encoder.encode(writer, value, BsonEncoder.EncoderContext.default)
              writer.writeEndDocument()
            }
          }

          def toBsonValue(value: A): BsonValue = {
            val caseIdx = discriminator.discriminate(value)

            if (transientCases(caseIdx)) {
              new org.bson.BsonDocument()
            } else {
              val caseName  = caseNames(caseIdx)
              val caseCodec = caseCodecs(caseIdx)

              val doc = new org.bson.BsonDocument()
              doc.put(caseName, caseCodec.encoder.toBsonValue(value))
              doc
            }
          }
        }

        val decoder = new BsonDecoder[A] {
          def decodeUnsafe(reader: BsonReader, trace: List[BsonTrace], ctx: BsonDecoder.BsonDecoderContext): A = {
            reader.readStartDocument()

            if (reader.readBsonType() == org.bson.BsonType.END_OF_DOCUMENT) {
              reader.readEndDocument()
              throw BsonDecoder.Error(trace, "Expected a case wrapper but got empty document")
            }

            val caseName   = reader.readName()
            val fieldTrace = BsonTrace.Field(caseName) :: trace

            caseNameToIndex.get(caseName) match {
              case Some(idx) =>
                val caseCodec = caseCodecs(idx)
                val decoded   = caseCodec.decoder.decodeUnsafe(reader, fieldTrace, BsonDecoder.BsonDecoderContext.default)

                // Skip any extra fields
                while (reader.readBsonType() != org.bson.BsonType.END_OF_DOCUMENT) {
                  reader.readName()
                  reader.skipValue()
                }

                reader.readEndDocument()
                decoded.asInstanceOf[A]

              case None =>
                throw BsonDecoder.Error(fieldTrace, s"Unknown case name: $caseName")
            }
          }

          def fromBsonValueUnsafe(value: BsonValue, trace: List[BsonTrace], ctx: BsonDecoder.BsonDecoderContext): A = {
            if (value.getBsonType() != org.bson.BsonType.DOCUMENT) {
              throw BsonDecoder.Error(trace, s"Expected DOCUMENT but got ${value.getBsonType()}")
            }

            val doc    = value.asDocument()
            val fields = doc.entrySet().iterator()

            if (!fields.hasNext()) {
              throw BsonDecoder.Error(trace, "Expected a case wrapper but got empty document")
            }

            val entry      = fields.next()
            val caseName   = entry.getKey()
            val fieldTrace = BsonTrace.Field(caseName) :: trace

            caseNameToIndex.get(caseName) match {
              case Some(idx) =>
                val caseCodec = caseCodecs(idx)
                caseCodec.decoder
                  .fromBsonValueUnsafe(entry.getValue(), fieldTrace, BsonDecoder.BsonDecoderContext.default)
                  .asInstanceOf[A]

              case None =>
                throw BsonDecoder.Error(fieldTrace, s"Unknown case name: $caseName")
            }
          }
        }

        BsonCodec(encoder, decoder)

      case SumTypeHandling.DiscriminatorField(discriminatorFieldName) =>
        // DiscriminatorField mode: { "type": "CaseName", ...case fields... }
        val encoder = new BsonEncoder[A] {
          def encode(writer: BsonWriter, value: A, ctx: BsonEncoder.EncoderContext): Unit = {
            val caseIdx = discriminator.discriminate(value)

            if (transientCases(caseIdx)) {
              writer.writeStartDocument()
              writer.writeEndDocument()
            } else {
              val caseName  = caseNames(caseIdx)
              val caseCodec = caseCodecs(caseIdx)

              writer.writeStartDocument()

              // Write discriminator field first
              writer.writeName(discriminatorFieldName)
              writer.writeString(caseName)

              // Write case value inline (assuming it's a record that will write its fields)
              // We need to encode the case value's fields directly into the current document
              val caseValue = caseCodec.encoder.toBsonValue(value)
              if (caseValue.isDocument()) {
                val caseDoc = caseValue.asDocument()
                val iter    = caseDoc.entrySet().iterator()
                while (iter.hasNext()) {
                  val entry = iter.next()
                  writer.writeName(entry.getKey())
                  // Write the BSON value directly
                  entry.getValue().getBsonType() match {
                    case org.bson.BsonType.STRING   => writer.writeString(entry.getValue().asString().getValue())
                    case org.bson.BsonType.INT32    => writer.writeInt32(entry.getValue().asInt32().getValue())
                    case org.bson.BsonType.INT64    => writer.writeInt64(entry.getValue().asInt64().getValue())
                    case org.bson.BsonType.DOUBLE   => writer.writeDouble(entry.getValue().asDouble().getValue())
                    case org.bson.BsonType.BOOLEAN  => writer.writeBoolean(entry.getValue().asBoolean().getValue())
                    case org.bson.BsonType.NULL     => writer.writeNull()
                    case org.bson.BsonType.DOCUMENT =>
                      BsonEncoder.bsonValueEncoder
                        .encode(writer, entry.getValue(), BsonEncoder.EncoderContext.default)
                    case org.bson.BsonType.ARRAY =>
                      BsonEncoder.bsonValueEncoder
                        .encode(writer, entry.getValue(), BsonEncoder.EncoderContext.default)
                    case _ =>
                      BsonEncoder.bsonValueEncoder
                        .encode(writer, entry.getValue(), BsonEncoder.EncoderContext.default)
                  }
                }
              } else {
                throw new RuntimeException(s"Cannot use DiscriminatorField mode for non-record case: $caseName")
              }

              writer.writeEndDocument()
            }
          }

          def toBsonValue(value: A): BsonValue = {
            val caseIdx = discriminator.discriminate(value)

            if (transientCases(caseIdx)) {
              new org.bson.BsonDocument()
            } else {
              val caseName  = caseNames(caseIdx)
              val caseCodec = caseCodecs(caseIdx)

              val caseValue = caseCodec.encoder.toBsonValue(value)
              if (caseValue.isDocument()) {
                val doc = caseValue.asDocument()
                // Add discriminator field
                doc.put(discriminatorFieldName, new org.bson.BsonString(caseName))
                doc
              } else {
                // If it's not a document, wrap it
                val doc = new org.bson.BsonDocument()
                doc.put(discriminatorFieldName, new org.bson.BsonString(caseName))
                doc.put("value", caseValue)
                doc
              }
            }
          }
        }

        val decoder = new BsonDecoder[A] {
          def decodeUnsafe(reader: BsonReader, trace: List[BsonTrace], ctx: BsonDecoder.BsonDecoderContext): A = {
            // We need to read the document to find the discriminator field first
            val mark = reader.getMark()

            reader.readStartDocument()

            var discriminatorValue: String = null
            var bsonType                   = reader.readBsonType()

            // Scan for discriminator field
            while (discriminatorValue == null && bsonType != org.bson.BsonType.END_OF_DOCUMENT) {
              val name = reader.readName()
              if (name == discriminatorFieldName && bsonType == org.bson.BsonType.STRING) {
                discriminatorValue = reader.readString()
              } else {
                reader.skipValue()
              }
              bsonType = reader.readBsonType()
            }

            reader.readEndDocument()

            if (discriminatorValue == null) {
              throw BsonDecoder.Error(trace, s"Missing discriminator field: $discriminatorFieldName")
            }

            caseNameToIndex.get(discriminatorValue) match {
              case Some(idx) =>
                // Reset and decode the whole document as the case type
                mark.reset()
                val caseCodec = caseCodecs(idx)
                // We pass a context that tells the decoder to ignore the discriminator field
                val nextCtx = ctx.copy(ignoreExtraField = Some(discriminatorFieldName))
                caseCodec.decoder.decodeUnsafe(reader, trace, nextCtx).asInstanceOf[A]

              case None =>
                throw BsonDecoder.Error(
                  BsonTrace.Field(discriminatorFieldName) :: trace,
                  s"Unknown case: $discriminatorValue"
                )
            }
          }

          def fromBsonValueUnsafe(value: BsonValue, trace: List[BsonTrace], ctx: BsonDecoder.BsonDecoderContext): A = {
            if (value.getBsonType() != org.bson.BsonType.DOCUMENT) {
              throw BsonDecoder.Error(trace, s"Expected DOCUMENT but got ${value.getBsonType()}")
            }

            val doc                = value.asDocument()
            val discriminatorField = doc.get(discriminatorFieldName)

            if (discriminatorField == null || discriminatorField.getBsonType() != org.bson.BsonType.STRING) {
              throw BsonDecoder.Error(trace, s"Missing or invalid discriminator field: $discriminatorFieldName")
            }

            val discriminatorValue = discriminatorField.asString().getValue()

            caseNameToIndex.get(discriminatorValue) match {
              case Some(idx) =>
                val caseCodec = caseCodecs(idx)
                // Decode using the same document (the case decoder will read its fields)
                // We pass a context that tells the decoder to ignore the discriminator field
                val nextCtx = ctx.copy(ignoreExtraField = Some(discriminatorFieldName))
                caseCodec.decoder.fromBsonValueUnsafe(value, trace, nextCtx).asInstanceOf[A]

              case None =>
                throw BsonDecoder.Error(
                  BsonTrace.Field(discriminatorFieldName) :: trace,
                  s"Unknown case: $discriminatorValue"
                )
            }
          }
        }

        BsonCodec(encoder, decoder)

      case SumTypeHandling.NoDiscriminator =>
        // NoDiscriminator mode: encode variant value directly without wrapper or discriminator
        // Case objects (zero fields) are encoded as strings
        val encoder = new BsonEncoder[A] {
          def encode(writer: BsonWriter, value: A, ctx: BsonEncoder.EncoderContext): Unit = {
            val caseIdx = discriminator.discriminate(value)
            if (transientCases(caseIdx)) {
              writer.writeStartDocument()
              writer.writeEndDocument()
            } else if (isCaseObject(caseIdx)) {
              // Case object: encode as string
              writer.writeString(caseNames(caseIdx))
            } else {
              // Regular case: encode value directly
              val caseCodec = caseCodecs(caseIdx)
              caseCodec.encoder.encode(writer, value, ctx)
            }
          }

          def toBsonValue(value: A): BsonValue = {
            val caseIdx = discriminator.discriminate(value)
            if (transientCases(caseIdx)) {
              new org.bson.BsonDocument()
            } else if (isCaseObject(caseIdx)) {
              // Case object: encode as string
              new org.bson.BsonString(caseNames(caseIdx))
            } else {
              // Regular case: encode value directly
              val caseCodec = caseCodecs(caseIdx)
              caseCodec.encoder.toBsonValue(value)
            }
          }
        }

        val decoder = new BsonDecoder[A] {
          def decodeUnsafe(reader: BsonReader, trace: List[BsonTrace], ctx: BsonDecoder.BsonDecoderContext): A = {
            // Check if it's a string (case object)
            val currentType = reader.getCurrentBsonType()
            val bsonType    = if (currentType == null) reader.readBsonType() else currentType

            if (bsonType == org.bson.BsonType.STRING) {
              // String value - match to case object by name
              val stringValue = reader.readString()
              caseNameToIndex.get(stringValue) match {
                case Some(idx) if isCaseObject(idx) =>
                  // Decode the case object using its codec
                  caseCodecs(idx).decoder
                    .fromBsonValueUnsafe(new org.bson.BsonDocument(), trace, ctx)
                    .asInstanceOf[A]
                case _ =>
                  throw BsonDecoder.Error(trace, s"Unknown case object name: $stringValue")
              }
            } else {
              // Try each case codec until one succeeds
              var idx                                  = 0
              var result: Option[A]                    = None
              var lastError: Option[BsonDecoder.Error] = None

              while (idx < caseCodecs.length && result.isEmpty) {
                if (!transientCases(idx) && !isCaseObject(idx)) {
                  val mark = reader.getMark()
                  try {
                    val decoded = caseCodecs(idx).decoder.decodeUnsafe(reader, trace, ctx)
                    result = Some(decoded.asInstanceOf[A])
                  } catch {
                    case e: BsonDecoder.Error =>
                      lastError = Some(e)
                      mark.reset()
                  }
                }
                idx += 1
              }

              result.getOrElse {
                throw lastError.getOrElse(
                  BsonDecoder.Error(trace, "Could not decode variant - no matching case found")
                )
              }
            }
          }

          def fromBsonValueUnsafe(value: BsonValue, trace: List[BsonTrace], ctx: BsonDecoder.BsonDecoderContext): A =
            // Check if it's a string (case object)
            if (value.getBsonType() == org.bson.BsonType.STRING) {
              val stringValue = value.asString().getValue()
              caseNameToIndex.get(stringValue) match {
                case Some(idx) if isCaseObject(idx) =>
                  // Decode the case object using its codec
                  caseCodecs(idx).decoder
                    .fromBsonValueUnsafe(new org.bson.BsonDocument(), trace, ctx)
                    .asInstanceOf[A]
                case _ =>
                  throw BsonDecoder.Error(trace, s"Unknown case object name: $stringValue")
              }
            } else {
              // Try each case codec until one succeeds
              var idx                                  = 0
              var result: Option[A]                    = None
              var lastError: Option[BsonDecoder.Error] = None

              while (idx < caseCodecs.length && result.isEmpty) {
                if (!transientCases(idx) && !isCaseObject(idx)) {
                  try {
                    val decoded = caseCodecs(idx).decoder.fromBsonValueUnsafe(value, trace, ctx)
                    result = Some(decoded.asInstanceOf[A])
                  } catch {
                    case e: BsonDecoder.Error =>
                      lastError = Some(e)
                  }
                }
                idx += 1
              }

              result.getOrElse {
                throw lastError.getOrElse(
                  BsonDecoder.Error(trace, "Could not decode variant - no matching case found")
                )
              }
            }
        }

        BsonCodec(encoder, decoder)
    }
  }

  // Wrapper (newtype) codec derivation
  override def deriveWrapper[F[_, _], A, B](
    wrapped: Reflect[F, B],
    typeId: TypeId[A],
    binding: Binding.Wrapper[A, B],
    doc: Doc,
    modifiers: Seq[Modifier.Reflect],
    defaultValue: Option[A],
    examples: Seq[A]
  )(implicit F: HasBinding[F], D: HasInstance[F]): Lazy[BsonCodec[A]] =
    if (!binding.isInstanceOf[Binding[?, ?]])
      binding.asInstanceOf[BindingInstance[BsonCodec, ?, A]].instance
    else if (typeId == TypeId.of[Json]) Lazy(deriveJsonWrapperCodec(binding))
    else {
      val isObjectId = typeId.name == "ObjectId" && typeId.owner.asString == "org.bson.types"
      if (isObjectId || (config.useNativeObjectId && isObjectId)) Lazy(BsonCodec.objectId.asInstanceOf[BsonCodec[A]])
      else {
        val wrapperBinding = binding.asInstanceOf[Binding.Wrapper[A, B]]
        D.instance(wrapped.metadata).map { wrappedCodec =>
          val encoder = new BsonEncoder[A] {
            def encode(writer: BsonWriter, value: A, ctx: BsonEncoder.EncoderContext): Unit =
              wrappedCodec.encoder.encode(writer, wrapperBinding.unwrap(value), ctx)

            def toBsonValue(value: A): BsonValue =
              wrappedCodec.encoder.toBsonValue(wrapperBinding.unwrap(value))
          }

          val decoder = new BsonDecoder[A] {
            def decodeUnsafe(reader: BsonReader, trace: List[BsonTrace], ctx: BsonDecoder.BsonDecoderContext): A = {
              val unwrapped = wrappedCodec.decoder.decodeUnsafe(reader, trace, ctx)
              try wrapperBinding.wrap(unwrapped)
              catch {
                case error: SchemaError => throw BsonDecoder.Error(trace, s"Failed to wrap value: ${error.message}")
              }
            }

            def fromBsonValueUnsafe(value: BsonValue, trace: List[BsonTrace], ctx: BsonDecoder.BsonDecoderContext): A = {
              val unwrapped = wrappedCodec.decoder.fromBsonValueUnsafe(value, trace, ctx)
              try wrapperBinding.wrap(unwrapped)
              catch {
                case error: SchemaError => throw BsonDecoder.Error(trace, s"Failed to wrap value: ${error.message}")
              }
            }
          }

          BsonCodec(encoder, decoder)
        }
      }
    }

  override def deriveDynamic[F[_, _]](
    binding: Binding.Dynamic,
    doc: Doc,
    modifiers: Seq[Modifier.Reflect],
    defaultValue: Option[DynamicValue],
    examples: Seq[DynamicValue]
  )(implicit F: HasBinding[F], D: HasInstance[F]): Lazy[BsonCodec[DynamicValue]] =
    if (binding.isInstanceOf[Binding[?, ?]]) Lazy(deriveDynamicCodec)
    else binding.asInstanceOf[BindingInstance[BsonCodec, ?, DynamicValue]].instance

  override def instanceOverrides: IndexedSeq[InstanceOverride] = {
    recursiveRecordCache.remove()
    super.instanceOverrides
  }

  private[this] val recursiveRecordCache =
    new ThreadLocal[java.util.HashMap[TypeId[?], Array[BsonCodec[Any]]]] {
      override def initialValue(): java.util.HashMap[TypeId[?], Array[BsonCodec[Any]]] =
        new java.util.HashMap[TypeId[?], Array[BsonCodec[Any]]]
    }
}
