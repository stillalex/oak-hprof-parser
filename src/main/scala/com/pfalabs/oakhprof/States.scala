package com.pfalabs.oakhprof

import scala.collection.mutable.{ ArrayBuffer, Map }

import com.google.common.base.Preconditions.checkArgument
import com.google.common.io.ByteStreams

object States {

  sealed trait State {
    def done(): Boolean
    def process(b: Byte)
  }

  case class Header() extends State {
    var buffer: ArrayBuffer[Byte] = ArrayBuffer[Byte]()
    var done = false
    override def process(b: Byte) {
      if (b == 0) {
        done = true
      } else {
        buffer += b
      }
    }
    def asString(): String = readString(buffer)
  }

  case class HeaderExtra() extends State with ByteReaderHelper {
    val len = 12
  }

  case class RecordHeader() extends State with ByteReaderHelper {
    val len = 9
  }

  case class HeapRecordTag(remaining: Int, len: Int = 1) extends State with ByteReaderHelper {
    checkArgument(remaining > 0)
  }

  trait ByteReaderHelper {

    def len(): Int

    var done = false
    var i = 0
    var buffer: ArrayBuffer[Byte] = ArrayBuffer[Byte]()

    def process(b: Byte) {
      buffer += b
      i += 1
      done = len == i
    }
  }

  case class STRINGUTF8Tag(len: Int) extends State with ByteReaderHelper {}

  case class LoadClassTag(len: Int) extends State with ByteReaderHelper {}

  case class HeapSummaryTag(len: Int = 24) extends State with ByteReaderHelper {}

  case class NotImplemented(len: Int) extends State with ByteSkipperHelper {}

  case class HeapNotImplemented(len: Int, remaining: Int) extends State with ByteSkipperHelper {}

  trait ByteSkipperHelper {

    def len(): Int

    var done = false
    var i = 0

    def process(b: Byte) {
      i += 1
      done = len == i
    }
  }

  case class HeapClassDump(remaining: Int, idSize: Int) extends State {

    var done = false
    var i = 0
    var buffer: ArrayBuffer[Byte] = ArrayBuffer[Byte]()
    var totalRead = 0

    var state = 0
    var processWith: Byte ⇒ Boolean = processHeader

    var classObjectId: Long = -1

    var constants: Short = 0
    var statics: Short = 0
    var fieldNo: Short = 0
    // (nameId, type)
    var fields: List[(Long, Byte)] = List()

    def process(b: Byte) = if (!processWith(b)) {
      state = transition(state)
    }

    def reset() {
      totalRead += i
      i = 0
      buffer = ArrayBuffer[Byte]()
    }

    def transition(current: Int): Int = current match {
      case 0 ⇒ {
        classObjectId = readId(buffer, 0, idSize)
        reset()
        processWith = processConsNo
        1
      }
      case 1 ⇒ {
        constants = readShort(buffer, 0)
        reset()
        if (constants > 0) {
          processWith = processConsEntries(constants)
          2
        } else {
          processWith = processStatNo
          3
        }

      }
      case 2 ⇒ {
        reset()
        processWith = processStatNo
        3
      }
      case 3 ⇒ {
        statics = readShort(buffer, 0)
        reset()
        if (statics > 0) {
          processWith = processStatEntries(statics)
          4
        } else {
          processWith = processFieldsNo
          5
        }
      }
      case 4 ⇒ {
        reset()
        processWith = processFieldsNo
        5
      }
      case 5 ⇒ {
        fieldNo = readShort(buffer, 0)
        reset()
        if (fieldNo > 0) {
          processWith = processFieldsEntries(fieldNo)
          6
        } else {
          done = true
          -1
        }
      }
      case 6 ⇒ {
        fields = fields.reverse
        reset()
        done = true
        -1
      }
    }

    def processHeader(b: Byte): Boolean = {
      buffer += b
      i += 1
      val stop = i == 7 * idSize + 8
      return !stop
    }

    def processConsNo(b: Byte): Boolean = {
      buffer += b
      i += 1
      return i != 2
    }

    def processConsEntries(constants: Int)(b: Byte): Boolean = {
      checkArgument(constants > 0)
      buffer += b
      i += 1
      if (i == 3) {
        // c[i]: constant pool index & type
        val ctype = buffer(buffer.length - 1)
        val size = typeToSize(ctype, idSize)
        reset()
        processWith = processEntryValue(constants, size, processConsEntries(constants - 1))
      }
      return true // constants - 1 != 0
    }

    def processStatNo(b: Byte): Boolean = {
      buffer += b
      i += 1
      return i != 2
    }

    def processStatEntries(statics: Int)(b: Byte): Boolean = {
      checkArgument(statics > 0)
      buffer += b
      i += 1
      if (i == idSize + 1) {
        // s[i]: ID & type
        val ctype = buffer(buffer.length - 1)
        val size = typeToSize(ctype, idSize)
        reset()
        processWith = processEntryValue(statics, size, processStatEntries(statics - 1))
      }
      return true // statics - 1 != 0
    }

    def processEntryValue(count: Int, len: Int, next: Byte ⇒ Boolean)(b: Byte): Boolean = {
      buffer += b
      i += 1
      if (i == len) {
        reset()
        processWith = next
        return count - 1 != 0
      }
      true
    }

    def processFieldsNo(b: Byte): Boolean = {
      buffer += b
      i += 1
      return i != 2
    }

    def processFieldsEntries(fieldNo: Int)(b: Byte): Boolean = {
      checkArgument(fieldNo > 0)
      buffer += b
      i += 1
      if (i == idSize + 1) {
        // f[i]: nameID & type
        val nameId = readId(buffer, 0, idSize)
        val ctype = buffer(buffer.length - 1)
        val size = typeToSize(ctype, idSize)
        fields = (nameId, ctype) :: fields
        reset()
        processWith = processFieldsEntries(fieldNo - 1)
        return fieldNo - 1 != 0
      }
      return true
    }
  }

  case class HeapInstanceDump(remaining: Int, filterClassIds: List[Long], idSize: Int) extends State {
    checkArgument(!filterClassIds.isEmpty)
    var done = false
    var i = 0
    var buffer: ArrayBuffer[Byte] = ArrayBuffer[Byte]()
    var totalRead = 0

    var state = 0
    var processWith: Byte ⇒ Boolean = processHeader

    var processed = false
    var bytes = 0
    var objectId: Long = 0
    var classObjectId: Long = 0
    var body: ArrayBuffer[Byte] = ArrayBuffer[Byte]()

    def process(b: Byte) = if (!processWith(b)) {
      state = transition(state)
    }

    def reset() {
      totalRead += i
      i = 0
      buffer = ArrayBuffer[Byte]()
    }

    def transition(current: Int): Int = current match {
      case 0 ⇒ {
        objectId = readId(buffer, 0, idSize)
        classObjectId = readId(buffer, idSize + 4, idSize)
        bytes = readInt(buffer, 2 * idSize + 4)
        processed = filterClassIds.contains(classObjectId)

        if (bytes > 0) {
          reset()
          processWith = processBody(bytes)
          1
        } else {
          reset()
          done = true
          -1
        }
      }
      case 1 ⇒ {
        if (processed) {
          body ++= buffer
        }
        reset()
        done = true
        -1
      }
    }

    def processHeader(b: Byte): Boolean = {
      buffer += b
      i += 1
      val stop = i == 2 * idSize + 8
      return !stop
    }

    def processBody(len: Int)(b: Byte): Boolean = {
      buffer += b
      i += 1
      if (i == len) {
        return false
      }
      true
    }

  }

  case class HeapObjArrayDump(remaining: Int, filterClassIds: List[Long], idSize: Int) extends State {
    var done = false
    var i = 0
    var buffer: ArrayBuffer[Byte] = ArrayBuffer[Byte]()
    var totalRead = 0

    var state = 0
    var processWith: Byte ⇒ Boolean = processHeader

    //TODO
    var processed = false
    var objectId: Long = 0
    var classObjectId: Long = 0
    var count = 0
    var refs: List[Long] = List()

    def process(b: Byte) = if (!processWith(b)) {
      state = transition(state)
    }

    def reset() {
      totalRead += i
      i = 0
      buffer = ArrayBuffer[Byte]()
    }

    def transition(current: Int): Int = current match {
      case 0 ⇒ {
        objectId = readId(buffer, 0, idSize)
        count = readInt(buffer, idSize + 4)
        classObjectId = readId(buffer, idSize + 8, idSize)
        processed = filterClassIds.contains(classObjectId)

        if (count > 0) {
          reset()
          processWith = processEntryValue(count, idSize)
          1
        } else {
          reset()
          done = true
          -1
        }
      }
      case 1 ⇒ {
        reset()
        if (processed) {
          checkArgument(refs.size == count)
        }
        done = true
        -1
      }
    }

    def processHeader(b: Byte): Boolean = {
      buffer += b
      i += 1
      val stop = i == 2 * idSize + 8
      return !stop
    }

    def processEntryValue(count: Int, len: Int)(b: Byte): Boolean = {
      buffer += b
      i += 1
      if (i == len) {
        if (processed) {
          val ref = readId(buffer, 0, idSize)
          refs = ref :: refs
        }
        reset()
        processWith = processEntryValue(count - 1, len)
        return count - 1 != 0
      }
      true
    }

  }

  case class HeapPrimArrayDump(remaining: Int, idSize: Int) extends State {

    var done = false
    var i = 0
    var buffer: ArrayBuffer[Byte] = ArrayBuffer[Byte]()
    var totalRead = 0

    var state = 0
    var processWith: Byte ⇒ Boolean = processHeader

    var count = 0

    def process(b: Byte) = if (!processWith(b)) {
      state = transition(state)
    }

    def reset() {
      totalRead += i
      i = 0
      buffer = ArrayBuffer[Byte]()
    }

    def transition(current: Int): Int = current match {
      case 0 ⇒ {
        val id = readId(buffer, 0, idSize)
        count = readInt(buffer, idSize + 4)
        val atype = buffer(buffer.length - 1)
        val len = typeToSize(atype, idSize);

        if (count > 0) {
          reset()
          processWith = processEntryValue(count, len)
          1
        } else {
          reset()
          done = true
          -1
        }
      }
      case 1 ⇒ {
        reset()
        done = true
        -1
      }
    }

    def processHeader(b: Byte): Boolean = {
      buffer += b
      i += 1
      val stop = i == idSize + 9
      return !stop
    }

    def processEntryValue(count: Int, len: Int)(b: Byte): Boolean = {
      buffer += b
      i += 1
      if (i == len) {
        reset()
        processWith = processEntryValue(count - 1, len)
        return count - 1 != 0
      }
      true
    }

  }

  def readInt(buffer: ArrayBuffer[Byte], index: Int): Int =
    ByteStreams.newDataInput(buffer.slice(index, index + 4).toArray).readInt()

  def readLong(buffer: ArrayBuffer[Byte], index: Int): Long =
    ByteStreams.newDataInput(buffer.slice(index, index + 8).toArray).readLong()

  def readId(buffer: ArrayBuffer[Byte], index: Int, idSize: Int): Long = {
    if (idSize == 4) {
      readInt(buffer, index)
    } else {
      readLong(buffer, index)
    }
  }

  def readShort(buffer: ArrayBuffer[Byte], index: Int): Short =
    ByteStreams.newDataInput(buffer.slice(index, index + 2).toArray).readShort()

  def readString(buffer: ArrayBuffer[Byte]): String = new String(buffer.toArray)

  def isIdType(ctype: Byte) = ctype == 2

  def typeToSize(ctype: Byte, idSize: Int): Int =
    ctype match {
      case 2 ⇒ {
        // object
        idSize
      }
      case 4 ⇒ {
        // boolean
        1
      }
      case 5 ⇒ {
        // char
        2
      }
      case 6 ⇒ {
        // float
        4
      }
      case 7 ⇒ {
        // double
        8
      }
      case 8 ⇒ {
        // byte
        1
      }
      case 9 ⇒ {
        // short
        2
      }
      case 10 ⇒ {
        // int
        4
      }
      case 11 ⇒ {
        // long
        8
      }
    }

  def typeToString(ctype: Byte): String =
    ctype match {
      case 2 ⇒ {
        "object"
      }
      case 4 ⇒ {
        "boolean"
      }
      case 5 ⇒ {
        "char"
      }
      case 6 ⇒ {
        "float"
      }
      case 7 ⇒ {
        "double"
      }
      case 8 ⇒ {
        "byte"
      }
      case 9 ⇒ {
        "short"
      }
      case 10 ⇒ {
        "int"
      }
      case 11 ⇒ {
        "long"
      }
    }

  def valToString(ctype: Byte, buffer: ArrayBuffer[Byte], index: Int, idSize: Int): String =
    ctype match {
      case 2 ⇒ {
        // object
        readId(buffer, index, idSize).toString
      }
      case 4 ⇒ {
        // TODO boolean
        ""
      }
      case 5 ⇒ {
        // TODO char
        ""
      }
      case 6 ⇒ {
        // TODO float
        ""
      }
      case 7 ⇒ {
        // TODO double
        ""
      }
      case 8 ⇒ {
        // byte
        buffer(index).toString()
      }
      case 9 ⇒ {
        // short
        readShort(buffer, index).toString()
      }
      case 10 ⇒ {
        // int
        readInt(buffer, index).toString
      }
      case 11 ⇒ {
        // long
        readLong(buffer, index).toString
      }
    }

  def loadValues(buffer: ArrayBuffer[Byte], fields: List[(Long, Byte)], strings: Map[Long, String], idSize: Int): Map[String, (Boolean, String)] = {
    var index = 0
    var values = Map[String, (Boolean, String)]()
    fields.foreach {
      case (id, ftype) ⇒ {
        var size = typeToSize(ftype, idSize)
        val k = strings.get(id).getOrElse("?" + id)
        val v1 = isIdType(ftype)
        val v2 = valToString(ftype, buffer, index, idSize)
        values += (k -> (v1, v2))
        index += size
      }
    }
    values
  }
}