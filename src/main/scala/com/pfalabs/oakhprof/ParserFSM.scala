package com.pfalabs.oakhprof

import scala.annotation.migration

import org.slf4j.LoggerFactory

import SegmentUtils.{ Segment, SegmentId, SegmentIdArray, SegmentIdArrayInfo, SegmentIdInfo, SegmentInfo, getSegmentIdArrayInstance, getSegmentIdInstance, getSegmentInstance, isInterestingType }
import States.{ Header, HeaderExtra, HeapClassDump, HeapInstanceDump, HeapNotImplemented, HeapObjArrayDump, HeapPrimArrayDump, HeapRecordTag, HeapSummaryTag, LoadClassTag, NotImplemented, RecordHeader, STRINGUTF8Tag, State, readId, readInt, readLong, readString }

class ParserFSM {

  val log = LoggerFactory.getLogger("Parser")

  val ctx: ParserFSMContext = new ParserFSMContext()

  def idSize = ctx.idSize

  // FSM
  // state 1: header (null-terminated sequence)
  // state 2: HeaderExtra [idSize (4) + start time (8)]
  // state 3: record(s)
  // state 4: read record header
  //
  // Transitions
  // 1 => 2
  // 2 => 3
  // 3 => 4
  // 4 => 3

  var state: State = Header()

  def process(b: Byte) = {
    state.process(b)
    if (state.done()) {
      state = transition(state)
    }
  }

  def transition(current: State): State = current match {
    case s @ Header() ⇒ {
      ctx.header = s.asString()
      HeaderExtra()
    }
    case s @ HeaderExtra() ⇒ {
      //id size + start time
      val buffer = s.buffer
      ctx.idSize = readInt(buffer, 0)
      ctx.startTime = readLong(buffer, 4)
      RecordHeader()
    }
    case s @ RecordHeader() ⇒ {
      //records loop: [tag + time + len + content]
      val buffer = s.buffer
      val tag = buffer(0)
      val time = readInt(buffer, 1)
      val length = readInt(buffer, 5)

      tag match {
        case 0x1 ⇒ {
          STRINGUTF8Tag(length)
        }
        case 0x2 ⇒ {
          LoadClassTag(length)
        }
        case 0x3 ⇒ {
          // TODO
          log.debug(s"[$tag] UNLOAD CLASS: not implemented.")
          NotImplemented(length)
        }
        case 0x4 ⇒ {
          // TODO 
          log.debug(s"[$tag] STACK FRAME: not implemented.")
          NotImplemented(length)
        }
        case 0x5 ⇒ {
          // TODO
          log.debug(s"[$tag] STACK TRACE: not implemented.")
          NotImplemented(length)
        }
        case 0x6 ⇒ {
          // TODO
          log.debug(s"[$tag] ALLOC SITES: not implemented.")
          NotImplemented(length)
        }
        case 0x7 ⇒ {
          HeapSummaryTag()
        }
        case 0xa ⇒ {
          // TODO
          log.debug(s"[$tag] START THREAD: not implemented.")
          NotImplemented(length)
        }
        case 0xb ⇒ {
          // TODO
          log.debug(s"[$tag] END THREAD")
          NotImplemented(length)
        }
        case 0xc ⇒ {
          log.debug(s"HEAP DUMP: $length.")
          HeapRecordTag(length)
        }
        case 0x1c ⇒ {
          // TODO
          log.warn(s"[$tag] HEAP DUMP SEGMENT: not implemented($length).")
          NotImplemented(length)
        }
        case 0x2c ⇒ {
          log.warn(s"[$tag] HEAP DUMP END")
          NotImplemented(length)
        }
        case 0xd ⇒ {
          // TODO
          log.debug(s"[$tag] CPU SAMPLES: not implemented.")
          NotImplemented(length)
        }
        case 0xe ⇒ {
          // TODO
          log.debug(s"[$tag] CONTROL SETTINGS: not implemented.")
          NotImplemented(length)
        }
        case _ ⇒ {
          log.warn(s"[$tag] Unexpected entry ($length)!")
          NotImplemented(length)
        }
      }
    }
    case NotImplemented(_) ⇒ {
      RecordHeader()
    }

    case s @ STRINGUTF8Tag(_) ⇒ {
      val buffer = s.buffer
      val id = readId(buffer, 0, idSize)
      val str = readString(s.buffer.slice(idSize, buffer.length))
      ctx.strings.put(id, str)
      RecordHeader()
    }

    case s @ LoadClassTag(_) ⇒ {
      val buffer = s.buffer
      val classId = readId(buffer, 4, idSize)
      val classNameId = readId(buffer, 8 + idSize, idSize)
      val name = ctx.strings.get(classNameId).getOrElse("unknown").replace('/', '.')
      ctx.classNames.put(classId, name)
      RecordHeader()
    }

    case s @ HeapSummaryTag(_) ⇒ {
      val buffer = s.buffer
      val liveBytes = readInt(buffer, 0)
      val liveInst = readInt(buffer, 4)
      val allocatedBytes = readLong(buffer, 8)
      val allocInst = readLong(buffer, 16)
      ctx.summary = s"liveBytes: $liveBytes / liveInst: $liveInst / allocatedBytes: $allocatedBytes / allocInst: $allocInst"
      RecordHeader()
    }

    // heap dump stuff
    case s @ HeapRecordTag(remaining, _) ⇒ {
      val tag = s.buffer(0)
      tag match {
        case 0xff ⇒ {
          // TODO ROOT UNKNOWN
          val len = idSize
          log.debug(s"[heap] ROOT UNKNOWN: not implemented ($len / $remaining).")
          HeapNotImplemented(len, remaining - 1)
        }
        case 0x1 ⇒ {
          // TODO ROOT JNI GLOBAL
          val len = 2 * idSize
          log.debug(s"[heap] ROOT JNI GLOBAL: not implemented ($len / $remaining).")
          HeapNotImplemented(len, remaining - 1)
        }
        case 0x2 ⇒ {
          // TODO ROOT JNI LOCAL
          val len = idSize + 8
          log.debug(s"[heap] ROOT JNI LOCAL: not implemented ($len / $remaining).")
          HeapNotImplemented(len, remaining - 1)
        }
        case 0x3 ⇒ {
          // TODO ROOT JAVA FRAME
          val len = idSize + 8
          log.debug(s"[heap] ROOT JAVA FRAME: not implemented ($len / $remaining).")
          HeapNotImplemented(len, remaining - 1)
        }
        case 0x4 ⇒ {
          // TODO ROOT NATIVE STACK
          val len = idSize + 4
          log.debug(s"[heap] ROOT NATIVE STACK: not implemented ($len / $remaining).")
          HeapNotImplemented(len, remaining - 1)
        }
        case 0x5 ⇒ {
          // TODO ROOT STICKY CLASS
          val len = idSize
          log.debug(s"[heap] ROOT STICKY CLASS: not implemented ($len / $remaining).")
          HeapNotImplemented(len, remaining - 1)
        }
        case 0x6 ⇒ {
          // TODO ROOT THREAD BLOCK
          val len = idSize + 4
          log.debug(s"[heap] ROOT THREAD BLOCK: not implemented ($len / $remaining).")
          HeapNotImplemented(len, remaining - 1)
        }
        case 0x7 ⇒ {
          // TODO ROOT MONITOR USED
          val len = idSize
          log.debug(s"[heap] ROOT MONITOR USED: not implemented ($len / $remaining).")
          HeapNotImplemented(len, remaining - 1)
        }
        case 0x8 ⇒ {
          // TODO ROOT THREAD OBJECT
          val len = idSize + 8
          log.debug(s"[heap] ROOT THREAD OBJECT: not implemented ($len / $remaining).")
          HeapNotImplemented(len, remaining - 1)
        }
        case 0x20 ⇒ {
          // CLASS DUMP
          HeapClassDump(remaining - 1, idSize)
        }
        case 0x21 ⇒ {
          // INSTANCE DUMP
          HeapInstanceDump(remaining - 1, ctx.processedClassIds.values.toList, idSize)
        }
        case 0x22 ⇒ {
          // OBJECT ARRAY DUMP
          HeapObjArrayDump(remaining - 1, ctx.processedClassIds.values.toList, idSize)
        }
        case 0x23 ⇒ {
          // PRIMITIVE ARRAY DUMP
          HeapPrimArrayDump(remaining - 1, idSize)
        }
      }
    }

    case s @ HeapClassDump(r, _) ⇒ {
      val len = s.totalRead
      val remaining = r - len
      val cid = s.classObjectId
      isInterestingType(s.classObjectId, ctx.classNames) match {
        case Some(SegmentId) ⇒ {
          ctx.processedClassIds.put(SegmentId, cid)
          ctx.classes.put(cid, new SegmentIdInfo(s.fields))
        }
        case Some(SegmentIdArray) ⇒ {
          ctx.processedClassIds.put(SegmentIdArray, cid)
          ctx.classes.put(cid, new SegmentIdArrayInfo(s.fields))
        }
        case Some(Segment) ⇒ {
          ctx.processedClassIds.put(Segment, cid)
          ctx.classes.put(cid, new SegmentInfo(s.fields))
        }
        case None ⇒ {}
      }

      if (remaining > 0) {
        HeapRecordTag(remaining)
      } else {
        RecordHeader()
      }
    }

    case s @ HeapInstanceDump(r, _, _) ⇒ {
      val len = s.totalRead
      val remaining = r - len
      val cid = s.classObjectId

      if (s.processed) {
        // WTF moment: JVisualVm counts 8 bytes less / instance ?
        val existing = ctx.instanceStats.get(cid).getOrElse((0l, 0l))
        ctx.instanceStats.put(cid, (existing._1 + 1, existing._2 + len))

        ctx.classes.get(cid) match {
          case Some(SegmentIdInfo(f)) ⇒ {
            val sidi = getSegmentIdInstance(s.body, f, ctx.strings, idSize)
            ctx.instances.put(s.objectId, sidi)
          }
          case Some(SegmentInfo(f)) ⇒ {
            val sidi = getSegmentInstance(s.body, f, ctx.strings, idSize)
            ctx.instances.put(s.objectId, sidi)
          }
          case x @ _ ⇒ { log.warn("Unexpected value " + x) }
        }
      }

      if (remaining > 0) {
        HeapRecordTag(remaining)
      } else {
        RecordHeader()
      }
    }

    case s @ HeapObjArrayDump(r, _, _) ⇒ {
      val len = s.totalRead
      val remaining = r - len
      val cid = s.classObjectId

      if (s.processed) {
        val existing = ctx.instanceStats.get(cid).getOrElse((0l, 0l))
        ctx.instanceStats.put(cid, (existing._1 + 1, existing._2 + len))
        ctx.classes.get(cid) match {
          case Some(SegmentIdArrayInfo(f)) ⇒ {
            val sidi = getSegmentIdArrayInstance(s.refs)
            ctx.instances.put(s.objectId, sidi)
          }
          case x @ _ ⇒ { log.warn("Unexpected value " + x) }
        }
      }

      if (remaining > 0) {
        HeapRecordTag(remaining)
      } else {
        RecordHeader()
      }
    }

    case s @ HeapPrimArrayDump(r, _) ⇒ {
      val len = s.totalRead
      val remaining = r - len
      if (remaining > 0) {
        HeapRecordTag(remaining)
      } else {
        RecordHeader()
      }
    }

    case h @ HeapNotImplemented(b, r) ⇒ {
      val remaining = r - b
      if (remaining > 0) {
        HeapRecordTag(remaining)
      } else {
        RecordHeader()
      }
    }
  }
}