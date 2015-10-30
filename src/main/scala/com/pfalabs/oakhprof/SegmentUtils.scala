package com.pfalabs.oakhprof

import java.util.UUID

import scala.collection.mutable.{ ArrayBuffer, Map }

import States.loadValues

object SegmentUtils {

  sealed trait ClassType
  case object SegmentId extends ClassType
  case object SegmentIdArray extends ClassType
  case object Segment extends ClassType

  sealed trait ClassInfo
  // (nameId, type)
  case class SegmentIdInfo(fields: List[(Long, Byte)]) extends ClassInfo
  case class SegmentIdArrayInfo(fields: List[(Long, Byte)]) extends ClassInfo
  case class SegmentInfo(fields: List[(Long, Byte)]) extends ClassInfo

  sealed trait ClassInstance
  case class SegmentIdInstance(uuid: UUID, segmentRef: Option[Long], creationTime: Long, generation: Int) extends ClassInstance
  case class SegmentIdArrayInstance(refs: List[Long]) extends ClassInstance
  case class SegmentInstance(refids: Long) extends ClassInstance

  def isInterestingType(classId: Long, classNames: Map[Long, String]): Option[ClassType] = {
    if (isSegmentIdType(classId, classNames)) {
      Some(SegmentId)
    } else if (isSegmentIdArrayType(classId, classNames)) {
      Some(SegmentIdArray)
    } else if (isSegmentType(classId, classNames)) {
      Some(Segment)
    } else {
      None
    }
  }

  private def isSegmentIdType(classId: Long, classNames: Map[Long, String]): Boolean = {
    return classNames.getOrElse(classId, "").equals("org.apache.jackrabbit.oak.plugins.segment.SegmentId")
  }

  private def isSegmentIdArrayType(classId: Long, classNames: Map[Long, String]): Boolean = {
    return classNames.getOrElse(classId, "").equals("[Lorg.apache.jackrabbit.oak.plugins.segment.SegmentId;")
  }

  private def isSegmentType(classId: Long, classNames: Map[Long, String]): Boolean = {
    return classNames.getOrElse(classId, "").equals("org.apache.jackrabbit.oak.plugins.segment.Segment")
  }

  def getSegmentIdInstance(buffer: ArrayBuffer[Byte], fields: List[(Long, Byte)], strings: Map[Long, String], idSize: Int): SegmentIdInstance = {
    var values: Map[String, (Boolean, String)] = loadValues(buffer, fields, strings, idSize)
    val msb = values.get("msb").get._2.toLong
    val lsb = values.get("lsb").get._2.toLong
    val uuid = new UUID(msb, lsb)
    val creationTime = values.get("creationTime").get._2.toLong
    val generation = values.get("generation").get._2.toInt
    val segmentRef: Option[Long] = values.get("segment").get._2.toLong match {
      case 0     ⇒ None
      case r @ _ ⇒ Some(r)
    }
    SegmentIdInstance(uuid, segmentRef, creationTime, generation)
  }

  def getSegmentIdArrayInstance(refs: List[Long]): SegmentIdArrayInstance = {
    SegmentIdArrayInstance(refs)
  }

  def getSegmentInstance(buffer: ArrayBuffer[Byte], fields: List[(Long, Byte)], strings: Map[Long, String], idSize: Int): SegmentInstance = {
    var values: Map[String, (Boolean, String)] = loadValues(buffer, fields, strings, idSize)
    val refids = values.get("refids").get._2.toLong
    SegmentInstance(refids)
  }
}