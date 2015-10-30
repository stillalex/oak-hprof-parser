package com.pfalabs.oakhprof

import scala.collection.mutable.Map

import SegmentUtils.{ ClassInfo, ClassInstance, ClassType }

class ParserFSMContext {

  var header: String = ""
  var idSize = 0
  var startTime = 0l
  var summary: String = ""

  // all strings, field name resolution mainly
  var strings: Map[Long, String] = Map()

  // all class names, used to determine 'processedClassIds' based on the class names
  var classNames: Map[Long, String] = Map()

  //[key -> classId] determines what is kept for later processing
  var processedClassIds: Map[ClassType, Long] = Map()

  //[classId -> ClassInfo], contains only 'processedClassIds'
  var classes: Map[Long, ClassInfo] = Map()

  //[classId -> (count, bytes)], contains only 'processedClassIds'
  var instanceStats: Map[Long, (Long, Long)] = Map()

  //[objectId -> ClassInstance], contains only 'processedClassIds'
  var instances: Map[Long, ClassInstance] = Map()

}
