package com.pfalabs.oakhprof

import java.util.{ Calendar, Date, UUID }

import scala.collection.mutable.Map

import org.slf4j.LoggerFactory

import com.pfalabs.oakhprof.SegmentUtils.{ ClassInstance, SegmentIdArrayInstance, SegmentIdInstance, SegmentInstance }

import SegmentGraph.{ doEpoch, getReferences, isDataSegmentId }

class SegmentGraph(val ctx: ParserFSMContext) {

  val log = LoggerFactory.getLogger("Parser")
  val export = LoggerFactory.getLogger("export")

  def generate() {
    log.info("Generating graph")
    val epoch = doEpoch(ctx.startTime).getTime

    export.info("nodedef>name VARCHAR, type VARCHAR, gc INT, t INT");
    ctx.instances.foreach {
      case (id, ci) ⇒
        ci match {
          case s @ SegmentIdInstance(_, _, _, _) ⇒ {
            exportSegmentId(s, epoch)
          }
          case _ ⇒ {}
        }
    }
    log.info("finished nodes, edges next.")

    export.info("edgedef>node1 VARCHAR, node2 VARCHAR")
    val uuidWithRefs = ctx.instances.map(f ⇒ {
      f match {
        case (id, SegmentIdInstance(uuid, segment, _, _)) if (isDataSegmentId(uuid) && segment.isDefined) ⇒ {
          Some((uuid, segment.get))
        }
        case _ ⇒ { None }
      }
    })
      .flatten
      .map(f ⇒ getReferences(f, ctx.instances))

    uuidWithRefs.foreach {
      case (uuid, refs) ⇒ {
        refs.foreach { x ⇒ export.info(s"$uuid,$x"); }
      }
    }

    log.info("finished edges, done.")
  }

  def exportSegmentId(info: SegmentIdInstance, epoch: Long) {
    val t = info.creationTime
    val ts = t - epoch
    if (isDataSegmentId(info.uuid)) {
      export.info(info.uuid + ",data," + info.generation + "," + ts)
    } else {
      export.info(info.uuid + ",bulk," + info.generation + "," + ts)
    }
  }

}
object SegmentGraph {

  def doEpoch(ms: Long): Date = {
    val c = Calendar.getInstance
    c.setTimeInMillis(ms)
    c.set(Calendar.HOUR_OF_DAY, 0)
    c.set(Calendar.MINUTE, 0)
    c.set(Calendar.SECOND, 0)
    c.set(Calendar.MILLISECOND, 0)
    c.getTime
  }

  def isDataSegmentId(uuid: UUID): Boolean = {
    return (uuid.getLeastSignificantBits >>> 60) == 0xAL;
  }

  def getReferences(in: (UUID, Long), instances: Map[Long, ClassInstance]): (UUID, List[UUID]) = {
    val refs: List[UUID] = instances.get(in._2) match {
      case Some(SegmentInstance(refid)) ⇒ {
        instances.get(refid) match {
          case Some(SegmentIdArrayInstance(refids)) ⇒ {
            refids.map { i ⇒
              instances.get(i) match {
                case Some(SegmentIdInstance(uuid, _, _, _)) if !uuid.equals(in._1) ⇒ { Some(uuid) }
                case _ ⇒ { None }
              }
            }.flatten
          }
          case _ ⇒ { List() }
        }
      }
      case _ ⇒ { List() }
    }
    (in._1, refs)
  }

}