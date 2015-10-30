package com.pfalabs.oakhprof

import java.io.File
import java.util.Date

import org.slf4j.LoggerFactory

import com.google.common.io.{ ByteProcessor, Files }

object Parser {

  val log = LoggerFactory.getLogger("Parser")

  def parse(h: String) {
    log.info(s"Parsing $h")
    val r = Files.readBytes(new File(h), new MyBP())
    log.info("Header " + r.ctx.header)
    log.info("Start Time " + new Date(r.ctx.startTime))
    if (!r.ctx.summary.isEmpty()) {
      log.info("Summary " + r.ctx.summary)
    }

    for {
      cid ← r.ctx.processedClassIds
      stat ← r.ctx.instanceStats.get(cid._2)
    } {
      log.info(cid._1 + " x " + stat._1 + " times, " + stat._2 + " bytes.")
    }

    r.generate()
  }

  class MyBP extends ByteProcessor[SegmentGraph] {

    val sm = new ParserFSM()

    override def processBytes(buf: Array[Byte], off: Int, len: Int): Boolean = {
      buf.foreach(sm.process)
      true
    }

    override def getResult(): SegmentGraph = {
      new SegmentGraph(sm.ctx)
    }
  }
}