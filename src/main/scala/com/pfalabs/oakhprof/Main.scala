package com.pfalabs.oakhprof

import Parser.parse

object Main {

  val usage = """
    Usage: java -jar *.jar heap.bin
  """

  def main(args: Array[String]): Unit =
    if (args.length == 0) {
      println(usage)
    } else {
      parse(args(0))
    }
}