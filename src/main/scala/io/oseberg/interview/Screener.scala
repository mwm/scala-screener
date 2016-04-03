package io.oseberg.interview

import scala.io._

object Screener extends App {
  val src = Source.fromFile("./src/main/resources/DoI.txt")
  val name = "DoI.txt" // Should extract this from input name...
  val data = new FileStats(src)

  println(s"""Stats for file: $name:
  Line Count: ${data.lineCount}
  Total Word Count: ${data.totalWordCount}
  Count for 'People': ${data.wordCount("people")}
  Top Three Words by Freq: ${data.orderedWords(0)}, ${data.orderedWords(1)}, ${data.orderedWords(2)}""")
}

class FileStats(source: BufferedSource) {
  type Word = String

  def lineCount: Int = _lineCount
  def totalWordCount: Int = _totalWordCount
  def wordCount(word: Word): Int = _map(word)

  // this should be implemented recursively.
  def wordFreq(src: Source, freq: Map[String, Int]): Map[String, Int] = {

    // This should also use foldLeft, but makes the recursion explicit.
    def countLine(data: (Map[String, Int], Int, Int), line: String) 
    : (Map[String, Int], Int, Int) = {
  
      val words = line.split("\\s+").map(prepWord).filter(_.length > 0)
      val newFreq = updateMap(words, data._1)
      return (newFreq, data._2 + 1, data._3 + words.length)
    }

    // And here's the recursive implementation asked for in the comment
    def updateMap(ws: Array[String], freq: Map[String, Int]) : Map[String, Int] = {
      if (ws.length == 0) return freq
      else {
        val word = ws.head
        return updateMap(ws.tail, 
                         freq updated (word, 1 + (freq getOrElse (word, 0))))
        }
    }

    val (newFreq, lc, twc): (Map[String, Int], Int, Int) 
      = src.getLines.foldLeft((freq, 0, 0))(countLine(_, _))
    _lineCount = lc
    _totalWordCount = twc
    return newFreq
  }

  // Implements definition of "word" as continuous string of alphanumerics plus
  // abbreviation "&" for "and"
  def prepWord(word: Word): Word =
    word.toLowerCase()
    .replaceFirst("\\A&\\Z", "and")
    .replaceFirst("\\A[^a-z0-9]", "")
    .replaceFirst("[^a-z0-9]\\Z", "")

  // Only compute the sorted list if we're asked for it. Just because I can.
  def orderedWords: List[String] = {
    _orderedWords match {
      case Some(words) => words
      case None => {
        val words =
          _map.keys.map(w => (_map(w), w)).toList.sortWith(_._1 > _._1).map(_._2)
        _orderedWords = Some(words)
        return words
      }
    }
  }

  // Initialize privates.
  private var _orderedWords: Option[List[String]] = None
  private var _lineCount = 0
  private var _totalWordCount = 0
  private val _map = wordFreq(source, Map())
}

