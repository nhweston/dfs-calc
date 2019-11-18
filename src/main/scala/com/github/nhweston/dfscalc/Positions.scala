package com.github.nhweston.dfscalc

import com.github.tototoshi.csv.CSVReader

object Positions {

    def fromFile (filePath: String) : Map[String, Int] = {
        CSVReader.open (filePath) .all.map {
            case pos :: num :: Nil => pos -> num.toInt
            case x => throw new MatchError (x)
        } .toMap
    }

}
