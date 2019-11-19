package com.github.nhweston.dfscalc

import com.github.tototoshi.csv.{CSVReader, CSVWriter}
import com.github.nhweston.mcknapsack.Category.ElemCategory
import com.github.nhweston.mcknapsack.Selectable.Element

object Selectables {

    def fromFile (filePath: String, positions: Map[String, Int]) (implicit writer: CSVWriter) : Seq[ElemCategory] = {
        CSVReader.open (filePath) .allWithHeaders.map { row =>
            Element (
                row ("name"),
                BigDecimal (row ("value")),
                BigDecimal (row ("cost")),
                row ("position")
            )
        } .toVector.groupBy (_.category) .toSeq.map {
            case (label, elems) => ElemCategory (label, elems, positions (label))
        }
    }

}
