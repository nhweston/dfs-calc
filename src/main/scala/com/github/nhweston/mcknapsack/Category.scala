package com.github.nhweston.mcknapsack

import com.github.nhweston.mcknapsack.Category.{ElemCategory, MetaCategory}
import com.github.nhweston.mcknapsack.Selectable.Combination
import com.github.tototoshi.csv.CSVWriter

sealed abstract class Category (val selectables: Seq[Selectable], val numToSelect: Int)
(implicit val writer: CSVWriter) {

    lazy val labels: Seq[String] = {
        this match {
            case ElemCategory (label, _, _) => Seq (label)
            case MetaCategory (categories, _) => categories.map (_.label)
        }
    }

    lazy val sorted: Seq[Selectable] = selectables.sortBy (s => (s.cost, -s.value))

    lazy val culled: Seq[Selectable] = {
        val builder = Seq.newBuilder[Selectable]
        var valuesMax = Seq.empty[BigDecimal]
        @inline
        def insert (selectable: Selectable) : Unit = {
            builder += selectable
            val idx = {
                val idx = valuesMax.indexWhere (_ <= selectable.value)
                if (idx < 0) valuesMax.size
                else idx
            }
            val (pre, post) = {
                if (valuesMax.size < numToSelect) valuesMax
                else valuesMax dropRight 1
            } .splitAt (idx)
            valuesMax = (pre :+ selectable.value) ++ post
        }
        @inline
        def isAdmissible (value: BigDecimal) : Boolean = valuesMax.size < numToSelect || value > valuesMax.last
        writer.writeRow (labels ++ Seq ("value", "cost", "decision"))
        for (selectable <- sorted) {
            val row = selectable.labels ++ Seq (selectable.value, selectable.cost)
            if (isAdmissible (selectable.value)) {
                insert (selectable)
                writer.writeRow (row :+ "ADMIT")
            }
            else writer.writeRow (row :+ "CULL")
        }
        writer.writeRow (Nil)
        builder.result ()
    }

}

object Category {

    case class ElemCategory (
        label: String,
        override val selectables: Seq[Selectable],
        override val numToSelect: Int
    ) (override implicit val writer: CSVWriter)
    extends Category (selectables, numToSelect) {
        lazy val combinations: Seq[Seq[Selectable]] = (culled combinations numToSelect) .toSeq
    }

    case class MetaCategory (
        categories: Seq[ElemCategory],
        override val selectables: Seq[Combination]
    ) (override implicit val writer: CSVWriter)
    extends Category (selectables, 1)

}
