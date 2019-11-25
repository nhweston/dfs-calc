package com.github.nhweston.mcknapsack

import com.github.nhweston.mcknapsack.Selectable.{Combination, Element}
import com.github.tototoshi.csv.CSVWriter

sealed abstract class Category[S <: Selectable] (val selectables: Seq[S], val numToSelect: Int)
(implicit val writer: CSVWriter) {

    def labels: Seq[String]

    lazy val sorted: Seq[S] = selectables.sortBy (s => (s.cost, -s.value))

    lazy val culled: Seq[S] = {
        val builder = Seq.newBuilder[S]
        var valuesMax = Seq.empty[BigDecimal]
        @inline
        def insert (selectable: S) : Unit = {
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
        override val selectables: Seq[Element],
        override val numToSelect: Int
    ) (override implicit val writer: CSVWriter)
    extends Category[Element] (selectables, numToSelect) {
        override lazy val labels: Seq[String] = Seq (label)
        lazy val combinations: Seq[Seq[Element]] = (culled combinations numToSelect) .toSeq
    }

    case class MetaCategory (
        categories: Seq[ElemCategory],
        override val selectables: Seq[Combination]
    ) (override implicit val writer: CSVWriter)
    extends Category[Combination] (selectables, 1) {
        override lazy val labels: Seq[String] = categories.flatMap (_.labels)
    }

}
