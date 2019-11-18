package org.bitbucket.nhweston.mcknapsack

import org.bitbucket.nhweston.mcknapsack.Selectable.Combination

sealed abstract class Category (val selectables: Seq[Selectable], val numToSelect: Int) {

    lazy val sorted: Seq[Selectable] = selectables.sortBy (_.cost)

    lazy val culled: Seq[Selectable] = {
        val builder = Seq.newBuilder[Selectable]
        var valuesMax = Seq.empty[BigDecimal]
        def insert (selectable: Selectable) : Unit = {
            builder += selectable
            val idx = valuesMax.indexWhere (_ <= selectable.value)
            val (pre, post) = valuesMax.dropRight (1) .splitAt (idx)
            valuesMax = (pre :+ selectable.value) ++ post
        }
        def isAdmissible (value: BigDecimal) : Boolean = valuesMax.size < numToSelect || value > valuesMax.last
        for (selectable <- sorted) if (isAdmissible (selectable.value)) insert (selectable)
        builder.result ()
    }

}

object Category {

    case class ElemCategory (
        label: String,
        override val selectables: Seq[Selectable],
        override val numToSelect: Int
    ) extends Category (selectables, numToSelect) {
        lazy val combinations: Seq[Seq[Selectable]] = (culled combinations numToSelect) .toSeq
    }

    case class MetaCategory (
        categories: Seq[ElemCategory],
        override val selectables: Seq[Combination]
    ) extends Category (selectables, 1)

}
