package com.github.nhweston.mcknapsack

sealed abstract class Selectable {
    val labels: Seq[String]
    val value: BigDecimal
    val cost: BigDecimal
}

object Selectable {

    case class Element (
        label: String,
        override val value: BigDecimal,
        override val cost: BigDecimal,
        category: String
    ) extends Selectable {
        override lazy val labels: Seq[String] = Seq (label)
    }

    case class Combination (elems: Seq[Element]) extends Selectable {
        override lazy val labels: Seq[String] = elems.flatMap (_.labels)
        override lazy val value: BigDecimal = elems.foldLeft (BigDecimal (0)) (_ + _.value)
        override lazy val cost: BigDecimal = elems.foldLeft (BigDecimal (0)) (_ + _.cost)
    }

}
