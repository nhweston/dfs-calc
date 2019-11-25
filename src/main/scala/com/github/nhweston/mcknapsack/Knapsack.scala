package com.github.nhweston.mcknapsack

import com.github.nhweston.mcknapsack.Category.{ElemCategory, MetaCategory}
import com.github.nhweston.mcknapsack.Selectable.{Combination, Element}
import com.github.tototoshi.csv.CSVWriter

case class Knapsack (
    categories: Seq[ElemCategory],
    budget: BigDecimal
) (implicit val writer: CSVWriter) {

    lazy val metaZero: Seq[MetaCategory] = {
        categories.map { category =>
            MetaCategory (
                Seq.fill (category.numToSelect) (category),
                category.combinations.map (comb => Combination (comb.flatMap (s => Seq (s))))
            )
        }
    }

    lazy val selections: Seq[Combination] = {
        @inline
        def pairs[T, U] (s1: Seq[T], s2: Seq[U]) : Seq[(T, U)] = s1.flatMap (t1 => s2.map (t2 => (t1, t2)))
        @inline
        def aggregate (c1: MetaCategory, c2: MetaCategory) : MetaCategory = {
            MetaCategory (
                c1.categories ++ c2.categories,
                pairs (c1.culled, c2.culled) .map {case (Combination (s1), Combination (s2)) => Combination (s1 ++ s2)}
            )
        }
        def aux (categories: Seq[MetaCategory]) : MetaCategory = {
            categories match {
                case Nil => throw new MatchError (Nil)
                case head +: Nil => head
                case _ =>
                    val (s1, s2) = categories.splitAt (categories.size / 2)
                    aggregate (aux (s1), aux (s2))
            }
        }
        aux (metaZero) .culled
    }

    lazy val result: Map[String, Seq[Element]] = {
        selections.filter (_.cost <= budget) .maxByOption (_.value) match {
            case Some (Combination (elems)) => elems.groupBy (_.category)
            case None => Map.empty
        }
    }

}
