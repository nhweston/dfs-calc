package org.bitbucket.nhweston.mcknapsack

import org.bitbucket.nhweston.mcknapsack.Category.{ElemCategory, MetaCategory}
import org.bitbucket.nhweston.mcknapsack.Selectable.{Combination, Element}

case class Knapsack (
    categories: Seq[ElemCategory],
    budget: BigDecimal
) {

    lazy val metaZero: Seq[MetaCategory] = {
        categories.map { category =>
            MetaCategory (
                Seq.fill (category.numToSelect) (category),
                category.combinations.map { comb =>
                    Combination (
                        comb.flatMap {
                            case elem: Element => Seq (elem)
                            case Combination (elems) => elems
                        }
                    )
                }
            )
        }
    }

    //    lazy val metaFirstOrder: Map[Category, MetaCategory] = categories.map {
//        category => category -> MetaCategory (
//            Seq.fill(category.numToSelect)(category),
//            category.combinations.map {
//                comb => Combination (
//                    comb.flatMap {
//                        case elem @ Element(_, _, _, _) => Seq(elem)
//                        case Combination(elems) => elems
//                    }
//                )
//            }
//        )
//    } .toMap

    lazy val selections: Seq[Combination] = {
        def pairs[T, U] (s1: Seq[T], s2: Seq[U]) : Seq[(T, U)] = s1.flatMap (t1 => s2.map (t2 => (t1, t2)))
        def aggregate (c1: MetaCategory, c2: MetaCategory) : MetaCategory = {
            MetaCategory (
                c1.categories ++ c2.categories,
                pairs (c1.culled, c2.culled) .map {
                    case (Combination (s1), Combination (s2)) => Combination (s1 ++ s2)
                    case x => throw new MatchError (x)
                }
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
        aux (metaZero) match {case MetaCategory (_, selections) => selections}
    }

//    lazy val combCull: Seq[Combination] = {
//        categories.binaryAggregate[MetaCategory] (
//            metaFirstOrder,
//            (cat1, cat2) => MetaCategory (
//                cat1.categories ++ cat2.categories,
//                (cat1.culled pairs cat2.culled).map {
//                    case (comb1 @ Combination(_), comb2 @ Combination(_)) => Combination(comb1.elems ++ comb2.elems)
//                },
//            )
//        ) match {
//            case MetaCategory (_, selections) => selections
//        }
//    }

    lazy val result: Map[String, Seq[Element]] = {
        selections.filter (_.cost < budget) .maxByOption (_.value) match {
            case Some (Combination (elems)) => elems.groupBy (_.category)
            case x => throw new MatchError (x)
        }
    }

}
