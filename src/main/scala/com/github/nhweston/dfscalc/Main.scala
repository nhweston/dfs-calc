package com.github.nhweston.dfscalc

import java.time.{Duration, Instant}

import com.github.nhweston.mcknapsack
import com.github.nhweston.mcknapsack.Knapsack
import com.github.tototoshi.csv.CSVWriter

object Main {

    val PATH: String = "./data/"

    def run (knapsack: Knapsack) : Unit = {
        val t0 = Instant.now
        val selection = knapsack.result.values.flatten
        val t1 = Instant.now
        var valueTotal = BigDecimal (0)
        var costTotal = BigDecimal (0)
        for (elem <- selection) {
            val name = elem.label
            val value = elem.value
            val cost = elem.cost
            println (f"$name%-24s $value%8.2f $cost%8.0f")
            valueTotal += value
            costTotal += cost
        }
        println(f"${"TOTAL"}%-24s $valueTotal%8.2f $costTotal%8.0f")
        println(s"Completed in ${Duration.between(t0, t1).toMillis} ms.")
    }

    def main (args: Array[String]) : Unit = {
        args.toSeq match {
            case positionsPath +: selectablesPath +: budgetStr +: Nil =>
                val outPath = PATH + selectablesPath.split ('.') .head + ".out.csv"
                implicit val writer: CSVWriter = CSVWriter.open (outPath)
                val positions = Positions fromFile (PATH + positionsPath)
                val categories = Selectables fromFile (PATH + selectablesPath, positions)
                val budget = BigDecimal (budgetStr)
                run (mcknapsack.Knapsack (categories, budget))
            case args => throw new MatchError (args)
        }
    }

}
