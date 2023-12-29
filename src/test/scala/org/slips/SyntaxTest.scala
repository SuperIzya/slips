package org.slips

import org.slips.Environment as SEnv
import org.slips.core.Empty
import org.slips.core.conditions.Condition
import org.slips.core.fact.*
import org.slips.core.rule.Rule
import org.slips.syntax.*
import scala.annotation.targetName

object SyntaxTest {
  lazy val rules             = (env: SEnv) ?=> Set(markWord, markText)
  val confidenceDrop: Double = 0.99

  private val shouldMarkText: Condition[(Category, Text)] = for {
    w <- all[Word]
    t <- all[Text] if t.test(_.categoryM.isEmpty)
    _ <- (t.value(_.word1) === w.value(_.word)) || (t.value(_.word2) === w.value(_.word))
  } yield (w.value(_.category), t)

  private val markText = (env: Environment) ?=>
    shouldMarkText.makeRule("mark text") { case (category, text) =>
      for {
        txt <- text.value
        cat <- category.value
        _   <- text.remove
        _   <- addFact(txt.copy(categoryM = Some(cat)))
      } yield ()
    }

  private val shouldMarkWord: Condition[(String, Option[Category], Option[Category])] = for {
    t1 <- all[Text] if t1.test(_.categoryM.isDefined)
    t2 <- all[Text] if t2.test(_.categoryM.isDefined)
    _  <- t1.value(_.word1) === t2.value(_.word1)
    _  <- t1.value(_.categoryM.map(_.theme)) === t2.value(_.categoryM.map(_.theme))

    w <- notExists[Word] if w.value(_.word) === t1.value(_.word1)
  } yield (t1.value(_.word1), t1.value(_.categoryM), t2.value(_.categoryM))

  private val markWord = (env: Environment) ?=>
    shouldMarkWord.makeRule("mark word") { case (w, c1, c2) =>
      for {
        word <- w.value
        cat1 <- c1.value
        cat2 <- c2.value
        _    <- addFact(Word(word, cat1.get :*: cat2.get))
      } yield ()
    }
// TODO: Make possible for combined `value` operator, e.g. (w, c1, c2).valueN{ case (word, cat1, cat2) => Word(word, cat1 :*: cat2) }

  case class Category(theme: Theme, confidence: Double) {

    @targetName("append")
    def :*:(other: Category): Category =
      copy(confidence = Math.min(confidence + other.confidence, 1) * confidenceDrop)
  }

  case class Word(word: String, category: Category)

  case class Text(word1: String, word2: String, categoryM: Option[Category])

  enum Theme:
    case War, Peace

  object Theme {
    given empty: Empty[Theme] with {
      override def empty: Theme = Theme.War
    }
  }
}
