package org.slips

import cats.Eq
import org.slips.Environment as SEnv
import org.slips.core.Empty
import org.slips.core.SignatureStrategy
import org.slips.core.conditions.Condition
import org.slips.core.fact.Fact
import org.slips.core.fact.FactOps
import org.slips.core.rule.Rule
import org.slips.syntax.*
import scala.compiletime.summonInline
import scala.util.NotGiven

object SyntaxTest {
  enum Theme:
    case War, Peace

  object Theme {
    given empty: Empty[Theme] with {
      override def empty: Theme = Theme.War
    }

    given eq: Eq[Theme] = (x: Theme, y: Theme) => x == y
  }

  val confidenceDrop: Double = 0.99
  case class Category(theme: Theme, confidence: Double) {
    def :*:(other: Category): Category =
      copy(confidence = Math.min(confidence + other.confidence, 1) * confidenceDrop)
  }

  case class Word(word: String, category: Category)
  case class Text(word1: String, word2: String, categoryM: Option[Category])

  val categoryEmpty: Text => Boolean = _.categoryM.isEmpty
  val firstWord: Text => String      = _.word1
  val secondWord: Text => String     = _.word2
  val wordValue: Word => String      = _.word
  val wordCategory: Word => Category = _.category

  private val shouldMarkText =
    all[Word]
      .flatMap(w =>
        all[Text]
          .map { t => val _ = t.test(categoryEmpty); t }
          .map { t => val _ = (t.value(firstWord) === w.value(wordValue)) || (t.value(secondWord) === w.value(wordValue)); t }
          .map(t => (w.value(wordCategory), t))
      )

  private val markText = (env: Environment) ?=>
    shouldMarkText
      .makeRule("mark text")
      .withAction { case (category, text) =>
        for {
          txt <- text.value
          cat <- category.value
          _   <- text.remove
          _   <- addFact(txt.copy(categoryM = Some(cat)))
        } yield ()
      }

  val categoryIsDefined: Text => Boolean      = _.categoryM.isDefined
  val textThemeM: Text => Option[Theme]       = _.categoryM.map(_.theme)
  val textCategoryM: Text => Option[Category] = _.categoryM
  val words                                   = Seq(firstWord, secondWord)

  private val shouldMarkWord =
    for {
      t1 <- all[Text] if t1.test(categoryIsDefined)
      t2 <- all[Text] if t2.test(categoryIsDefined)
      _ = (for {
        w1 <- words
        w2 <- words
      } yield t1.value(w1) === t2.value(w2)).reduceLeft(_.or(_))
      _ = t1.value(textThemeM) === t2.value(textThemeM)
      _ = notExists[Word] { w => w.value(wordValue) === t1.value(firstWord) }
    } yield (t1.value(firstWord), t1.value(textCategoryM), t2.value(textCategoryM))

  private val markWord = (env: Environment) ?=>
    shouldMarkWord
      .makeRule("mark word")
      .withAction { case (w, c1, c2) =>
        for {
          word <- w.value
          cat1 <- c1.value
          cat2 <- c2.value
          _    <- addFact(Word(word, cat1.get :*: cat2.get))
        } yield ()
      }
  // TODO: Fix mapN
  /*
        shouldMarkWord.makeRule("mark word") {
          _.mapN { case (word, Some(cat1), Some(cat2)) => assert(Word(word, cat1 :*: cat2)) }
        }*/

  lazy val rules = (env: SEnv) ?=> Set(markWord, markText)
}
