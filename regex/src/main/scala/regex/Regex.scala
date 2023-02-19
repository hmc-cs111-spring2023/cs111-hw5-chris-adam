package regex

/** *****************************************************************************
  * Regular Languages
  *
  * data structure definitions for regular languages
  */

// Add your definitions here

trait RegularLanguage

case object Empty extends RegularLanguage
case object Epsilon extends RegularLanguage
case class Union(l1 : RegularLanguage, l2 : RegularLanguage) extends RegularLanguage
case class Concat(l1 : RegularLanguage, l2 : RegularLanguage) extends RegularLanguage
case class Character(value: Char) extends RegularLanguage
case class Star(value: RegularLanguage) extends RegularLanguage

/** *****************************************************************************
  * Derivatives
  *
  * Fill in the function definitions below
  */

/** Simplifies a regular language */
def simplify(lang: RegularLanguage): RegularLanguage = lang match
    case Concat(Epsilon, sublang) => simplify(sublang)
    case Concat(sublang, Epsilon) => simplify(sublang)
    case Concat(Empty, sublang) => simplify(Empty)
    case Concat(sublang, Empty) => simplify(Empty)
    case Concat(sublang1, sublang2) => Concat(simplify(sublang1), simplify(sublang2))
    case Union(Empty, sublang) => simplify(sublang)
    case Union(sublang, Empty) => simplify(sublang)
    case Union(sublang1, sublang2) => Union(simplify(sublang1), simplify(sublang2))
    case Star(Epsilon) => Epsilon
    case Star(Empty) => Empty
    case Star(sublang) => Star(simplify(sublang))
    case _ => lang
/** A language is nullable if it contains ε */
def nullable(lang: RegularLanguage): Boolean = ???

/** Computes the derivative of a language, with respect to a character */
def derivative(l: RegularLanguage)(c: Char): RegularLanguage = ???

/** *****************************************************************************
  * String-matching with regular expressions
  */

/** Given a string s and a language l, returns true if the string matches the
  * pattern described by l
  */
def matches(s: String, l: RegularLanguage): Boolean =
  if (s.isEmpty) then nullable(l)
  else matches(s.tail, derivative(l)(s.head))
