/**
 * HasLang syntax analyser.
 *
 * Copyright 2021, Anthony Sloane, Matthew Roberts, Kym Haines, Macquarie University, All rights reserved.
 */
//Runde Jia 44434065

package haslang

import org.bitbucket.inkytonik.kiama.parsing.Parsers
import org.bitbucket.inkytonik.kiama.util.Positions

/**
 * Module containing parsers for HasLang.
 */
class SyntaxAnalysis (positions : Positions) extends Parsers (positions) {

    import HasLangTree._
    import scala.language.postfixOps

    lazy val parser : PackratParser[Program] =
        phrase (program)

    lazy val program : PackratParser[Program] =
        exp ^^ Program

    lazy val literal : PackratParser[Exp] =
        "false" ^^^ BoolExp (false) |
        "true" ^^^ BoolExp (true) |
        integer ^^ (s => IntExp (s.toInt))

    lazy val factor : PackratParser[Exp] =
        // FIXME
        literal |
        identifier ^^ IdnUse |
        "(" ~> exp <~ ")" |
        failure ("exp expected")

    // FIXME   add parsers between factor and exp

    lazy val term : PackratParser[Exp] =                                          //Add some operations have higher precedence than in method 'exp'
        term ~ ("*" ~> term) ^^ { case t ~ f => StarExp (t, f) } |                
        term ~ ("/" ~> term) ^^ { case t ~ f => SlashExp (t, f) } |
        ("\\" ~> idndef) ~ ("->" ~> exp) ^^ {case id ~ e => LamExp (id, e)}|      //Lambda Experssion
        term ~ term ^^ { case t1 ~ t2 => AppExp (t1, t2) } |
        ("let" ~> definitions) ~ ("in" ~> exp) ^^ {case d ~ e => LetExp(d, e)}|    //let expression let ... in ...
        "(" ~> term <~ ")"|                                                         //bracket expression
        "(" ~> repsep(term, ",") <~ ")" ^^ {case t => TupleExp(t)} |
        factor                                                                      //factor has highest precedence

    lazy val exp : PackratParser[Exp] =
        // FIXME
        // "(" ~> exp <~ ")"|                                                          
        exp ~ (":" ~> exp) ^^ {case e1 ~ e2 => ConsExp(e1, e2)}|
        "[" ~> repsep(exp, ",") <~ "]" ^^ {case e => ListExp(e)} |                  //[exp]*
        "if" ~> factor ~ ("then" ~> exp) ~ ("else" ~> exp) ^^ { case f ~ e1 ~ e2 => IfExp (f, e1, e2)}|
        exp ~ ("==" ~> exp) ^^ { case e1 ~ e2 => EqualExp (e1, e2) } |
        exp ~ ("<" ~> exp) ^^ { case e1 ~ e2 => LessExp (e1, e2) } |
        exp ~ ("+" ~> exp) ^^ { case e1 ~ e2 => PlusExp (e1, e2) } |
        exp ~ ("-" ~> exp) ^^ { case e1 ~ e2 => MinusExp (e1, e2) } |
        term                                                                         //other expressions have higher precedence

    lazy val definitions : PackratParser[Vector[Defn]] =
        // FIXME
        rep1sep(defn, ";")                                                          // Structure like "defn; defn; defn; defn"

    lazy val defn : PackratParser[Defn] =
        // FIXME
        idndef ~ rep1sep(funline,".")  ^^ {case i ~ f => Defn(i, f)}                // "idndef ~ funline. funline. funline"

    lazy val funline : PackratParser[FunLine] =
        // FIXME
        identifier ~ rep1sep(pat,"") ~ ("=" ~> exp) ^^ {case i ~ p ~ e => FunLine(i, p, e)}|        //funline = "identifier = exp" or "= exp"
        "=" ~> exp ^^ {case  e => FunLine("", Vector(), e)}

    lazy val pat : PackratParser[Pat] =
        // FIXME
        "["~> repsep(pat, ",") <~"]" ^^ {case p => ListPat(p)}|         //[pat*], ListPat can have zero or more elements
        pat ~ (":" ~> pat) ^^ {case p1 ~ p2 => ConsPat(p1, p2)}|        // pat : pat
        "(" ~> pat <~ ")"|                                              // (pat)
        "(" ~> rep1sep(pat, ",")  <~ ")" ^^ {case p => TuplePat(p)}|    // (pat, pat) Tuple pat has at least two elements
        basicpat

    lazy val basicpat : PackratParser[Pat] =
        // FIXME
        literal ^^ LiteralPat|                                           //Some basic pats
        identifier ^^ IdentPat|
        "_" ^^^ AnyPat()

    lazy val tipe : PackratParser[Type] =
        // FIXME
        tipe ~ ("->" ~> tipe) ^^ {case t1 ~ t2 => FunType(t1, t2)}|                                 // Function Type a -> b
        //  ("(" ~> tipe <~ ")") ~ ("->" ~> tipe) ^^  {case t1 ~ t2 => FunType(t1, t2)}|
        "[" ~> tipe <~ "]" ^^ {case t => ListType(t)}| 
        "(" ~> tipe <~ ")" |      
        "(" ~> rep1sep(tipe, ",")  <~ ")" ^^ {case t => TupleType(t)}|                              //Tuple Type (a, b)
        //  "(" ~> tipe ~ (("->" ~> tipe)) <~ ")" ^^  {case t1 ~ t2 => FunType(t1, t2)}|
        basictipe

    lazy val basictipe : PackratParser[Type] =
        // FIXME
        "Int" ^^^ IntType() |                                           //Some basic Types
        "Bool" ^^^ BoolType()|
        "()" ^^^ UnitType()
        

    // NOTE: You should not change anything below here...

    lazy val integer =
        regex ("[0-9]+".r)

    lazy val idndef =
        (identifier <~ "::") ~ tipe ^^ IdnDef

    val keywordStrings =
        List ("let", "else", "false", "if", "then", "true", "in")

    lazy val keyword =
        keywords ("[^a-zA-Z0-9_]".r, keywordStrings)

    lazy val identifier =
        not (keyword) ~> identifierBase

    lazy val identifierBase =
        regex ("[a-zA-Z][a-zA-Z0-9_]*".r) |
        failure ("identifier expected")

    lazy val whitespaceParser : PackratParser[Any] =
        rep ( """\s""".r | comment)

    lazy val comment : PackratParser[Any] =
        "{-" ~ rep (not ("-}") ~ (comment | any)) ~ "-}" |
        "--.*(\n|\\z)".r

}
