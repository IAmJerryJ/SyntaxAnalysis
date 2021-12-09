/**
 * HasLang syntax analysis tests.
 *
 * Copyright 2021, Anthony Sloane, Matthew Roberts, Kym Haines, Macquarie University, All rights reserved.
 */

//Runde Jia 44434065

package haslang

import org.bitbucket.inkytonik.kiama.util.ParseTests
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * Tests that check that the syntax analyser works correctly.  I.e., it accepts
 * correct input and produces the appropriate trees, and it rejects illegal input.
 */
@RunWith(classOf[JUnitRunner])
class SyntaxAnalysisTests extends ParseTests {

    import HasLangTree._

    val parsers = new SyntaxAnalysis (positions)
    import parsers._

    // Tests of parsing basic expressions

    test ("equal expression") {
        exp ("a == 1") should parseTo[HasLangNode] (EqualExp (IdnUse ("a"), IntExp (1)))
    }

    test ("less than expression") {
        exp ("a < 1") should parseTo[HasLangNode] (LessExp (IdnUse ("a"), IntExp (1)))
    }

    test ("addition expression") {
        exp ("a + 1") should parseTo[HasLangNode] (PlusExp (IdnUse ("a"), IntExp (1)))
    }

    test ("subtraction expression") {
        exp ("a - 1") should parseTo[HasLangNode] (MinusExp (IdnUse ("a"), IntExp (1)))
    }

    test ("multiplication expression") {
        exp ("a * 1") should parseTo[HasLangNode] (StarExp (IdnUse ("a"), IntExp (1)))
    }

    test ("division expression") {
        exp ("a / 1") should parseTo[HasLangNode] (SlashExp (IdnUse ("a"), IntExp (1)))
    }

    test ("integer expression") {
        exp ("823") should parseTo[HasLangNode] (IntExp (823))
    }

    test ("true expression") {
        exp ("true") should parseTo[HasLangNode] (BoolExp (true))
    }

    test ("false expression") {
        exp ("false") should parseTo[HasLangNode] (BoolExp (false))
    }

    test ("identifier expression") {
        exp ("v123") should parseTo[HasLangNode] (IdnUse ("v123"))
    }

    test ("parenthesized expression") {
        exp ("(a + 5)") should parseTo[HasLangNode] (PlusExp (IdnUse ("a"), IntExp (5)))
    }

    test ("application expression 1") {
        exp ("a b") should parseTo[HasLangNode] (AppExp (IdnUse ("a"), IdnUse ("b")))
    }

    test ("expression containing an application expression") {
        exp ("1 + foo 2") should parseTo[HasLangNode] (PlusExp(IntExp(1), AppExp (IdnUse ("foo"), IntExp (2))))
    }

    test ("if expression") {
        exp ("if (true) then 3 else 4") should parseTo[HasLangNode] (IfExp (BoolExp (true), IntExp (3), IntExp (4)))
    }

    test ("lambda expression") {
        exp ("\\a :: Int -> a + 1") should parseTo[Exp] (LamExp(
                            IdnDef("a", IntType()),
                            PlusExp(IdnUse("a"), IntExp(1))))
    }

    test ("basic type") {
        tipe ("Bool") should parseTo[Type] (BoolType())
    }

    test ("parsing unit type") {
        tipe ("()") should parseTo[Type] (UnitType())
    }

    test ("parsing list type") {
        tipe ("[Int]") should parseTo[Type] (ListType(IntType()))
    }

    test ("parsing tuple type") {
        tipe ("(Int,Bool,[Bool])") should parseTo[Type] (TupleType(Vector(IntType(), BoolType(), ListType(BoolType()))))
    }

    test ("parsing function type") {
      tipe ("Int->Bool->[Int]") should parseTo[Type] (FunType(IntType(), FunType(BoolType(), ListType(IntType()))))
    }

    test ("parsing bracketted function type") {
      tipe ("(Int->Bool)->[Int]") should parseTo[Type] (FunType(FunType(IntType(), BoolType()), ListType(IntType())))
    }

    test ("empty list") {
        exp ("[]") should parseTo[HasLangNode] (ListExp (Vector()))
    }

    test ("cons expression") {
        exp ("3 : []") should parseTo[HasLangNode] (ConsExp (IntExp (3), ListExp (Vector())))
    }

    test ("list expression") {
        exp ("[3, 4, 5]") should parseTo[HasLangNode] (ListExp (Vector(IntExp(3), IntExp(4), IntExp(5))))
    }

    test ("tuple expression") {
        exp ("(3, 4, 5)") should parseTo[HasLangNode] (TupleExp (Vector(IntExp(3), IntExp(4), IntExp(5))))
    }

    test ("underscore pattern") {
        pat ("_") should parseTo[Pat] (AnyPat())
    }

    test ("literal pattern") {
        pat ("3") should parseTo[Pat] (LiteralPat(IntExp(3)))
    }

    test ("list pattern") {
        pat ("[3, _, 5]") should parseTo[Pat] (ListPat(Vector(LiteralPat(IntExp(3)), AnyPat(), LiteralPat(IntExp(5)))))
    }

    test ("cons pattern") {
        pat ("3 : []") should parseTo[Pat] (ConsPat(LiteralPat(IntExp(3)), ListPat(Vector())))
    }

    test ("tuple pattern") {
        pat ("(3, _, 5)") should parseTo[Pat] (TuplePat(Vector(LiteralPat(IntExp(3)), AnyPat(), LiteralPat(IntExp(5)))))
    }

    test ("simple function line") {
        funline ("fac 0 = 1") should parseTo[FunLine] (FunLine("fac", Vector(LiteralPat(IntExp(0))), IntExp(1)))
    }

    test ("more complicated function line") {
        funline ("length h:t = 1 + length t") should parseTo[FunLine] (FunLine("length", Vector(ConsPat(IdentPat("h"), IdentPat("t"))), PlusExp(IntExp(1), AppExp(IdnUse("length"), IdnUse("t")))))
    }

    test ("simple variable") {
        defn ("x :: Int = 100") should parseTo[Defn] (Defn(IdnDef("x", IntType()), Vector(FunLine("", Vector(), IntExp(100)))))
    }

    test ("function with two lines") {
      defn ("""inc :: Int -> Int
               inc n = n + 1
            """) should parseTo[Defn] (Defn(
                  IdnDef("inc", FunType(IntType(), IntType())),
                  Vector(FunLine("inc", Vector(IdentPat("n")),
                                 PlusExp(IdnUse("n"), IntExp(1))))))
    }

    test ("function with three lines") {
      defn ("""fac :: Int -> Int
               fac 0 = 1.
               fac n = n * fac (n - 1)
            """) should parseTo[Defn] (Defn(
                  IdnDef("fac", FunType(IntType(), IntType())),
                  Vector(FunLine("fac", Vector(LiteralPat(IntExp(0))),
                                 IntExp(1)),
                         FunLine("fac", Vector(IdentPat("n")),
                                 StarExp(IdnUse("n"),
                                         AppExp(IdnUse("fac"),
                                                MinusExp(IdnUse("n"),
                                                         IntExp(1))))))))
    }

    test ("one definition") {
      definitions ("""x   :: Int        = 100
                """) should parseTo[Vector[Defn]] (Vector(Defn(
                            IdnDef("x", IntType()),
                            Vector(FunLine("", Vector(), IntExp(100))))))
    }

    test ("one definition with lambda") {
      definitions ("""inc :: Int -> Int = \a :: Int -> a + 1
                """) should parseTo[Vector[Defn]] (Vector(Defn(
                            IdnDef("inc", FunType(IntType(), IntType())),
                            Vector(FunLine("", Vector(), LamExp(
                                              IdnDef("a", IntType()),
                                              PlusExp(IdnUse("a"), IntExp(1)))))
                                        )))
    }

    test ("two definitions") {
      definitions ("""x   :: Int        = 100;
                      y   :: Bool       = false
                """) should parseTo[Vector[Defn]] (Vector(
                       Defn(IdnDef("x", IntType()),
                            Vector(FunLine("", Vector(), IntExp(100)))),
                       Defn(IdnDef("y", BoolType()),
                            Vector(FunLine("", Vector(), BoolExp(false))))))
    }

    test ("let with one definition") {
      program ("""let
                    x   :: Int        = 100
                  in
                    inc x
                """) should parseTo[Program] (Program(LetExp(
                    Vector(Defn(
                            IdnDef("x", IntType()),
                            Vector(FunLine("", Vector(), IntExp(100))))),
                    AppExp (IdnUse ("inc"), IdnUse ("x")))))
    }

    test ("let with two definitions") {
      program ("""let
                    x   :: Int        = 100;
                    y   :: Bool       = false
                  in
                    inc x
                """) should parseTo[Program] (Program(LetExp(
                    Vector(Defn(
                             IdnDef("x", IntType()),
                             Vector(FunLine("", Vector(), IntExp(100)))),
                           Defn(
                             IdnDef("y", BoolType()),
                             Vector(FunLine("", Vector(), BoolExp(false))))),
                    AppExp (IdnUse ("inc"), IdnUse ("x")))))
    }

    test ("program with two definitions including lambda") {
      program ("""let
                    x   :: Int        = 100;
                    inc :: Int -> Int = \a :: Int -> a + 1
                  in
                    inc x
                """) should parseTo[Program] (Program(LetExp(
                    Vector(Defn(
                            IdnDef("x", IntType()),
                            Vector(FunLine("", Vector(), IntExp(100)))),
                           Defn(
                            IdnDef("inc", FunType(IntType(), IntType())),
                            Vector(FunLine("", Vector(), LamExp(
                                              IdnDef("a", IntType()),
                                              PlusExp(IdnUse("a"), IntExp(1)))))
                                        )),
                    AppExp (IdnUse ("inc"), IdnUse ("x")))))
    }

    test ("program with definitions including lambda and multiline fun") {
      program ("""let
                    x   :: Int        = 100;
                    inc :: Int -> Int = \a :: Int -> a + 1;
                    length :: [Int] -> Int
                    length [] = 0.
                    length h:t = 1 + length t
                  in
                    inc x
                """) should parseTo[Program] (Program(LetExp(
                    Vector(Defn(
                            IdnDef("x", IntType()),
                            Vector(FunLine("", Vector(), IntExp(100)))),
                           Defn(
                            IdnDef("inc", FunType(IntType(), IntType())),
                            Vector(FunLine("", Vector(), LamExp(
                                            IdnDef("a", IntType()),
                                            PlusExp(IdnUse("a"), IntExp(1)))))),
                           Defn(
                            IdnDef("length", FunType(ListType(IntType()),
                                                     IntType())),
                            Vector(FunLine("length", Vector(ListPat(Vector())),
                                           IntExp(0)),
                                   FunLine("length",
                                           Vector(ConsPat(IdentPat("h"),
                                                          IdentPat("t"))),
                                           PlusExp(IntExp(1),
                                                   AppExp(IdnUse("length"),
                                                          IdnUse("t")))))
                                        )),
                     AppExp (IdnUse ("inc"), IdnUse ("x")))))
    }

    // FIXME: more tests here...

    test ("Student: equal expression") {
        exp ("hh == 5") should parseTo[HasLangNode] (EqualExp (IdnUse ("hh"), IntExp (5)))
    }

    test ("Student: less than expression") {
        exp ("b < 100") should parseTo[HasLangNode] (LessExp (IdnUse ("b"), IntExp (100)))
    }

    test ("Student: addition expression") {
        exp ("k + 2") should parseTo[HasLangNode] (PlusExp (IdnUse ("k"), IntExp (2)))
    }

    test ("Student: subtraction expression") {
        exp ("b - 100") should parseTo[HasLangNode] (MinusExp (IdnUse ("b"), IntExp (100)))
    }

    test ("Student: multiplication expression") {
        exp ("1 * a") should parseTo[HasLangNode] (StarExp (IntExp (1), IdnUse ("a")))
    }

    test ("Student: division expression") {
        exp ("b / 5") should parseTo[HasLangNode] (SlashExp (IdnUse ("b"), IntExp (5)))
    }

    test ("Student: integer expression") {
        exp ("70000") should parseTo[HasLangNode] (IntExp (70000))
    }

    test ("Student: true expression") {
        exp ("true") should parseTo[HasLangNode] (BoolExp (true))
    }

    test ("Student: false expression") {
        exp ("false") should parseTo[HasLangNode] (BoolExp (false))
    }

    test ("Student: identifier expression") {
        exp ("vx1999") should parseTo[HasLangNode] (IdnUse ("vx1999"))
    }

    test ("Student: parenthesized expression") {
        exp ("(4 * 5)") should parseTo[HasLangNode] (StarExp (IntExp (4), IntExp (5)))
    }

    test ("Student: application expression 1") {
        exp ("7 0") should parseTo[HasLangNode] (AppExp (IntExp (7), IntExp (0)))
    }

    test ("Student: expression containing an application expression") {
        exp ("foo 2 + 1") should parseTo[HasLangNode] (PlusExp(AppExp (IdnUse ("foo"), IntExp (2)), IntExp (1)))
    }

    test ("Student: if expression") {
        exp ("if (false) then 7 else 4") should parseTo[HasLangNode] (IfExp (BoolExp (false), IntExp (7), IntExp (4)))
    }

    test ("Student: lambda expression") {
        exp ("\\fal :: Bool -> a + 1") should parseTo[Exp] (LamExp(
                            IdnDef("fal", BoolType()),
                            PlusExp(IdnUse("a"), IntExp(1))))
    }

    test ("Student: basic type") {
        tipe ("Int") should parseTo[Type] (IntType())
    }

    test ("Student: parsing unit type") {
        tipe ("()") should parseTo[Type] (UnitType())
    }

    test ("Student: parsing list type") {
        tipe ("[Bool]") should parseTo[Type] (ListType(BoolType()))
    }

    test ("Student: parsing tuple type") {
        tipe ("([Int], ([Bool]),[Bool])") should parseTo[Type] (TupleType(Vector(ListType(IntType()), ListType(BoolType()), ListType(BoolType()))))
    }

    test ("Student: parsing function type") {
      tipe ("Int->Bool->Bool->Bool") should parseTo[Type] (FunType(IntType(), FunType(BoolType(),FunType(BoolType(),BoolType()))))
    }

    test ("Student: parsing bracketted function type") {
      tipe ("((Int->Int)->Bool)->[Int]") should parseTo[Type] (FunType(FunType(FunType(IntType(),IntType()),BoolType()),ListType(IntType())))
    }

    test ("Student: empty list") {
        exp ("[]") should parseTo[HasLangNode] (ListExp (Vector()))
    }

    test ("Student: cons expression") {
        exp ("3 : false") should parseTo[HasLangNode] (ConsExp (IntExp (3), BoolExp(false)))
    }

    test ("Student: list expression") {
        exp ("[3, true]") should parseTo[HasLangNode] (ListExp (Vector(IntExp(3), BoolExp(true))))
    }

    test ("Student: tuple expression") {
        exp ("(true, (false, 5))") should parseTo[HasLangNode] (TupleExp (Vector(BoolExp(true), TupleExp(Vector(BoolExp(false), IntExp(5))))))
    }

    test ("Student: underscore pattern") {
        pat ("_") should parseTo[Pat] (AnyPat())
    }

    test ("Student: literal pattern") {
        pat ("true") should parseTo[Pat] (LiteralPat(BoolExp(true)))
    }

    test ("Student: list pattern") {
        pat ("[_, _, _]") should parseTo[Pat] (ListPat(Vector(AnyPat(), AnyPat(), AnyPat())))
    }

    test ("Student: cons pattern") {
        pat ("3 : 5") should parseTo[Pat] (ConsPat(LiteralPat(IntExp(3)), LiteralPat(IntExp(5))))
    }

    test ("Student: tuple pattern") {
        pat ("(3, 8)") should parseTo[Pat] (TuplePat(Vector(LiteralPat(IntExp(3)), LiteralPat(IntExp(8)))))
    }

    test ("Student: simple function line") {
        funline ("ok p = 1") should parseTo[FunLine] (FunLine("ok", Vector(IdentPat("p")), IntExp(1)))
    }

    test ("Student: more complicated function line") {
        funline ("length [5] = a k") should parseTo[FunLine] (FunLine("length", Vector(ListPat(Vector(LiteralPat(IntExp(5))))), AppExp(IdnUse("a"), IdnUse("k"))))
    }

    test ("Student: simple variable") {
        defn ("y :: Bool = false") should parseTo[Defn] (Defn(IdnDef("y", BoolType()), Vector(FunLine("", Vector(), BoolExp(false)))))
    }

    test ("Student: function with two lines") {
      defn ("""Jerry :: Bool -> Int
               Tom n = n / 1
            """) should parseTo[Defn] (Defn(
                  IdnDef("Jerry", FunType(BoolType(), IntType())),
                  Vector(FunLine("Tom", Vector(IdentPat("n")),
                                 SlashExp(IdnUse("n"), IntExp(1))))))
    }

    test ("Student: function with three lines") {
      defn ("""hi :: [Int]
               hi 0 = false.
               hi n = fac (n - 1)
            """) should parseTo[Defn] (Defn(
                  IdnDef("hi", ListType(IntType())),
                  Vector(FunLine("hi", Vector(LiteralPat(IntExp(0))),
                                 BoolExp(false)),
                         FunLine("hi", Vector(IdentPat("n")),                                
                                         AppExp(IdnUse("fac"),
                                                MinusExp(IdnUse("n"),
                                                         IntExp(1)))))))
    }

    test ("Student: one definition") {
      definitions ("""y   :: Bool        = true
                """) should parseTo[Vector[Defn]] (Vector(Defn(
                            IdnDef("y", BoolType()),
                            Vector(FunLine("", Vector(), BoolExp(true))))))
    }

    test ("Student: one definition with lambda") {
      definitions ("""y :: Bool -> Bool = \a :: Bool -> false
                """) should parseTo[Vector[Defn]] (Vector(Defn(
                            IdnDef("y", FunType(BoolType(), BoolType())),
                            Vector(FunLine("", Vector(), LamExp(
                                              IdnDef("a", BoolType()),
                                              BoolExp(false))))
                                        )))
    }

    test ("Student: two definitions") {
      definitions ("""a   :: Int        = 50;
                      b   :: Int       = x + 3
                """) should parseTo[Vector[Defn]] (Vector(
                       Defn(IdnDef("a", IntType()),
                            Vector(FunLine("", Vector(), IntExp(50)))),
                       Defn(IdnDef("b", IntType()),
                            Vector(FunLine("", Vector(), PlusExp(IdnUse("x"), IntExp(3)))))))
    }

    test ("Student: let with one definition") {
      program ("""let
                    a   :: Int        = 50
                  in
                    x / y
                """) should parseTo[Program] (Program(LetExp(
                    Vector(Defn(
                            IdnDef("a", IntType()),
                            Vector(FunLine("", Vector(), IntExp(50))))),
                    SlashExp(IdnUse("x"), IdnUse("y")))))
    }

    test ("Student: let with two definitions") {
      program ("""let
                    a   :: Int        = 100;
                    a   :: Int       = 1 + 1
                  in
                    inc x
                """) should parseTo[Program] (Program(LetExp(
                    Vector(Defn(
                             IdnDef("a", IntType()),
                             Vector(FunLine("", Vector(), IntExp(100)))),
                           Defn(
                             IdnDef("a", IntType()),
                             Vector(FunLine("", Vector(), PlusExp(IntExp(1), IntExp(1)))))),
                    AppExp (IdnUse ("inc"), IdnUse ("x")))))
    }

    test ("Student: program with two definitions including lambda") {
      program ("""let
                    x   :: Int        = 100;
                    inc :: Bool -> Bool = \a :: Int -> false
                  in
                    inc x
                """) should parseTo[Program] (Program(LetExp(
                    Vector(Defn(
                            IdnDef("x", IntType()),
                            Vector(FunLine("", Vector(), IntExp(100)))),
                           Defn(
                            IdnDef("inc", FunType(BoolType(), BoolType())),
                            Vector(FunLine("", Vector(), LamExp(
                                              IdnDef("a", IntType()),
                                              BoolExp(false))))
                                        )),
                    AppExp (IdnUse ("inc"), IdnUse ("x")))))
    }

    test ("Student: program with definitions including lambda and multiline fun") {
      program ("""let
                    x   :: Int        = 100;
                    inc :: Int -> Int = \a :: Int -> a + 1;
                    a   :: Bool       = false;
                    length :: [Int]
                    length [] = 0.
                    length _  = 8.
                    length h:t = 1 + length t
                  in
                    y * 9
                """) should parseTo[Program] (Program(LetExp(
                    Vector(Defn(
                            IdnDef("x", IntType()),
                            Vector(FunLine("", Vector(), IntExp(100)))),
                           Defn(
                            IdnDef("inc", FunType(IntType(), IntType())),
                            Vector(FunLine("", Vector(), LamExp(
                                            IdnDef("a", IntType()),
                                            PlusExp(IdnUse("a"), IntExp(1)))))),
                            Defn(
                                IdnDef("a", BoolType()),
                                Vector(FunLine("", Vector(), BoolExp(false)))
                            ),
                           Defn(
                            IdnDef("length", ListType(IntType())),
                            Vector(FunLine("length", Vector(ListPat(Vector())),
                                           IntExp(0)),
                                    FunLine("length", Vector(AnyPat()), IntExp(8)),
                                   FunLine("length",
                                           Vector(ConsPat(IdentPat("h"),
                                                          IdentPat("t"))),
                                           PlusExp(IntExp(1),
                                                   AppExp(IdnUse("length"),
                                                          IdnUse("t")))))
                                        )),
                     StarExp (IdnUse ("y"), IntExp(9)))))
    }

    test ("Student: right associative") {
        exp ("a - b - c") should parseTo[HasLangNode] (MinusExp (IdnUse("a"), MinusExp(IdnUse("b"), IdnUse("c"))))
    }

    test ("Student: right associative2") {
        exp ("a / b / c") should parseTo[HasLangNode] (SlashExp (IdnUse("a"), SlashExp(IdnUse("b"), IdnUse("c"))))
    }
}
