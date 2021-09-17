

Test for the lexer

> {-# LANGUAGE OverloadedStrings #-}
> module Language.SQL.SimpleSQL.LexerTests (lexerTests) where

> import Language.SQL.SimpleSQL.TestTypes
> import Language.SQL.SimpleSQL.Lex (SQLToken(..),tokenListWillPrintAndLex)
> import Data.List
> import qualified Data.Text as T

> lexerTests :: TestItem
> lexerTests = Group "lexerTests" $
>     [Group "lexer token tests" [ansiLexerTests
>                                ,postgresLexerTests
>                                ,sqlServerLexerTests
>                                ,oracleLexerTests
>                                ,mySqlLexerTests
>                                ,odbcLexerTests]]

> ansiLexerTable :: [(String,[SQLToken])]
> ansiLexerTable =
>     -- single char symbols
>     map (\s -> ([s],[Symbol $ T.singleton s])) "+-^*/%~&|?<>[]=,;()"
>     -- multi char symbols
>     ++ map (\s -> (s,[Symbol $ T.pack s])) [">=","<=","!=","<>","||"]
>     ++ (let idens = ["a", "_a", "test", "table", "Stuff", "STUFF"]
>         -- simple identifiers
>         in map (\i -> (i, [Identifier Nothing (T.pack i)])) idens
>            ++ map (\i -> ("\"" ++ i ++ "\"", [Identifier (Just ("\"","\"")) $ T.pack i])) idens
>            -- todo: in order to make lex . pretty id, need to
>            -- preserve the case of the u
>            ++ map (\i -> ("u&\"" ++ i ++ "\"", [Identifier (Just ("u&\"","\"")) $ T.pack i])) idens
>            -- host param
>            ++ map (\i -> (':':i, [PrefixedVariable ':' $ T.pack i])) idens
>        )
>     -- quoted identifiers with embedded double quotes
>     -- the lexer doesn't unescape the quotes
>     ++ [("\"normal \"\" iden\"", [Identifier (Just ("\"","\"")) "normal \"\" iden"])]
>     -- strings
>     -- the lexer doesn't apply escapes at all
>     ++ [("'string'", [SqlString "'" "'" "string"])
>        ,("'normal '' quote'", [SqlString "'" "'" "normal '' quote"])
>        ,("'normalendquote '''", [SqlString "'" "'" "normalendquote ''"])
>        ,("'\n'", [SqlString "'" "'" "\n"])]
>     -- csstrings
>     ++ map (\c -> (c ++ "'test'", [SqlString (T.pack c <> "'") "'" "test"]))
>        ["n", "N","b", "B","x", "X", "u&"]
>     -- numbers
>     ++ [("10", [SqlNumber "10"])
>        ,(".1", [SqlNumber ".1"])
>        ,("5e3", [SqlNumber "5e3"])
>        ,("5e+3", [SqlNumber "5e+3"])
>        ,("5e-3", [SqlNumber "5e-3"])
>        ,("10.2", [SqlNumber "10.2"])
>        ,("10.2e7", [SqlNumber "10.2e7"])]

> ansiLexerTests :: TestItem
> ansiLexerTests = Group "ansiLexerTests" $
>     [Group "ansi lexer token tests" $ [LexTest ansi2011 s t |  (s,t) <- ansiLexerTable]
>     ,Group "ansi generated combination lexer tests" $
>     [ LexTest ansi2011 (s <> s1) (t <> t1)
>     | (s,t) <- ansiLexerTable
>     , (s1,t1) <- ansiLexerTable
>     , tokenListWillPrintAndLex ansi2011 $ t <> t1

>     ]
>     ,Group "ansiadhoclexertests" $
>        map (\(n, s) -> LexTest ansi2011 n s)
>        [("", [])
>        ,("-- line com\nstuff", [Identifier Nothing "stuff"])
>        ] ++
>        [-- want to make sure this gives a parse error
>         LexFails ansi2011 "*/"
>         -- combinations of pipes: make sure they fail because they could be
>         -- ambiguous and it is really unclear when they are or not, and
>         -- what the result is even when they are not ambiguous
>        ,LexFails ansi2011 "|||"
>        ,LexFails ansi2011 "||||"
>        ,LexFails ansi2011 "|||||"
>        -- another user experience thing: make sure extra trailing
>        -- number chars are rejected rather than attempting to parse
>        -- if the user means to write something that is rejected by this code,
>        -- then they can use whitespace to make it clear and then it will parse
>        ,LexFails ansi2011 "12e3e4"
>        ,LexFails ansi2011 "12e3e4"
>        ,LexFails ansi2011 "12e3e4"
>        ,LexFails ansi2011 "12e3.4"
>        ,LexFails ansi2011 "12.4.5"
>        ,LexFails ansi2011 "12.4e5.6"
>        ,LexFails ansi2011 "12.4e5e7"]
>      ]


todo: lexing tests
do quickcheck testing:
can try to generate valid tokens then check they parse

same as above: can also try to pair tokens, create an accurate
  function to say which ones can appear adjacent, and test

I think this plus the explicit lists of tokens like above which do
basic sanity + explicit edge casts will provide a high level of
assurance.


> postgresLexerTable :: [(String,[SQLToken])]
> postgresLexerTable =
>     -- single char symbols
>     map (\s -> ([s],[Symbol $ T.singleton s])) "+-^*/%~&|?<>[]=,;():"
>     -- multi char symbols
>     ++ map (\s -> (s,[Symbol $ T.pack s])) [">=","<=","!=","<>","||", "::","..",":="]
>     -- generic symbols

>     ++ (let idens = ["a", "_a", "test", "table", "Stuff", "STUFF"]
>         -- simple identifiers
>         in map (\i -> (i, [Identifier Nothing $ T.pack i])) idens
>            ++ map (\i -> ("\"" ++ i ++ "\"", [Identifier (Just ("\"","\"")) $ T.pack i])) idens
>            -- todo: in order to make lex . pretty id, need to
>            -- preserve the case of the u
>            ++ map (\i -> ("u&\"" ++ i ++ "\"", [Identifier (Just ("u&\"","\"")) $ T.pack i])) idens
>            -- host param
>            ++ map (\i -> (':':i, [PrefixedVariable ':' $ T.pack i])) idens
>        )
>     -- positional var
>     ++ [("$1", [PositionalArg 1])]
>     -- quoted identifiers with embedded double quotes
>     ++ [("\"normal \"\" iden\"", [Identifier (Just ("\"","\"")) "normal \"\" iden"])]
>     -- strings
>     ++ [("'string'", [SqlString "'" "'" "string"])
>        ,("'normal '' quote'", [SqlString "'" "'" "normal '' quote"])
>        ,("'normalendquote '''", [SqlString "'" "'" "normalendquote ''"])
>        ,("'\n'", [SqlString "'" "'" "\n"])
>        ,("E'\n'", [SqlString "E'" "'" "\n"])
>        ,("e'this '' quote'", [SqlString "e'" "'" "this '' quote"])
>        ,("e'this \\' quote'", [SqlString "e'" "'" "this \\' quote"])
>        ,("'not this \\' quote", [SqlString "'" "'" "not this \\"
>                                 ,Identifier Nothing "quote"])
>        ,("$$ string 1 $$", [SqlString "$$" "$$" " string 1 "])
>        ,("$$ string $ 2 $$", [SqlString "$$" "$$" " string $ 2 "])
>        ,("$a$ $$string 3$$ $a$", [SqlString "$a$" "$a$" " $$string 3$$ "])
>        ]
>     -- csstrings
>     ++ map (\c -> (c ++ "'test'", [SqlString (T.pack (c ++ "'")) "'" "test"]))
>        ["n", "N","b", "B","x", "X", "u&", "e", "E"]
>     -- numbers
>     ++ [("10", [SqlNumber "10"])
>        ,(".1", [SqlNumber ".1"])
>        ,("5e3", [SqlNumber "5e3"])
>        ,("5e+3", [SqlNumber "5e+3"])
>        ,("5e-3", [SqlNumber "5e-3"])
>        ,("10.2", [SqlNumber "10.2"])
>        ,("10.2e7", [SqlNumber "10.2e7"])]

An operator name is a sequence of up to NAMEDATALEN-1 (63 by default) characters from the following list:

+ - * / < > = ~ ! @ # % ^ & | ` ?

There are a few restrictions on operator names, however:
-- and /* cannot appear anywhere in an operator name, since they will be taken as the start of a comment.

A multiple-character operator name cannot end in + or -, unless the name also contains at least one of these characters:

~ ! @ # % ^ & | ` ?

todo: 'negative' tests
symbol then --
symbol then /*
operators without one of the exception chars
  followed by + or - without whitespace

also: do the testing for the ansi compatibility special cases

> postgresShortOperatorTable :: [(String,[SQLToken])]
> postgresShortOperatorTable =
>     [ (x, [Symbol $ T.pack x]) | x <- someValidPostgresOperators 2]


> postgresExtraOperatorTable :: [(String,[SQLToken])]
> postgresExtraOperatorTable =
>     [ (x, [Symbol $ T.pack x]) | x <- someValidPostgresOperators 4]


> someValidPostgresOperators :: Int -> [String]
> someValidPostgresOperators l =
>        [ x
>        | n <- [1..l]
>        , x <- combos "+-*/<>=~!@#%^&|`?" n
>        , not ("--" `isInfixOf` x || "/*" `isInfixOf` x || "*/" `isInfixOf` x)
>        , not (last x `elem` ("+-" :: String))
>          || or (map (`elem` x) "~!@#%^&|`?")
>        ]

These are postgres operators, which if followed immediately by a + or
-, will lex as separate operators rather than one operator including
the + or -.

> somePostgresOpsWhichWontAddTrailingPlusMinus :: Int -> [String]
> somePostgresOpsWhichWontAddTrailingPlusMinus l =
>        [ x
>        | n <- [1..l]
>        , x <- combos "+-*/<>=" n
>        , not ("--" `isInfixOf` x || "/*" `isInfixOf` x || "*/" `isInfixOf` x)
>        , not (last x `elem` ("+-" :: String))
>        ]

> {-
> tokensToStream :: [SQLToken] -> SQLTokenStream
> tokensToStream l = SQLTokenStream (map wpos l)
>
> tokenAssocsToStream :: [(String, [SQLToken])] -> [(String, SQLTokenStream)]
> tokenAssocsToStream = map (\(n,s) -> (n, tokensToStream s))
> -}
> postgresLexerTests :: TestItem
> postgresLexerTests = Group "postgresLexerTests" $
>     [Group "postgres lexer token tests" $
>      [LexTest postgres s t | (s,t) <- postgresLexerTable]
>     ,Group "postgres generated lexer token tests" $
>      [LexTest postgres s t | (s,t) <- postgresShortOperatorTable ++ postgresExtraOperatorTable]
>     ,Group "postgres generated combination lexer tests" $
>     [ LexTest postgres (s <> s1) (t <> t1)
>     | (s,t) <- postgresLexerTable ++ postgresShortOperatorTable
>     , (s1,t1) <- postgresLexerTable ++ postgresShortOperatorTable
>     , tokenListWillPrintAndLex postgres $ t <> t1

>     ]
>     ,Group "generated postgres edgecase lexertests" $
>      [LexTest postgres s t
>      | (s,t) <- edgeCaseCommentOps
>                 ++ edgeCasePlusMinusOps
>                 ++ edgeCasePlusMinusComments]

>     ,Group "adhoc postgres lexertests" $
>       -- need more tests for */ to make sure it is caught if it is in the middle of a
>       -- sequence of symbol letters
>         [LexFails postgres "*/"
>         ,LexFails postgres ":::"
>         ,LexFails postgres "::::"
>         ,LexFails postgres ":::::"
>         ,LexFails postgres "@*/"
>         ,LexFails postgres "-*/"
>         ,LexFails postgres "12e3e4"
>         ,LexFails postgres "12e3e4"
>         ,LexFails postgres "12e3e4"
>         ,LexFails postgres "12e3.4"
>         ,LexFails postgres "12.4.5"
>         ,LexFails postgres "12.4e5.6"
>         ,LexFails postgres "12.4e5e7"
>          -- special case allow this to lex to 1 .. 2
>          -- this is for 'for loops' in plpgsql
>         ,LexTest postgres "1..2" [SqlNumber "1", Symbol "..", SqlNumber "2"]]
>     ]
>  where
>    edgeCaseCommentOps =
>      [ (x ++ "/*<test*/", [Symbol $ T.pack x])
>      | x <- eccops
>      , not (last x == '*')
>      ] ++
>      [ (x ++ "--<test", [Symbol $ T.pack x])
>      | x <- eccops
>      , not (last x == '-')
>      ]
>    eccops = someValidPostgresOperators 2
>    edgeCasePlusMinusOps = concat
>      [ [ (x ++ "+", [Symbol $ T.pack x, Symbol "+"])
>        , (x ++ "-", [Symbol $ T.pack x, Symbol "-"]) ]
>      | x <- somePostgresOpsWhichWontAddTrailingPlusMinus 2
>      ]
>    edgeCasePlusMinusComments =
>      [("---", [])
>      ,("+--", [Symbol "+"])
>      ,("-/**/", [Symbol "-"])
>      ,("+/**/", [Symbol "+"])
>      ]


> sqlServerLexerTests :: TestItem
> sqlServerLexerTests = Group "sqlServerLexTests" $
>     [ LexTest sqlserver s t | (s,t) <-
>     [("@variable", [(PrefixedVariable '@' "variable")])
>     ,("#variable", [(PrefixedVariable '#' "variable")])
>     ,("[quoted identifier]", [(Identifier (Just ("[", "]")) "quoted identifier")])
>     ]]

> oracleLexerTests :: TestItem
> oracleLexerTests = Group "oracleLexTests" $
>     [] -- nothing oracle specific atm

> mySqlLexerTests :: TestItem
> mySqlLexerTests = Group "mySqlLexerTests" $
>     [ LexTest mysql s t | (s,t) <-
>     [("`quoted identifier`", [(Identifier (Just ("`", "`")) "quoted identifier")])
>     ]
>     ]

> odbcLexerTests :: TestItem
> odbcLexerTests = Group "odbcLexTests" $
>     [ LexTest sqlserver {diOdbc = True} s t | (s,t) <-
>     [("{}", [Symbol "{", Symbol "}"])
>     ]]
>     ++ [LexFails sqlserver "{"
>        ,LexFails sqlserver "}"]

> combos :: [a] -> Int -> [[a]]
> combos _ 0 = [[]]
> combos l n = [ x:tl | x <- l, tl <- combos l (n - 1) ]

figure out a way to do quickcheck testing:
1. generate valid tokens and check they parse

2. combine two generated tokens together for the combo testing

this especially will work much better for the postgresql extensible
operator tests which doing exhaustively takes ages and doesn't bring
much benefit over testing a few using quickcheck.
