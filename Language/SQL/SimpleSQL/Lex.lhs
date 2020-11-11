
The parser uses a separate lexer for two reasons:

1. sql syntax is very awkward to parse, the separate lexer makes it
easier to handle this in most places (in some places it makes it
harder or impossible, the fix is to switch to something better than
parsec)

2. using a separate lexer gives a huge speed boost because it reduces
backtracking. (We could get this by making the parsing code a lot more
complex also.)

3. we can test the lexer relatively exhaustively, then even when we
don't do nearly as comprehensive testing on the syntax level, we still
have a relatively high assurance of the low level of bugs. This is
much more difficult to get parity with when testing the syntax parser
directly without the separately testing lexing stage.

> -- | Lexer for SQL.
> {-# LANGUAGE TupleSections #-}
> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> module Language.SQL.SimpleSQL.Lex
>     (SQLToken(..)
>     ,SQLTokenStream(..)
>     ,WithPos(..)
>     ,prettyToken
>     ,prettyTokens
>     ,Dialect(..)
>     ,tokenListWillPrintAndLex
>     ,lexSQL
>     ,initialPos
>     ,sqlTokenStreamAsList
>     ) where

> import Language.SQL.SimpleSQL.Dialect

> import Text.Megaparsec (option,manyTill
>                    ,try,oneOf,(<|>),choice,eof
>                    ,many,lookAhead,satisfy,takeWhileP, chunk, Parsec, Stream(..), getSourcePos, initialPos
>                    ,defaultTabWidth, mkPos, runParser', getParserState, takeWhile1P, ParseErrorBundle(..),single
>                    ,notFollowedBy, anySingle, State(..), PosState(..), SourcePos(..), between, anySingleBut)
> import qualified Text.Megaparsec.Char.Lexer as Lex
> import Text.Megaparsec.Char (string, char, space1)
> --import Language.SQL.SimpleSQL.Combinators
> import Control.Applicative hiding ((<|>), many)
> import Data.Char
> import Control.Monad
> import Data.Maybe
> import qualified Data.Text as T
> import Data.Text (Text, unpack)
> import Data.Void
> import Data.Proxy
> import qualified Data.List.NonEmpty as NE
> import Data.List
> import Data.Semigroup (Semigroup, (<>))
>
> -- | Represents a lexed token
> data SQLToken
>     -- | A symbol (in ansi dialect) is one of the following
>     --
>     -- * multi char symbols <> \<= \>= != ||
>     -- * single char symbols: * + -  < >  ^ / %  ~ & | ? ( ) [ ] , ; ( )
>     --
>     = Symbol Text
>
>     -- | This is an identifier or keyword. The first field is
>     -- the quotes used, or nothing if no quotes were used. The quotes
>     -- can be " or u& or something dialect specific like []
>     | Identifier (Maybe (Text,Text)) Text
>
>     -- | This is a prefixed variable symbol, such as :var, @var or #var
>     -- (only :var is used in ansi dialect)
>     | PrefixedVariable Char Text
>
>     -- | This is a positional arg identifier e.g. $1
>     | PositionalArg Int
>
>     -- | This is a string literal. The first two fields are the --
>     -- start and end quotes, which are usually both ', but can be
>     -- the character set (one of nNbBxX, or u&, U&), or a dialect
>     -- specific string quoting (such as $$ in postgres)
>     | SqlString Text Text Text
>
>     -- | A number literal (integral or otherwise), stored in original format
>     -- unchanged
>     | SqlNumber Text
>
>       deriving (Eq,Show, Ord)


> type Parser = Parsec Void Text
> type ParseError = ParseErrorBundle Text Void
>

> data WithPos a = WithPos
>  { startPos :: SourcePos
>  , endPos :: SourcePos
>  , tokenVal :: a }
>  deriving (Eq, Ord, Show)
>  
> newtype SQLTokenStream = SQLTokenStream { unSQLTokenStream :: [WithPos SQLToken] }
>   deriving (Eq, Show, Semigroup)
>
> -- Discards positioning.
> sqlTokenStreamAsList :: SQLTokenStream -> [SQLToken]
> sqlTokenStreamAsList (SQLTokenStream l) = map tokenVal l
> 
> instance Stream SQLTokenStream where
>   type Token SQLTokenStream = WithPos SQLToken
>   type Tokens SQLTokenStream = [WithPos SQLToken]
>
>   tokenToChunk Proxy t = [t]
>   tokensToChunk Proxy ts = ts
>   chunkToTokens Proxy = id
>   chunkLength Proxy = length
>   chunkEmpty Proxy = null
>   take1_ (SQLTokenStream []) = Nothing
>   take1_ (SQLTokenStream (t:ts)) = Just (t, SQLTokenStream ts)
>   takeN_ n (SQLTokenStream ts) | n <= 0 = Just ([], SQLTokenStream ts)
>                                | null ts = Nothing
>                                | otherwise = let (x, ts') = splitAt n ts in Just (x, SQLTokenStream ts')
>   takeWhile_ f (SQLTokenStream ts) = let (x, ts') = span f ts in (x, SQLTokenStream ts')
>   showTokens Proxy = intercalate "," . NE.toList . fmap (unpack . prettyToken ansi2011 . tokenVal)
>   reachOffset o pst@PosState{} =
>     let offendingLineTokens line = takeWhile (\tok -> line == sourceLine (startPos tok)) (scrollToLine line)
>         currentStream = unSQLTokenStream (pstateInput pst)
>         scrollToLine line = dropWhile (\tok -> line /= sourceLine (startPos tok)) currentStream
>         offendingLine line = T.unpack (prettyTokens ansi2011 (map tokenVal (offendingLineTokens line)))
>     in --since we toss away whitespace from the lexer stream, we make a reasonable guess at the column position, but it can be wrong
>     case drop (o - pstateOffset pst) currentStream of
>       [] -> ( offendingLine (sourceLine (pstateSourcePos pst))
>             , pst { pstateInput = SQLTokenStream [] })
>       (x:xs) -> 
>                 ( offendingLine (sourceLine (startPos x))
>                 , pst { pstateInput = SQLTokenStream (x: (x `seq` xs)) })
> 
>

> -- | Pretty printing, if you lex a bunch of tokens, then pretty
> -- print them, should should get back exactly the same string
> prettyToken :: Dialect -> SQLToken -> Text
> prettyToken _ (Symbol s) = s
> prettyToken _ (Identifier Nothing t) = t
> prettyToken _ (Identifier (Just (q1,q2)) t) = q1 <> t <> q2
> prettyToken _ (PrefixedVariable c p) = T.cons c p
> prettyToken _ (PositionalArg p) = T.cons '$' (T.pack (show p))
> prettyToken _ (SqlString s e t) = s <> t <> e
> prettyToken _ (SqlNumber r) = r
>
> prettyTokens :: Dialect -> [SQLToken] -> Text
> prettyTokens d ts = T.intercalate " " $ map (prettyToken d) ts

TODO: try to make all parsers applicative only

> -- | Lex some SQL to a list of tokens.
> lexSQL :: Dialect
>                   -- ^ dialect of SQL to use
>                -> FilePath
>                   -- ^ filename to use in error messages
>                -> Maybe (Int,Int)
>                   -- ^ line number and column number of the first character
>                   -- in the source to use in error messages
>                -> String
>                   -- ^ the SQL source to lex
>                -> Either ParseError SQLTokenStream
> lexSQL dialect fn' p src =
>     let (l',c') = fromMaybe (1,1) p
>         freshState = State { stateInput = T.pack src,
>                              stateOffset = 0,
>                              statePosState = freshPosState }
>         freshPosState = PosState {
>           pstateOffset = 0,
>           pstateInput = T.pack src,
>           pstateSourcePos = SourcePos {
>               sourceName = fn',
>               sourceLine = mkPos l',
>               sourceColumn = mkPos c'},
>             pstateTabWidth = defaultTabWidth,
>             pstateLinePrefix = ""}
>     in 
>       snd $ runParser' (SQLTokenStream <$> (sc *> many (sqlToken dialect) <* eof)) freshState

>       
  

> -- | parser for a sql token
> sqlToken :: Dialect -> Parser (WithPos SQLToken)
> sqlToken d = do
>     pstate <- getParserState

The order of parsers is important: strings and quoted identifiers can
start out looking like normal identifiers, so we try to parse these
first and use a little bit of try. Line and block comments start like
symbols, so we try these before symbol. Numbers can start with a . so
this is also tried before symbol (a .1 will be parsed as a number, but
. otherwise will be parsed as a symbol).

>     tok <- choice [sqlString d
>                   ,identifier d
>                   ,try (sqlNumber d)
>                   ,positionalArg d
>                   ,prefixedVariable d
>                   ,symbol d
>                   ] <* sc
>     epos <- getSourcePos
>     let spos = pstateSourcePos (statePosState pstate)
>     pure (WithPos spos epos tok)

Parses identifiers:

simple_identifier_23
u&"unicode quoted identifier"
"quoted identifier"
"quoted identifier "" with double quote char"
`mysql quoted identifier`

> identifier :: Dialect -> Parser SQLToken
> identifier d =
>     choice
>     [guard (diDoubleQuotedIden d) >> quotedIden
>     ,unicodeQuotedIden
>     ,regularIden
>     ,guard (diBackquotedIden d) >> backtickQuotedIden
>     ,guard (diSquareBracketQuotedIden d) >> sqlServerQuotedIden
>     ]
>   where
>     regularIden = Identifier Nothing <$> identifierString d
>     quotedIden = Identifier (Just ("\"","\"")) <$> qidenPart
>     backtickQuotedIden = Identifier (Just ("`","`"))
>                       <$> (char '`' *> takeWhile1P (Just "backticked token") (/='`') <* char '`')
>     sqlServerQuotedIden = Identifier (Just ("[","]"))
>                           <$> (char '[' *> takeWhile1P (Just "bracketed token") (`notElem` ['[',']']) <* char ']')
>     -- try is used here to avoid a conflict with identifiers
>     -- and quoted strings which also start with a 'u'
>     unicodeQuotedIden = Identifier
>                         <$> (f <$> try (oneOf ['u','U'] <* string "&"))
>                         <*> qidenPart
>       where f x = Just (T.cons x "&\"", "\"")
>     qidenPart = char '"' *> qidenSuffix ""
>     qidenSuffix :: Text -> Parser Text
>     qidenSuffix t = do
>         s <- takeWhileP (Just "qidenSuffix token") (/='"')
>         void $ char '"'
>         -- deal with "" as literal double quote character
>         choice [do
>                 void $ char '"'
>                 qidenSuffix $ T.concat [t,s,"\"\""]
>                ,return $ T.concat [t,s]]


This parses a valid identifier without quotes.

> identifierString :: Dialect -> Parser Text
> identifierString di = 
>     startsWith (\c -> c == '_' || isAlpha c) (isIdentifierChar di)

this can be moved to the dialect at some point

> isIdentifierChar :: Dialect -> Char -> Bool
> isIdentifierChar di c = c == '_' || isAlphaNum c || if diTableAsteriskSuffix di then c == '*' else False

use try because : and @ can be part of other things also

> prefixedVariable :: Dialect -> Parser SQLToken
> prefixedVariable  d = try $ choice
>     [PrefixedVariable <$> char ':' <*> identifierString d
>     ,guard (diAtIdentifier d) >>
>      PrefixedVariable <$> char '@' <*> identifierString d
>     ,guard (diHashIdentifier d) >>
>      PrefixedVariable <$> char '#' <*> identifierString d
>     ]

> positionalArg :: Dialect -> Parser SQLToken
> positionalArg d =
>   guard (diPositionalArg d) >>
>   -- use try to avoid ambiguities with other syntax which starts with dollar
>   PositionalArg <$> try (char '$' *> Lex.decimal)


Parse a SQL string. Examples:

'basic string'
'string with '' a quote'
n'international text'
b'binary string'
x'hexidecimal string'


> sqlString :: Dialect -> Parser SQLToken
> sqlString d = dollarString <|> csString <|> normalString
>   where
>     dollarString = do
>         guard $ diDollarString d
>         -- use try because of ambiguity with symbols and with
>         -- positional arg
>         delim <- (\x -> T.concat ["$",x,"$"])
>                  <$> try (char '$' *> option "" (identifierString d) <* char '$')
>         SqlString delim delim  <$> (T.pack <$> manyTill anySingle (try $ string delim))
>     escapeChar = do
>       _ <- single '\\'
>       oneOf ['\\', '\'', '"'] <|> (single 'n' *> pure '\n') <|> (single 't' *> pure '\t')
>     normalString | diBackslashEscapeQuotedString d =
>       SqlString "'" "'" <$> between (char '\'') (char '\'') (T.pack <$> many (escapeChar <|> anySingleBut '\''))
>                  | otherwise = SqlString "'" "'" <$> (char '\'' *> normalStringSuffix (diBackslashEscapeQuotedString d || False) "")
>     normalStringSuffix :: Bool -> Text -> Parser Text
>     normalStringSuffix allowBackslash t = do
>         let accepted = '\'' : if allowBackslash then ['\\'] else []
>         s <- takeWhileP (Just "string suffix") (`notElem` accepted)
>         -- deal with '' or \' as literal quote character
>         choice [do
>                 ctu <- choice ["'" <$ try (string "''")
>                               ,"'" <$ string "\\'"
>                               ,"\\" <$ char '\\']
>                 normalStringSuffix allowBackslash $ T.concat [t,s,ctu]
>                ,T.concat [t,s] <$ char '\'']
>     -- try is used to to avoid conflicts with
>     -- identifiers which can start with n,b,x,u
>     -- once we read the quote type and the starting '
>     -- then we commit to a string
>     -- it's possible that this will reject some valid syntax
>     -- but only pathalogical stuff, and I think the improved
>     -- error messages and user predictability make it a good
>     -- pragmatic choice
>     csString
>       | diEString d =
>         choice [SqlString <$> try (string "e'" <|> string "E'")
>                           <*> return "'" <*> normalStringSuffix True ""
>                ,csString']
>       | otherwise = csString'
>     csString' = SqlString
>                 <$> try cs
>                 <*> return "'"
>                 <*> normalStringSuffix False ""
>     csPrefixes = "nNbBxX"
>     cs = choice $ (map (\x -> string (T.cons x "'")) csPrefixes)
>                   ++ [string "u&'"
>                      ,string "U&'"]

numbers

digits
digits.[digits][e[+-]digits]
[digits].digits[e[+-]digits]
digitse[+-]digits

where digits is one or more decimal digits (0 through 9). At least one
digit must be before or after the decimal point, if one is used. At
least one digit must follow the exponent marker (e), if one is
present. There cannot be any spaces or other characters embedded in
the constant. Note that any leading plus or minus sign is not actually
considered part of the constant; it is an operator applied to the
constant.

> sqlNumber :: Dialect -> Parser SQLToken
> sqlNumber _ =
>     SqlNumber <$> completeNumber
>  where
>     completeNumber :: Parser Text
>     completeNumber = do
>       let digits = takeWhile1P (Just "digit") isDigit
>           dot = T.singleton <$> single '.'
>           exponent' = do
>              e <- single 'e' <|> single 'E'
>              sign <- optE neg
>              exp' <- digits
>              pure (e `T.cons` sign <> exp')
>           optE = option ""
>           neg = T.singleton <$> single '-'
>           decDot = do
>             a <- digits
>             b <- optE dot
>             c <- optE digits
>             pure (T.concat [a,b,c])
>           docDec = do
>             a <- dot
>             b <- digits
>             pure (T.concat [a,b])
>       mantissa <- decDot <|> docDec
>       exp' <- optE exponent'
>       pure (T.concat [mantissa,exp'])

Symbols

A symbol is an operator, or one of the misc symbols which include:
. .. := : :: ( ) ? ; , { } (for odbc)

The postgresql operator syntax allows a huge range of operators
compared with ansi and other dialects

> symbol :: Dialect -> Parser SQLToken
> symbol d  = Symbol <$> choice (concat
>    [dots
>    ,if diPostgresSymbols d
>     then postgresExtraSymbols
>     else []
>    ,miscSymbol
>    ,if diOdbc d then odbcSymbol else []
>    ,if diPostgresSymbols d
>     then generalizedPostgresqlOperator
>     else basicAnsiOps
>    ])
>  where
>    dots = [T.pack <$> some (char '.')]
>    odbcSymbol = [string "{", string "}"]
>    postgresExtraSymbols =
>        [try (string ":=")
>         -- parse :: and : and avoid allowing ::: or more
>        ,try (string "::" <* notFollowedBy (char ':'))
>        ,try (string ":" <* notFollowedBy (char ':'))]
>    miscSymbol = map (string . T.pack . (:[])) $
>                 case () of
>                     _ | diSqlServerSymbols d -> ",;():?"
>                       | diPostgresSymbols d -> "[],;()"
>                       | otherwise -> "[],;():?"

try is used because most of the first characters of the two character
symbols can also be part of a single character symbol

>    basicAnsiOps = map (try . string) [">=","<=","!=","<>"]
>                   ++ map (chunk . T.singleton) "+-^*/%~&<>="
>                   ++ pipes
>    pipes :: [Parser Text]
>    pipes = -- what about using some (char '|'), then it will
>            -- fail in the parser? Not sure exactly how
>            -- standalone the lexer should be
>     [char '|' *>
>       choice ["||" <$ char '|' <* notFollowedBy (char '|')
>              ,return "|"]]

postgresql generalized operators

this includes the custom operators that postgres supports,
plus all the standard operators which could be custom operators
according to their grammar

rules

An operator name is a sequence of up to NAMEDATALEN-1 (63 by default) characters from the following list:

+ - * / < > = ~ ! @ # % ^ & | ` ?

There are a few restrictions on operator names, however:
-- and /* cannot appear anywhere in an operator name, since they will be taken as the start of a comment.

A multiple-character operator name cannot end in + or -, unless the name also contains at least one of these characters:

~ ! @ # % ^ & | ` ?

which allows the last character of a multi character symbol to be + or
-

> generalizedPostgresqlOperator :: [Parser Text]
> generalizedPostgresqlOperator = [singlePlusMinus,opMoreChars]
>   where
>     allOpSymbols = "+-*/<>=~!@#%^&|`?"
>     -- these are the symbols when if part of a multi character
>     -- operator permit the operator to end with a + or - symbol
>     exceptionOpSymbols :: String
>     exceptionOpSymbols = "~!@#%^&|`?"

>     -- special case for parsing a single + or - symbol
>     singlePlusMinus = try $ do
>       c <- oneOf ['+','-']
>       notFollowedBy $ oneOf allOpSymbols
>       return (T.singleton c)

>     -- this is used when we are parsing a potentially multi symbol
>     -- operator and we have alread seen one of the 'exception chars'
>     -- and so we can end with a + or -
>     moreOpCharsException = do
>        c <- oneOf (filter (`notElem` ['-','/','*']) allOpSymbols)
>             -- make sure we don't parse a comment starting token
>             -- as part of an operator
>             <|> try (char '/' <* notFollowedBy (char '*'))
>             <|> try (char '-' <* notFollowedBy (char '-'))
>             -- and make sure we don't parse a block comment end
>             -- as part of another symbol
>             <|> try (char '*' <* notFollowedBy (char '/'))
>        T.cons c <$> option T.empty moreOpCharsException
>     opMoreChars :: Parser Text
>     opMoreChars = choice
>        [-- parse an exception char, now we can finish with a + -
>         T.cons
>         <$> oneOf exceptionOpSymbols
>         <*> option T.empty moreOpCharsException
>        ,T.cons
>         <$> (-- parse +, make sure it isn't the last symbol
>              try (char '+' <* lookAhead (oneOf allOpSymbols))
>              <|> -- parse -, make sure it isn't the last symbol
>                  -- or the start of a -- comment
>              try (char '-'
>                   <* notFollowedBy (char '-')
>                   <* lookAhead (oneOf allOpSymbols))
>              <|> -- parse / check it isn't the start of a /* comment
>              try (char '/' <* notFollowedBy (char '*'))
>              <|> -- make sure we don't parse */ as part of a symbol
>              try (char '*' <* notFollowedBy (char '/'))
>              <|> -- any other ansi operator symbol
>              oneOf ['<','>','='])
>         <*> option T.empty opMoreChars
>        ]

> sc :: Parser ()
> sc = Lex.space space1 (Lex.skipLineComment "--") (Lex.skipBlockComment "/*" "*/")

Some helper combinators

> startsWith :: (Char -> Bool) -> (Char -> Bool) -> Parser Text
> startsWith p ps = do
>   c <- satisfy p
>   choice [T.cons c <$> (takeWhile1P (Just "startsWith") ps)
>          ,return (T.singleton c)]


This utility function will accurately report if the two tokens are
pretty printed, if they should lex back to the same two tokens. This
function is used in testing (and can be used in other places), and
must not be implemented by actually trying to print both tokens and
then lex them back from a single string (because then we would have
the risk of thinking two tokens cannot be together when there is bug
in the lexer, which the testing is supposed to find).

maybe do some quick checking to make sure this function only gives
true negatives: check pairs which return false actually fail to lex or
give different symbols in return: could use quickcheck for this

a good sanity test for this function is to change it to always return
true, then check that the automated tests return the same number of
successes. I don't think it succeeds this test at the moment

> -- | Utility function to tell you if a list of tokens
> -- will pretty print then lex back to the same set of tokens.
> -- Used internally, might be useful for generating SQL via lexical tokens.
> tokenListWillPrintAndLex :: Dialect -> [SQLToken] -> Bool
> tokenListWillPrintAndLex _ [] = True
> tokenListWillPrintAndLex _ [_] = True
> tokenListWillPrintAndLex d (a:b:xs) =
>     tokensWillPrintAndLex d a b && tokenListWillPrintAndLex d (b:xs)

> tokensWillPrintAndLex :: Dialect -> SQLToken -> SQLToken -> Bool
> tokensWillPrintAndLex d a b

a : followed by an identifier character will look like a host param
followed by = or : makes a different symbol

>     | Symbol ":" <- a
>     , checkFirstBChar (\x -> isIdentifierChar d x || x `elem` [':','=']) = False

two symbols next to eachother will fail if the symbols can combine and
(possibly just the prefix) look like a different symbol

>     | diPostgresSymbols d
>     , Symbol a' <- a
>     , Symbol b' <- b
>     , b' `notElem` ["+", "-"] || or (map (\c -> T.singleton c `T.isInfixOf` a') "~!@#%^&|`?") = False

check two adjacent symbols in non postgres where the combination
possibilities are much more limited. This is ansi behaviour, it might
be different when the other dialects are done properly

>    | Symbol a' <- a
>    , Symbol b' <- b
>    , (a',b') `elem` [("<",">")
>                     ,("<","=")
>                     ,(">","=")
>                     ,("!","=")
>                     ,("|","|")
>                     ,("||","|")
>                     ,("|","||")
>                     ,("||","||")
>                     ,("<",">=")
>                     ] = False

check the last character of the first token and the first character of
the second token forming a comment start or end symbol

>    | let f '-' '-' = True
>          f '/' '*' = True
>          f '*' '/' = True
>          f _ _ = False
>      in checkBorderChars f = False

a symbol will absorb a following .
TODO: not 100% on this always being bad

>    | Symbol {} <- a
>    , checkFirstBChar (=='.') = False

cannot follow a symbol ending in : with another token starting with :

>    | let f ':' ':' = True
>          f _ _ = False
>      in checkBorderChars f = False

unquoted identifier followed by an identifier letter

>    | Identifier Nothing _ <- a
>    , checkFirstBChar (isIdentifierChar d) = False

a quoted identifier using ", followed by a " will fail

>    | Identifier (Just (_,"\"")) _ <- a
>    , checkFirstBChar (=='"') = False

prefixed variable followed by an identifier char will be absorbed

>    | PrefixedVariable {} <- a
>    , checkFirstBChar (isIdentifierChar d)= False

a positional arg will absorb a following digit

>    | PositionalArg {} <- a
>    , checkFirstBChar isDigit = False

a string ending with ' followed by a token starting with ' will be absorbed

>    | SqlString _ "'" _ <- a
>    , checkFirstBChar (=='\'') = False

a number followed by a . will fail or be absorbed

>    | SqlNumber {} <- a
>    , checkFirstBChar (=='.') = False

a number followed by an e or E will fail or be absorbed

>    | SqlNumber {} <- a
>    , checkFirstBChar (\x -> x =='e' || x == 'E') = False

two numbers next to eachother will fail or be absorbed

>    | SqlNumber {} <- a
>    , SqlNumber {} <- b = False


>    | otherwise = True

>   where
>     prettya = prettyToken d a
>     prettyb = prettyToken d b
>     -- helper function to run a predicate on the
>     -- last character of the first token and the first
>     -- character of the second token
>     checkBorderChars f = if T.length prettya > 0 && T.length prettyb > 0 then
>                            f (T.last prettya) (T.head prettyb)
>                          else
>                            False
>     checkFirstBChar f = case T.uncons prettyb of
>                           Just (b', _) -> f b'
>                           Nothing -> False


TODO:

make the tokenswill print more dialect accurate. Maybe add symbol
  chars and identifier chars to the dialect definition and use them from
  here

start adding negative / different parse dialect tests

add token tables and tests for oracle, sql server
review existing tables

look for refactoring opportunities, especially the token
generation tables in the tests

do some user documentation on lexing, and lexing/dialects

start thinking about a more separated design for the dialect handling

lexing tests are starting to take a really long time, so split the
tests so it is much easier to run all the tests except the lexing
tests which only need to be run when working on the lexer (which
should be relatively uncommon), or doing a commit or finishing off a
series of commits,

start writing the error message tests:
  generate/write a large number of syntax errors
  create a table with the source and the error message
  try to compare some different versions of code to compare the
    quality of the error messages by hand

  get this checked in so improvements and regressions in the error
    message quality can be tracked a little more easily (although it will
    still be manual)

try again to add annotation to the ast
