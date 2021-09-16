{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | helpers to work with parsec errors more nicely
module Language.SQL.SimpleSQL.Errors
  ( convertErrorBundle,
    fixupErrorBundle,
  )
where

import Data.List
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Proxy
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Void
import Language.SQL.SimpleSQL.Dialect
import Language.SQL.SimpleSQL.Lex
import Text.Megaparsec

convertErrorBundle :: ParseErrorBundle T.Text Void -> ParseErrorBundle SQLTokenStream Void
convertErrorBundle errorBundle@ParseErrorBundle {bundleErrors = allBundleErrors@(firstBundleError NE.:| _), bundlePosState = posState@PosState {pstateInput = source, pstateSourcePos = spos}} = do
  let (linuNumber, columnNumber, tagetLine) = scrollToLine (extractErrorOffset firstBundleError) (T.lines source)
      sourcePosition = spos {sourceLine = mkPos linuNumber, sourceColumn = mkPos columnNumber}
  makeBundle
    errorBundle
    posState
    (NE.map (convertError posState) allBundleErrors)
    sourcePosition
    tagetLine
    sourcePosition

fixupErrorBundle :: ParseErrorBundle SQLTokenStream Void -> ParseErrorBundle SQLTokenStream Void
fixupErrorBundle errorBundle@ParseErrorBundle {bundleErrors = allBundleErrors@(firstBundleError NE.:| _), bundlePosState = posState@PosState {pstateInput = stream, pstateSourcePos = spos}} = do
  let firstErrorPosition = fromMaybe spos $ extractErrorPosition firstBundleError
      sourceLines = prettifyTokens $ tokenVal <$> scrollLine (sourceLine firstErrorPosition) stream
  makeBundle
    errorBundle
    posState
    allBundleErrors
    firstErrorPosition
    sourceLines
    (correctErrorPosition stream firstErrorPosition)

makeBundle :: ParseErrorBundle s Void -> PosState s -> NE.NonEmpty (ParseError SQLTokenStream Void) -> SourcePos -> T.Text -> SourcePos -> ParseErrorBundle SQLTokenStream Void
makeBundle errorBundle posState errors pos source sourcePos =
  errorBundle {bundlePosState = posState {pstateInput = SQLTokenStream [WithPos pos pos (Symbol source)], pstateSourcePos = sourcePos}, bundleErrors = errors}

scrollLine :: Pos -> SQLTokenStream -> [WithPos SQLToken]
scrollLine line stream = takeWhile (\tok -> line == sourceLine (startPos tok)) $ dropWhile (\tok -> line /= sourceLine (startPos tok)) $ unSQLTokenStream stream

scrollColumn :: Pos -> [WithPos a] -> [WithPos a]
scrollColumn column = takeWhile (\tok -> sourceColumn (startPos tok) <= column)

correctErrorPosition :: SQLTokenStream -> SourcePos -> SourcePos
correctErrorPosition currentStream originalErrorPosition@SourcePos {sourceLine = originalLineNumber, sourceColumn = originalColumnNumber} = do
  let affectedTokens = tokenVal <$> scrollColumn originalColumnNumber (scrollLine originalLineNumber currentStream)
  originalErrorPosition {sourceColumn = mkPos (lengthTillLast affectedTokens)}

extractErrorPosition :: (Token s ~ WithPos a) => ParseError s e -> Maybe SourcePos
extractErrorPosition (TrivialError _ (Just (Tokens (WithPos {startPos = startErrorPosition} NE.:| _))) _) = Just startErrorPosition
extractErrorPosition _ = Nothing

lengthTillLast :: [SQLToken] -> Int
lengthTillLast toks = len toks - len (lst toks) + 1
  where
    lst [] = []
    lst [tok] = [tok]
    lst (_ : xs) = lst xs
    len = T.length . prettifyTokens

extractErrorOffset :: ParseError s e -> Int
extractErrorOffset e = 1 + go e
  where
    go (TrivialError n _ _) = n
    go (FancyError n _) = n

scrollToLine :: Int -> [T.Text] -> (Int, Int, T.Text)
scrollToLine = go 1
  where
    go _ _ [] = error "Logic error. Should never happen"
    go currentLineNumber currentCharacterOffset [singleLine] = (currentLineNumber, currentCharacterOffset, singleLine)
    go currentLineNumber currentCharacterOffset (currentLine : remainingLines) =
      if T.length currentLine >= currentCharacterOffset
        then (currentLineNumber, currentCharacterOffset, currentLine)
        else go (succ currentLineNumber) (currentCharacterOffset - T.length currentLine - 1) remainingLines

instance VisualStream SQLTokenStream where
  showTokens Proxy = intercalate "," . NE.toList . fmap (T.unpack . prettifyToken . tokenVal)

instance TraversableStream SQLTokenStream where
  reachOffset offset pst@PosState {pstateInput = stream, pstateSourcePos = sourcePos} =
    let offendingLine line = T.unpack (prettifyTokens (tokenVal <$> scrollLine line stream))
     in -- since we toss away whitespace from the lexer stream, we make a reasonable guess at the column position, but it can be wrong
        case drop (offset - pstateOffset pst) (unSQLTokenStream stream) of
          [] ->
            ( Just (offendingLine $ sourceLine sourcePos),
              pst {pstateInput = SQLTokenStream []}
            )
          (x@WithPos {startPos = start} : xs) ->
            ( Just (offendingLine $ sourceLine start),
              pst {pstateSourcePos = correctErrorPosition stream start, pstateInput = SQLTokenStream (x : (x `seq` xs))}
            )

convertError :: PosState T.Text -> ParseError T.Text Void -> ParseError SQLTokenStream Void
convertError _ (FancyError a b) = FancyError a b
convertError PosState {pstateSourcePos = spos} (TrivialError a b c) =
  TrivialError a (convertErrorItem <$> b) (S.map convertErrorItem c)
  where
    convertErrorItem :: ErrorItem (Token T.Text) -> ErrorItem (Token SQLTokenStream)
    convertErrorItem (Tokens ts) = Tokens (WithPos spos spos (Symbol (T.pack (NE.toList ts))) NE.:| [])
    convertErrorItem (Label la) = Label la
    convertErrorItem EndOfInput = EndOfInput

prettifyTokens :: [SQLToken] -> T.Text
prettifyTokens = prettyTokens ansi2011

prettifyToken :: SQLToken -> T.Text
prettifyToken = prettyToken ansi2011
