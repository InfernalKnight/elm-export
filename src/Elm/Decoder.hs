{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Elm.Decoder
  ( toElmDecoderRef
  , toElmDecoderRefWith
  , toElmDecoderSource
  , toElmDecoderSourceWith
  ) where

import           Control.Monad.Reader
import           Data.Semigroup
import           Data.Text hiding (map)
import           Elm.Common
import           Elm.Type
import           Formatting

class HasDecoder a where
  render :: a -> Reader Options Text

class HasDecoderRef a where
  renderRef :: a -> Reader Options Text

instance HasDecoder ElmDatatype where
    render d@(ElmDatatype name mc@(MultipleConstructors _)) = do
      fnName <- renderRef d
      let fnBody  =
            "    in \"" <> toLower name <> "\" := string `andThen` stringTo" <> name
          catch = "            _ -> fail <| \"unexpected " <> name <> " -- \" ++ s"
          localFn = "    let stringTo" <> name <> " s ="
          caseHead = "        case s of"
          bot = sformat (cr % stext % cr % stext) catch fnBody
          top = sformat
            (stext % " : Decoder " % stext % cr % stext % " =" % cr %
             stext % cr % stext % stext)
            fnName
            name
            fnName
            localFn
            caseHead <$>
            render mc
      (flip mappend bot) <$> top
    render d@(ElmDatatype name constructor) = do
        fnName <- renderRef d
        sformat
            (stext % " : Decoder " % stext % cr % stext % " =" % cr % stext)
            fnName
            name
            fnName <$>
            render constructor
    render (ElmPrimitive primitive) = renderRef primitive

instance HasDecoderRef ElmDatatype where
    renderRef (ElmDatatype name _) =
        pure $ sformat ("decode" % stext) name

    renderRef (ElmPrimitive primitive) =
        renderRef primitive


instance HasDecoder ElmConstructor where
    render (NamedConstructor name value) =
        sformat ("    decode " % stext % cr % stext) name <$> render value
    render (RecordConstructor name value) =
        sformat ("    decode  " % stext % cr % stext) name <$> render value
    render (MultipleConstructors xs) =
      let renderLine t (NamedConstructor name value) =
            sformat (stext % cr % "            " %
                     "\"" % stext % "\" -> succeed " % stext % stext)
            t (toLower name) name <$> render value
          renderLine t (RecordConstructor name value) =
           sformat (stext % cr % stext % " record " % stext % stext)
           t (toLower name) name <$> render value
          renderLine t (MultipleConstructors ys) = foldM renderLine t ys
      in foldM renderLine mempty xs

instance HasDecoder ElmValue where
    render (ElmRef name) = pure (sformat ("decode" % stext) name)
    render (ElmPrimitiveRef primitive) = renderRef primitive
    render (Values x y) = sformat (stext % cr % stext) <$> render x <*> render y
    render (ElmField name value) = do
        fieldModifier <- asks fieldLabelModifier
        sformat
            ("        |> required \"" % stext % "\" " % stext)
            (fieldModifier name) <$>
            render value
    render ElmEmpty = pure mempty


instance HasDecoderRef ElmPrimitive where
    renderRef (EList (ElmPrimitive EChar)) = pure "string"
    renderRef (EList datatype) =
        sformat ("(list " % stext % ")") <$> renderRef datatype
    renderRef (EDict key value) =
        sformat ("(map Dict.fromList " % stext % ")") <$>
        renderRef (EList (ElmPrimitive (ETuple2 (ElmPrimitive key) value)))
    renderRef (EMaybe datatype) =
        sformat ("(maybe " % stext % ")") <$> renderRef datatype
    renderRef (ETuple2 x y) =
        sformat ("(tuple2 (,) " % stext % " " % stext % ")") <$> renderRef x <*>
        renderRef y
    renderRef EUnit = pure "(succeed ())"
    renderRef EDate = pure "(customDecoder string Date.fromString)"
    renderRef EInt = pure "int"
    renderRef EBool = pure "bool"
    renderRef EChar = pure "char"
    renderRef EFloat = pure "float"
    renderRef EString = pure "string"
    renderRef EUuid = pure
      "(customDecoder string (Result.FromMaybe \"Error decoding Uuid\" << Uuid.fromString))"


toElmDecoderRefWith :: ElmType a => Options -> a -> Text
toElmDecoderRefWith options x = runReader (renderRef (toElmType x)) options


toElmDecoderRef :: ElmType a => a -> Text
toElmDecoderRef = toElmDecoderRefWith defaultOptions


toElmDecoderSourceWith :: ElmType a => Options -> a -> Text
toElmDecoderSourceWith options x = runReader (render (toElmType x)) options


toElmDecoderSource :: ElmType a => a -> Text
toElmDecoderSource = toElmDecoderSourceWith defaultOptions
