{-# LANGUAGE OverloadedStrings #-}

module Analyses.Xss
  ( analyse
  ) where

import           Data.Incident
import           Data.List                  (elem)
import           Data.Log
import           Data.Text
import           Data.Text.Internal.Builder (toLazyText)
import           Data.Text.Lazy             (toStrict)
import           HTMLEntities.Decoder       (htmlEncodedText)
import           Network.HTTP.Base          (urlDecode)
import           Text.HTML.Parser           (Attr (..), AttrName, TagName,
                                             Token (..), parseTokens)

analyse :: Log -> Maybe Incident
analyse a
  | not isAllowed = Just Incident {reason = "XSS attack", Data.Incident.log = a}
  | otherwise = Nothing
  where
    isAllowed =
      Prelude.foldl (\x xs -> x && checkIfAllElementIsAllowed xs) True $
      parseTokens . decodeEntity . request $ a
    checkIfAllElementIsAllowed :: Token -> Bool
    checkIfAllElementIsAllowed (TagOpen name attributes) =
      name `elem` allowedElemets &&
      Prelude.foldl
        (\x xs -> x && checkIfAtttributeIsAllowed xs)
        True
        attributes
    checkIfAllElementIsAllowed _ = True
    checkIfAtttributeIsAllowed :: Attr -> Bool
    checkIfAtttributeIsAllowed (Attr name value) =
      name `elem` allowedAttibutes && checkIfValuesIsAllowed value
    checkIfValuesIsAllowed :: Text -> Bool
    checkIfValuesIsAllowed "" = True
    checkIfValuesIsAllowed value
      | Prelude.foldl (\x xs -> x || xs `isInfixOf` value) True disallowedValues =
        False
      | otherwise = True
    decodeEntity :: Text -> Text
    decodeEntity =
      toStrict . toLazyText . htmlEncodedText . pack . urlDecode . unpack

allowedElemets :: [Text.HTML.Parser.TagName]
allowedElemets =
  [ "a"
  , "base"
  , "html"
  , "head"
  , "link"
  , "meta"
  , "style"
  , "title"
  , "address"
  , "article"
  , "aside"
  , "footer"
  , "h1"
  , "h2"
  , "h3"
  , "h4"
  , "h5"
  , "h6"
  , "header"
  , "hgroup"
  , "nav"
  , "section"
  , "blockquote"
  , "dd"
  , "div"
  , "dl"
  , "dt"
  , "figcaption"
  , "figure"
  , "hr"
  , "li"
  , "main"
  , "ol"
  , "p"
  , "pre"
  , "ul"
  , "a"
  , "abbr"
  , "b"
  , "bdi"
  , "bdo"
  , "br"
  , "cite"
  , "code"
  , "data"
  , "dfn"
  , "em"
  , "i"
  , "kbd"
  , "mark"
  , "q"
  , "rp"
  , "rt"
  , "rtc"
  , "ruby"
  , "s"
  , "samp"
  , "small"
  , "span"
  , "strong"
  , "sub"
  , "sup"
  , "time"
  , "u"
  , "var"
  , "wbr"
  , "area"
  , "audio"
  , "img"
  , "map"
  , "track"
  , "video"
  , "source"
  , "embed"
  , "object"
  , "param"
  , "canvas"
  , "noscript"
  , "del"
  , "ins"
  , "caption"
  , "col"
  , "colgorup"
  , "table"
  , "tbody"
  , "td"
  , "tfoot"
  , "th"
  , "thead"
  , "tr"
  , "button"
  , "datalist"
  , "fieldset"
  , "form"
  , "input"
  , "label"
  , "legend"
  , "meter"
  , "optgroup"
  , "option"
  , "output"
  , "progress"
  , "select"
  , "textarea"
  , "details"
  , "dialog"
  , "menu"
  , "menuitem"
  , "summary"
  , "slot"
  , "template"
  ]

allowedAttibutes :: [Text.HTML.Parser.AttrName]
allowedAttibutes =
  [ "src"
  , "accept"
  , "accept-charset"
  , "accesskey"
  , "action"
  , "align"
  , "alt"
  , "autocomplete"
  , "autofocus"
  , "autoplay"
  , "autosave"
  , "bgcolor"
  , "border"
  , "buffered"
  , "charset"
  , "checked"
  , "cite"
  , "class"
  , "codebase"
  , "color"
  , "cols"
  , "colspan"
  , "content"
  , "contenteditable"
  , "contextmenu"
  , "controls"
  , "cords"
  , "data"
  , "datetime"
  , "default"
  , "dir"
  , "dirname"
  , "disabled"
  , "download"
  , "draggable"
  , "dropzone"
  , "enctype"
  , "for"
  , "form"
  , "formaction"
  , "headers"
  , "height"
  , "hidden"
  , "high"
  , "href"
  , "hreflang"
  , "http-equiv"
  , "icon"
  , "id"
  , "integrity"
  , "ismap"
  , "itemprop"
  , "keytype"
  , "kind"
  , "label"
  , "lang"
  , "list"
  , "loop"
  , "low"
  , "manifest"
  , "max"
  , "maxLength"
  , "minLength"
  , "media"
  , "method"
  , "min"
  , "multiple"
  , "muted"
  , "name"
  , "novalidate"
  , "open"
  , "optimum"
  , "pattern"
  , "ping"
  , "placeholder"
  , "poster"
  , "preload"
  , "radiogroup"
  , "readonly"
  , "rel"
  , "required"
  , "reversed"
  , "rows"
  , "rowspan"
  , "scope"
  , "scoped"
  , "selected"
  , "shape"
  , "size"
  , "sizes"
  , "slot"
  , "span"
  , "spellcheck"
  , "srclang"
  , "srcset"
  , "start"
  , "step"
  , "style"
  , "summary"
  , "tabinedx"
  , "target"
  , "title"
  , "type"
  , "usemap"
  , "value"
  , "width"
  , "wrap"
  ]

disallowedValues :: [Text]
disallowedValues =
  [ "script"
  , "javascript"
  , "vbscript"
  , "expression"
  , "applet"
  , "embed"
  , "iframe"
  , "frame"
  , "frameset"
  ]
