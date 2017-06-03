{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Text.Html.Nice.Writer.Html5 where
import qualified Language.Haskell.TH     as TH
import           Text.Html.Nice.Internal (AttrName)
import           Text.Html.Nice.Writer   (Markup, makeElement, makeVoidElement)

$(let
    parents :: [String]
    parents =
      [ "a", "abbr", "address", "article", "aside", "audio", "b"
      , "bdo", "blockquote", "body", "button", "canvas", "caption", "cite"
      , "code", "colgroup", "command", "datalist", "dd", "del", "details"
      , "dfn", "div", "dl", "dt", "em", "fieldset", "figcaption", "figure"
      , "footer", "form", "h1", "h2", "h3", "h4", "h5", "h6", "head", "header"
      , "hgroup", "html", "i", "iframe", "ins", "kbd", "label"
      , "legend", "li", "main", "map", "mark", "menu", "meter", "nav"
      , "noscript", "object", "ol", "optgroup", "option", "output", "p"
      , "pre", "progress", "q", "rp", "rt", "ruby", "samp", "script"
      , "section", "select", "small", "span", "strong", "style", "sub"
      , "summary", "sup", "table", "tbody", "td", "textarea", "tfoot", "th"
      , "thead", "time", "title", "tr", "ul", "var", "video"
      ]

    leafs :: [String]
    leafs =
      [ "area", "base", "br", "col", "embed", "hr", "img", "input", "keygen"
      , "link", "menuitem", "meta", "param", "source", "track", "wbr"
      ]

    attributes :: [String]
    attributes =
        [ "accept", "accept-charset", "accesskey", "action", "alt", "async"
        , "autocomplete", "autofocus", "autoplay", "challenge", "charset"
        , "checked", "cite", "class", "cols", "colspan", "content"
        , "contenteditable", "contextmenu", "controls", "coords", "data"
        , "datetime", "defer", "dir", "disabled", "draggable", "enctype", "for"
        , "form", "formaction", "formenctype", "formmethod", "formnovalidate"
        , "formtarget", "headers", "height", "hidden", "high", "href"
        , "hreflang", "http-equiv", "icon", "id", "ismap", "item", "itemprop"
        , "itemscope", "itemtype"
        , "keytype", "label", "lang", "list", "loop", "low", "manifest", "max"
        , "maxlength", "media", "method", "min", "multiple", "name"
        , "novalidate", "onbeforeonload", "onbeforeprint", "onblur", "oncanplay"
        , "oncanplaythrough", "onchange", "oncontextmenu", "onclick"
        , "ondblclick", "ondrag", "ondragend", "ondragenter", "ondragleave"
        , "ondragover", "ondragstart", "ondrop", "ondurationchange", "onemptied"
        , "onended", "onerror", "onfocus", "onformchange", "onforminput"
        , "onhaschange", "oninput", "oninvalid", "onkeydown", "onkeyup"
        , "onload", "onloadeddata", "onloadedmetadata", "onloadstart"
        , "onmessage", "onmousedown", "onmousemove", "onmouseout", "onmouseover"
        , "onmouseup", "onmousewheel", "ononline", "onpagehide", "onpageshow"
        , "onpause", "onplay", "onplaying", "onprogress", "onpropstate"
        , "onratechange", "onreadystatechange", "onredo", "onresize", "onscroll"
        , "onseeked", "onseeking", "onselect", "onstalled", "onstorage"
        , "onsubmit", "onsuspend", "ontimeupdate", "onundo", "onunload"
        , "onvolumechange", "onwaiting", "open", "optimum", "pattern", "ping"
        , "placeholder", "preload", "pubdate", "radiogroup", "readonly", "rel"
        , "required", "reversed", "rows", "rowspan", "sandbox", "scope"
        , "scoped", "seamless", "selected", "shape", "size", "sizes", "span"
        , "spellcheck", "src", "srcdoc", "start", "step", "style", "subject"
        , "summary", "tabindex", "target", "title", "type", "usemap", "value"
        , "width", "wrap", "xmlns"
        ]

    fun :: TH.Name -> String -> TH.ExpQ
    fun f x = TH.appE (TH.varE f) (TH.stringE x)

    hs :: Char -> Char
    hs '-' = '_'
    hs a = a

    parentQ :: String -> TH.DecsQ
    parentQ name = do
      decName <- TH.newName (map hs name ++ "_")
      sig <- TH.sigD decName [t| forall t a. Markup t a -> Markup t a |]
      val <- TH.funD decName [TH.clause [] (TH.normalB (fun 'makeElement name)) []]
      return [sig, val]

    voidQ :: String -> TH.DecsQ
    voidQ name = do
      decName <- TH.newName (map hs name ++ "_")
      sig <- TH.sigD decName [t| forall t. Markup t () |]
      val <- TH.funD decName [TH.clause [] (TH.normalB (fun 'makeVoidElement name)) []]
      return [sig, val]

    attrQ :: String -> TH.DecsQ
    attrQ name = do
      decName <- TH.newName (map hs name ++ if elem name (parents ++ leafs)
                                            then "__"
                                            else "_")
      sig <- TH.sigD decName [t| AttrName |]
      val <- TH.funD decName [TH.clause [] (TH.normalB (TH.stringE name)) []]
      return [sig, val]

  in concat <$> sequence
     [ fmap concat (mapM parentQ parents)
     , fmap concat (mapM voidQ leafs)
     , fmap concat (mapM attrQ attributes)
     ])

