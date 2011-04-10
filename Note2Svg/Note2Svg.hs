import Data.Maybe
import Text.XML.Light
import Text.XML.Light.Cursor
import System.Environment

main :: IO ()
main = do
    [f] <- getArgs
    putStrLn =<< note2img `fmap` readFile f

note2img :: String -> String
note2img = showTopElement . removeNamespaces . fromJust . filterElementName ((== "svg") . qName) . fromJust . parseXMLDoc

removeNamespaces :: Element -> Element
removeNamespaces = cont2el . toTree . walk removeNamespace . fromElement

cont2el :: Content -> Element
cont2el (Elem e) = e
cont2el _ = error "not an Elem"

walk :: (Content -> Content) -> Cursor -> Cursor
walk f c = maybe c' (walk f) $ nextDF c'
    where c' = modifyContent f c

removeNamespace :: Content -> Content
removeNamespace (Elem e) = Elem $ e { elName = unqual $ qName $ elName e }
removeNamespace x = x
