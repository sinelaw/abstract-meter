module Main
where
 
import Text.XML.HXT.Core
import Text.XML.HXT.Curl -- use libcurl for HTTP access
                         -- only necessary when reading http://...
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import Data.Tree.NTree.TypeDefs 
import System.Environment
import qualified Data.Vector as Vector

import qualified Data.Text as T
 

--instance XmlPickler Aeson.Value where
--  xpickle = xpJSON
    
--xpJSON :: PU Aeson.Value
--xpJSON = xpElem "
  
main :: IO ()
main
    = do
      [src, dst] <- getArgs
      [rootElem] <- runX ( readDocument [withValidate no
                                      ,withCurl []
                                      ] src
                         >>> getChildren 
                         >>> isElem
                       )
      print $ xmlTreeToJSON rootElem
      return ()
      
liftObject :: (HashMap.HashMap T.Text Aeson.Value -> HashMap.HashMap T.Text Aeson.Value) -> Aeson.Value -> Aeson.Value
liftObject f (Aeson.Object map) = Aeson.Object (f map)
liftObject f _ = error "Can only process json object values"

concatMapValues :: (Eq k, Hashable k) => [HashMap.HashMap k v] -> HashMap.HashMap k [v] 
concatMapValues maps = foldr concatMapValue' HashMap.empty maps
  where concatMapValue' map res = HashMap.unionWith (++) (HashMap.map (:[]) map) res
  
xmlTreeToJSON :: XmlTree -> Aeson.Value
xmlTreeToJSON (NTree (XTag qName attrs) children) = Aeson.object [((T.pack $ show qName), 
                                                                   liftObject (arrayValuesToJSONArrays . concatMapValues) $ map xmlTreeToJSON children
                                                                   )]
  where arrayValuesToJSONArrays :: (Eq k, Hashable k) => HashMap.HashMap k [Aeson.Value] -> HashMap.HashMap k Aeson.Value
        arrayValuesToJSONArrays map = HashMap.map (Aeson.Array . Vector.fromList) map
          
xmlTreeToJSON (NTree _ children) = Aeson.Null 
  --                          foldr addChild singleton children
  -- where
  --   addChild :: (NTree XmlTree) -> Aeson.Object -> Aeson.Object
  --   addChild tree@(NTree (XTag childName attrs) _) childLists = 
  --     case (HMap.lookup (== child) ) of
  --       Nothing                -> (xmlTreeToJSON tree) : childLists
  --       Just existingChildName -> 


  