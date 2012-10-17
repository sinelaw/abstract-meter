module Main
where
 
import Text.XML.HXT.Core
import Text.XML.HXT.Curl -- use libcurl for HTTP access
                         -- only necessary when reading http://...
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as M
import Data.Hashable (Hashable)
import Data.Tree.NTree.TypeDefs 
import System.Environment
import qualified Data.Vector as Vector
import Data.Maybe (isJust, fromJust)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BS

--instance XmlPickler Aeson.Value where
--  xpickle = xpJSON
    
--xpJSON :: PU Aeson.Value
--xpJSON = xpElem "
  
main :: IO ()
main
    = do
      [src] <- getArgs
      [rootElem] <- runX ( readDocument [withValidate no
                                      ,withCurl []
                                      ] src
                         >>> getChildren 
                         >>> isElem
                       )
      BS.putStr . Aeson.encode $ xmlTreeToJSON rootElem
      return ()
      
concatMapValues :: (Eq k, Hashable k) => [M.HashMap k v] -> M.HashMap k [v] 
concatMapValues = foldr (M.unionWith (++) . M.map (:[])) M.empty 

fromObject :: Aeson.Value -> Maybe Aeson.Object
fromObject (Aeson.Object objMap) = Just objMap
fromObject _                     = Nothing

getAttrVals :: XmlTree -> [(String, String)]
getAttrVals node = runLA (getAttrl >>> getName &&& (getChildren >>> getText)) node

xmlTreeToJSON :: XmlTree -> Maybe (T.Text, Aeson.Value)
xmlTreeToJSON node@(NTree (XTag qName attrs) children) = Just (T.pack (localPart qName), 
                                                          Aeson.Object objMap)
  where objMap = arrayValuesToJSONArrays . concatMapValues -- unify into a single object, 
                                                           -- grouping into arrays by pair name
               . map (uncurry M.singleton)    -- convert pairs to json objects
               . (++) attrVals
               . map fromJust . filter isJust -- filter out the nothings (unconvertable nodes)
               $ map xmlTreeToJSON children  -- convert xml nodes to (name, json value) pairs

        arrayValuesToJSONArrays :: (Eq k, Hashable k) => M.HashMap k [Aeson.Value] -> M.HashMap k Aeson.Value
        arrayValuesToJSONArrays = M.map fromJust
                                . M.filter isJust
                                . M.map (\v -> case v of 
                                            []  -> Nothing -- will be discarded
                                            [x] -> Just x  -- don't store as array, just a single value
                                            xss -> Just $ Aeson.Array . Vector.fromList $ xss) -- arrays with more than one element are kept
                                
        -- apply on object internal maps, ignore non-object json values (don't apply on arrays)
        fmapObj :: (Aeson.Object -> Aeson.Object) -> Aeson.Value -> Aeson.Value
        fmapObj f (Aeson.Object objMap) = Aeson.Object (f objMap)
        fmapObj f val                   = val 
        
        attrVals = map (\(name, val) -> (T.pack name, Aeson.String (T.pack val))) $ getAttrVals node        

xmlTreeToJSON (NTree _ children) = Nothing 
                

          
  --                          foldr addChild singleton children
  -- where
  --   addChild :: (NTree XmlTree) -> Aeson.Object -> Aeson.Object
  --   addChild tree@(NTree (XTag childName attrs) _) childLists = 
  --     case (HMap.lookup (== child) ) of
  --       Nothing                -> (xmlTreeToJSON tree) : childLists
  --       Just existingChildName -> 


  