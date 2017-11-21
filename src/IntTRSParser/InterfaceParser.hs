module InterfaceParser where

import Interface
import Text.JSON
import Text.JSON.String
import PolyParser (parse)

getObjField :: JSON a => String -> JSValue -> a
getObjField id (JSObject ob) =
    case valFromObj id ob of
      Ok a -> a
      Error err -> error err
getObjField _ x = error $ "getObjField: expected " ++ show x ++ " to be an object!"


polyFromString :: String -> Maybe Polynomial
polyFromString = parse

ifaceFromJSON :: String -> FunIface
ifaceFromJSON s = case runGetJSON readJSObject s of
                    Left err -> error err
                    Right o -> 
                        let name = getObjField "name" o
                            varNames = getObjField "varNames" o
                            isPure = getObjField "isPure" o
                            secureArgs = getObjField "secureArgs" o
                            runTime = polyFromString $ getObjField "runTime" o
                            runSpace = polyFromString $ getObjField "runSpace" o
                            outputSize = polyFromString $ getObjField "outputSize" o
                        in
                          FunIface name varNames isPure secureArgs runTime runSpace outputSize
