module ImportExport where

import Container
import Vec3
import Impulse
import Scene

import Text.JSON

instance (JSON a, Fractional a) => JSON (C3 a) where
    showJSON (C3 x y z) = JSObject $ toJSObject [("C3", JSArray [showJSON x, showJSON y, showJSON z])]
    readJSON (JSObject obj) = match $ fromJSObject obj
        where   match [("C3", JSArray [JSRational _ a, JSRational _ b, JSRational _ c])] = 
                     Ok $ C3 (fromRational a) (fromRational b) (fromRational c)
                match _ = Error "expected object with field 'C3'"
    readJSON _ = Error "expected object with field 'C3'"

instance (JSON a, Fractional a) => JSON (Vec3 a) where
    showJSON (Vec3 x y z) = JSObject $ toJSObject [("x", showJSON x), ("y", showJSON y), ("z", showJSON z)]
    readJSON (JSObject obj) = match $ fromJSObject obj
        where   match [("x", JSRational _ x), ("y", JSRational _ y), ("z", JSRational _ z)] =
                    Ok $ Vec3 (fromRational x) (fromRational y) (fromRational z)
                match _ = Error "expected object with fields 'x' 'y' 'z'"
    readJSON _ = Error "expected object with fields 'x' 'y' 'z'"

instance JSON Impulse where
    showJSON (Impulse t a) = JSObject $ toJSObject [("p", showJSON t), ("a", showJSON a)]
    readJSON (JSObject obj) = match $ fromJSObject obj
        where   match [("p", JSRational _ p), ("a", a)] = do
                    amp <- readJSON a
                    return $ Impulse (fromRational p) amp
                match _ = Error "expected object with fields 'p' 'a'"
    readJSON _ = Error "expected object with fields 'p' 'a'"

instance JSON RayTrace where
    showJSON (RayTrace dir impulses) = JSObject $ toJSObject [("d", showJSON dir), ("i", showJSONs impulses)]
    readJSON (JSObject obj) = match $ fromJSObject obj
        where   match [("d", d), ("i", i)] = do
                    dir <- readJSON d
                    imp <- readJSONs i
                    return $ RayTrace dir imp
                match _ = Error "expected object with fields 'd' 'i'"
    readJSON _ = Error "expected object with fields 'd' 'i'"
