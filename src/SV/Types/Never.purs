module SV.Light.Types.Never where

import SV.Prelude

import SecureVote.Utils.Json (mkFErr)
import Simple.JSON (class ReadForeign)



foreign import data Never :: Type
instance rfNever :: ReadForeign Never where
    readImpl _ = mkFErr "Cannot decode 'Never' value"
instance showNever :: Show Never where
    show _ = "( Never )"
instance eqNever :: Eq Never where
    eq _ _ = true
