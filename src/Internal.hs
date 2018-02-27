{-# LANGUAGE CPP #-}

module Internal
    ( bsToStrict
    , bsFromStrict
    , (<$>)
    ) where

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative
#endif

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BS.L

{-# INLINE bsToStrict #-}
bsToStrict :: BS.L.ByteString -> BS.ByteString
#if MIN_VERSION_bytestring(0,10,0)
bsToStrict = BS.L.toStrict
#else
bsToStrict = BS.concat . BS.L.toChunks
#endif

{-# INLINE bsFromStrict #-}
bsFromStrict :: BS.ByteString -> BS.L.ByteString
#if MIN_VERSION_bytestring(0,10,0)
bsFromStrict = BS.L.fromStrict
#else
bsFromStrict = BS.L.fromChunks . (:[])
#endif
