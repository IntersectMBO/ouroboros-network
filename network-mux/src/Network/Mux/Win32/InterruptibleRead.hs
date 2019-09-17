{-# LANGUAGE CApiFFI             #-}
{-# LANGUAGE InterruptibleFFI    #-}
{-# LANGUAGE RecordWildCards     #-}

module Network.Mux.Win32.InterruptibleRead (
    iread
  ) where


import           Control.Monad.Class.MonadThrow
import           Data.Word
import           Data.ByteString.Internal (createAndTrim)
import qualified Data.ByteString as BS
import           Foreign
import           Foreign.C
import           GHC.IO.Handle.Types (Handle__(..))
import           GHC.IO.FD (fdFD)
import           GHC.IO.Handle.Internals (wantReadableHandle_)
import           System.IO (Handle)
import           System.IO.Error (isEOFError)
import           Data.Typeable

import Text.Printf

iread :: Handle
      -> Int
      -> IO BS.ByteString
iread h len
    | len < 0   = do
        printf "negative iread\n"
        return BS.empty
    | otherwise = createAndTrim len $ \ptr ->
        catch (ireadBuf h ptr len)
                (\e -> if isEOFError e then return 0 else throwM e)

ireadBuf :: Handle
        -> Ptr Word8
        -> Int
        -> IO Int
ireadBuf h ptr len = do
  wantReadableHandle_ "iread" h $ \ Handle__{..} -> do
    let Just fd = cast haDevice
    r <- c_read (fdFD fd) ptr (fromIntegral len)
    printf "FD %s: read %s\n" (show fd) (show r)
    return $ fromIntegral r

foreign import capi interruptible "HsBase.h _read"
   c_read :: CInt -> Ptr Word8 -> CUInt -> IO CInt

