module Test.Framework.Runners.XML.HostName (getHostName) where

import Foreign.C.Error
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Array

#ifdef WINDOWS

import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

import System.Win32.Types

foreign import stdcall unsafe "windows.h GetComputerNameExW" getComputerNameEx :: COMPUTER_NAME_FORMAT -> LPTSTR -> LPDWORD -> IO BOOL

type COMPUTER_NAME_FORMAT = CInt

computerNamePhysicalDnsHostname :: COMPUTER_NAME_FORMAT
computerNamePhysicalDnsHostname = 5

getHostName :: IO HostName
getHostName = with 0 $ \charcount -> do
    -- On the first run, determine the character count and ignore any error we get
    _ <- getComputerNameEx computerNamePhysicalDnsHostname nullPtr p_charcount
    charcount <- peek p_charcount
    
    -- The second time around, use the correct character count to retrieve the data
    withTString (replicate (fromIntegral charcount) ' ') $ \name -> do
        failIfFalse_ "GetComputerNameExW" $ getComputerNameEx computerNamePhysicalDnsHostname name p_charcount
        peekTString name

#else

foreign import ccall unsafe "gethostname" gethostname :: CString -> CSize -> IO CInt

getHostName :: IO HostName
getHostName = allocaArray0 size $ \cstr -> do
        throwErrnoIfMinus1_ "getHostName" $ gethostname cstr (fromIntegral size)
        peekCString cstr
    where size = 256

#endif

type HostName = String
