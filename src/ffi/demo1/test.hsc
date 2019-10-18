{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}

import System.IO.Unsafe

import Foreign
import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types

#include "test.h"

data MyStruct = MyStruct {
  foo :: Int
, bar :: CString
} deriving (Show)

instance Storable MyStruct where
  sizeOf _ = #{size mystruct}
  alignment _ = #{alignment mystruct}
  peek ptr = do
    foo <- #{peek mystruct, foo} ptr
    bar <- #{peek mystruct, bar} ptr
    return MyStruct{foo = foo, bar = bar}
  poke ptr (MyStruct{..}) = do
    #{poke mystruct, foo} ptr foo
    #{poke mystruct, bar} ptr bar

foreign import ccall unsafe "test.h project" c_project :: Ptr MyStruct -> CString

foreign import ccall unsafe "test.h make" c_make :: Int -> CString -> Ptr MyStruct

project :: MyStruct -> String
project s = unsafePerformIO $ with s $ \ptr -> peekCString . c_project $ ptr

make :: Int -> String -> ForeignPtr MyStruct
make foo bar = unsafePerformIO $ do
  s <- newCString bar --TODO this currently leaks memory
  let st = c_make foo s
  --let finalize = FunPtr $ \ptr -> do
  --  free s
  --  free ptr
  peek st >>= print --TODO new to convert CString -> String and work out why CInt doesn't print correctly
  newForeignPtr finalizerFree st
