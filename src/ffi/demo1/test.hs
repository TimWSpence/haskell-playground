{-# LINE 1 "test.hsc" #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}

import System.IO.Unsafe

import Foreign
import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types



data MyStruct = MyStruct {
  foo :: Int
, bar :: CString
} deriving (Show)

instance Storable MyStruct where
  sizeOf _ = (16)
{-# LINE 20 "test.hsc" #-}
  alignment _ = 8
{-# LINE 21 "test.hsc" #-}
  peek ptr = do
    foo <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 23 "test.hsc" #-}
    bar <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 24 "test.hsc" #-}
    return MyStruct{foo = foo, bar = bar}
  poke ptr (MyStruct{..}) = do
    (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr foo
{-# LINE 27 "test.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr bar
{-# LINE 28 "test.hsc" #-}

foreign import ccall unsafe "test.h project" c_project :: Ptr MyStruct -> CString

foreign import ccall unsafe "test.h make" c_make :: Int -> CString -> Ptr MyStruct

project :: MyStruct -> String
project s = unsafePerformIO $ with s $ \ptr -> peekCString . c_project $ ptr

make :: Int -> String -> ForeignPtr MyStruct
make foo bar = unsafePerformIO $ do
  s <- newCString bar
  let st = c_make foo s
  --let finalize = FunPtr $ \ptr -> do
  --  free s
  --  free ptr
  peek st >>= print
  newForeignPtr finalizerFree st
