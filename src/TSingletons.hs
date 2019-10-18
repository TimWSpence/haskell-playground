{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}

module TSingletons () where

data DoorState = Open | Closed | Locked

data Sing (s :: DoorState) where
  SOpen :: Sing 'Open
  SClosed :: Sing 'Closed
  SLocked :: Sing 'Locked

data Door (s :: DoorState) = UnsafeMkDoor { getMaterial :: String }

class SingI (s :: DoorState) where
  sing :: Sing s

instance SingI 'Open where
  sing = SOpen

instance SingI 'Closed where
  sing = SClosed

instance SingI 'Locked where
  sing = SLocked

closeDoor :: Door 'Open -> Door 'Closed
closeDoor = UnsafeMkDoor . getMaterial

lockDoor :: Door 'Closed -> Door 'Locked
lockDoor = UnsafeMkDoor . getMaterial

lockAnyDoor_ :: Sing s -> Door s -> Door 'Locked
lockAnyDoor_ s = case s of
  SOpen -> lockDoor . closeDoor
  SClosed -> lockDoor
  SLocked -> id

lockAnyDoor :: SingI s => Door s -> Door 'Locked
lockAnyDoor = lockAnyDoor_ sing
