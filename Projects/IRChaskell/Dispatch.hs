module Dispatch (loginThreadWrapper) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Data.Map as M hiding (filter, map)
import Debug.Trace
import System.IO

import Prelude hiding (catch)

import DataStores
import Message

-- This module is responsible for broadcasting messages
-- to the proper users.

-- wrap the thread in a function - necessary because loginThread uses
-- recursion to continue. if we didn't wrap this, the function would
-- add another finally clause every time the function was called.
loginThreadWrapper userStore roomStore handle =
  loginThread userStore roomStore handle
  `finally`
  (trace "Doing final cleanup." (hClose handle))

-- waits on the provided handle for the user's login message
loginThread :: UserStore ->
               RoomStore ->
               Handle ->
               IO ()
loginThread userStore roomStore handle = do
  let repeat = loginThread userStore roomStore handle in do
    msg <- unsafeReadMessage handle
    (responseMsg, cont) <- do
      case msg of
        Login name -> do
          user <- atomically $ tryLogin userStore name handle
          case user of
            Just u ->
              return (Info $ "Logged in as " ++ name, trace (name ++ " logged in") $
                          dispatcherThreadWrapper userStore roomStore handle u)
            Nothing ->
              return (Error "Username already in use", repeat)
        otherwise ->
          return (Error "Not logged in", repeat)
    unsafePutMsg handle responseMsg
    cont

-- same comment above as loginThreadWrapper - recursion forces us to
-- wrap this
dispatcherThreadWrapper :: UserStore ->
                           RoomStore ->
                           Handle ->
                           User ->
                           IO ()
dispatcherThreadWrapper userStore roomStore handle user =
  dispatcherThread userStore roomStore handle user
  `finally` do
    atomically $ logout userStore roomStore user
    trace ("Thread for " ++ (userName user) ++ " dying.") $ return ()

-- This is the main handler loop for a client. It is fairly straightforward
-- except for one thing: each one of the functions to process a
-- message return a message to forward to this user, and an IO
-- thunk containing the rest of the stuff to do (forwarding
-- messages to other users, etc). We separate them so that we
-- can respond to the user quickly and do all of the heavy lifting
-- of dispatching messages in a different thread.
dispatcherThread :: UserStore ->
                    RoomStore ->
                    Handle ->
                    User ->
                    IO ()
dispatcherThread userStore roomStore handle user =
  let repeat = dispatcherThread userStore roomStore handle in do
    msg <- unsafeReadMessage handle
    (responseMsg, cont) <- atomically $ do
      case msg of
        Login _ ->
          return (Error "Already logged in", repeat user)
        SPrivateMessage to msg ->
          privateMessage userStore user to msg repeat
        SRoomMessage room msg ->
          roomMessage roomStore user room msg repeat
        Join room ->
          joinRoom userStore roomStore user room repeat
        Blank ->
          return (Ok, repeat user)
        Part room ->
          partRoom userStore roomStore user room repeat
        Logout ->
          -- we don't need to call logout explicity
          -- (see the finally block above, and exception handler below)
          logout userStore roomStore user
        Invalid err ->
          return (Error ("Invalid Command: " ++ err), repeat user)
    sendMessages [(connection user, responseMsg)]
    cont

tryLogin :: UserStore ->
            String ->
            Handle ->
            STM (Maybe User)
tryLogin userStore name handle = do
    user <- maybeGrabFromSTM userStore name
    case user of
      Just u -> return Nothing
      Nothing -> do
        newLock <- newTMVar handle
        let newUser = User {
              userName = name,
              connection = newLock,
              rooms = []
            }
        updateSTM userStore newUser
        return (Just newUser)

logout :: UserStore ->
          RoomStore ->
          User ->
          STM (ClientMessage, IO ())
logout userStore roomStore user = do
  userMap <- readTVar userStore
  writeTVar userStore (M.delete (userName user) userMap)
  return (Ok, trace ((userName user) ++ " has left")
              (atomically $ removeUserFromRooms user userStore roomStore))

privateMessage :: UserStore ->
                  User ->
                  String ->
                  String ->
                  (User -> IO ()) ->
                  STM (ClientMessage, IO ())
privateMessage userStore from toName msg cont = do
  maybeUser <- maybeGrabFromSTM userStore toName
  case maybeUser of
    Just toUser ->
      return (Ok,
              (sendMessages
               [(buildPrivateMessage toUser from msg)]) >>
              cont from)
    Nothing -> return (Error "User is not logged in", cont from)

roomMessage :: RoomStore ->
               User ->
               String ->
               String ->
               (User -> IO ()) ->
               STM (ClientMessage, IO ())
roomMessage roomStore user toRoom msg cont = do
  maybeRoom <- maybeGrabFromSTM roomStore toRoom
  case maybeRoom of
    Just room ->
      if user `elem` (users room)
         then return (Ok,
                      (sendMessages (buildRoomMessages room user msg)) >>
                      cont user)
         else return (Error ("Not in room: " ++ (roomName room)), cont user)
    Nothing -> return (Error ("Not in room: " ++ toRoom), cont user)

joinRoom :: UserStore ->
            RoomStore ->
            User ->
            String ->
            (User -> IO ()) ->
            STM (ClientMessage, IO ())
joinRoom userStore roomStore user roomName cont = do
  (newUser, wasSuccessful) <- addUserToRoom userStore roomStore user roomName
  if wasSuccessful
    then return (Ok, cont newUser)
    else return (Error ("Already in room: #" ++ roomName), cont user)

partRoom :: UserStore ->
            RoomStore ->
            User ->
            String ->
            (User -> IO ()) ->
            STM (ClientMessage, IO ())
partRoom userStore roomStore user roomName cont = do
  (newUser, wasSuccessful) <- removeUserFromRoom userStore roomStore user roomName
  if wasSuccessful
    then return (Ok, cont newUser)
    else return (Error ("Not in room: #" ++ roomName), cont user)

-- send a list of messages. this spawns another thread so that
-- our main handler thread doesn't need to wait for it to finish.
-- uses safePutMsg to ensure that the messages are sent in the
-- proper order.
sendMessages :: [(TMVar Handle, ClientMessage)] ->
                IO ThreadId
sendMessages = forkIO .
               Prelude.foldr (>>) (return ()) .
               map (\(h, msg) -> safePutMsg h msg)

-- each user has an MVar () which is used as a mutex to
-- ensure no interleaving of messages.
safePutMsg :: TMVar Handle -> ClientMessage -> IO ()
safePutMsg lock msg = do
  handle <- atomically $ takeTMVar lock
  unsafePutMsg handle msg
  atomically $ putTMVar lock handle

-- puts a message on a handle without a lock. used
-- in the login thread (since we haven't created an
-- MVar for them yet)
unsafePutMsg :: Handle -> ClientMessage -> IO ()
unsafePutMsg handle msg = hPutStrLn handle (show msg)

-- grabs the next message from the handle and parses it
unsafeReadMessage :: Handle -> IO ServerMessage
unsafeReadMessage handle = do
  line <- hGetLine handle
  let msg = parseMsg line in return msg

buildPrivateMessage :: User ->
                       User ->
                       String ->
                       (TMVar Handle, ClientMessage)
buildPrivateMessage to from msg =
  let cMessage = CPrivateMessage (userName from) msg
      conn = connection to in
    (conn `seq` conn, cMessage `seq` cMessage)

buildRoomMessages :: Room ->
                     User ->
                     String ->
                     [(TMVar Handle, ClientMessage)]
buildRoomMessages room from msg =
  map (\u ->
          let cMessage = CRoomMessage (userName from) (roomName room) msg
              conn = connection u in
            (conn `seq` conn, cMessage `seq` cMessage))
  (filter (/= from) (users room))
