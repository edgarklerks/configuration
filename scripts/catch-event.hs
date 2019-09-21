#!env ghc

import Graphics.X11.Types
import Graphics.X11.Xlib.Event
import Graphics.X11.Xlib.Display
import Graphics.X11.XInput.Types
import Graphics.X11.XInput.Functions
import Data.Bits
import Graphics.X11.Xlib.Extras as E
import Control.Applicative 


main :: IO ()
main = do
  print "hello"
  disp <- openDisplay ":0"
  blub <- xinputInit disp 
  allocaXEvent $ \ptr -> 
    case blub of
        InitOK opcode -> handleXCookie disp opcode ptr doEvent nopEvent
        _ -> error "Couldn't initialize thing"

  closeDisplay disp
nopEvent :: a -> IO ()
nopEvent _ = return ()
doEvent :: E.Event -> IO ()
doEvent e = case eventButton e <|> eventMousePos e of
               Nothing -> return ()
               Just a -> error "Done"
             
