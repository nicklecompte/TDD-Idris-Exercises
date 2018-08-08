import Data.Primitives.Views
import System

data Input = Answer Int |
             QuitCmd

data Command : Type -> Type where
    PutStr : String -> Command ()
    GetLine : Command String
    Pure : ty -> Command ty
    Bind : Command a -> (a -> Command b) -> Command b
    
namespace CommandDo
    (>>=) : Command a -> (a -> Command b) -> Command b
    (>>=) = Bind    


Functor Command where
  map func x = Bind (x) (\a => Pure (func a))

Applicative Command where
  pure x = Pure x
  (<*>) x y = Bind (x) (\lamAtoB => Bind y (\aVal => Pure (lamAtoB aVal)))

Monad Command where
  (>>=) x f = Bind x f
  join x = Bind x (id) 

runCommand : Command a -> IO a
runCommand (PutStr x) = putStr x
runCommand GetLine = getLine
runCommand (Pure x) = pure x
runCommand (Bind x f) = do res <- runCommand x
                           runCommand (f res)

data ConsoleIO : Type -> Type where
    Quit : a -> ConsoleIO a
    Do : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b

namespace ConsoleDo
    (>>=) : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b
    (>>=) = Do                               