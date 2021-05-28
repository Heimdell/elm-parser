
module Core exposing (..)

type alias MonoidalAction s d c =
  { update : d -> s -> s
  , append : d -> d -> d
  , one    : d
  , empty  : d
  , next   : s -> Maybe (c, s)
  , grab   : d -> s -> s
  }

type alias Parser e s d c a = MonoidalAction s d c -> s -> (Result (Forest e) a, d, s)

type Tree e = Tree e (Forest e)

type alias Forest e = List (Tree e)

run : MonoidalAction s d c -> s -> Parser e s d c a -> Result (Forest e, d) a
run ctx s p =
  case p ctx s of
    (Ok a, _, _) -> Ok a
    (Err e, d, s1) -> Err (e, d)

at : d -> Forest e -> Result (Forest e, d) a
at d e = Err (e, d)

err : e -> Forest e
err e = [Tree e []]

with : Forest e -> e -> Forest e
with es e = [Tree e es]

noErr : Forest e
noErr = []

pure : a -> Parser e s d c a
pure a {empty} s = (Ok a, empty, s)

fail : e -> Parser e s d c a
fail e {empty} s = (Err <| err e, empty, s)

mapErrors : (Forest e -> Forest e) -> Parser e s d c a -> Parser e s d c a
mapErrors f p ctx s =
  case p ctx s of
    (Ok _, _, _) as result -> result
    (Err e, d, s1) -> (Err (f e), d, s1)

aka : e -> Parser e s d c a -> Parser e s d c a
aka e = mapErrors <| \_ -> err e

while : e -> Parser e s d c a -> Parser e s d c a
while e = mapErrors <| \trees -> e |> with trees

try : Parser e s d c a -> Parser e s d c a
try p ctx s =
  case p ctx s of
    (Ok _, _, _) as result -> result
    (Err e, d, s1) -> (Err e, ctx.empty, s1)

ap : Parser e s d c a -> Parser e s d c (a -> b) -> Parser e s d c b
ap px pf ctx s =
  case pf ctx s of
    (Ok f, d1, s1) ->
      case px ctx s1 of
        (Ok  x, d2, s2) -> (Ok (f x), ctx.append d1 d2, s2)
        (Err e, d2, s2) -> (Err e,    ctx.append d1 d2, s1)

    (Err e, d, s1) ->
      (Err e, d, s1)

andThen : (a -> Parser e s d c b) -> Parser e s d c a -> Parser e s d c b
andThen k pa ctx s =
  case pa ctx s of
    (Ok a, d1, s1) ->
      case k a ctx s of
        (Ok  b, d2, s2) -> (Ok  b, ctx.append d1 d2, s2)
        (Err e, d2, s2) -> (Err e, ctx.append d1 d2, s1)

    (Err e, d, s1) ->
      (Err e, d, s1)

map : (a -> b) -> Parser e s d c a -> Parser e s d c b
map f p = pure f |> ap p

void : Parser e s d c a -> Parser e s d c ()
void p = p |> map (always ())

use : Parser e s d c b -> Parser e s d c a -> Parser e s d c b
use pb pa = pure (flip always) |> ap pa |> ap pb

omit : Parser e s d c a -> Parser e s d c b -> Parser e s d c b
omit pb pa = pure always |> ap pa |> ap pb

flip : (a -> b -> c) -> b -> a -> c
flip f x y = f y x

or : Parser e s d c a -> Parser e s d c a -> Parser e s d c a
or r l ctx s =
  case l ctx s of
    (Err e, d, _) as result ->
      if d == ctx.empty
      then
        case r ctx s of
          (Err e1, d1, _) as result2 ->
            if d1 == ctx.empty
            then (Err (e ++ e1), ctx.empty, s)
            else result2
          other -> other
      else
        result

    other -> other

satisfy : (c -> Bool) -> Parser e s d c c
satisfy predicate {one, next, empty} s =
  case next s of
    Just (c, s1) ->
      if predicate c
      then (Ok c, one, s1)
      else (Err noErr, empty, s)
    
    Nothing ->
      (Err noErr, empty, s)

token : Parser e s d c c
token = satisfy (always True)

match : c -> Parser e s d c c
match = satisfy << (==)

many : Parser e s d c a -> Parser e s d c (List a)
many p ctx =
  let
    loop acc delta s1 =
      case p ctx s1 of
        (Ok a, d, s2) -> 
          if d == ctx.empty
          then
            Debug.todo
              <| "the argument to `many` succeeded, but consumed nothing, "
              ++ "good luck searching where it happened!"
          else
            loop (a :: acc) (ctx.append delta d) s2
        (Err e, d, s2) ->
          if d == ctx.empty
          then
            (Ok (List.reverse acc), delta, s1)
          else
            (Err e, ctx.append delta d, s2)
  in
    loop [] ctx.empty

some : Parser e s d c a -> Parser e s d c (List a)
some p =
  pure (::)
    |> ap p
    |> ap (many p)

sepBy1 : Parser e s d c sep -> Parser e s d c a -> Parser e s d c (List a)
sepBy1 sep p =
  pure (::)
    |> ap p
    |> ap ((sep |> use p) |> many)

sepBy : Parser e s d c sep -> Parser e s d c a -> Parser e s d c (List a)
sepBy sep p = sepBy1 sep p |> or (pure [])

optional : Parser e s d c a -> Parser e s d c (Maybe a)
optional p ctx s =
  case p ctx s of
    (Ok a, d, s1) -> (Ok (Just a), d, s1)
    (Err e, d, s1) ->
      if d == ctx.empty
      then (Ok Nothing, ctx.empty, s)
      else (Err e, d, s1)

trace : Parser e s d c a -> Parser e s d c s
trace p ctx s =
  case p ctx s of
    (Ok _,  d, s1) -> (Ok (ctx.grab d s), d, s1)
    (Err e, d, s1) -> (Err e, d, s1)

delay : (() -> Parser e s d c a) -> Parser e s d c a
delay thunk ctx s =
  thunk () ctx s

recursive : (Parser e s d c a -> Parser e s d c a) -> Parser e s d c a
recursive fixpoint = fixpoint <| delay <| \() -> recursive fixpoint

stringMonoid : MonoidalAction String Int Char
stringMonoid =
  { update = String.dropLeft
  , append = (+)
  , one    = 1
  , empty  = 0
  , next   = String.uncons
  , grab   = String.left
  }

runString : String -> Parser e String Int Char a -> Result (Forest e, Int) a
runString = run stringMonoid
