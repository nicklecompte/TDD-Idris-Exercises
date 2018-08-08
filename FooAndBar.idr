%default total

data Foo = Bar | Baz

foo1 : Foo -> Foo
foo1 Bar = Baz
foo1 Baz = Bar

foo2 : Foo -> Bool
foo2 f = case foo1 f of 
  Bar => True
  Baz => False

fooProof : (f: Foo) -> (f=Bar) -> foo2 f = False
fooProof f prf = rewrite__impl
    (\x => (case foo1 x of Bar => True; Baz => False) = False) prf (the (False=False) Refl)