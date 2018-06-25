%default total

--interface Bar (a:Type) (func: a -> a -> a)

interface Foo (a : Type) (func : a -> a -> a)

interface Foo a func => Bar a func where
[NumFoo] Num a => Foo a (+) where
[NatBar] Bar Integer (+) using NumFoo where    -- Doesn't typecheck...

ConcatNum : Num a => List a -> List a -> List a
ConcatNum = (++)
ZipAdd : Num a => List a -> List a -> List a
ZipAdd = zipWith (+)

[ListFoo] Num a => Foo (List a) ConcatNum where
[ListBar] Bar (List Integer) ConcatNum using ListFoo where    -- ...but this works! Yet...

[ListFoo2] Num a => Foo (List a) ZipAdd where
[ListBar2] Bar (List Integer) ZipAdd using ListFoo2 where    -- ...this doesn't!!!