import DataStore 

testStore : DataStore (SString .+. SString .+. SInt)
testStore = addToStorePublic ("Mercury","Mariner 10",1974)
                (addToStorePublic ("Venus","Venera",1961) 
                    (addToStorePublic ("Uranus", "Voyager 2", 1986)
                        (addToStorePublic ("Pluto","New Horizon", 2005)
                            empty)))

listItems : DataStore schema -> List (SchemaType schema)                            
listItems x with (storeView x)
  listItems x | SNil = []
  listItems (addToStorePublic value store) | (SAdd rec) = value :: (listItems store | rec)

filterKeys : (test : SchemaType val_schema -> Bool) -> DataStore (SString .+. val_schema) -> List String
filterKeys test x with (storeView x)
  filterKeys test x | SNil = []
  filterKeys test (addToStorePublic (key,value) store) | (SAdd rec) = if test value
                                                                    then key :: (filterKeys test store | rec)
                                                                    else (filterKeys test store | rec)
