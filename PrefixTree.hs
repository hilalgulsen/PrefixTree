--Functional Programming Assignment2
--Hilal Gülşen 150120023
module Assignment2 where
--Trie is type constructor and 'k' and 'a' are type parameters.
data Trie k a = Leaf a | Branch [(Maybe k, Trie k a)] deriving Show --There are Leaf and Branch constructors

--An empty trie value
empty' :: Trie k a
empty' = Branch[]

--A function to insert an item into the trie
insert' :: Eq k => [k] -> a -> Trie k a -> Trie k a
insert' [] n _  = Branch [(Nothing, Leaf n)]                               --If just arbitrary value will be added, it is added like Leaf
insert' (x:xs) n (Branch []) = Branch [(Just x, insert' xs n (Branch []))] --If trie is empty before, create Branch and move down over Branch adding list members recursively
insert' (x:xs) n (Branch (c:cs)) = case fst c of                           --If trie is not empty and
    Just m -> if x == m                                                    --Trie is starting with Just and list's first member which will be added is equal to this Just item
                then Branch ([(Just m, insert' xs n (snd c))] ++ cs)       --Then, pass Just item and move down over this branch and call insert function passing remaining parts as input.At the end concatenate with other Branches.
                else case insert' (x:xs) n (Branch cs) of                  --If it is not equal, look at other Branches in trie and insert to proper place or create new Branch including input if there is no branch.
                    Branch b -> Branch (c:b)                               --Then, concatenate "branches which we checked" with "branch with additional input"
    Nothing ->  case insert' (x:xs) n (Branch cs) of                       --If trie is starting with Nothing, look at other Branches in trie and insert to proper place or create new Branch including input if there is no branch.
                    Branch b -> Branch (c : b)                             --Then, concatenate "branches which we checked" with "branch with additional input"
{-|
foldl :: (b -> a -> b) -> b -> [a] -> b
It reduces the elements of a list to a single value.fromList is a function to create a trie from a list of items.
There is an anonymous function which takes 2 parameters as x and y. It applies insert' using these x and y values.
Here, currying is used. foldl applies anonymous function on (Branch[]) and input of fromList function which is not stated here because of the currying process.
-}
fromList':: Eq k => [([k], a)] -> Trie k a
fromList' = foldl (\x y -> insert' (fst y) (snd y) x) (Branch[])

--A function to retrieve a value for a given key, if it exists. Otherwise return Nothing
lookup' :: Eq k => [k] -> Trie k a -> Maybe a
lookup' _ (Branch []) = Nothing                                            --If Trie is empty return Nothing regardless of given key
lookup'(x:xs) (Branch (c:cs)) = case fst c of                              --If Trie is not empty, look first Branch
    Nothing -> Nothing                                                     --If there is only Leaf node return Nothing
    Just m -> if x == m                                                    --Trie is starting with Just and If node is equal to wanted key,
                then (lookup' xs (snd c))                                  --Then, move down over this branch and call lookup function passing remaining parts as input
                else (lookup' (x:xs) (Branch cs))                          --If it is not equal to wanted key, look at other Branches in trie
lookup' [] (Branch (c:cs)) = if (fst c) == Nothing                         --If we checked whole key and we came Nothing item,
                               then case snd c of                          --Then, look at second part
                                   Leaf x -> Just x                        --If it is Leaf return Just this value
                                else Nothing                               --If we checked whole key and we did not come Nothing item, return Nothing

--A function to delete a given key (and the value associated with it) from the trie. If the key doesn't exist, return the unmodified trie
delete' :: Eq k => [k] -> Trie k a -> Trie k a
delete' (x:xs) (Branch (c:cs))  = case fst c of                            --If trie and given key is not empty, look first Branch
    Just m -> if x == m                                                    --If it is Just item and arbitrary value is equal to given key
                then case delete' xs (snd c) of                            --Then, move down over this branch and call delete function recursively passing remaining parts as input
                    Branch b -> if null b                                  --When it is over, we should concatenate them. If returning Branch is empty,
                                  then Branch(cs)                          --Then, return other branches
                                  else Branch([(Just x, Branch b)] ++ cs)  --If returning Branch is not empty, combine it with other branches
                else case delete' (x:xs) (Branch cs) of                    --If it is not equal to wanted key, look at other branches in trie
                    Branch b -> Branch (c:b)                               --Then combine them
    Nothing -> case delete' (x:xs) (Branch cs) of                          --If it is Nothing item, look at other branches
       Branch b -> Branch(c : b)                                           --Then combine them
delete' [] (Branch (c:cs)) = case fst c of                                 --If we moved down over Branch and checked whole given key, look where we are
    Nothing -> (Branch cs)                                                 --If it is Nothing, we found given key. Therefore, ignore it and return remaining branches
    Just m -> delete' [] (Branch cs)                                       --If it is Just, look at other branches
delete' (x:xs) (Branch []) = Branch[]                                      --If trie is empty return empty trie

