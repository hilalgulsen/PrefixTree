# PrefixTree
Implement a prefix tree, also known as a trie. Use it to store a mapping between a
list [k] and an arbitrary value a. We can define a trie recursively, as follows:

    data Trie k a = Leaf a | Branch [(Maybe k, Trie k a)]

We can represent an empty tree with Branch []. Similarly, a tree containing the
items ("more", 1) and ("most", 2) would look like this:


    Branch [

      (Just `m', Branch [
  
        (Just `o', Branch [
    
          (Just `r', Branch [
      
            (Just `e', Branch [
        
              (Nothing, Leaf 1)])],
          
          (Just `s', Branch [
      
            (Just `t', Branch [
        
              (Nothing, Leaf 2)])])])])]
            
          
We assume no key can be a prefix of another key. Each node can be either a branch
or a leaf, but not both. To ensure this, we terminate each key with a special value
(e.g., Nothing).
Using such a structure, define the following functions.

(a) An empty trie value.


    empty' :: Trie k a


(b) A function to insert an item into the trie.


    insert' :: Eq k => [k] -> a -> Trie k a -> Trie k a

    insert' "more" 1 $ insert' "most" 2 empty' ) (the trie above)


(c) A function to create a trie from a list of items. (Hint: use insert' and a fold function.)

    fromList' :: Eq k => [([k], a)] -> Trie k a

    fromList' [("more",1),("most",2)] => (the trie above)

(d) A function to retrieve a value for a given key, if it exists. Otherwise return Nothing.

    lookup' :: Eq k => [k] -> Trie k a -> Maybe a

    lookup' "more" $ fromList' [("more",1)] ) Just 1

    lookup' "meow" $ fromList' [("most",2)] ) Nothing

(e) A function to delete a given key (and the value associated with it) from the trie. If the key doesn't exist, return the unmodied trie.

    delete' :: Eq k => [k] -> Trie k a -> Trie k a

    delete' "more" $ fromList' [("more",1)] ) empty'
