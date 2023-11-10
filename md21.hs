aa ::
  (Eq dictKey, Eq dictVal) =>
  -- | input dictionary
  [(dictKey, dictVal)] ->
  -- | input values to be translated
  [dictKey] ->
  -- | output translations
  [dictVal]
aa translations inputs = aaInputs translations inputs []

-- Iterates over all input values
aaInputs ::
  (Eq dictKey, Eq dictVal) =>
  -- | input dictionary
  [(dictKey, dictVal)] ->
  -- | input values to be translated
  [dictKey] ->
  -- | current output translations
  [dictVal] ->
  -- | final output translations
  [dictVal]
aaInputs dictionary [] result_translations = removeDuplicatesAA result_translations []
-- Given a list of inputs, translates the first input and calls aaInputs on the rest of the inputs
aaInputs dictionary (curr_input : rest_input) result_translations = aaInputs dictionary rest_input (result_translations ++ aaTransl dictionary curr_input [])

-- returns all translations of given input value
aaTransl ::
  (Eq dictKey, Eq dictVal) =>
  -- | input dictionary
  [(dictKey, dictVal)] ->
  -- | input value to be translated
  dictKey ->
  -- | current output translations
  [dictVal] ->
  -- | final output translations
  [dictVal]
-- all translations iterated
aaTransl [] input input_translations = input_translations
aaTransl ((transl_key, transl_res) : rest_transl) input input_translations =
  if transl_key == input
    then -- If the key matches the input, add the translation to the result and check the next translation
      aaTransl rest_transl input (input_translations ++ [transl_res])
    else -- Otherwise, check the next translation
      aaTransl rest_transl input input_translations

-- Iterates over all input values and removes duplicates
removeDuplicatesAA ::
  (Eq keyType) =>
  -- | input values
  [keyType] ->
  -- | current output values without duplicates
  [keyType] ->
  -- | final output values without duplicates
  [keyType]
removeDuplicatesAA [] result = result
removeDuplicatesAA (currKey : restKeys) result = removeDuplicatesAA (removeDuplicateKeyAA currKey restKeys) (result ++ [currKey])

-- Given a key, removes all other instances of that key
removeDuplicateKeyAA ::
  (Eq keyType) =>
  -- | key to be removed
  keyType ->
  -- | input values
  [keyType] ->
  -- | output values without duplicates
  [keyType]
removeDuplicateKeyAA keyToRemove [] = []
removeDuplicateKeyAA keyToRemove (currKey : restKeys) =
  if keyToRemove == currKey
    then removeDuplicateKeyAA keyToRemove restKeys
    else currKey : removeDuplicateKeyAA keyToRemove restKeys

bb ::
  (Eq dict1Key, Eq intermediate, Eq dict2Val) =>
  -- | input dictionary 1
  [(dict1Key, intermediate)] ->
  -- | input dictionary 2
  [(intermediate, dict2Val)] ->
  -- | output dictionary
  [(dict1Key, dict2Val)]
bb dict1 dict2 = bbDict dict1 dict2 []

bbDict ::
  (Eq dict1Key, Eq intermediate, Eq dict2Val) =>
  -- | input dictionary 1
  [(dict1Key, intermediate)] ->
  -- | input dictionary 2
  [(intermediate, dict2Val)] ->
  -- | current output dictionary
  [(dict1Key, dict2Val)] ->
  -- | final output dictionary
  [(dict1Key, dict2Val)]
-- Once all pairs from dict 1 are checked, return the result
bbDict [] dict2 results = removeDuplicatesBB results []
-- Given a list of tuples, combines the first tuple with all tuples from dict 2, then call bbDict on the next tuple
bbDict ((key1, val1) : rest_dict1) dict2 results = bbDict rest_dict1 dict2 (results ++ bbInputs (key1, val1) dict2 [])

-- Combines translations for one translation pair from dictionary 1 with all translation pairs from dictionary 2
bbInputs ::
  (Eq dict1Key, Eq intermediate, Eq dict2Val) =>
  -- | input pair to be combined
  (dict1Key, intermediate) ->
  -- | input dictionary 2
  [(intermediate, dict2Val)] ->
  -- | current output dictionary
  [(dict1Key, dict2Val)] ->
  -- | final output dictionary
  [(dict1Key, dict2Val)]
-- Once all pairs from dict 2 are checked, return the result
bbInputs pair [] resolutDictionary = resolutDictionary
bbInputs (keyStart, valStart) ((keyEnd, valEnd) : rest_dict2) resultDictionary =
  if valStart == keyEnd
    then -- If the value from dict 1 and key from dict 2 matches, add the combined pair to the result dictionary and check the next pair
      bbInputs (keyStart, valStart) rest_dict2 (resultDictionary ++ [(keyStart, valEnd)])
    else -- Otherwise, check the next pair
      bbInputs (keyStart, valStart) rest_dict2 resultDictionary

-- Iterates over all input values and calls removeDuplicatesBB to remove duplicates for each tuple
removeDuplicatesBB ::
  (Eq keyType, Eq valType) =>
  -- | input values
  [(keyType, valType)] ->
  -- | current output values without duplicates
  [(keyType, valType)] ->
  -- | final output values without duplicates
  [(keyType, valType)]
-- Once all tuples are checked, return the result
removeDuplicatesBB [] result = result
-- Given a list of tuples, removes all duplicate occurrences of the first tuple, then call removeDuplicatesBB on the next tuple
removeDuplicatesBB (currKey : restKeys) result = removeDuplicatesAA (removeDuplicateKeyAA currKey restKeys) (result ++ [currKey])

-- Given a key, removes all other instances of that key from the given list of tuples
removeDuplicateEntryBB ::
  (Eq keyType, Eq valType) =>
  -- | key to be removed
  (keyType, valType) ->
  -- | input values
  [(keyType, valType)] ->
  -- | output values without duplicates
  [(keyType, valType)]
-- once all entries are checked, return the result
removeDuplicateEntryBB removeEntry [] = []
removeDuplicateEntryBB (removeKey, removeVal) ((currKey, currVal) : restEntries) =
  if removeKey == currKey && removeVal == currVal
    then -- if the key and value match, remove the entry
      removeDuplicateKeyAA (removeKey, removeVal) restEntries
    else -- otherwise, keep the entry and check the next one
      (currKey, currVal) : removeDuplicateKeyAA (removeKey, removeVal) restEntries

aa1 = aa [("a", "aa"), ("a", "bbb"), ("bb", "bbb"), ("c", "def"), ("bb", "b12312")] ["a", "bb"]

aa2 = aa [("aa", "a"), ("bbb", "b"), ("def", "c"), ("b12312", "bb")] ["aa", "bbb", "aa"]

bb1 = bb [("a", "b"), ("b", "c"), ("c", "d"), ("a", "e")] [("b", "x"), ("c", "y"), ("d", "z"), ("c", "a"), ("e", "x")]

bb2 = bb [("your", "tavs"), ("name", "vards"), ("is", "ir"), ("name", "vards")] [("tavs", "your"), ("vards", "name"), ("ir", "is"), ("vards", "name")]

main :: IO ()
main = do
  print aa1
  print aa2
  print bb1
  print bb2
