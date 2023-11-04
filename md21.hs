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
aaInputs dictionary (curr_input : rest_input) result_translations = aaInputs dictionary rest_input (result_translations ++ aaTransl dictionary curr_input [])
aaInputs dictionary [] result_translations = removeDuplicatesAA result_translations []

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
aaTransl ((transl_key, transl_res) : rest_transl) input input_translations =
  if transl_key == input
    then aaTransl rest_transl input (input_translations ++ [transl_res])
    else aaTransl rest_transl input input_translations
-- all translations iterated
aaTransl [] input input_translations = input_translations

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
bbDict [] dict2 results = removeDuplicatesBB results []
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
bbInputs pair [] resolutDictionary = resolutDictionary
bbInputs (keyStart, valStart) ((keyEnd, valEnd) : rest_dict2) resultDictionary =
  if valStart == keyEnd
    then bbInputs (keyStart, valStart) rest_dict2 (resultDictionary ++ [(keyStart, valEnd)])
    else bbInputs (keyStart, valStart) rest_dict2 resultDictionary

-- Iterates over all input values and removes duplicates
removeDuplicatesBB ::
  (Eq keyType, Eq valType) =>
  -- | input values
  [(keyType, valType)] ->
  -- | current output values without duplicates
  [(keyType, valType)] ->
  -- | final output values without duplicates
  [(keyType, valType)]
removeDuplicatesBB [] result = result
removeDuplicatesBB (currKey : restKeys) result = removeDuplicatesAA (removeDuplicateKeyAA currKey restKeys) (result ++ [currKey])

-- Given a key, removes all other instances of that key
removeDuplicateEntryBB ::
  (Eq keyType, Eq valType) =>
  -- | key to be removed
  (keyType, valType) ->
  -- | input values
  [(keyType, valType)] ->
  -- | output values without duplicates
  [(keyType, valType)]
removeDuplicateEntryBB removeEntry [] = []
removeDuplicateEntryBB (removeKey, removeVal) ((currKey, currVal) : restEntries) =
  if removeKey == currKey && removeVal == currVal
    then removeDuplicateKeyAA (removeKey, removeVal) restEntries
    else (currKey, currVal) : removeDuplicateKeyAA (removeKey, removeVal) restEntries

main :: IO ()
main = do
  -- 1.uzd
  -- let vardnica = [("a", "aa"), ("a", "bbb"), ("bb", "bbb"), ("c", "def"), ("bb", "b12312")]
  -- let input = ["a", "bb"]
  -- print (aa vardnica input)

  -- 2.uzd
  -- let vardnica1 = [("a", "b"), ("b", "c"), ("c", "d"), ("a", "e")]
  -- let vardnica2 = [("b", "x"), ("c", "y"), ("d", "z"), ("c", "a"), ("e", "x")]
  -- print (bb vardnica1 vardnica2)

  print 2
