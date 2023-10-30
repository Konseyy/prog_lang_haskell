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
aaInputs dictionary [] result_translations = removeDuplicates result_translations []

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
removeDuplicates ::
  (Eq keyType) =>
  -- | input values
  [keyType] ->
  -- | current output values without duplicates
  [keyType] ->
  -- | final output values without duplicates
  [keyType]
removeDuplicates [] result = result
removeDuplicates (currKey : restKeys) result = removeDuplicates (removeDuplicateKey currKey restKeys) (result ++ [currKey])

-- Given a key, removes all other instances of that key
removeDuplicateKey ::
  (Eq keyType) =>
  -- | key to be removed
  keyType ->
  -- | input values
  [keyType] ->
  -- | output values without duplicates
  [keyType]
removeDuplicateKey keyToRemove [] = []
removeDuplicateKey keyToRemove (currKey : restKeys) =
  if keyToRemove == currKey
    then removeDuplicateKey keyToRemove restKeys
    else currKey : removeDuplicateKey keyToRemove restKeys

main :: IO ()
main = do
  let vardnica = [("a", "aa"), ("a", "bbb"), ("bb", "bbb"), ("c", "def"), ("bb", "b12312")]
  let input = ["a", "bb"]
  print (aa vardnica input)
