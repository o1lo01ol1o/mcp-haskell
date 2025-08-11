{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Module designed to test get_completions functionality
-- Contains various scenarios where HLS should provide helpful completions
module Demo.Completions where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (sort, nub, intercalate)
import Control.Monad (when, unless, forM_)

-- | Record type for testing record completions
data User = User
  { userName :: Text
  , userAge :: Int
  , userEmail :: Text
  , userEnabled :: Bool
  , userPreferences :: UserPreferences
  } deriving (Show, Eq)

data UserPreferences = UserPreferences
  { prefLanguage :: Text
  , prefTheme :: Text
  , prefNotifications :: Bool
  } deriving (Show, Eq)

-- | Test record construction completions
-- When typing "User {", HLS should complete with field names
createUser :: Text -> Int -> User
createUser name age = User 
  { userName = name
  , userAge = age
  , userEmail = name <> "@example.com"
  , userEnabled = True
  , userPreferences = defaultPreferences
  }

-- | Test record update completions  
-- When typing "user { user", HLS should complete field names
updateUser :: User -> User
updateUser user = user 
  { userEnabled = not (userEnabled user)
  -- When typing more fields, should get completions
  }

-- | Test record field access completions
-- After typing "user.", should get field completions
getUserInfo :: User -> Text
getUserInfo user = userName user <> " (" <> T.pack (show (userAge user)) <> ")"

-- | Default preferences for testing
defaultPreferences :: UserPreferences
defaultPreferences = UserPreferences
  { prefLanguage = "en"
  , prefTheme = "dark"
  , prefNotifications = True
  }

-- | Test module-qualified completions
-- After typing "T.", should complete with Text functions
-- After typing "Map.", should complete with Map functions
textOperations :: Text -> Map Text Int -> (Text, Map Text Int)
textOperations input mapping = 
  let processed = T.toUpper input  -- T. should complete
      wordCount = Map.size mapping  -- Map. should complete
  in (processed, mapping)

-- | Test local variable completions
-- When typing, should complete with local variables in scope
complexFunction :: [Text] -> Map Text Int -> Text
complexFunction inputTexts wordCounts = 
  let processedTexts = map T.strip inputTexts  -- inputTexts should complete
      filteredTexts = filter (not . T.null) processedTexts  -- processedTexts should complete
      uniqueTexts = nub filteredTexts  -- filteredTexts should complete
      sortedTexts = sort uniqueTexts  -- uniqueTexts should complete
      combinedText = T.intercalate ", " sortedTexts  -- sortedTexts should complete
      finalCount = Map.findWithDefault 0 combinedText wordCounts  -- wordCounts should complete
  in combinedText <> " (count: " <> T.pack (show finalCount) <> ")"

-- | Test function parameter completions
-- When defining function body, parameters should be available for completion
multiParameterFunction :: Text -> Int -> Bool -> User -> UserPreferences -> Text
multiParameterFunction name age enabled user prefs = 
  -- All parameters should be available for completion when typing
  if enabled 
    then name <> " is enabled"
    else "Disabled user"

-- | Test pattern match completions
-- When pattern matching, constructors should be available
patternMatchUser :: User -> Text
patternMatchUser User{..} = -- RecordWildCards pattern should complete field names
  userName <> " prefers " <> prefLanguage userPreferences

-- | Test case expression completions
-- When typing case branches, constructors should complete
processBoolean :: Bool -> Text
processBoolean flag = case flag of
  True -> "yes"   -- True should complete
  False -> "no"   -- False should complete

-- | Test do-notation completions
-- In do blocks, should complete with monadic functions and local bindings
doNotationExample :: IO ()
doNotationExample = do
  putStrLn "Enter your name:"
  name <- getLine  -- getLine should complete
  putStrLn ("Hello, " ++ name)  -- name should complete from binding above
  when (not (null name)) $ do  -- when should complete, name should complete
    putStrLn "Nice to meet you!"

-- | Test where clause completions
-- Functions defined in where should be available for completion
whereClauseExample :: [Int] -> Int
whereClauseExample numbers = processNumbers numbers + helper 42
  where
    processNumbers = sum . filter (> 0)  -- Should complete with numbers, helper
    helper x = x * 2  -- Should complete with x, processNumbers

-- | Test let binding completions
letBindingExample :: Int -> Int
letBindingExample x = 
  let doubled = x * 2      -- x should complete
      tripled = x * 3      -- x should complete  
      combined = doubled + tripled  -- doubled, tripled should complete
  in combined * 2          -- combined should complete

-- | Test lambda parameter completions
lambdaExample :: [User] -> [Text]
lambdaExample users = map (\user -> userName user) users
  -- In lambda body, user parameter should complete
  -- userName should complete as record field

-- | Test qualified import completions
-- Should complete with qualified functions after typing module prefix
qualifiedImportExample :: Text -> [Text] -> Text
qualifiedImportExample separator items = 
  T.intercalate separator (map T.strip items)
  -- T. should complete with Text functions

-- | Test type completions
-- When writing type signatures, should complete with available types
typeCompletionExample :: User -> UserPreferences -> Bool
typeCompletionExample user prefs = userEnabled user && prefNotifications prefs

-- | Test constructor completions in expressions
constructorCompletionExample :: Text -> Int -> User
constructorCompletionExample name age = User 
  -- After typing "User ", should complete with construction syntax
  name age (name <> "@test.com") True defaultPreferences

-- | Test completion with imports
-- Functions from imported modules should be available
importCompletionExample :: [Int] -> [Int]
importCompletionExample numbers = 
  sort (nub (filter (> 0) numbers))
  -- sort, nub should complete from Data.List
  -- filter should complete from Prelude

-- | Test completion in complex expressions
complexExpressionExample :: Map Text User -> Text -> Maybe Text
complexExpressionExample userMap searchName = 
  case Map.lookup searchName userMap of
    Nothing -> Nothing
    Just user -> Just (userName user)
    -- Map.lookup should complete, userName should complete

-- | Test completion with type constraints
constrainedFunction :: (Show a, Ord a) => [a] -> Text
constrainedFunction items = 
  T.pack (show (sort items))
  -- show should complete (from Show constraint)
  -- sort should complete (from Ord constraint + import)

-- | Test field completion with RecordWildCards
recordWildcardsExample :: User -> Text
recordWildcardsExample User{..} = 
  userName <> " (" <> T.pack (show userAge) <> ")"
  -- All User fields should be available for completion without user. prefix