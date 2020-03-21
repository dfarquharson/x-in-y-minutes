-- Single line comments
{- Multi
 - Line
 - Comments
 -}

-- My own shitty little test framework
data TestResult = Pass Bool | Fail Bool deriving (Show)
data Expectation = Expect Bool Bool deriving (Show, Eq)
data Result = Actual Expectation Bool deriving (Show)

expectT :: Bool -> Expectation
expectT x = Expect True x

expectF :: Bool -> Expectation
expectF x = Expect False x

test :: Expectation -> Result
test (Expect a b) = Actual (Expect a b) (a == b)

passed :: Result -> Bool
passed (Actual _ result) = result

-- Print whether all results are indeed True as expected
main = putStrLn $ show $ foldr (&&) True $ map passed $ map test $ map expectT
  [
    (expectT True == Expect True True),
    (expectF False == Expect False False),
    (3 == 3),
    (1 + 1 == 2),
    (8 - 1 == 7),
    (10 * 2 == 20),
    (35 / 5 == 7),
    (35 / 5 == 7.0),
    (35 / 4 == 8.75),
    (35 `div` 4 == 8),
    (35 `rem` 4 == 3),
    (True),
    (True /= False),
    (False == False),
    (not True == False),
    (not False == True),
    ((1 /= 1) == False),
    (1 < 10),
    (11 > 10),
    ("String" == "String"),
    ('a' == 'a'),
    ("Hello, World!" == "Hello," ++ " World!"),
    (['h', 'e', 'l', 'l', 'o'] == "hello"),
    ("This is a String" !! 0 == 'T'),
    ("This is a String" !! 4 == ' '),
    ([1, 2, 3, 4, 5] == [1..5]),
    (['A'..'F'] == "ABCDEF"), -- list of char are strings
    ([0, 2 .. 10] == [0, 2, 4, 6, 8, 10]), -- give a step and it'll keep working
    ([5..1] == []), -- default in incrementing, so this doesn't work
    ([5,4..1] == [5, 4, 3, 2, 1]), -- but if you provide a step, then it works
    ([1..10] !! 3 == 4), --zero-based indexing as all sane languages should :)
    (take 3 [1..] == [1, 2, 3]), -- infinite lists!
    ([1..] !! 999 == 1000),
    ([1..5] ++ [6..10] == [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]),
    (0:[1..5] == [0, 1, 2, 3, 4, 5]), -- `:` is like cons
    (head [1..5] == 1),
    (tail [1..5] == [2, 3, 4, 5]),
    (init [1..5] == [1, 2, 3, 4]),
    (last [1..5] == 5),
    ([x * 2 | x <- [1..5]] == [2, 4, 6, 8, 10]), -- list comprehensions
    ([x * 2 | x <- [1..5], x * 2 > 4] == [6, 8, 10]), -- list comprehensions with a filter
    (fst ("haskell", 1) == "haskell"), -- first thing in a tuple
    (snd ("haskell", 1) == 1), -- second thing in a tuple
    (True) -- this is the placeholder for the last list item, so I don't have to worry about commas :)
  ]

