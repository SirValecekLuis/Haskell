data Tree a = Leaf a 
            | Branch a (Tree a) (Tree a) deriving (Show)

tree1 = Branch 12 (Leaf 5) (Leaf 6)  

tree2 = Branch 'u' (Branch 'x' (Leaf 'a') (Leaf 'b')) (Leaf 'z')

toString (Leaf x) = show x
toString (Branch a b c) = show a ++ "(" ++ toString b ++ "," ++ toString c ++ ")"

fromString inp = fst (fromString2 inp) where
    fromString2 inp = let 
        before = takeWhile (\x -> x /= '(' && x /= ',' && x /= ')') inp
        after = dropWhile (\x -> x /= '(' && x /= ',' && x /= ')') inp
        value = read before
        in if null after || head after /= '(' then (Leaf value, after)
            else let 
                (l, after2) = fromString2 (tail after)
                (r, after3) = fromString2 (tail after2)
                in (Branch value l r, tail after3 )


-- TEST 1

-- 1
data Company = Company {name :: String, employees :: Int, ownerOf :: [Company]} deriving Show
companytest :: Company
companytest = Company "Ananas" 2500 [Company "Jablko" 2300 []]

-- 2
data Entity = Point {x :: Double, y :: Double} | Circle {x :: Double, y :: Double, r :: Int} | Container [Entity] deriving Show
entitytest :: Entity
entitytest = Container [Point 2.5 1.2]

-- 3



-- 4 



-- 5
data HTML = Attribute {name1 :: String, value :: String}
          | Tag {name2 :: String, list :: [HTML], list2 :: [HTML]}
          | HTMLDocument [HTML] deriving (Show)

myHTMLDocument = HTMLDocument [
                    Tag {name2 = "body", list = [Attribute {name1 = "style=", value = "color:blue;"}, Attribute {name1 = "lang=", value = "en"}], list2 = []},
                    Tag {name2 = "section", list = [Attribute {name1 = "href=", value = "yt.com"}, Attribute {name1 = "style=", value = "font:Arial;"}], list2 = [Tag {name2 = "p", list = [], list2 = []}]},
                    Tag {name2 = "/section", list = [], list2 = []},
                    Tag {name2 = "/body", list = [], list2 = []}
                    ]

-- TEST 2
data FileType = Image | Executable | SourceCode | TextFile deriving (Eq, Show)

data Entry = File {nam :: String, size :: Int, ftype :: FileType}
           | Directory {nam :: String, entries :: [Entry]} deriving (Eq, Show)

root :: Entry
root = Directory "root"
    [
    File "logo.jpg" 5000 Image,
    Directory "classes"
        [
        File "notes-fpr.txt" 200 TextFile,
        File "presentation.jpg" 150 Image,
        File "first_test.hs" 20 SourceCode
        ]
    ]

-- 1
countFiles File {} = 1
countFiles (Directory _ entries) = sum $ map countFiles entries


-- 3


-- 4

-- 5
countSize :: Entry -> Int
countSize (File _ size _) = size
countSize (Directory _ files) = sum (map countSize files)

-- 6

-- TEST 3

-- 1
fullNames :: Entry -> [String]
fullNames entry = getNames [entry] ""
  where
    getPath p1 p2 = p1 ++ "/" ++ p2
    getNames [] _ = []
    getNames ((File name _ _) : es) path = getPath path name : getNames es path
    getNames ((Directory name files) : es) path = getNames files (getPath path name) ++ getNames es path

-- 2

-- 3

-- 4
directorySizes :: Entry -> [(String, Int)]
directorySizes File {} = []
directorySizes (Directory name files) = (name, sum [countSize x | x <- files]) : concat [directorySizes x | x <- files]

-- 5
check :: String -> [Entry] -> [Entry]
check _ [] = []
check str ((Directory name files):xs) = Directory name files : check str xs
check str ((File name size ftype):xs) | name == str = check str xs
                                      | otherwise = File name size ftype : check str xs

removeFile :: String -> Entry -> Entry
removeFile str (File name size ftype) = File name size ftype
removeFile str (Directory name files) = Directory name (check str [removeFile str x | x <- files]);