-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 3
--
-- Week 4(08-12 Oct.)

module Tutorial3 where

import Data.List (nub)
import Data.Char
import Test.QuickCheck
import Network.HTTP (simpleHTTP,getRequest,getResponseBody)

-- <type decls>

type Link = String
type Name = String
type Email = String
type HTML = String
type URL = String

-- </type decls>
-- <sample data>

testURL     = "http://www.inf.ed.ac.uk/teaching/courses/inf1/A/testpage.html"

testHTML :: String
testHTML =    "<html>"
           ++ "<head>"
           ++ "<title>FP: Tutorial 3</title>"
           ++ "</head>"
           ++ "<body>"
           ++ "<h1>A Boring test page</h1>"
           ++ "<h2>for tutorial 3</h2>"
           ++ "<a href=\"http://www.inf.ed.ac.uk/teaching/courses/inf1/A/testpage.html\">FP Website</a><br>"
           ++ "<b>Lecturer:</b> <a href=\"mailto:dts@inf.ed.ac.uk\">Don Sannella</a><br>"
           ++ "<b>TA:</b> <a href=\"mailto:irene.vp@ed.ac.uk\">Irene Vlassi</a>"
           ++ "</body>"
           ++ "</html>"

testHTML2 :: String
testHTML2 =    "<html>"
           ++ "<head>"
           ++ "<title>FP: Tutorial 3</title>"
           ++ "</head>"
           ++ "<body>"
           ++ "<h1>A Boring test page</h1>"
           ++ "<h2>for tutorial 3</h2>"
           ++ "<a href=\"http://www.inf.ed.ac.uk/teaching/courses/inf1/A/testpage.html\">FP Website</a><br>"
           ++ "<b>Lecturer:</b> <a href=\"mailto:dts@inf.ed.ac.uk\">Don T. Sannella</a><br>"
           ++ "<b>TA:</b> <a href=\"mailto:irene.vp@ed.ac.uk\">Irene Vlassi</a>"
           ++ "</body>"
           ++ "</html>"

testLinks :: [Link]
testLinks = [ "http://www.inf.ed.ac.uk/teaching/courses/inf1/A/testpage.html\">FP Website</a><br><b>Lecturer:</b> "
            , "mailto:dts@inf.ed.ac.uk\">Don Sannella</a><br><b>TA:</b> "
            , "mailto:irene.vp@ed.ac.uk\">Irene Vlassi</a></body></html>" ]


testAddrBook :: [(Name,Email)]
testAddrBook = [ ("Don Sannella","dts@inf.ed.ac.uk")
               , ("Irene Vlassi","irene.vp@ed.ac.uk")]

-- </sample data>
-- <system interaction>

getURL :: String -> IO String
getURL url = simpleHTTP (getRequest url) >>= getResponseBody

emailsFromURL :: URL -> IO ()
emailsFromURL url =
  do html <- getURL url
     let emails = (emailsFromHTML html)
     putStr (ppAddrBook emails)

emailsByNameFromURL :: URL -> Name -> IO ()
emailsByNameFromURL url name =
  do html <- getURL url
     let emails = (emailsByNameFromHTML html name)
     putStr (ppAddrBook emails)

-- </system interaction>
-- <exercises>

-- 1.
sameString :: String -> String -> Bool
sameString a b = map toUpper a == map toUpper b


-- 2.
prefix :: String -> String -> Bool
prefix a b = sameString a (take (length a) b)

prop_prefix_pos :: String -> Int -> Bool
prop_prefix_pos str n =  prefix substr (map toLower str) &&
                         prefix substr (map toUpper str)
                           where
                             substr  =  take n str

prop_prefix_neg :: String -> Int -> Bool --returns false for other cases
prop_prefix_neg str n = sameString str substr || (not $ prefix str substr)
                          where substr = take n str
        
        
-- 3.
contains :: String -> String -> Bool
contains [] _ = False
contains (x:xs) subStr
    | prefix subStr (x:xs) = True
    | otherwise = contains xs subStr

prop_contains :: String -> Int -> Int -> Bool
prop_contains str a b
    | length str>0 && a<b = contains str substr
    | length str>0 && b<a = prop_contains str b a
    | otherwise = True
    where
        substr = [fst x | x<-zip str [0..], snd x>a, snd x<b]


-- 4.
takeUntil :: String -> String -> String
takeUntil _ [] = []
takeUntil subStr (x:xs)
    | prefix subStr (x:xs) = []
    | otherwise = x : takeUntil subStr xs

dropUntil :: String -> String -> String
dropUntil _ [] = []
dropUntil subStr (x:xs)
    | prefix subStr (x:xs) = drop (length subStr) (x:xs)
    | otherwise = dropUntil subStr xs


-- 5.
split :: String -> String -> [String]
split _ [] = []
split subStr str = added : split subStr (drop (length added + length subStr) str)
    where added = takeUntil subStr str

reconstruct :: String -> [String] -> String
reconstruct _ [] = []
reconstruct subStr (x:xs)
    | length (x:xs) > 1 = x ++ subStr ++ reconstruct subStr xs
    | otherwise = x

prop_split :: Char -> String -> String -> Bool
prop_split c sep str = reconstruct sep' (split sep' str) `sameString` str
  where sep' = c : sep

-- 6.
linksFromHTML :: HTML -> [Link]
linksFromHTML str = split linkStarter (dropUntil linkStarter str)
    where linkStarter = "<a href=\""

testLinksFromHTML :: Bool
testLinksFromHTML  =  linksFromHTML testHTML == testLinks


-- 7.
takeEmails :: [Link] -> [Link]
takeEmails [] = []
takeEmails (x:xs)
    | prefix "mailto:" x = x : takeEmails xs
    | otherwise = takeEmails xs


-- 8.
link2pair :: Link -> (Name, Email)
link2pair str = (takeUntil nameEnd (dropUntil nameStart str), takeUntil emailEnd (dropUntil emailStart str))
    where
        nameStart = "\">"
        nameEnd = "</a>"
        emailStart = "mailto:"
        emailEnd = "\">"


-- 9.
emailsFromHTML :: HTML -> [(Name,Email)]
emailsFromHTML str = [ link2pair x | x<-(takeEmails (linksFromHTML str)) ]

testEmailsFromHTML :: Bool
testEmailsFromHTML  =  emailsFromHTML testHTML == testAddrBook


-- 10.
findEmail :: Name -> [(Name, Email)] -> [(Name, Email)]
findEmail _ [] = []
findEmail str ((a,b):xs)
    | contains a str = (a,b) : findEmail str xs
    | otherwise = findEmail str xs


-- 11.
emailsByNameFromHTML :: HTML -> Name -> [(Name,Email)]
emailsByNameFromHTML str name = findEmail name (emailsFromHTML str)


-- Optional Material

-- 14.
hasInitials :: String -> Name -> Bool
hasInitials str name = str == [x | x<-name, isUpper x]

-- 15.
emailsByMatchFromHTML :: (Name -> Bool) -> HTML -> [(Name, Email)]
emailsByMatchFromHTML f html = [(a,b) | (a,b)<-(emailsFromHTML html), f a]

emailsByInitialsFromHTML :: String -> HTML -> [(Name, Email)]
emailsByInitialsFromHTML initials html = emailsByMatchFromHTML f html
    where f name = hasInitials initials name

-- 16.

-- If your criteria use parameters (like hasInitials), change the type signature.
-- Testing if has initials that start with the same letter and have the other letters trailing (capitalized)
myCriteria :: Name -> String -> Bool
myCriteria (a:a') (b:b') = (a == b) && (myCriteria' a' b')
    where
        myCriteria' :: Name -> String -> Bool
        myCriteria' [] _ = True
        myCriteria' _ [] = False
        myCriteria' (x:xs) (y:ys)
            | isUpper y && x==y = myCriteria xs ys
            | isUpper y && length (x:xs)>0 = myCriteria (x:xs) ys
            | otherwise = myCriteria (x:xs) ys

emailsByMyCriteriaFromHTML :: String -> HTML -> [(Name, Email)]
emailsByMyCriteriaFromHTML initials html = emailsByMatch myCriteria initials html
    where
        f name = myCriteria initials name
        emailsByMatch :: (Name -> String -> Bool) -> String -> HTML -> [(Name,Email)]
        emailsByMatch f inits html = [(a,b) | (a,b)<-(emailsFromHTML html), f inits a]

-- 17.
ppAddrBook :: [(Name, Email)] -> String
ppAddrBook [] = []
ppAddrBook (x:xs) = (orgName (fst x) ++ "\t\t" ++ (snd x) ++ "\n") ++ ppAddrBook xs
        where
            orgName :: String -> String
            orgName name
                | contains ", " name = name
                | otherwise = (dropUntil " " name) ++ ", " ++ (takeUntil " " name)

--unlines [ name ++ ": " ++ email | (name,email) <- addr ]
