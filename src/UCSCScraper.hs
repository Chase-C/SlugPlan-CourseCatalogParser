{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module UCSCScraper where

import Prelude              hiding (readFile, putStrLn)
import Pdf.Toolbox.Document hiding (isLeft)
import Data.Text.IO  (readFile, putStrLn, putStr)
import Data.Maybe    (catMaybes, mapMaybe)
import Data.Char     (toUpper)
import Data.List     (sort, group, partition)
import Data.Text     (Text, unpack, pack)
import Data.Either   (isLeft, lefts, rights)
import Control.Monad (void)

import Text.Parsec         ((<|>))
import Control.Applicative ((<*), (<$>))
import Control.Monad       (when)

import qualified Data.Text       as T
import qualified Text.Parsec     as Parsec
import qualified Data.Map.Strict as Map

import ParserTypes
import Subjects
import PrereqParser

parseText :: FilePath -> IO ()
parseText filename = do
    input <- readFile filename
    let classes = Parsec.parse getAllSubjectCourses filename input
    case classes of
        (Right cs) -> printMap cs
        (Left err) -> print err

printMap :: SubjectMap -> IO ()
printMap =
    mapM_ (\(subject, classes) -> do
        putStrLn $ pack $ subjectName subject
        mapM_ printCourse classes
        ) . Map.toList

printCourse :: Course -> IO ()
printCourse (Course { .. }) =
    putStrLn $ T.concat [num, ". ", name, ": ", desc, ": ",pack $ show preq]
    where num = T.justifyRight 4 ' ' number

test :: UCSCParser a -> UCSCParser a
test p = Parsec.try $ (Parsec.try p) <|> (Parsec.anyChar >> test p)

getOut3 :: IO Text
getOut3 = readFile "out3.txt"

getPrereqsAt :: Text -> Int -> (Subject, [Text])
getPrereqsAt input n =
    let t = Parsec.parse (go n) "" input
    in  case t of
        Right t' -> let preqs = filter (not . T.null) $ map preq $ snd t'
                        sub   = fst t'
                    in  (sub, preqs)
        Left err -> (AcadEnglish, [pack $ show err])

compareAt :: Subject -> [Text] -> Int -> (Text, Either Parsec.ParseError PrereqTree)
compareAt sub preqs n =
    (preqs !! n, parsePrereqs sub (preqs !! n))

compareAll :: Subject -> [Text] -> [(Text, Either Parsec.ParseError PrereqTree)]
compareAll sub preqs =
    let comps        = zip preqs $ map (parsePrereqs sub) preqs
        notOK (p, t) = countCourses sub p /= countPreqs t
    in  filter notOK comps

go :: Int -> UCSCParser (Subject, [Course])
go 1 = getNextSubjectCourses
go n = getNextSubjectCourses >> go (n - 1)

classAt :: Subject -> Int -> UCSCParser Course
classAt sub 1 = getCourse sub
classAt sub n = getCourse sub >> classAt sub (n - 1)

classes :: UCSCParser (Subject, [Course])
classes = do
    subject <- test subjectBegin
    courses <- Parsec.manyTill (getCourse subject) (Parsec.try subjectEnd)
    return (subject, courses)

getAllSubjectCourses :: UCSCParser SubjectMap
getAllSubjectCourses = Map.fromList <$> (Parsec.many $ Parsec.try getNextSubjectCourses)

getNextSubjectCourses :: UCSCParser (Subject, [Course])
getNextSubjectCourses = (Parsec.try $ do
    subject <- subjectBegin
    courses <- Parsec.manyTill (getCourse subject) (Parsec.try subjectEnd)
    return (subject, courses)
    ) <|> (Parsec.anyChar >> getNextSubjectCourses)

anySubjectHeader :: UCSCParser String
anySubjectHeader = Parsec.try $ createMultiParser $
    map ((map toUpper) . subjectName) [AcadEnglish ..]

subjectBegin :: UCSCParser Subject
subjectBegin = do
    header <- anySubjectHeader
    Parsec.spaces
    Parsec.optional $ Parsec.string "PROGRAM"
    Parsec.spaces
    Parsec.optional $ Parsec.string "COURSES"
    Parsec.spaces
    courseHeader
    return $ fst $ head $ filter (\(_, s) -> s == header) subjects
    where subjects = map (\s -> (s, map toUpper $ subjectName s)) [AcadEnglish ..]

subjectEnd :: UCSCParser ()
subjectEnd = do
    Parsec.spaces
    void $ Parsec.string "* Not offered"
            <|> Parsec.try revisedString
            <|> Parsec.lookAhead anySubjectHeader

revisedString :: UCSCParser String
revisedString = do
    Parsec.string "Revised"
    Parsec.optional $ Parsec.char ':'
    Parsec.char ' '
    Parsec.count 2 Parsec.digit
    Parsec.char '/'
    Parsec.count 2 Parsec.digit
    Parsec.char '/'
    (Parsec.try $ Parsec.count 4 Parsec.digit) <|> Parsec.count 2 Parsec.digit

subjectHeader :: Subject -> UCSCParser String
subjectHeader = Parsec.string . (map toUpper) . subjectName

courseHeader :: UCSCParser String
courseHeader = do
    (Parsec.char 'L' >> (Parsec.string "OWER-DIVISION COURSES"
                     <|> Parsec.string "ower-Division Courses"))
        <|> (Parsec.char 'U' >> ((Parsec.string "PPER"
                                  >> Parsec.oneOf " -"
                                  >> Parsec.string "DIVISION COURSES")
                             <|> Parsec.string "pper-Division Courses"))
        <|> (Parsec.char 'G' >> (Parsec.string "RADUATE COURSES"
                             <|> Parsec.string "raduate Courses"))

uniqueSubjectParser :: Subject -> (Bool, UCSCParser String)
uniqueSubjectParser HistoryArt = (True, Parsec.choice
    [ Parsec.string "Europe and the Americas"
    , Parsec.string "Modern Art and Visual Culture in \nEurope and the Americas"
    , Parsec.string "Renaissance"
    , Parsec.string "Oceania and its Diaspora"
    , Parsec.string "Cross-Regional Studies"
    ])
uniqueSubjectParser subject    = (False, return "")

getCourse :: Subject -> UCSCParser Course
getCourse sub = do
    courseBegin
    num     <- pack <$>                    getCourseNum
    name    <- pack <$> removeNewlines <$> getCourseName
    qrtrs   <-                             getCourseQuarters
    desc    <- pack <$> removeNewlines <$> getCourseDescription
    prereqs <- pack <$> removeNewlines <$> getCoursePrereqString
    gotoCourseEnd
    courseEnd

    let (unique, parser) = uniqueSubjectParser sub

    Parsec.optional $ Parsec.try $ newPage sub
    when unique $ Parsec.optional $ Parsec.try parser

    return Course
        { subject = pack $ subjectName   sub
        , prefix  = pack $ subjectPrefix sub
        , number  = num
        , name    = name
        , qrtrs   = qrtrs
        , desc    = desc
        , preq    = prereqs
        }

courseBegin :: UCSCParser ()
courseBegin = do
    Parsec.spaces
    Parsec.optional $ Parsec.try courseHeader
    Parsec.spaces

courseEnd :: UCSCParser ()
courseEnd = do
    Parsec.space
    Parsec.spaces
    Parsec.try profName
        <|> (Parsec.string "The" >> Parsec.spaces >> Parsec.string "Staff")
    Parsec.optional $ Parsec.char ',' >> courseEnd
    Parsec.spaces

gotoCourseEnd :: UCSCParser String
gotoCourseEnd = Parsec.manyTill Parsec.anyChar $ Parsec.try $ Parsec.lookAhead courseEnd

newPage :: Subject -> UCSCParser ()
newPage subject = do
    Parsec.optional $ Parsec.many Parsec.digit >> newline'
    Parsec.string $ subjectSection subject
    spaces
    newline'

getCourseNum :: UCSCParser String
getCourseNum = do
    num <- Parsec.many1 Parsec.digit
    sub <- Parsec.many  Parsec.letter
    Parsec.char '.'
    Parsec.spaces
    return $ num ++ sub

getCourseName :: UCSCParser String
getCourseName = Parsec.manyTill Parsec.anyChar $ Parsec.try endName
    where endName = do
            Parsec.char '.'
            (Parsec.try $ do
                Parsec.many1 Parsec.space
                Parsec.lookAhead $ Parsec.oneOf "FSW*" >> void (Parsec.oneOf " ,*\n")
                ) <|> (spaces >> newline' >> Parsec.notFollowedBy Parsec.lower)

getCourseQuarters :: UCSCParser [Quarter]
getCourseQuarters = (Parsec.try $ do
    val    <- Parsec.oneOf "FSW*"
    others <- (Parsec.try $ do
        Parsec.spaces
        Parsec.char ','
        Parsec.spaces
        getCourseQuarters
        ) <|> (spaces >> Parsec.lookAhead newline' >> return [])

    return $ case val of
        'F' -> Fall   : others
        'S' -> Spring : others
        'W' -> Winter : others
        _   ->          others
    ) <|> (Parsec.spaces >> return [])

getCourseDescription :: UCSCParser String
getCourseDescription = Parsec.manyTill Parsec.anyChar
                                     $ Parsec.try $ Parsec.lookAhead descEnd
    where descEnd = Parsec.try (void $ Parsec.string "Prerequisite(s): ") <|> prereqEnd

getCoursePrereqString :: UCSCParser String
getCoursePrereqString = (Parsec.try $ do
    Parsec.string "Prerequisite(s):"
    Parsec.spaces
    Parsec.manyTill Parsec.anyChar $ Parsec.try $ Parsec.lookAhead prereqEnd
    ) <|> return ""

prereqEnd :: UCSCParser ()
prereqEnd = Parsec.try (void $ Parsec.string "(General") <|> courseEnd

profName :: UCSCParser String
profName = do
    Parsec.notFollowedBy $ Parsec.choice $ map Parsec.string $
        [ "I. Readings"
        , "A. The"
        ]
    initial <- Parsec.upper
    Parsec.char '.'
    Parsec.many1 Parsec.space
    first  <- Parsec.upper
    second <- Parsec.lower
    tail   <- Parsec.manyTill Parsec.letter $ Parsec.lookAhead $ Parsec.oneOf " -,\n"
    extra  <- Parsec.option "" $ Parsec.try $ do
        Parsec.oneOf " -"
        Parsec.optional newline'
        Parsec.notFollowedBy $ Parsec.string "Revised"
        first'  <- Parsec.upper
        second' <- Parsec.lower
        tail'   <- Parsec.manyTill Parsec.letter $ Parsec.lookAhead $ Parsec.oneOf " ,\n"
        return (' ':first':second':tail')
    endName
    return $ (initial : ". ") ++ (first:second:tail) ++ extra
    where endName = (Parsec.char ',' >> void courseEnd) <|> (spaces >> newline')

newline' :: UCSCParser ()
newline' = void $ Parsec.newline

spaces :: UCSCParser ()
spaces = void $ Parsec.many $ Parsec.char ' '

removeNewlines :: String -> String
removeNewlines = filter ((/=) '\n') . removeNewlines'

removeNewlines' :: String -> String
removeNewlines' (x:y:z:xs)
    | noSpace   = x : ' ' : z : removeNewlines' xs
    | newline   = x       : z : removeNewlines' xs
    | otherwise = x           : removeNewlines' (y : z : xs)
    where newline = y == '\n'
          noSpace = and [newline, x /= ' ', z /= ' ']
removeNewlines' xs = xs

--parsePdf :: FilePath -> IO String
--parsePdf filename =
--    withBinaryFile filename ReadMode $ \handle -> do
--        res <- runPdfWithHandle handle knownFilters $ do
--            pdf      <- document
--            catalog  <- documentCatalog pdf
--            rootNode <- catalogPageNode catalog
--            kids     <- pageNodeKids rootNode
--            count    <- pageNodeNKids rootNode
--
--            concat <$> mapM (getPageText rootNode) [3..(count - 1)]
--            --mapM_ printKid kids
--            --page <- pageNodePageByNum rootNode 2
--            --printPage page
--        case res of
--            (Left err)   -> print err >> return ""
--            (Right strn) -> return strn
--
--printKid :: (MonadPdf m, MonadIO m) => Ref -> PdfE m ()
--printKid kid = do
--    pageNode <- loadPageNode kid
--    case pageNode of
--        (PageTreeNode node) -> do
--            kids <- pageNodeKids node
--            count <- pageNodeNKids node
--            liftIO $ print $ "Node with " ++ show count ++ " children"
--            mapM_ printKid kids
--        (PageTreeLeaf leaf) -> do
--            text <- pageExtractText leaf
--            liftIO $ print text
--
--getPageText :: (MonadPdf m, MonadIO m) => PageNode -> Int -> PdfE m String
--getPageText node n = do
--    page <- pageNodePageByNum node n
--    text <- pageExtractText page
--    return $ unpack text
