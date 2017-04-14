--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
import           Data.Monoid (mappend)
import           Hakyll
import           Data.Time.Clock               (UTCTime (..))
import           Data.Time.Locale.Compat       (defaultTimeLocale)
import           Control.Monad
import           Control.Arrow
import           Control.Applicative
import           Data.Maybe
import           Data.List
import           Data.Ord
import           Data.Function
import qualified Data.Map as M
import           Text.Pandoc.Shared (headerShift)
import           Data.Time.Format

--------------------------------------------------------------------------------
newtype Year = Year Int deriving (Eq, Ord)
data Quarter = Fall Year | Winter Year | Spring Year deriving (Eq)
instance Ord Quarter where
  (Fall y1)   `compare` (Fall y2)   = y1 `compare` y2
  (Fall y1)   `compare` (Winter y2) = if y1 < y2 then LT else GT
  (Fall y1)   `compare` (Spring y2) = if y1 < y2 then LT else GT
  (Winter y1) `compare` (Fall y2)   = if y1 <= y2 then LT else GT
  (Winter y1) `compare` (Winter y2) = y1 `compare` y2
  (Winter y1) `compare` (Spring y2) = if y1 <= y2 then LT else GT
  (Spring y1) `compare` (Fall y2)   = if y1 <= y2 then LT else GT
  (Spring y1) `compare` (Winter y2) = if y1 < y2 then LT else GT
  (Spring y1) `compare` (Spring y2) = y1 `compare` y2
toQuarter :: String -> Quarter
toQuarter ('f':'a':xs) = Fall (Year $ read xs)
toQuarter ('w':'i':xs) = Winter (Year $ read xs)
toQuarter ('s':'p':xs) = Spring (Year $ read xs)
fromQuarter :: Quarter -> String
fromQuarter (Fall (Year y)) = "fa" ++ show y
fromQuarter (Winter (Year y)) = "wi" ++ show y
fromQuarter (Spring (Year y)) = "sp" ++ show y

main :: IO ()
main = hakyll $ do
    match "static/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "events/*/*" $ do
        route $ setExtension "html"
        let pandocCompilerLevelShift = pandocCompilerWithTransform
                                           defaultHakyllReaderOptions
                                           defaultHakyllWriterOptions
                                           (headerShift 1)
        compile $ pandocCompilerLevelShift
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/event.html"   eventCtx
            >>= loadAndApplyTemplate "templates/default.html" eventCtx
            >>= relativizeUrls

    pag <- buildPaginateWith quarterGrouper "events/*/*" eventPageId
    paginateRules pag $ \num pat -> do
      let q num = fromQuarter
                . getQuarter
                . head
                . fromJust
                . M.lookup num
                $ paginateMap pag
      let thisQ = q num
      route $ customRoute $ \_ -> "events-" ++ thisQ ++ ".html"
      let allQs = reverse
                . M.elems
                . M.mapWithKey (curry $ (id *** getQuarter . head))
                $ paginateMap pag
      let allQItems = sequence $ map makeItem allQs
      compile $ do
        events <- chronological' =<< loadAll pat
        let pageCtx =
              boolField "isCurrent" ((num==) . fst . itemBody)     `mappend`
              field "qa"  (return . fromQuarter . snd . itemBody)  `mappend`
              field "num" (return . show . fst . itemBody)         `mappend`
              field "url" ( fmap fromJust
                          . getRoute
                          . eventPageId
                          . fst
                          . itemBody
                          )
        let eventsCtx =
              constField "title" ("Events List - " ++ thisQ) `mappend`
              extraCss ["/css/paginate.css"]                 `mappend`
              listField "pages" pageCtx allQItems            `mappend`
              listField "events" eventCtx (return events)    `mappend`
              paginateContext pag num                        `mappend`
              defaultContext
        makeItem ""
          >>= loadAndApplyTemplate "templates/event-paginate.html" eventsCtx
          >>= loadAndApplyTemplate "templates/default.html"        eventsCtx
          >>= relativizeUrls

    create ["events.html"] $ do
      route idRoute
      -- Figure out which quarter to do by looking at the next
      -- upcoming event and then reverse-looking-up
      compile $ do
        event <- ((head <$> (nextNEvents 1 =<< loadAll "events/*/*")) :: Compiler (Item String))
        let q = getQuarter (itemIdentifier event)
        let n = fst . M.findMin . M.filter ((q==) . snd)
              $ M.map ((id &&& getQuarter . head)) (paginateMap pag)
        loadBody (eventPageId n) >>= makeItem :: Compiler (Item String)

    create ["events.ics"] $ do
      route idRoute
      compile $ do
        events <- loadAll "events/*/*"
        let icalDateTimeFmt = ":%Y%m%dT%H%M%S"
            icalDateFmt = ";VALUE=DATE:%Y%m%d"
            fmtDateTime = fmap (formatTime defaultTimeLocale icalDateTimeFmt)
            fmtDate     = fmap (formatTime defaultTimeLocale icalDateFmt)
            fmtdTime f = liftA2 (<|>)
              (fmtDateTime . itemTime' [dateTimeFormat] f)
              (fmtDate     . itemTime' [dateFormat] f)
            icalEvCtx =
              field "uid"   (fmtDateTime . itemTime "start")       `mappend`
              field "start" (fmtdTime "start")                     `mappend`
              field "end"   (fmtdTime "end")                       `mappend`
              modificationTimeField "lastmodified" icalDateTimeFmt `mappend`
              eventCtx
            eventsCtx =
              listField "events" icalEvCtx (return events) `mappend`
              defaultContext
        makeItem ""
          >>= loadAndApplyTemplate "templates/ical.ics" eventsCtx

    match "index.html" $ do
        route idRoute
        compile $ do
          events <- nextNEvents 5 =<< loadAll "events/*/*"
          let indexCtx =
                extraCss ["/css/gcal.css"] `mappend`
                listField "events" eventCtx (return events) `mappend`
                defaultContext

          getResourceBody
            >>= applyAsTemplate indexCtx
            >>= loadAndApplyTemplate "templates/default.html" indexCtx
            >>= relativizeUrls

    match "business.md" $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "talks.md" $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
eventCtx :: Context String
eventCtx =
    --dateField "date" "%B %e, %Y %l:%M %p"       `mappend`
    --constField "date" "This is a constant"      `mappend`
    field "date" formatItemDate                 `mappend`
    autoTeaserField "teaser" "content"          `mappend`
    defaultContext
  where formatItemDate i = do
          (s, ss) <- (formatFirstTime <$> itemTime' [dateTimeFormat] "start" i)
                 <|> (formatFirstDate <$> itemTime' [dateFormat] "start" i)
          (formatTwoTimes s ss <$> itemTime' [dateTimeFormat] "end" i)
            <|> (formatTwoDates s ss <$> itemTime' [dateFormat] "end" i)
            <|> (return ss)
        dtFmt = "%B %e, %Y"
        tmFmt = "%l:%M %p"
        dtTmFmt = dtFmt ++ " " ++ tmFmt
        formatFirstTime :: UTCTime -> (UTCTime, String)
        formatFirstTime = id &&& formatTime defaultTimeLocale dtTmFmt
        formatFirstDate :: UTCTime -> (UTCTime, String)
        formatFirstDate = id &&& formatTime defaultTimeLocale dtFmt
        formatTwoTimes :: UTCTime -> String -> UTCTime -> String
        formatTwoTimes s ss e | utctDay s == utctDay e =
                                  ss ++
                                  " to " ++
                                  formatTime defaultTimeLocale tmFmt e
                              | otherwise =
                                  ss ++
                                  " to " ++
                                  formatTime defaultTimeLocale dtTmFmt e
        formatTwoDates :: UTCTime -> String -> UTCTime -> String
        formatTwoDates s ss e | utctDay s == utctDay e = ss
                              | otherwise =
                                ss ++
                                " to " ++
                                formatTime defaultTimeLocale dtFmt e
        {-formatItemDate i = do
          s <- itemTime "start" i
          (formatTwoTimes s <$> itemTime "end" i) <|> return (formatOneTime s)
        formatTwoTimes s e =
          if (utctDay s == utctDay e)
          then ( formatTime defaultTimeLocale "%B %e, %Y %l:%M %p" s
              ++ " to "
              ++ formatTime defaultTimeLocale "%l:%M %p" e
               )
          else ( formatTime defaultTimeLocale "%B %e, %Y %l:%M %p" s
              ++ " to "
              ++ formatTime defaultTimeLocale "%B %e, %Y %l:%M %p" e
               )
        formatOneTime = formatTime defaultTimeLocale "%B %e, %Y %l:%M %p"-}

autoTeaserField :: String -> Snapshot -> Context String
autoTeaserField key snapshot = field key $ \item -> do
    body <- itemBody <$> loadSnapshot (itemIdentifier item) snapshot
    return $ (unwords . take 50 . words) body

chronological' :: MonadMetadata m => [Item a] -> m [Item a]
chronological' = fmap sortNewest . annotateTimes
  where annotateTimes :: MonadMetadata m => [Item a] -> m [(Item a, UTCTime)]
        annotateTimes = mapM $ sequence . (id &&& itemTime "start")
        sortNewest :: [(Item a, UTCTime)] -> [Item a]
        sortNewest = map fst . sortOn snd
nextNEvents :: MonadMetadata m => Int -> [Item a] -> m [Item a]
nextNEvents n = fmap (take n) . chronological' <=< dropOld
  where dropOld :: MonadMetadata m => [Item a] -> m [Item a]
        dropOld = filterM ( fmap isJust
                          . flip getMetadataField "future"
                          . itemIdentifier
                          )

itemTime :: (MonadMetadata m) => String -> Item a -> m UTCTime
itemTime = itemTime' [dateTimeFormat, dateFormat]
dateTimeFormat :: String
dateTimeFormat = "%Y-%m-%d %H:%M:%S"
dateFormat :: String
dateFormat = "%Y-%m-%d"
itemTime' :: (MonadMetadata m) => [String] -> String -> Item a -> m UTCTime
itemTime' fmts f =  tryFormats fmts
                <=< flip getMetadataField' f
                 .  itemIdentifier
  where tryFormats :: (ParseTime t, Monad m) => [String] -> String -> m t
        tryFormats fmts s = maybe (fail "No time parse") return . msum $
          map (\x -> parseTimeM True defaultTimeLocale x s) fmts

getQuarter :: Identifier -> Quarter
getQuarter = toQuarter . head . fromJust . capture (fromGlob "events/*/*")

quarterGrouper :: [Identifier] -> Rules [[Identifier]]
quarterGrouper = return . compareQuarters . annotateQuarters
  where annotateQuarters :: [Identifier] -> [(Identifier, Quarter)]
        annotateQuarters = map (id &&& getQuarter)
        compareQuarters :: [(Identifier, Quarter)] -> [[Identifier]]
        compareQuarters = fmap (fst<$>) . groupBy ((==) `on` snd)
                        . sortBy (comparing (Down . snd))

eventPageId :: PageNumber -> Identifier
eventPageId num = fromFilePath $ "events-" ++ (show num) ++ ".html" -- Not actually used

extraCss :: [String] -> Context String
extraCss = listField "extraCss" (bodyField "url") . mapM makeItem
