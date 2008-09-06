#!/usr/bin/env runhaskell

\begin{code}
import Text.HTML.Download
import Text.HTML.TagSoup
import Network.URL
import Control.Monad


pROJECT_NAME = "test-framework"
rEADME_PAGE = "readme"
rEADME_FILENAME = "README.textile"


encURL :: String -> String
encURL = encString False ok_url

gitHubEditWikiURL :: String -> String -> String
gitHubEditWikiURL project page = encURL $ "http://github.com/batterseapower/" ++ project ++ "/wikis/" ++ page ++ "/edit"

main :: IO ()
main = do
   tags <- liftM parseTags $ openURL (gitHubEditWikiURL pROJECT_NAME rEADME_PAGE)
   let text = innerText $ takeWhile (~/= "</textarea>") $ head $ sections (~== "<textarea id=wiki_body>") tags
   writeFile rEADME_FILENAME text
\end{code}