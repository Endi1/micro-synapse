{-# LANGUAGE OverloadedStrings #-}
module Builder.Templating.Styling
  ( pageStyle
  )
where

import           Clay


primaryColor :: Color
primaryColor = "#1947BD"

blackColor :: Color
blackColor = "#1E212B"

dangerColor :: Color
dangerColor = "#ED254E"

successColor :: Color
successColor = "#97CC04"

containerCss :: Css
containerCss = ".container" ? do
  display flex
  justifyContent center
  marginLeft $ em 15
  marginRight $ em 20

pageStyle :: Css
pageStyle = do
  Clay.html ? do
    color blackColor
    fontSize $ px 20
    fontFamily [] [sansSerif]
    background (lighten 0.9 blackColor)
  containerCss
  ul ? do
    listStyle none outside none
  a ? do
    textDecorationLine none
    color primaryColor
    ":hover" & do
      color (lighten 0.5 primaryColor)
