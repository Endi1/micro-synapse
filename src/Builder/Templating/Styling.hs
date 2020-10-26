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
  marginLeft $ em 15
  marginRight $ em 20

bodyCss :: Css
bodyCss = ".body" ? do
  display flex
  ".center" & do
    justifyContent center

navCss :: Css
navCss = ".navbar" ? do
  display flex
  justifyContent center
  background primaryColor
  ul ? do
    display flex
    li ? do
      marginRight $ em 3
      a ? do
        color $ lighten 0.8 primaryColor
        ":hover" & do
          color $ lighten 1 primaryColor

pageStyle :: Css
pageStyle = do
  Clay.html ? do
    color blackColor
    fontSize $ px 20
    fontFamily [] [sansSerif]
    background (lighten 0.9 blackColor)
  containerCss
  bodyCss
  navCss
  ul ? do
    listStyle none outside none
  a ? do
    textDecorationLine none
    color primaryColor
    ":hover" & do
      color (lighten 0.5 primaryColor)
