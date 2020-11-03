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
  marginTop $ em 1
  borderColor primaryColor
  borderStyle solid
  borderWidth $ em 0.05
  borderRadius (em 0.3) (em 0.3) (em 0.3) (em 0.3)
  paddingLeft $ em 1
  paddingRight $ em 1
  ".center" & do
    justifyContent center

navCss :: Css
navCss = ".navbar" ? do
  display flex
  justifyContent center
  borderBottomStyle solid
  borderWidth $ em 0.01
  ul ? do
    listStyle none outside none
    display flex
    li ? do
      marginRight $ em 3

pageStyle :: Css
pageStyle = do
  Clay.html ? do
    color $ lighten 0.1 blackColor
    fontSize $ px 20
    fontFamily [] [sansSerif]
    background (lighten 0.9 blackColor)
  containerCss
  bodyCss
  navCss
  a ? do
    textDecorationLine none
    color primaryColor
    ":hover" & do
      color (lighten 0.5 primaryColor)
