{-# LANGUAGE OverloadedStrings #-}
module Builder.Styling.Style
  ( pageStyle
  )
where

import           Clay
import           Builder.Styling.Variables      ( appColors
                                                , Colors(..)
                                                )

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
    color $ primary1 appColors
    fontSize $ px 20
    fontFamily ["Roboto"] [sansSerif]
    background (primary8 appColors)
  containerCss
  bodyCss
  navCss
  a ? do
    -- textDecorationLine none
    color $ primary1 appColors
    ":hover" & do
      fontWeight bold
