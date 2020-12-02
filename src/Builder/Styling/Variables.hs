{-# LANGUAGE OverloadedStrings #-}
module Builder.Styling.Variables
  ( appColors
  , Colors(..)
  )
where

import           Clay

data Colors = Colors {
    primary8 :: Color,
    primary7 :: Color,
    primary6 :: Color,
    primary5 :: Color,
    primary4 :: Color,
    primary3 :: Color,
    primary2 :: Color,
    primary1 :: Color,
    grey8 :: Color,
    grey7 :: Color,
    grey6 :: Color,
    grey5 :: Color,
    grey4 :: Color,
    grey3 :: Color,
    grey2 :: Color,
    grey1 :: Color,
    success5 :: Color,
    success4 :: Color,
    success3 :: Color,
    success2 :: Color,
    success1 :: Color
}

primaryCode :: Color
primaryCode = "#222831"

greyCode :: Color
greyCode = "#111"

successCode :: Color
successCode = "#61b15a"

appColors :: Colors
appColors = Colors { primary8 = primaryCode
                   , primary7 = lighten 0.15 primaryCode
                   , primary6 = lighten 0.3 primaryCode
                   , primary5 = lighten 0.45 primaryCode
                   , primary4 = lighten 0.55 primaryCode
                   , primary3 = lighten 0.65 primaryCode
                   , primary2 = lighten 0.75 primaryCode
                   , primary1 = lighten 0.8 primaryCode
                   , grey8    = greyCode
                   , grey7    = lighten 0.15 greyCode
                   , grey6    = lighten 0.3 greyCode
                   , grey5    = lighten 0.45 greyCode
                   , grey4    = lighten 0.6 greyCode
                   , grey3    = lighten 0.75 greyCode
                   , grey2    = lighten 0.85 greyCode
                   , grey1    = lighten 0.9 greyCode
                   , success5 = successCode
                   , success4 = lighten 0.1 greyCode
                   , success3 = lighten 0.2 greyCode
                   , success2 = lighten 0.3 greyCode
                   , success1 = lighten 0.4 greyCode
                   }


-- $primary-8: #222831;
-- $primary-7: lighten($primary-8, 15);
-- $primary-6: lighten($primary-8, 30);
-- $primary-5: lighten($primary-8, 45);
-- $primary-4: lighten($primary-8, 55);
-- $primary-3: lighten($primary-8, 65);
-- $primary-2: lighten($primary-8, 75);
-- $primary-1: lighten($primary-8, 80);
