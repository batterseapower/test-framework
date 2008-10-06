module Test.Framework.Runners.Console.Colors where

import Text.PrettyPrint.ANSI.Leijen


colorFail, colorPass :: Doc -> Doc
colorFail = red
colorPass = green

colorPassOrFail :: Bool -> Doc -> Doc
colorPassOrFail True  = colorPass
colorPassOrFail False = colorFail