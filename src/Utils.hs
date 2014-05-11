module Utils where

import Language.C

parseFromString :: String -> CTranslUnit
parseFromString s =
  case (parseC (inputStreamFromString s) (nopos)) of
    Left err -> error $ show err
    Right tu -> tu

parseStmtFromString :: String -> CStat
parseStmtFromString s =
  case (execParser statementP (inputStreamFromString s) (nopos) [] newNameSupply) of
    Left err -> error $ show err
    Right (tu, _) -> tu
