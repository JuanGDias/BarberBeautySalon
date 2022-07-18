module Membro where
  data Membro = Membro {
    nomeMembro :: String,
    cpfMembro :: String,
    nomeEmpresa :: String
  } deriving (Read, Show)