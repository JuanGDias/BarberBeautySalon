module Servico where
  data Servico = Servico {
    nomeServico :: String,
    descricaoServico :: String,
    nomeEmpresa :: String
  } deriving (Read, Show)