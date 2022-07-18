module Empresa where
  data Empresa = Empresa {
    nomeEmpresa :: String,
    emailEmpresa :: String,
    telefoneEmpresa :: String,
    senhaEmpresa :: String
  } deriving (Read, Show)