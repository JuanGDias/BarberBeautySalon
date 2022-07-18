module Agendamento where
  data Agendamento = Agendamento {
    numIdent :: String,
    nomeEmpresa :: String,
    cpfCliente :: String,
    nomeServico :: String,
    diaServico :: String,
    horaServico :: String,
    statusServico :: String
  } deriving (Read, Show)