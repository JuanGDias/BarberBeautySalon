module Cliente where
  data Cliente = Cliente {
    nomeCliente :: String,
    emailCliente :: String,
    cpfCliente :: String,
    telefoneCliente :: String,
    senhaCliente :: String
  } deriving (Read, Show)