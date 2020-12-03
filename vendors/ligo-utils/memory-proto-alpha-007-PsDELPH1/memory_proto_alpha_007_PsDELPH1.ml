module Name = struct let name = "alpha" end
module Alpha_environment = Tezos_protocol_ligo007_PsDELPH1.Protocol.Environment

type alpha_error = Alpha_environment.Error_monad.error
type 'a alpha_tzresult = 'a Alpha_environment.Error_monad.tzresult
module Alpha_error_monad = Alpha_environment.Error_monad
module Proto = Tezos_protocol_ligo007_PsDELPH1
include Proto
