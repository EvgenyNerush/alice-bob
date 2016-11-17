module Main where

import Options.Applicative
import Lib

-- data Options = Options { send :: Bool }
data Options = Options { port :: Int, to :: String, size :: Int }

options :: Parser Options
options = Options <$> option auto ( long "port"
                               <> metavar "PORT"
                               <> help "port to listen or to send to" )
                  <*> strOption ( long "to"
                                <> metavar "IPADDR"
                                <> value "" -- default value
                                <> help "IPv4 address; if set, data will be sent there" )
                  <*> option auto ( long "size"
                               <> metavar "SIZE"
                               <> value 1 -- default value
                               <> help "size of data to be sent, in KB (1024 bytes)" )

run :: Options -> IO ()
run (Options port "" _) = bob port
run (Options port to size) = alice port to size

main :: IO ()
main = execParser opts >>= run
    where opts = info (helper <*> options)
              ( fullDesc
             <> progDesc "Listens PORT for data, or if IPADDR is set, sends data there. \
                         \Try --help for details." )
