module Proto3.Suite.Extra
  ( throwParserError,
  )
where

import Data.Text.Lazy (Text)
import Proto3.Wire.Decode

throwParserError :: Text -> Parser a b
throwParserError t = Parser $ \_ -> Left $ BinaryError t
