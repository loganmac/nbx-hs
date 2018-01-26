module Process.Types where

import           Concurrency     (Chan)
import qualified Data.ByteString as B

data Output = Msg B.ByteString | Err B.ByteString | Success | Failure Int

data Processor = Processor (Chan Output)
