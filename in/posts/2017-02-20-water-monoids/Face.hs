import Data.Map (Map)
import qualified Data.Map as Map

type Corners = Map Height Width

newtype LeftFace = LeftFace Corners

newtype RightFace = RightFace Corners

instance Monoid LeftFace where
    mempty = emptyFace
    mappend near far = overlapFaces near far

instance Monoid RightFace where
    mempty = emptyFace
    mappend far near = overlapFaces near far
