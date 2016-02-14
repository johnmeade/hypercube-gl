module Hypercube (rotMats, vertices, matDim, verticesWithEdges) where


{-| A library implementing N-dimensional hypercube geometry and rotations.
For use with ThreeJS, and more specifically my simulation wrapper library
SimShim.

# Core functionality
@docs matDim
@docs rotMats
@docs vertices
@docs verticesWithEdges
-}


{------------------------------------------------------------------------------}
{-- Imports --}


import List exposing (..)
import Maybe exposing (andThen)
import String
import Signal
-- import Graphics.Element exposing (show)


{------------------------------------------------------------------------------}
{-- Types --}


type alias Vec = List Float
type alias Matrix = List (List Float)
type alias BlockMatrix = List (List Matrix)
type alias Vertex = List Float


{------------------------------------------------------------------------------}
{-- Misc Helpers --}


zip : List x -> List y -> List (x, y)
zip xs ys =
  case (xs, ys) of
    (x::xs', y::ys') -> (x,y) :: zip xs' ys'
    (_, _) -> []


{-| map a list, where the list has been zipped with an index, and flatten the
result
-}
concatIndexedMap : (Int -> a -> List b) -> List a -> List b
concatIndexedMap fn list =
  concat <| indexedMap fn list


{-| map a list, where each list has been zipped with an index, and filter
the result based on whether the element is a (Just a) or Nothing
-}
indexedFilterMap : (Int -> a -> Maybe b) -> List a -> List b
indexedFilterMap fn list =
  let len = length list
      fn' = \(x, y) -> fn x y
  in filterMap fn' <| zip [0..len-1] list


{-| Appends the contents of a 2-tuple together
-}
appendTuple : ((List a), (List a)) -> (List a)
appendTuple (xs, ys) =
  append xs ys


{-| Duplicate a string n times, returning one big string
-}
dupeString : String -> Int -> String
dupeString s n =
  if n <= 0
    then ""
    else s ++ (dupeString s (n-1))


{-| Convert and Int to a binary string
-}
binString : Int -> String
binString i =
  String.fromList <| binStringAcc i [] --<< List.reverse


{-| Accumulator helper
-}
binStringAcc : Int -> List Char -> List Char
binStringAcc i sofar =
  let parity = i % 2
      next = i // 2
  in case i of
    0 -> '0'::sofar
    1 -> '1'::sofar
    _ ->
      case parity of
        1 -> binStringAcc next <| '1'::sofar
        _ -> binStringAcc next <| '0'::sofar


{-| Convert and Int to a binary string and pad the left side with zeros until
a specified length is reached
-}
paddedBinString : Int -> Int -> String
paddedBinString len i =
  String.padLeft len '0' (binString i)


{-| Convert a binary string to a List of Floats
-}
bin2vec : String -> Vec
bin2vec bin =
  let binList = String.split "" bin
  in map (\x -> if x == "0" then 0 else 1) binList


{-| Return a list of all (Just a) values in a list, but return Nothing if
any of the list elements are Nothing
-}
allOrNothing : List (Maybe a) -> Maybe (List a)
allOrNothing maybeList =
  let maybeAccumulate = \x acc ->
    case (x, acc) of
      (Nothing, _) -> Nothing
      (_, Nothing) -> Nothing
      (Just x', Just acc') -> Just (x'::acc')
  in foldl maybeAccumulate (Just []) maybeList


{------------------------------------------------------------------------------}
{-- Math Helpers --}


{-| Empty matrix
-}
emptyMatrix : Matrix
emptyMatrix = [[]]


{-| Compute the factorial of an Int
-}
factorial : Int -> Int
factorial n =
  if n < 1
    then n
    else n * factorial (n-1)

{-| Compute the dimension of a matrix
-}
matDim : Matrix -> Int
matDim m =
  case m of
    (row::_) -> length row
    _ -> 0


{-| Create a square identity matrix of dimension n
-}
idMat : Int -> Matrix
idMat n =
  if n <= 0
    then emptyMatrix
    else
      let same = \x y -> if x == y then 1 else 0
          idRow = (\x -> map (same x) [0..n-1])
      in map idRow [0..n-1]


{-| Append a row of matrices to each other, ie the k'th row of each matrix
is appended together and become the k'th row of the resulting matrix
-}
appendBlockRow : List Matrix -> Matrix
appendBlockRow blocks =
  case blocks of
    (block::[]) -> block
    (block::restBlocks) ->
      let appendedRow = appendBlockRow restBlocks
      in map appendTuple <| zip block appendedRow
    _ -> emptyMatrix


{-| Append the blocks of a block matrix (4D Float List) together into one matrix
-}
appendBlocks : BlockMatrix -> Matrix
appendBlocks blockMat =
  case blockMat of
    (blockRow::[]) -> appendBlockRow blockRow
    (blockRow::restBlockMat) ->
      case blockRow of
        [] -> appendBlocks restBlockMat
        ([[]]::_) -> appendBlocks restBlockMat
        _ ->
          let appendedBlocks = appendBlockRow blockRow
              restRows = appendBlocks restBlockMat
          in append appendedBlocks restRows
    _ -> emptyMatrix -- empty block matrix input


{-| Wrapper to easily stitch a 2 by 2 block matrix together
-}
appendBlocks2x2 : Matrix -> Matrix -> Matrix -> Matrix -> Matrix
appendBlocks2x2 tl tr bl br =
  appendBlocks [[tl,tr],[bl,br]]


{-| Creates an m by n zero matrix
-}
zeroMat : Int -> Int -> Matrix
zeroMat m n =
  repeat m (repeat n 0)


{-| Create a block diagonal matrix by providing the blocks found on the diagonal

eg:
```
> blockDiag [ [[1]], [[2,2],[2,2]] ]
[ [1, 0, 0]
, [0, 2, 2]
, [0, 2, 2]
]
```
-}
blockDiag : List Matrix -> Matrix
blockDiag blocks =
  case blocks of
    (block::restBlocks) ->
      case block of
        (row::rows) ->
          let m = length block
              n = length row
              topLeft = block
              bottomRight = blockDiag restBlocks
          in case bottomRight of
            [[]] -> topLeft
            (row'::rows') ->
              let m' = (length rows') + 1
                  n' = length row'
                  topRight = zeroMat m n'
                  bottomLeft = zeroMat m' n
              in appendBlocks2x2 topLeft topRight bottomLeft bottomRight
            _ -> emptyMatrix -- dummy value, won't happen
        _ -> blockDiag restBlocks -- this block was an empty matrix
    _ -> emptyMatrix


{-| Pads a float with zeros on the right and bottom up to an m by n matrix
-}
zeroPadMat : Int -> Int -> Matrix -> Matrix
zeroPadMat m n subMat =
  let n' = n-1
      m' = m-1
      tl = subMat
      tr = zeroMat 1  n'
      bl = zeroMat m' 1
      br = zeroMat m' n'
  in appendBlocks2x2 tl tr bl br


{------------------------------------------------------------------------------}
{-- N Dimensional Rotations --}


{-| Finds R of a given size by considering the block matrix form:

[ [ C, 0, -S ]
, [ 0, I,  0 ]
, [ S, 0,  C ]
]

-}
rotMatR : Float -> Int -> Matrix
rotMatR theta n =
  let cosTheta = cos theta
      sinTheta = sin theta
      negSinTheta = negate sinTheta
      rankI = n-2
      zRow = zeroMat 1 rankI
      zCol = zeroMat rankI 1
      c = [[cosTheta]]
      s = [[sinTheta]]
      ns = [[negSinTheta]]
      i = idMat rankI
      blocks = [ [  c   , zRow ,  ns  ]
               , [ zCol ,  i   , zCol ]
               , [  s   , zRow ,  c   ]
               ]
  in appendBlocks blocks


{-| Construct the submatrix of the solution, namely all n by n block matrices

[ [ R,  0 ]
, [ 0, I2 ]
]

-}
rotMatsRI2 : Float -> Int -> List Matrix
rotMatsRI2 theta n =
  let makeRI2mat = \ rankI2 ->
    let blockI2 = idMat rankI2
        blockR = rotMatR theta (n-rankI2)
    in blockDiag [blockR, blockI2]
  in map makeRI2mat [0..n-2]


{-| Compute a set of rotation matrices corresponding to a given dimension.
There will be 2^(n-1) rotation elements for an n-dimensional space.

We will construct all rank n block matrices

[ [ I1, 0,  0 ]
, [  0, R,  0 ]
, [  0, 0, I2 ]
]

such that I1 and I2 are square identity matrices and R is of the form

[ [ cos, 0, ..., 0, -sin ]
, [   0, 0, ..., 0,    0 ]
, [         ...          ]
, [   0, 0, ..., 0,    0 ]
, [ sin, 0, ..., 0,  cos ]
]

Note that the rank of I1 and I2 is in the range [0..n-2] inclusive, and
the size of R is in the range [2..n] inclusive.
-}
rotMats : Float -> Int -> List Matrix
rotMats theta n =
  let -- let X be the rank of I1
      blockMatricesForXEqualTo = \ m ->
        let lenRest = n-m
            topLeft = idMat m
            bottomRightMats = rotMatsRI2 theta lenRest
            appendBottomRight = \ br ->
              blockDiag [topLeft, br]
        in map appendBottomRight bottomRightMats
  in concat <| map blockMatricesForXEqualTo [0..n-2]


{-| Compute the vertices of a hypercube of dimension n
-}
vertices : Int -> List Vec
vertices n =
  let numVerts = 2^n
  in map bin2vec <| map (paddedBinString n) [0..numVerts-1]


{-| Compute the vertices of a hypercube of dimension n, and also finds the
indeces of the pairs of vertices which correspond to edges of the cube.

This is done by mapping vertices into a list of their neighbours (only consider
the neighbours with larger index values to avoid double-counting edges) and
flattening the result
-}
verticesWithEdges : Int -> (List Vec, List (Int, Int))
verticesWithEdges n =
  let numVerts = 2^n
      verts = vertices n
      biggerNeighbours = \index vertex ->
        let getEdgeMates = \twosPlaceInverted digit ->
              let twosPlace = n - twosPlaceInverted
              in case digit of
                0 -> Just (index, index + 2^(twosPlace - 1))
                _ -> Nothing
        in indexedFilterMap getEdgeMates vertex
      pairs = concatIndexedMap biggerNeighbours verts
    in (verts, pairs)


{------------------------------------------------------------------------------}
{-- Interop --}


type alias Bundle =
  { vertices : List Vec
  , edges : List (Int, Int)
  , rotationMatrices : List Matrix
  }


port requestDimension : Signal (Float, Int)


port responseDimension : Signal Bundle
port responseDimension =
  let bundler = \(theta, dim) ->
        let (verts, edgePairs) = verticesWithEdges dim
        in { vertices = verts
           , edges = edgePairs
           , rotationMatrices = rotMats theta dim
           }
  in Signal.map bundler requestDimension
