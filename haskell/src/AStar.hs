{-# LANGUAGE ScopedTypeVariables #-}

module AStar
  ( astar,
    Mode (ModeSingle, ModeAll),
    Result (NoPathFound, PathsFound),
    Path (Path),
    pathNodes,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.PQueue.Prio.Min (MinPQueue)
import qualified Data.PQueue.Prio.Min as PQ

type ReachedEndFn node = node -> Bool

type SuccessorFn action node = node -> [(action, node)]

type PrimaryCostFn action node cost = (action, node) -> cost

type HeuristicFn node cost = node -> cost

-- | Result of path finding.
data Result action node
  = -- | No paths were found.
    NoPathFound
  | -- | At least one path was found.
    PathsFound [Path action node]
  deriving (Eq, Show)

-- | A path from the start node to the end of the path.
data Path action node
  = Path node [(action, node)]
  deriving (Eq, Show)

-- | Extract all nodes (including the start) from a Path.
pathNodes :: Path action node -> [node]
pathNodes (Path hd ans) = hd : fmap snd ans

-- | Mode of A* - find all paths or just one.
data Mode = ModeSingle | ModeAll deriving (Eq, Show)

-- | A*-Dallas-Multi-Path
--
--   This version of A* finds all paths from the start fo the end node that
--   have equal-best score.
astar ::
  forall action node cost.
  (Ord node, Num cost, Ord cost) =>
  Mode ->
  node ->
  ReachedEndFn node ->
  SuccessorFn action node ->
  PrimaryCostFn action node cost ->
  HeuristicFn node cost ->
  Result action node
astar mode start_node end_fn successor primary_cost heuristic =
  stepUntilDone mode $
    initState start_node end_fn successor primary_cost heuristic

-- | Partial path from the start node to some place within the domain.
data PartialPath action node cost = PartialPath
  { -- | Path taken so far, in reverse order.
    ppVisited :: [(Maybe action, node)],
    -- | Accumulated primary cost (g(n))
    ppAccumulatedPrimaryCost :: cost,
    -- | Total cost (f(n) = g(n) + h(n))
    ppTotalCost :: cost
  }
  deriving (Eq, Show)

-- | State of the A* evaluation loop.
data State action node cost = State
  { -- | Map of visited nodes to their lowest accumulated primary cost.
    stateVisitedNodes :: Map node cost,
    -- | Priority queue of nodes sorted according to `ppTotalCost`.
    statePriorityQueue :: MinPQueue cost (PartialPath action node cost),
    -- | Target node.
    stateReachedEndFn :: ReachedEndFn node,
    -- | Successor function.
    stateSuccessor :: SuccessorFn action node,
    -- | Primary cost function.
    statePrimaryCost :: PrimaryCostFn action node cost,
    -- | Heuristic function.
    stateHeuristic :: HeuristicFn node cost,
    -- | Best `ppTotalCost` cost, if found.
    stateOptimalCost :: Maybe cost,
    -- | Paths already found.
    statePathsFound :: [Path action node]
  }

-- | Set up the initial state for iterations of the A* algorithm.
initState ::
  forall action node cost.
  (Num cost) =>
  node ->
  ReachedEndFn node ->
  SuccessorFn action node ->
  PrimaryCostFn action node cost ->
  HeuristicFn node cost ->
  State action node cost
initState start_node end_fn successor primary_cost heuristic =
  let pp_start :: PartialPath action node cost
      pp_start = PartialPath [(Nothing, start_node)] 0 (heuristic start_node)
   in State
        (Map.singleton start_node 0)
        (PQ.singleton (ppTotalCost pp_start) pp_start)
        end_fn
        successor
        primary_cost
        heuristic
        Nothing
        []

-- | Result of taking a step.
data StepResult action node cost
  = -- | Evaluation should continue using the state contained.
    StepContinue (State action node cost)
  | -- | Evaluation is complete, and we have a result.
    StepDone (State action node cost)

-- | Repeatedly call the `step` function until we have found a result.
stepUntilDone ::
  forall node action cost.
  (Ord node, Num cost, Ord cost) =>
  Mode ->
  State action node cost ->
  Result action node
stepUntilDone mode state =
  case step mode state of
    StepContinue state' -> stepUntilDone mode state'
    StepDone state' ->
      case statePathsFound state' of
        [] -> NoPathFound
        paths -> PathsFound paths

step ::
  forall node action cost.
  (Ord node, Num cost, Ord cost) =>
  Mode ->
  State action node cost ->
  StepResult action node cost
step mode state =
  case PQ.minView (statePriorityQueue state) of
    Nothing -> StepDone state
    Just (min_path, pqueue) ->
      let min_node = ppHead min_path
          end_fn = stateReachedEndFn state
          successor = stateSuccessor state
          primary_cost = statePrimaryCost state
          heuristic = stateHeuristic state
       in if end_fn min_node
            then case mode of
              ModeAll ->
                case stateOptimalCost state of
                  Nothing ->
                    StepContinue $
                      state
                        { statePriorityQueue = pqueue,
                          statePathsFound = [constructPath (ppVisited min_path)],
                          stateOptimalCost =
                            Just $ ppAccumulatedPrimaryCost min_path
                        }
                  Just opt_cost ->
                    if ppAccumulatedPrimaryCost min_path > opt_cost
                      then StepDone state
                      else
                        StepContinue $
                          state
                            { statePriorityQueue = pqueue,
                              statePathsFound =
                                constructPath (ppVisited min_path)
                                  : statePathsFound state
                            }
              ModeSingle ->
                StepDone $
                  state
                    { statePathsFound = [constructPath (ppVisited min_path)]
                    }
            else
              let successors :: [PartialPath action node cost]
                  successors =
                    getSuccessorPaths successor primary_cost heuristic min_path

                  fold_init :: (Map node cost, [PartialPath action node cost])
                  fold_init = (stateVisitedNodes state, [])

                  fold_fn ::
                    PartialPath action node cost ->
                    (Map node cost, [PartialPath action node cost]) ->
                    (Map node cost, [PartialPath action node cost])
                  fold_fn pp (visited, accum) =
                    let n = ppHead pp
                        apc = ppAccumulatedPrimaryCost pp
                     in case Map.lookup n visited of
                          Nothing ->
                            (Map.insert n apc visited, pp : accum)
                          Just visited_cost ->
                            let cmpop =
                                  case mode of
                                    ModeSingle -> (<=)
                                    ModeAll -> (<)
                             in if visited_cost `cmpop` apc
                                  then (visited, accum)
                                  else (Map.insert n apc visited, pp : accum)

                  (visited', esuccessors) = foldr fold_fn fold_init successors

                  pqueue' =
                    foldr (\x -> PQ.insert (ppTotalCost x) x) pqueue esuccessors
               in StepContinue $
                    state
                      { stateVisitedNodes = visited',
                        statePriorityQueue = pqueue'
                      }

-- | Get all the successor paths for a given path.
getSuccessorPaths ::
  forall action node cost.
  (Num cost) =>
  SuccessorFn action node ->
  PrimaryCostFn action node cost ->
  HeuristicFn node cost ->
  PartialPath action node cost ->
  [PartialPath action node cost]
getSuccessorPaths successor primary_cost heuristic pp =
  mkPP <$> (successor . ppHead $ pp)
  where
    mkPP :: (action, node) -> PartialPath action node cost
    mkPP (a, n) =
      let g, f :: cost
          g = primary_cost (a, n) + ppAccumulatedPrimaryCost pp
          f = g + heuristic n
       in PartialPath ((Just a, n) : ppVisited pp) g f

-- | Return the head (last visited) node from a `PartialPath`.
ppHead :: PartialPath action node cost -> node
ppHead = snd . head . ppVisited

-- | Given a final path to a node, construct it as a `Result`.
constructPath :: [(Maybe action, node)] -> Path action node
constructPath xs =
  case reverse xs of
    (Nothing, start_node) : ys -> Path start_node (fmap f ys)
      where
        f :: (Maybe action, node) -> (action, node)
        f (Just a, n) = (a, n)
        f _ = error "constructPath: non-start node was missing action"
    _ -> error "constructPath: start node had an associated action"
