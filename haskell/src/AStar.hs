{-# LANGUAGE ScopedTypeVariables #-}

module AStar (astar, Result (NoPathFound, PathFound)) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.PQueue.Prio.Min (MinPQueue)
import qualified Data.PQueue.Prio.Min as PQ

type ReachedEndFn node = node -> Bool

type SuccessorFn action node = node -> [(action, node)]

type PrimaryCostFn action node cost = (action, node) -> cost

type HeuristicFn node cost = node -> cost

data Result action node
  = NoPathFound
  | PathFound node [(action, node)]
  deriving (Eq, Show)

astar ::
  forall action node cost.
  (Ord node, Num cost, Ord cost, Show action, Show node, Show cost) =>
  node ->
  ReachedEndFn node ->
  SuccessorFn action node ->
  PrimaryCostFn action node cost ->
  HeuristicFn node cost ->
  Result action node
astar start_node end_fn successor primary_cost heuristic =
  stepUntilDone $
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
    stateHeuristic :: HeuristicFn node cost
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

-- | Result of taking a step.
data StepResult action node cost
  = -- | Evaluation should continue using the state contained.
    StepContinue (State action node cost)
  | -- | Evaluation is complete, and we have a result.
    StepDone (Result action node)

-- | Repeatedly call the `step` function until we have found a result.
stepUntilDone ::
  forall node action cost.
  (Ord node, Num cost, Ord cost, Show action, Show node, Show cost) =>
  State action node cost ->
  Result action node
stepUntilDone state =
  case step state of
    StepContinue state' -> stepUntilDone state'
    StepDone result -> result

step ::
  forall node action cost.
  (Ord node, Num cost, Ord cost, Show action, Show node, Show cost) =>
  State action node cost ->
  StepResult action node cost
step state =
  case PQ.minView (statePriorityQueue state) of
    Nothing -> StepDone NoPathFound
    Just (min_path, pqueue) ->
      let min_node = ppHead min_path
          end_fn = stateReachedEndFn state
          successor = stateSuccessor state
          primary_cost = statePrimaryCost state
          heuristic = stateHeuristic state
       in if end_fn min_node
            then StepDone (constructPath . ppVisited $ min_path)
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
                            if visited_cost <= apc
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
constructPath :: [(Maybe action, node)] -> Result action node
constructPath xs =
  case reverse xs of
    (Nothing, start_node) : ys -> PathFound start_node (fmap f ys)
      where
        f :: (Maybe action, node) -> (action, node)
        f (Just a, n) = (a, n)
        f _ = error "constructPath: non-start node was missing action"
    _ -> error "constructPath: start node had associated action"
