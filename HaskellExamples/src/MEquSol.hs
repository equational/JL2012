-----------------------------------------------------------------------------
--
-- Module      :  MEquSol
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- | A generalized equation solver family of classtypes
-- | Was initialy a DPLL solver which I rewrote to abtract the variable type.
-- | This module only contains the class types needed to define the solver.
-- Other class types typically needed are defined in the MEquUtl module.
-----------------------------------------------------------------------------

module MEquSol (
    Heap(..),
    SettableVar(..),
    EqSystem(..),
    Equation(..),
    Projectable(..),
    Solver(..)
) where

import Data.List (delete, union, map, find, elem, insert,foldr)
import Data.Maybe (isJust)
import PrivateUtl
import Control.Monad.State


-- | Has variables that can be set
class SettableVar x vd | x -> vd where
    -- | bind variable (to a value or another variable)
    setVar::vd->x->x

-- | Is a heap to which values can be added and removed.
-- | No sorting is implied by default.
class Heap h x | h -> x where
    -- | add element to heap
    insert::x->h->h
    -- | get top of heap and left over heap or nothing when heap is empty
    deCons::h->(Maybe(x,h))
    -- | Like insert but with many values
    insertMany::[x]->h->h

-- | Properties of equations
class Equation eq where
    -- | They can be tested for Bool
    -- | 'True' if equation has no contradictions
    -- within the solution's variable assignment
    valid:: eq -> Bool

-- | Properties of systems of equations
class SettableVar eq vd => EqSystem eq vd | eq -> vd where
    -- | Each equation can be used independently to find variable bindings.
    -- | Possible bindings of one variable of the equation to possible values
    solveOne:: eq->(Maybe[vd])

-- | Projecting an equation on a partial solution may produce more partial solutions
class SettableVar eq vd => Projectable sol eq vd | sol -> eq where
    -- | Pivot solution around equation.
    -- Equation will be removed and a variable will be set to one or more values;
    -- producing one or more refined equations.
    reduce::vd->sol->sol


-- | Define a measurable metric on solution to direct the search heuristic
class Optimizable sol maxType | sol -> maxType where
    -- | Heuristic value to help direct search
    merit::sol->maxType

type InferenceSet sol = [(Bool,sol)]

-- | The main "solver" produces a list of solution attempts.
-- | Head solution is tagged 'True' if solution is found
-- | Unsuccessful search history are tagged with 'False'.
-- | Note that history is provided is provided for analysis but could be taken out.
class  (
    Heap space sol,
    Heap sol eq,
    Equation eq,
    EqSystem eq vd,
    Projectable sol eq vd
    ) =>
    Solver space sol eq vd | space sol -> eq where
    solve::
        space               -- ^ Space to solve
        -> InferenceSet sol     -- list of failures
        -> InferenceSet sol     -- Possible solution and list of failures
    solve s failed =
        case deCons s of
        Just (sys, tailSpace) ->
            --  work on top seach space
            case deCons sys of
            Just (eq, tailSys) ->
                -- and top equation
                -- which must be valid
                if valid eq then
                    -- with True equation
                    case solveOne eq of
                    Nothing ->
                        -- The equation has no free variables and so is dropped.
                        -- Continue with rest of system.
                        solve (MEquSol.insert tailSys tailSpace) ((False,sys):failed)
                    Just vdl ->
                        -- Reduce the system with each bound variable
                        let reducedSystems = Data.List.map (\vd -> reduce vd sys) vdl
                        -- push all reduced system back into heap
                        in  solve (MEquSol.insertMany reducedSystems tailSpace) failed
                else
                    -- Continue with rest of search space
                    -- add current system to failures
                    solve (tailSpace) ((False,sys):failed)
            Nothing ->
                -- Put success at head of return list
                -- Try to produce more solutions in tail of return
                (True,sys):(solve tailSpace failed)
        Nothing ->
            -- Add now include all the failed searches
            failed

class  (
    Heap space sol,
    Heap sol eq,
    Equation eq,
    EqSystem eq vd,
    Projectable sol eq vd
    ) =>
    Solver2 space sol eq vd | space sol -> eq where
    solve2::
        space                       -- ^ Space to solve
        -> StateT space IO (Maybe sol)
    solve2 s = do
      state <- get
      put state
      solve2 s

