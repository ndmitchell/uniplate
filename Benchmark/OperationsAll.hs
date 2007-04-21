
module OperationsAll(tasksExpr, tasksStm, tasksPar) where

import qualified Operations
import qualified OperationsTypeable
import qualified OperationsData
import OperationsCommon

import Data.List


tasksExpr = Operations.tasksExpr ++
            renPlay "play typ" OperationsTypeable.tasksExpr ++
            renPlay "play data" OperationsData.tasksExpr

tasksStm  = Operations.tasksStm ++
            renPlay "play typ" OperationsTypeable.tasksStm ++
            renPlay "play data" OperationsData.tasksStm

tasksPar  = Operations.tasksPar ++
            renPlay "play typ" OperationsTypeable.tasksPar ++
            renPlay "play data" OperationsData.tasksPar



renPlay name xs = [(a,b2,c) | (a,b,c) <- xs
                  ,let b2 = if "play" `isPrefixOf` b then name ++ drop 4 b else b]
