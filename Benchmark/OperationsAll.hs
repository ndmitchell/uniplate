
module OperationsAll(tasksExpr, tasksStm, tasksPar) where

import qualified Operations
import qualified OperationsTypeable
import qualified OperationsData
import qualified OperationsManual
import OperationsCommon

import Data.List


tasksExpr = Operations.tasksExpr ++
            renPlay "play typ" OperationsTypeable.tasksExpr ++
            renPlay "play data" OperationsData.tasksExpr ++
            renPlay "play man" OperationsManual.tasksExpr

tasksStm  = Operations.tasksStm ++
            renPlay "play typ" OperationsTypeable.tasksStm ++
            renPlay "play data" OperationsData.tasksStm ++
            renPlay "play man" OperationsManual.tasksStm

tasksPar  = Operations.tasksPar ++
            renPlay "play typ" OperationsTypeable.tasksPar ++
            renPlay "play data" OperationsData.tasksPar ++
            renPlay "play man" OperationsManual.tasksPar



renPlay name xs = [(a,b2,c) | (a,b,c) <- xs
                  ,let b2 = if "play" `isPrefixOf` b then name ++ drop 4 b else b]
