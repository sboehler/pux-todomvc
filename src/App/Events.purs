module App.Events where

import Prelude

import App.Effects (AppEffects)
import App.Routes (Route, match)
import App.State (State(..), Todo(..))
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Except (runExcept)
import DOM.Event.Event (preventDefault)
import DOM.Event.KeyboardEvent (eventToKeyboardEvent, key)
import DOM.HTML (window)
import DOM.HTML.History (DocumentTitle(..), URL(..), pushState)
import DOM.HTML.Window (history)
import Data.Array (filter, last, snoc)
import Data.Either (either)
import Data.Foreign (toForeign)
import Data.Maybe (Maybe(..), maybe)
import Pux (EffModel)
import Pux.DOM.Events (DOMEvent, targetValue)

type TodoId = Int

data Event
  = PageView Route
  | Navigate String DOMEvent
  | NewTodoInput DOMEvent
  | TodoInput DOMEvent
  | EditTodo Todo DOMEvent
  | SaveTodo Todo 
  | ToggleCompleted TodoId DOMEvent
  | ToggleAllCompleted
  | RemoveCompleted
  | RemoveTodo TodoId DOMEvent

_loaded:: Event -> Boolean -> Boolean
_loaded (PageView route) _ = true
_loaded _ s = s

_route:: Event -> Route -> Route
_route (PageView route) r = route
_route _ r = r

_newTodo :: Event -> String -> String
_newTodo (NewTodoInput ev) todo = 
  if (eventToKeyPressed ev) == "Enter"
    then todo
    else targetValue ev 
_newTodo _ t = t

_todos :: Event -> Array Todo -> Array Todo
_todos (NewTodoInput ev) todos = if (eventToKeyPressed ev) == "Enter" 
  then snoc todos $ Todo
        { id: maybe 1 (\(Todo todo) -> todo.id + 1) $ last todos
        , text: targetValue ev
        , completed: false
        }
  else todos
_todos (RemoveTodo id ev) todos = flip filter todos \(Todo t) -> t.id /= id 
_todos RemoveCompleted todos = flip filter todos \(Todo t) -> not t.completed 
_todos ToggleAllCompleted todos = flip map todos \(Todo todo) -> Todo todo { completed = not todo.completed }
_todos (SaveTodo (Todo todo')) todos = flip map todos \(Todo todo) -> if todo'.id == todo.id then (Todo todo') else (Todo todo)
_todos (ToggleCompleted id ev) todos = flip map todos \(Todo t) -> if t.id == id then (Todo t { completed = not t.completed }) else (Todo t)
_todos _ todos = todos

_editedTodo :: Event -> Maybe Todo -> Maybe Todo
_editedTodo (TodoInput ev) (Just (Todo editedTodo)) = case eventToKeyPressed ev of
  "Escape" -> Nothing
  "Enter" -> Nothing
  _ -> Just $ Todo editedTodo { text = targetValue ev } 
_editedTodo (EditTodo todo ev) _ = Just todo
_editedTodo _ e = e

_effects :: ∀ fx. Event -> State -> State -> Array (Aff (AppEffects fx) (Maybe Event))
_effects (Navigate url ev) _ _ = [ do
    liftEff do
      preventDefault ev
      h <- history =<< window
      pushState (toForeign {}) (DocumentTitle "") (URL url) h
    pure $ Just $ PageView (match url)
  ]
_effects (TodoInput ev) (State st) _ = [
    case eventToKeyPressed ev of
      "Enter" -> pure $ SaveTodo <$> (st.editedTodo) 
      _ -> pure Nothing
  ]
_effects _ _ _ = []


foldp :: ∀ fx. Event -> State -> EffModel State Event (AppEffects fx)
foldp ev (State st) = 
  let 
    st' = st { 
             route = _route ev st.route, 
             loaded = _loaded ev st.loaded, 
             newTodo = _newTodo ev st.newTodo, 
             todos = _todos ev st.todos,
             editedTodo = _editedTodo ev st.editedTodo
             }
    effects' = _effects ev (State st) (State st') 
  in { state: (State st'), effects: effects' }

eventToKeyPressed :: DOMEvent -> String
eventToKeyPressed ev = either (const "") key $ runExcept $ eventToKeyboardEvent ev
