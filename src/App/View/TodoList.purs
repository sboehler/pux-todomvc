module App.View.TodoList where

import Prelude hiding (div)
import App.Routes (Route(..))
import App.State (State(..), Todo(..), getVisibleTodos, getActiveTodos, getEditedVersion)
import App.Events (Event(..))
import Data.Array (length)
import Data.Maybe (Maybe(..))
import Data.Foldable (for_)
import Data.Monoid (mempty)
import Pux.DOM.Events (onClick, onChange, onDoubleClick, onKeyUp)
import Pux.DOM.HTML (HTML)
import Pux.DOM.HTML.Attributes (focused, key)
import Text.Smolder.HTML (a, button, div, footer, h1, header, input, label, li, p, section, span, strong, ul)
import Text.Smolder.HTML.Attributes (checked, className, for, href, placeholder, type', value)
import Text.Smolder.Markup ((!), (!?), (#!), text)

renderEditedTodo :: Todo -> HTML Event
renderEditedTodo = \(Todo todo) ->
  li
    ! className "editing"
    ! key (show todo.id) $ do
      input
        #! onKeyUp TodoInput
        ! type' "text"
        ! className "edit"
        ! focused
        ! value todo.text

renderTodo :: Todo -> HTML Event
renderTodo = \(Todo todo) ->
  (li
    !? todo.completed) (className "completed")
    ! key (show todo.id) $ do
      div ! className "view" $ do
        (input
          !? todo.completed) (checked "checked")
          #! onChange (ToggleCompleted todo.id)
          ! className "toggle"
          ! type' "checkbox"
        label #! onDoubleClick (ToggleEditing todo.id) $ text todo.text
        button
          #! onClick (RemoveTodo todo.id)
          ! className "destroy"
          $ mempty

view :: State -> HTML Event
view (State st) =
  let
    renderFilter = let
        renderFilterLink r l t = li $
          (a
             !? (st.route == r)) (className "selected")
             ! href l
             #! onClick (Navigate l) $
             text t
      in footer
        ! className "footer" $ do
          span ! className "todo-count" $ do
            let len = length $ getActiveTodos (State st)
            strong $ text (show len)
            span $ text $ if len == 1 then " item left" else " items left"
          ul ! className "filters" $ do
            renderFilterLink All "/" "All"
            renderFilterLink Active "/active" "Active"
            renderFilterLink Completed "/completed" "Completed"
          button
            #! onClick (const RemoveCompleted)
            ! className "clear-completed"
            $ text "Clear completed"

    renderHeader = header ! className "header" $ do
      h1 $ text "Todos"
      input
        #! onKeyUp NewTodoInput
        ! className "new-todo"
        ! placeholder "What needs to be done?"
        ! value st.newTodo

    renderMain = section ! className "main" $ do
      input
        #! onClick (const ToggleAllCompleted)
        ! className "toggle-all"
        ! type' "checkbox"
      label
        ! for "toggle-all"
        $ text "Mark all as complete"
      ul
        ! className "todo-list" $ do
          for_ (getVisibleTodos (State st)) $ \(Todo todo) ->
            case getEditedVersion (State st) (Todo todo) of
              Nothing       -> renderTodo (Todo todo)
              Just (Todo e) -> renderEditedTodo (Todo e)

    renderFooter = footer ! className "info" $ do
      p $ text "Double-click to edit a todo"
      p $ do
        span $ text "Template by "
        a ! href "http://sindresorhus.com" $ text "Sindre Sorhus"
      p $ text "Created by Alex Mingoia"
      p $ do
        span $ text "Part of "
        a ! href "http://todomvc.com" $ text "TodoMVC"

  in
    div do
      section ! className "todoapp" $ do
        renderHeader
        renderMain
        if (length st.todos) > 0
           then renderFilter
           else mempty
      renderFooter
