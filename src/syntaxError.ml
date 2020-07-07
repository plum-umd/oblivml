open Core

exception SyntaxError of Position.t * String.t
