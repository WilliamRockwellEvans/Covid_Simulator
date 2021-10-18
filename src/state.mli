(** Infection network stepping

    This module implements stepping from one infection state to the next
    for the network *)

type t
(** The abstract type representing infection networks*)

val update_state : t -> t
(** [update_state state] is the infection network generated from [state]
    after one time. Requires: [state] is a valid infection network*)
