(** Infection network stepping

    This module implements stepping from one infection state to the next
    for the network *)

type t = Network.t
(** The abstract type representing infection networks*)

val update_state : Network.t -> (Network.person_id * Network.infected) list
(** [update_state state] is the infection network generated from [state]
    after one time. Requires: [state] is a valid infection network*)
