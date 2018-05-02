val join_inner_by :
  ('a -> 'b -> int) ->
  ('c -> 'a) -> ('d -> 'b) -> 'c Enum.t -> 'd Enum.t -> ('c * 'd) Enum.t
val join_inner_by_key :
  ('a -> 'a -> int) ->
  ('b -> 'a) -> 'b Enum.t -> 'b Enum.t -> ('b * 'b) Enum.t
val join_left_by :
  ('a -> 'b -> int) ->
  ('c -> 'a) ->
  ('d -> 'b) -> 'c Enum.t -> 'd Enum.t -> ('c * 'd option) Enum.t
val join_left_by_key :
  ('a -> 'a -> int) ->
  ('b -> 'a) -> 'b Enum.t -> 'b Enum.t -> ('b * 'b option) Enum.t
val join_right_by :
  ('a -> 'b -> int) ->
  ('c -> 'a) ->
  ('d -> 'b) -> 'c Enum.t -> 'd Enum.t -> ('c option * 'd) Enum.t
val join_right_by_key :
  ('a -> 'a -> int) ->
  ('b -> 'a) -> 'b Enum.t -> 'b Enum.t -> ('b option * 'b) Enum.t
val join_full_by :
  ('a -> 'b -> int) ->
  ('c -> 'a) ->
  ('d -> 'b) ->
  'c Enum.t ->
  'd Enum.t -> [> `Both of 'c * 'd | `Left of 'c | `Right of 'd ] Enum.t
val join_full_by_key :
  ('a -> 'a -> int) ->
  ('b -> 'a) ->
  'b Enum.t ->
  'b Enum.t -> [> `Both of 'b * 'b | `Left of 'b | `Right of 'b ] Enum.t
val join_inner_uniq_by :
  ('a -> 'b -> int) ->
  ('c -> 'a) -> ('d -> 'b) -> 'c Enum.t -> 'd Enum.t -> ('c * 'd) Enum.t
val join_inner_uniq_by_key :
  ('a -> 'a -> int) ->
  ('b -> 'a) -> 'b Enum.t -> 'b Enum.t -> ('b * 'b) Enum.t
val join_left_uniq_by :
  ('a -> 'b -> int) ->
  ('c -> 'a) ->
  ('d -> 'b) -> 'c Enum.t -> 'd Enum.t -> ('c * 'd option) Enum.t
val join_left_uniq_by_key :
  ('a -> 'a -> int) ->
  ('b -> 'a) -> 'b Enum.t -> 'b Enum.t -> ('b * 'b option) Enum.t
val join_right_uniq_by :
  ('a -> 'b -> int) ->
  ('c -> 'a) ->
  ('d -> 'b) -> 'c Enum.t -> 'd Enum.t -> ('c option * 'd) Enum.t
val join_right_uniq_by_key :
  ('a -> 'a -> int) ->
  ('b -> 'a) -> 'b Enum.t -> 'b Enum.t -> ('b option * 'b) Enum.t
val join_full_uniq_by :
  ('a -> 'b -> int) ->
  ('c -> 'a) ->
  ('d -> 'b) ->
  'c Enum.t ->
  'd Enum.t -> [> `Both of 'c * 'd | `Left of 'c | `Right of 'd ] Enum.t
val join_full_uniq_by_key :
  ('a -> 'a -> int) ->
  ('b -> 'a) ->
  'b Enum.t ->
  'b Enum.t -> [> `Both of 'b * 'b | `Left of 'b | `Right of 'b ] Enum.t
val join_assoc_inner :
  ('a -> 'b -> int) ->
  ('a * 'c) Enum.t -> ('b * 'd) Enum.t -> ('a * 'c * 'd) Enum.t
val join_assoc_left :
  ('a -> 'b -> int) ->
  ('a * 'c) Enum.t -> ('b * 'd) Enum.t -> ('a * 'c * 'd option) Enum.t
val join_assoc_right :
  ('a -> 'a -> int) ->
  ('a * 'b) Enum.t -> ('a * 'c) Enum.t -> ('a * 'b option * 'c) Enum.t
val join_assoc_full :
  ('a -> 'a -> int) ->
  ('a * 'b) Enum.t ->
  ('a * 'c) Enum.t ->
  ('a * [> `Both of 'b * 'c | `Left of 'b | `Right of 'c ]) Enum.t
val join_assoc_inner_uniq :
  ('a -> 'b -> int) ->
  ('a * 'c) Enum.t -> ('b * 'd) Enum.t -> ('a * 'c * 'd) Enum.t
val join_assoc_left_uniq :
  ('a -> 'b -> int) ->
  ('a * 'c) Enum.t -> ('b * 'd) Enum.t -> ('a * 'c * 'd option) Enum.t
val join_assoc_right_uniq :
  ('a -> 'a -> int) ->
  ('a * 'b) Enum.t -> ('a * 'c) Enum.t -> ('a * 'b option * 'c) Enum.t
val join_assoc_full_uniq :
  ('a -> 'a -> int) ->
  ('a * 'b) Enum.t ->
  ('a * 'c) Enum.t ->
  ('a * [> `Both of 'b * 'c | `Left of 'b | `Right of 'c ]) Enum.t
val merge :
  ('a -> 'b -> int) ->
  'a Enum.t -> 'b Enum.t -> ('a option * 'b option) Enum.t
val merge_assoc :
  ('a -> 'a -> int) ->
  ('a * 'b) Enum.t -> ('a * 'c) Enum.t -> ('a * 'b option * 'c option) Enum.t
